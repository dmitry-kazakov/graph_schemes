--                                                                    --
--  package Fuzzy.Graph             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with System;

package body Fuzzy.Graph is
   use type System.Address;

   protected Node_Sequence is
      procedure Next (No : out Sequence_No);
   private
      Count : Sequence_No := 0;
   end Node_Sequence;

   protected Number_Of_Nodes is
      procedure Decrement;
      function Get return Natural;
      procedure Increment;
   private
      Count : Natural := 0;
   end Number_Of_Nodes;

   protected body Node_Sequence is
      procedure Next (No : out Sequence_No) is
      begin
         Count := Count + 1;
         No    := Count;
      end Next;
   end Node_Sequence;

   protected body Number_Of_Nodes is
      function Get return Natural is
      begin
         return Count;
      end Get;

      procedure Decrement is
      begin
         Count := Count - 1;
      end Decrement;

      procedure Increment is
      begin
         Count := Count + 1;
      end Increment;
   end Number_Of_Nodes;

   function Get_Sequence_No return Sequence_No is
      Result : Sequence_No;
   begin
      Node_Sequence.Next (Result);
      return Result;
   end Get_Sequence_No;

   procedure Finalize (Node : in out Graph_Node) is
   begin
      Number_Of_Nodes.Decrement;
      Object.Archived.Finalize (Deposit (Node));
   end Finalize;

   function Get_Number_Of_Nodes return Natural is
   begin
      return Number_Of_Nodes.Get;
   end Get_Number_Of_Nodes;

   procedure Initialize (Node : in out Graph_Node) is
   begin
      Object.Archived.Initialize (Deposit (Node));
      Number_Of_Nodes.Increment;
   end Initialize;

   function Is_Modified (Node : Graph_Node) return Boolean is
   begin
      return Node.Sequence /= 0;
   end Is_Modified;

   procedure Release (Node : Graph_Node_Ptr) is
      Ptr : Object.Entity_Ptr := Object.Entity_Ptr (Node);
   begin
      Object.Release (Ptr);
   end Release;

   procedure Reset_Modified (Node : in out Graph_Node) is
   begin
      Node.Sequence := 0;
   end Reset_Modified;

   function From_Deposit_Ptr is
      new Ada.Unchecked_Conversion (Deposit_Ptr, Graph_Node_Ptr);

   function To_Graph_Node_Ptr (Ptr : Deposit_Ptr)
      return Graph_Node_Ptr is
   begin
      if Ptr.all in Graph_Node'Class then
         return From_Deposit_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Graph_Node_Ptr;

   function "<" (Left, Right : Classification) return Boolean is
   begin
      for Index in 1..Left.Cardinality loop
         if Left.Possibility (Index) /= Right.Possibility (Index) then
            return Left.Possibility (Index) < Right.Possibility (Index);
         end if;
         if Left.Necessity (Index) /= Right.Necessity (Index) then
            return Left.Necessity (Index) < Right.Necessity (Index);
         end if;
      end loop;
      return False;
   end "<";

end Fuzzy.Graph;
