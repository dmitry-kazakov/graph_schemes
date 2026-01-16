--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Manual                   Luebeck            --
--  Implementation                                 Autumn, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Ada.Exceptions;  use Ada.Exceptions;

package body Fuzzy.Graph.Handle.Manual is

   function Create_Branch
            (  Feature  : Feature_Handle;
               Children : Node_Array
            )  return Node_Handle is
      Factory : Node_Factory_Ptr;
      Arcs    : Node_Ptr_Array (1..Children'Length);
   begin
      for Index in Arcs'Range loop
         Arcs (Index) := Ptr (Children (Index + Children'First - 1));
         if Arcs (Index) /= null then
            if Factory = null then
               Factory := Get_Factory (Arcs (Index).all);
            end if;
         end if;
      end loop;
      if Factory = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Creating branch with no successors"
         );
      end if;
      return
         Ref (Create (Factory, Ptr (Feature).all, Arcs));
   end Create_Branch;

   function Create_Branch
            (  Feature  : Feature_Handle;
               Children : Bounded_Array
            )  return Node_Handle is
      Factory : Node_Factory_Ptr;
      Arcs    : Node_Ptr_Array (1..Children.Last - Children.First + 1);
   begin
      for Index in Arcs'Range loop
         Arcs (Index) := Get (Children, Index + Children.First - 1);
         if Arcs (Index) /= null then
            if Factory = null then
               Factory := Get_Factory (Arcs (Index).all);
            end if;
         end if;
      end loop;
      if Factory = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Creating branch with no successors"
         );
      end if;
      return
         Ref (Create (Factory, Ptr (Feature).all, Arcs));
   end Create_Branch;

   function Create_Leaf
            (  Feature      : Feature_Handle;
               Distribution : Classification;
               Factory      : not null access Node_Factory'Class :=
                                 Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle is
   begin
      return Ref (Create (Factory, Ptr (Feature).all, Distribution));
   end Create_Leaf;

   function Create_Leaf
            (  Feature      : Feature_Handle;
               Distribution : Classification;
               Node         : Node_Handle
            )  return Node_Handle is
   begin
      return
         Ref
         (  Create
            (  Get_Factory (Ptr (Node).all),
               Ptr (Feature).all,
               Distribution
         )  );
   end Create_Leaf;

end Fuzzy.Graph.Handle.Manual;
