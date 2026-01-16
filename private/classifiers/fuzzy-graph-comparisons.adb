--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Comparisons                     Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with System;  use System;

package body Fuzzy.Graph.Comparisons is

   function Equal (Left : Graph_Node_Ref; Right : Graph_Node_Ref)
      return Boolean is
   begin
      return Left = Right;
   end Equal;

   function Equal (Left : Graph_Node_Ptr; Right : Graph_Node_Ptr)
      return Boolean is
   begin
      return Left = Right;
   end Equal;

   function Less (Left : Graph_Node_Ref; Right : Graph_Node_Ref)
      return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = null then
         return True;
      elsif Right = null then
         return False;
      else
         return Left.all'Address < Right.all'Address;
      end if;
   end Less;

   function Less (Left : Graph_Node_Ptr; Right : Graph_Node_Ptr)
      return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = null then
         return True;
      elsif Right = null then
         return False;
      else
         return Left.all'Address < Right.all'Address;
      end if;
   end Less;

   function Like (Left : Graph_Node_Ref; Right : Graph_Node_Ref)
      return Boolean is
   begin
      if Left = Right then
         return True;
      elsif Left = null or Right = null then
         return False;
      else
         return Like (Left.all, Right.all);
      end if;
   end Like;

   function Like (Left : Graph_Node_Ptr; Right : Graph_Node_Ptr)
      return Boolean is
   begin
      if Left = Right then
         return True;
      elsif Left = null or Right = null then
         return False;
      else
         return Like (Left.all, Right.all);
      end if;
   end Like;

   function Precedent (Left : Graph_Node_Ref; Right : Graph_Node_Ref)
      return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = null then
         return True;
      elsif Right = null then
         return False;
      else
         return Precedent (Left.all, Right.all);
      end if;
   end Precedent;

   function Precedent (Left : Graph_Node_Ptr; Right : Graph_Node_Ptr)
      return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = null then
         return True;
      elsif Right = null then
         return False;
      else
         return Precedent (Left.all, Right.all);
      end if;
   end Precedent;

end Fuzzy.Graph.Comparisons;
