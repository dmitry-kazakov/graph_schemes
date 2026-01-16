--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Ranges.Integers                          Summer, 2005       --
--  Implementation                                                    --
--                                Last revision :  08:28 21 Jul 2016  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Confidence_Factors;             use Confidence_Factors;
with Fuzzy.Feature.Domain_Floats;    use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Domain_Integers;  use Fuzzy.Feature.Domain_Integers;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Subsets.Integers;
with Parsers.FCL.Code.Subsets.Ranges.Reals;

package body Parsers.FCL.Code.Subsets.Ranges.Integers is
   use Integer_Edit;
   use Variables;

   function Get_Range (Tree : Node'Class) return Integer_Range'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      declare
         Result : constant Lattice'Class := Get_Lattice (Context, Tree);
      begin
         if Result in Integer_Range'Class then
            return Integer_Range'Class (Result);
         end if;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Integer range is expected at "
         &  Image (Get_Location (Tree))
      )  );
   end Get_Range;

   function Get_Preference (Left : Integer_Range) return Preference is
   begin
      return Integer_Range_Preference;
   end Get_Preference;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Range;
               Right    : Measure
            )  return Constant_Value'Class is
   begin
      return
         Reals.Set_Dimension
         (  Location,
            Context,
            Reals.To_Range (Left),
            Right
         );
   end Set_Dimension;

   function Equal (Left, Right : Interval) return Fuzzy_Boolean is
   begin
      if Left = Right then
         return Certain_True;
      elsif Left & Right then
         return Uncertain;
      else
         return Certain_False;
      end if;
   end Equal;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Range;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      if Right in Integer_Range'Class then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Right.Location & Location,
               Equal (Left.Value, Integer_Range'Class (Right).Value)
            );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Integer range is expected at "
         &  Image (Right.Location)
      )  );
   end EQ;

   function Image
            (  Feature : Feature_Handle;
               Item    : Integer_Range;
               Mode    : Code_Set
            )  return String is
   begin
      return
      (  Image (Item.Value.From)
      &  ".."
      &  Image (Item.Value.To)
      );
   end Image;

   function To_Set (Left : Integer_Range) return Subset'Class is
      From : constant Domain_Float := Domain_Float (Left.Value.From);
      To   : constant Domain_Float := Domain_Float (Left.Value.To);
   begin
      return
         Subsets.Integers.Set'
         (  Left.Location,
            (  Pair'(Domain_Float'Pred (From), Confidence'First)
            &  Pair'(From,                     Confidence'Last )
            &  Pair'(To,                       Confidence'Last )
            &  Pair'(Domain_Float'Succ (To),   Confidence'First)
         )  );
   end To_Set;

   function To_Set
            (  Context : Resolution_Context;
               Left    : Integer_Range
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Context, To_Set (Left));
   end To_Set;

end Parsers.FCL.Code.Subsets.Ranges.Integers;
