--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Ranges.Reals                             Summer, 2005       --
--  Implementation                                                    --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Confidence_Factors;           use Confidence_Factors;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Parsers.FCL.Code;             use Parsers.FCL.Code;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Subsets.Ranges.Integers;
with Parsers.FCL.Code.Subsets.Reals;
with Units.Base;

package body Parsers.FCL.Code.Subsets.Ranges.Reals is
   use Float_Edit;
   use Measure_Edit;
   use Variable_Measures;
   use Parsers.FCL.Code.Subsets.Reals;

   function Get_Dimension (Left : Real_Range'Class) return String is
   begin
      return
      (  "["
      &  Image
         (  Measure'(Left.Value.SI, 1.0, 0.0),
            Messaging_Parameters.Mode
         )
      &  "]"
      );
   end Get_Dimension;

   function Get_Preference (Left : Real_Range) return Preference is
   begin
      return Real_Range_Preference;
   end Get_Preference;

   function Get_Range (Tree : Node'Class) return Real_Range'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      return To_Range (Get_Lattice (Context, Tree));
   end Get_Range;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Range;
               Right    : Measure
            )  return Constant_Value'Class is
   begin
      declare
         Value : Interval_Measure;
      begin
         Value := Left.Value * Right;
         if Value.From'Valid and then Value.To'Valid then
            return Real_Range'(Left.Location & Location, Value);
         end if;
      exception
         when Unit_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unit error in scaling at "
               &  Image (Left.Location)
               &  " by the scale at "
               &  Image (Location)
            )  );
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error in scaling at "
         &  Image (Left.Location)
         &  " by the scale at "
         &  Image (Location)
      )  );
   end Set_Dimension;

   function To_Range (Left : Constant_Value'Class) return Real_Range is
   begin
      if Left in Real_Range'Class then
         return Real_Range (Left);
      elsif Left in Integers.Integer_Range'Class then
         declare
            Value : Integers.Integer_Range'Class renames
                       Integers.Integer_Range'Class (Left);
         begin
            return
               Real_Range'
               (  Left.Location,
                  To_Interval_Measure
                  (  Domain_Float (Value.Value.From),
                     Domain_Float (Value.Value.To)
               )  );
         end;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         "Real range is expected at " & Image (Left.Location)
      );
   end To_Range;

   function Equal (Left, Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if Left = Right then
         return Certain_True;
      elsif Left & Right then
         return Uncertain;
      else
         return Certain_False;
      end if;
   exception
      when Unit_Error =>
         return Certain_False;
   end Equal;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Range;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      Other : Real_Range renames To_Range (Right);
   begin
      if Left.Value.SI = Other.Value.SI then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Right.Location & Location,
               Equal (Left.Value, Other.Value)
            );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible dimensions "
            &  Get_Dimension (Left)
            &  " at "
            &  Image (Left.Location)
            &  " and "
            &  Get_Dimension (Other)
            &  " at "
            &  Image (Right.Location)
            &  " as found in comparison at "
            &  Image (Location)
         )  );
      end if;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Numeric error in operands at "
            &  Image (Left.Location)
            &  " and at "
            &  Image (Right.Location)
            &  " in comparison at "
            &  Image (Location)
         )  );
   end EQ;

   function Image
            (  Feature : Feature_Handle;
               Item    : Real_Range;
               Mode    : Code_Set
            )  return String is
   begin
      if Item.Value.SI = Units.Base.Unitless then
         return
         (  Image (Get_Value (Item.Value).From)
         &  ".."
         &  Image (Get_Value (Item.Value).To)
         );
      else
         return
         (  Image (Get_Value (Item.Value).From)
         &  ".."
         &  Image (Get_Value (Item.Value).To)
         &  " "
         &  Get_Dimension (Item)
         );
      end if;
   end Image;

   function To_Set (Left : Real_Range) return Subset'Class is
      Lower : constant Measure := From (Left.Value);
      Upper : constant Measure := To   (Left.Value);
   begin
      return
         Subsets.Reals.Set'
         (  Left.Location,
            (  Pair_Measure'
               (  (  Lower.SI,
                     Domain_Float'Pred (Lower.Gain),
                     Lower.Offset
                  ),
                  Confidence'First
               )
            &  Pair_Measure'(Lower, Confidence'Last )
            &  Pair_Measure'(Upper,   Confidence'Last )
            &  Pair_Measure'
               (  (  Upper.SI,
                     Domain_Float'Succ (Upper.Gain),
                     Upper.Offset
                  ),
                  Confidence'First
         )  )  );
   end To_Set;

   function To_Set
            (  Context : Resolution_Context;
               Left    : Real_Range
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Context, To_Set (Left));
   end To_Set;

end Parsers.FCL.Code.Subsets.Ranges.Reals;
