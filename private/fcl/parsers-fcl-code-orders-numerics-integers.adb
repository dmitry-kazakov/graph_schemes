--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numeric.Integers                         Winter, 2005       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Confidence_Factors;        use Confidence_Factors;
with Fuzzy;                     use Fuzzy;
with Parsers.FCL.Code.Subsets;  use Parsers.FCL.Code.Subsets;

with Fuzzy.Feature.Domain_Float_Handle;
with Fuzzy.Feature.Domain_Integer_Handle;
with Parsers.FCL.Code.Logic.Plain;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Parsers.FCL.Code.Subsets.Ranges.Integers;

package body Parsers.FCL.Code.Orders.Numerics.Integers is
   use Fuzzy.Feature.Domain_Float_Handle;
   use Fuzzy.Feature.Domain_Integer_Handle;
   use Fuzzy.Feature.Domain_Integers.Integer_Edit;
   use Fuzzy.Feature.Domain_Integers.Integer_Intervals;

   function Get_Integer (Tree : Node'Class) return Int'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      declare
         Result : Numeric'Class renames Get_Numeric (Context, Tree);
      begin
         if Result in Int'Class then
            return Int'Class (Result);
         end if;
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer value is expected at "
            &  Image (Result.Location)
         )  );
      end;
   end Get_Integer;

   generic
      Name : String;
      with function Op (Left, Right : Domain_Integer)
              return Domain_Integer;
   function Int_Operation
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;

   function To_Integer (Value : Constant_Value'Class)
      return Domain_Integer is
   begin
      if Value in Int'Class then
         return Int'Class (Value).Value;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Integer value is expected at "
         &  Image (Value.Location)
      )  );
   end To_Integer;

   function Int_Operation
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class is
      Result : Domain_Integer;
   begin
      begin
         Result := Op (Left.Value, To_Integer (Right));
      exception
         when Constraint_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Numeric error with the operands at "
               &  Image (Left.Location)
               &  " and at "
               &  Image (Right.Location)
               &  " in "
               &  Name
               &  " at "
               &  Image (Location)
            )  );
      end;
      return Int'(Left.Location & Right.Location & Location, Result);
   end Int_Operation;

   function Abs_Value
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class is
   begin
      return Int'(Left.Location & Location, abs Left.Value);
   end Abs_Value;

   function Int_Add is new Int_Operation ("'+'", "+");
   function Add
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Add;

   function Int_Div is new Int_Operation ("'/'", "/");
   function Div
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Div;

   function Div_Left (Left, Right : Domain_Integer)
      return Domain_Integer is
   begin
      return Right / Left;
   end Div_Left;
   function Int_Div_Inv is new Int_Operation ("'/'", Div_Left);
   function Div_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Div_Inv;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value = To_Integer (Right))
         );
   end EQ;

   function Image
            (  Feature : Feature_Handle;
               Item    : Int;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Item.Value);
   end Image;

   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value >= To_Integer (Right))
         );
   end GE;

   function Get_Preference (Left : Int) return Preference is
   begin
      return Integer_Preference;
   end Get_Preference;

   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value > To_Integer (Right))
         );
   end GT;

   function Int_Mul is new Int_Operation ("'*'", "*");
   function Mul
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Mul;

   function Minus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class is
   begin
      return Int'(Left.Location & Location, -Left.Value);
   end Minus;

   function Plus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class is
   begin
      return Int'(Left.Location & Location, Left.Value);
   end Plus;

   function Pow_Right (Left, Right : Domain_Integer)
      return Domain_Integer is
   begin
      return Left ** Integer (Right);
   end Pow_Right;

   function Pow_Left (Left, Right : Domain_Integer)
      return Domain_Integer is
   begin
      return Pow_Right (Right, Left);
   end Pow_Left;

   function Int_Pow is new Int_Operation ("'**'", Pow_Right);
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Pow;

   function Int_Pow_Inv is new Int_Operation ("'**'", Pow_Left);
   function Pow_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Pow_Inv;

   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Domain_Integer
            )  return Numeric'Class is
   begin
      return
         Int'
         (  Left.Location & Location,
            Left.Value ** Natural (Right)
         );
   exception
      when others =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Numeric error at "
            &  Image (Left.Location)
            &  " in postifx exponentiation at "
            &  Image (Location)
         )  );
   end Pow;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Measure;
   begin
      begin
         Result := Domain_Float (Left.Value) * Right;
         if Result.Gain'Valid then
            return Numerics.Reals.Real'(Left.Location & Location, Result);
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

   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class is
      Other : constant Domain_Integer := To_Integer (Right);
   begin
      if Inversed then
         if Left.Value < Other then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "The lower interval bound at "
               &  Image (Right.Location)
               &  " is greater than the upper one at "
               &  Image (Left.Location)
            )  );
         end if;
         return
            Ranges.Integers.Integer_Range'
            (  Location & Left.Location & Right.Location,
               Interval'(Other, Left.Value)
            );
      else
         if Left.Value > Other then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "The lower interval bound at "
               &  Image (Left.Location)
               &  " is greater than the upper one at "
               &  Image (Right.Location)
            )  );
         end if;
         return
            Subsets.Ranges.Integers.Integer_Range'
            (  Location & Left.Location & Right.Location,
               Interval'(Left.Value, Other)
            );
      end if;
   end Stretch;

   function Int_Sub is new Int_Operation ("'-'", "-");
   function Sub
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Sub;

   function Sub_Left (Left, Right : Domain_Integer)
      return Domain_Integer is
   begin
      return Right - Left;
   end Sub_Left;
   function Int_Sub_Inv is new Int_Operation ("'-'", Sub_Left);
   function Sub_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class renames Int_Sub_Inv;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed in the operation at "
            &  Image (Location)
         )  );
      elsif Is_Domain_Float (Context.Expected.Feature) then
         return
            Numerics.Reals.Equal_Logical_Term
            (  Location,
               Context,
               Numerics.Reals.Real'
               (  Location => Value.Location,
                  Value    => To_Measure (Domain_Float (Value.Value))
            )  );
      elsif Is_Domain_Integer (Context.Expected.Feature) then
         declare
            Set : Fuzzy.Intuitionistic.Classification renames
                     Classify
                     (  Context.Expected.Feature,
                        Value.Value
                     );
         begin
            return
            (  Has_In      => Create (Set.Possibility),
               Has_Not     => Create (not Set.Necessity),
               Has_Out     => Null_Handle,
               Has_Not_Out => Null_Handle
            );
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed for '"
            &  Get_Name (Context.Expected.Feature)
            &  "' in the operation at "
            &  Image (Location)
         )  );
      end if;
   end Equal_Logical_Term;

   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed in the operation at "
            &  Image (Location)
         )  );
      elsif Is_Domain_Float (Context.Expected.Feature) then
         return
            Numerics.Reals.Greater_Or_Equal_Logical_Term
            (  Location,
               Context,
               Numerics.Reals.Real'
               (  Location => Value.Location,
                  Value    => To_Measure (Domain_Float (Value.Value))
            )  );
      elsif Is_Domain_Integer (Context.Expected.Feature) then
         declare
            Set : Fuzzy.Intuitionistic.Classification renames
                     Classify
                     (  Context.Expected.Feature,
                        Integer_Intervals.Interval'
                        (  Value.Value,
                           Domain_Integer'Last
                     )  );
         begin
            return
            (  Has_In      => Create (Set.Possibility),
               Has_Not     => Create (not Set.Necessity),
               Has_Out     => Null_Handle,
               Has_Not_Out => Null_Handle
            );
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed for '"
            &  Get_Name (Context.Expected.Feature)
            &  "' in the operation at "
            &  Image (Location)
         )  );
      end if;
   end Greater_Or_Equal_Logical_Term;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed in the operation at "
            &  Image (Location)
         )  );
      elsif Is_Domain_Float (Context.Expected.Feature) then
         return
            Numerics.Reals.Greater_Logical_Term
            (  Location,
               Context,
               Numerics.Reals.Real'
               (  Location => Value.Location,
                  Value    => To_Measure (Domain_Float (Value.Value))
            )  );
      elsif Is_Domain_Integer (Context.Expected.Feature) then
         declare
            Set : Fuzzy.Intuitionistic.Classification renames
                     Classify
                     (  Context.Expected.Feature,
                        Integer_Intervals.Interval'
                        (  Value.Value + 1,
                           Domain_Integer'Last
                     )  );
         begin
            return
            (  Has_In      => Create (Set.Possibility),
               Has_Not     => Create (not Set.Necessity),
               Has_Out     => Null_Handle,
               Has_Not_Out => Null_Handle
            );
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer operand at "
            &  Image (Value.Location)
            &  " is not allowed for '"
            &  Get_Name (Context.Expected.Feature)
            &  "' in the operation at "
            &  Image (Location)
         )  );
      end if;
   end Greater_Logical_Term;

end Parsers.FCL.Code.Orders.Numerics.Integers;
