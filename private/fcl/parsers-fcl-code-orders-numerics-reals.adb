--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numeric.Reals                            Winter, 2005       --
--  Implementation                                                    --
--                                Last revision :  11:45 29 May 2020  --
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
with Fuzzy;           use Fuzzy;
with Units.Base;      use Units.Base;

with Fuzzy.Feature.Domain_Float_Handle;
with Parsers.FCL.Code.Logic.Plain;
with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Subsets.Ranges.Reals;

package body Parsers.FCL.Code.Orders.Numerics.Reals is
   use Measure_Edit;
   use Interval_Measures;
   use Variable_Measures;
   use Fuzzy.Feature.Domain_Float_Handle;

   generic
      Name : String;
      with function Op (Left, Right : Measure) return Measure;
   function Real_Operation
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;

   function Real_Operation
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class is
      Result : Measure;
   begin
      begin
         Result := Op (Left.Value, To_Measure (Right));
         if Result.Gain'Valid then
            return Real'(Left.Location & Right.Location, Result);
         end if;
      exception
         when Unit_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unit error in the operands at "
               &  Image (Left.Location)
               &  " and at "
               &  Image (Right.Location)
               &  " of "
               &  Name
               &  " at "
               &  Image (Location)
            )  );
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error with the operands at "
         &  Image (Left.Location)
         &  " and at "
         &  Image (Right.Location)
         &  " of "
         &  Name
         &  " at "
         &  Image (Location)
      )  );
   end Real_Operation;

   function Abs_Value
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class is
   begin
      return Real'(Left.Location & Location, abs Left.Value);
   end Abs_Value;

   function Real_Add is new Real_Operation ("'+'", "+");
   function Add
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Add;

   function Real_Div is new Real_Operation ("'/'", "/");
   function Div
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Div;

   function Div_Left (Left, Right : Measure) return Measure is
   begin
      return Right / Left;
   end Div_Left;
   function Real_Div_Inv is new Real_Operation ("'/'", Div_Left);
   function Div_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Div_Inv;

   function Get_Dimension (Left : Measure) return String is
   begin
      return
      (  "["
      &  Image
         (  Measure'(Left.SI, 1.0, 0.0),
            Messaging_Parameters.Mode
         )
      &  "]"
      );
   end Get_Dimension;

   function Get_Dimension (Left : Real'Class) return String is
   begin
      return Get_Dimension (Left.Value);
   end Get_Dimension;

   function Get_Preference (Left : Real) return Preference is
   begin
      return Real_Preference;
   end Get_Preference;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      Other : constant Measure := To_Measure (Right);
   begin
      if Other.SI = Left.Value.SI then
         return
            Logic.Plain.Truth_Value'
            (  Left.Location & Right.Location & Location,
               To_Confidence (Left.Value = Other)
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
   end EQ;

   function Exp
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Real
            )  return Real is
   begin
      if Right.Value.SI /= Unitless or else Right.Value.Offset /= 0.0
      then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Exponent cannot be dimensioned as found at "
            &  Image (Right.Location)
            &  " in '**' at "
            &  Image (Location)
         )  );
      elsif Left.Value.Offset /= 0.0 then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Exponent radix cannot be shifted as found at "
            &  Image (Left.Location)
            &  " in '**' at "
            &  Image (Location)
         )  );
      elsif Left.Value.SI /= Unitless then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Exponent radix cannot be dimensioned as found at "
            &  Image (Left.Location)
            &  " with fractional exponent at "
            &  Image (Right.Location)
            &  " in '**' at "
            &  Image (Location)
         )  );
      end if;
      declare
         Result : Measure;
      begin
         Result.Gain := Left.Value.Gain ** Right.Value.Gain;
         if Result.Gain'Valid then
            return
               Real'(Location & Left.Location & Right.Location, Result);
         end if;
      exception
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error with the operands at "
         &  Image (Left.Location)
         &  " and at "
         &  Image (Right.Location)
         &  " of '**' at "
         &  Image (Location)
      )  );
   end Exp;

   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value >= To_Measure (Right))
         );
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible dimensions "
            &  Get_Dimension (Left)
            &  " at "
            &  Image (Left.Location)
            &  " and "
            &  Get_Dimension (To_Measure (Right))
            &  " at "
            &  Image (Right.Location)
            &  " as found in comparison at "
            &  Image (Location)
         )  );
   end GE;

   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value > To_Measure (Right))
         );
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible dimensions "
            &  Get_Dimension (Left)
            &  " at "
            &  Image (Left.Location)
            &  " and "
            &  Get_Dimension (To_Measure (Right))
            &  " at "
            &  Image (Right.Location)
            &  " as found in comparison at "
            &  Image (Location)
         )  );
   end GT;

   function Real_Mul is new Real_Operation ("'*'", "*");
   function Mul
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Mul;

   function Minus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class is
   begin
      return Real'(Left.Location & Location, -Left.Value);
   end Minus;

   function Plus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class is
   begin
      return Real'(Left.Location & Location, Left.Value);
   end Plus;

   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Domain_Integer
            )  return Numeric'Class is
      Result : Measure;
   begin
      begin
         Result := Left.Value ** Natural (Right);
         if Result.Gain'Valid then
            return Real'(Left.Location & Location, Result);
         end if;
      exception
         when Constraint_Error =>
            null;
         when Unit_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unit error at "
               &  Image (Left.Location)
               &  " in postifx exponentiation at "
               &  Image (Location)
            )  );
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error at "
         &  Image (Left.Location)
         &  " in postifx exponentiation at "
         &  Image (Location)
      )  );
   end Pow;

   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class is
   begin
      if Right in Integers.Int'Class then
         return Pow (Location, Left, Integers.Int'Class (Right).Value);
      else
         return
            Exp
            (  Location,
               Left,
               Real'(Right.Location, To_Measure (Right))
            );
      end if;
   end Pow;

   function Pow_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class is
   begin
      return
         Exp
         (  Location,
            Real'(Right.Location, To_Measure (Right)),
            Left
         );
   end Pow_Inv;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Measure;
   begin
      begin
         Result := Left.Value * Right;
         if Result.Gain'Valid then
            return Real'(Left.Location & Location, Result);
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
               Left     : Real;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class is
      Other : constant Measure := To_Measure (Right);
      Value : Interval_Measure;
   begin
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
            Value := To_Interval_Measure (Other, Left.Value);
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
            Value := To_Interval_Measure (Left.Value, Other);
         end if;
         if Value.From'Valid and then Value.To'Valid then
            return
               Subsets.Ranges.Reals.Real_Range'
               (  Location & Left.Location & Right.Location,
                  Value
               );
         end if;
      exception
         when Constraint_Error =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error in evaluation range at "
         &  Image (Location)
      )  );
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension "
            &  Get_Dimension (Left)
            &  " of the lower interval bound at "
            &  Image (Left.Location)
            &  " is incompatible with "
            &  Get_Dimension (Other)
            &  " of the upper bound at "
            &  Image (Right.Location)
         )  );
   end Stretch;

   function Real_Sub is new Real_Operation ("'-'", "-");
   function Sub
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Sub;

   function Sub_Left (Left, Right : Measure) return Measure is
   begin
      return Right - Left;
   end Sub_Left;
   function Real_Sub_Inv is new Real_Operation ("'-'", Sub_Left);
   function Sub_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class renames Real_Sub_Inv;

   function Image
            (  Feature : Feature_Handle;
               Item    : Real;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Item.Value, Mode);
   end Image;

   function To_SI
            (  Location : Parsers.Multiline_Source.Location;
               Value    : Measure
            )  return Domain_Float is
      Result : Domain_Float;
   begin
      begin
         Result := Get_Value (Value);
         if Result'Valid then
            return Result;
         end if;
      exception
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Out of range value at "
         &  Image (Location)
      )  );
   end To_SI;

   function To_SI (Value : Real) return Domain_Float is
   begin
      return To_SI (Value.Location, Value.Value);
   end To_SI;

   function To_Measure (Left : Constant_Value'Class) return Measure is
   begin
      if Left in Integers.Int'Class then
         return
            To_Measure
            (  Domain_Float (Numerics.Integers.Int'Class (Left).Value)
            );
      elsif Left in Real'Class then
         return Real'Class (Left).Value;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Real value is expected at "
         &  Image (Left.Location)
      )  );
   end To_Measure;

   function To_Measure
            (  Context : Resolution_Context;
               Value   : Constant_Value'Class
            )  return Measure is
      Result : Measure := To_Measure (Value);
   begin
      Result := Convert (Result, Context.Dimension);
      if Result.Gain'Valid then
         return Result;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Out of range value at "
         &  Image (Value.Location)
      )  );
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Expected dimension "
            &  Dimension_Image (Context, Context.Dimension)
            &  " is incompatible with "
            &  Dimension_Image (Context, Result)
            &  " as found at "
            &  Image (Value.Location)
         )  );
   end To_Measure;

   function To_Real
            (  Context : Resolution_Context;
               Name    : Identifier'Class
            )  return Real is
      Result  : Real;
      Pointer : Integer := Name.Value'First;
   begin
      Result.Location := Name.Location;
      begin
         Get
         (  Name.Value,
            Pointer,
            Result.Value,
            Parsing_Mode
         );
         if Pointer > Name.Value'Last then
            return Result;
         end if;
      exception
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Malformed dimension or invalid domain value found at "
         &  Image (Name.Location)
      )  );
   end To_Real;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term is
   begin
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
   exception
      when Constraint_Error =>
         if Is_Valid (Context.Expected.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed for '"
               &  Get_Name (Context.Expected.Feature)
               &  "' in the operation at "
               &  Image (Location)
            )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed in the operation at "
               &  Image (Location)
            )  );
         end if;
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension "
            &  Get_Dimension (Value)
            &  " at "
            &  Image (Value.Location)
            &  " is incompatible with "
            &  Dimension_Image (Context, Context.Dimension)
            &  " expected in the operation at "
            &  Image (Location)
         )  );
   end Equal_Logical_Term;

   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term is
      function Pred (Value : Measure) return Measure is
      begin
         return
         (  SI     => Value.SI,
            Gain   => Domain_Float'Pred (Value.Gain),
            Offset => Value.Offset
         );
      end Pred;
   begin
      declare
         Set : Fuzzy.Intuitionistic.Classification renames
                  Classify
                  (  Context.Expected.Feature,
                     (  Pair_Measure'
                        (  Pred (Value.Value),
                           Confidence'First
                        )
                     &  (Value.Value, Confidence'Last)
                  )  );
      begin
         return
         (  Has_In      => Create (Set.Possibility),
            Has_Not     => Create (not Set.Necessity),
            Has_Out     => Null_Handle,
            Has_Not_Out => Null_Handle
         );
      end;
   exception
      when Constraint_Error =>
         if Is_Valid (Context.Expected.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed for '"
               &  Get_Name (Context.Expected.Feature)
               &  "' in the operation at "
               &  Image (Location)
            )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed in the operation at "
               &  Image (Location)
            )  );
         end if;
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension "
            &  Get_Dimension (Value)
            &  " at "
            &  Image (Value.Location)
            &  " is incompatible with "
            &  Dimension_Image (Context, Context.Dimension)
            &  " expected in the operation at "
            &  Image (Location)
         )  );
   end Greater_Or_Equal_Logical_Term;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term is
      function Succ (Value : Measure) return Measure is
      begin
         return
         (  SI     => Value.SI,
            Gain   => Domain_Float'Succ (Value.Gain),
            Offset => Value.Offset
         );
      end Succ;
   begin
      declare
         Set : Fuzzy.Intuitionistic.Classification renames
                  Classify
                  (  Context.Expected.Feature,
                     (  Pair_Measure'(Value.Value, Confidence'First)
                     &  (Succ (Value.Value), Confidence'Last)
                  )  );
      begin
         return
         (  Has_In      => Create (Set.Possibility),
            Has_Not     => Create (not Set.Necessity),
            Has_Out     => Null_Handle,
            Has_Not_Out => Null_Handle
         );
      end;
   exception
      when Constraint_Error =>
         if Is_Valid (Context.Expected.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed for '"
               &  Get_Name (Context.Expected.Feature)
               &  "' in the operation at "
               &  Image (Location)
            )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real operand at "
               &  Image (Value.Location)
               &  " is not allowed in the operation at "
               &  Image (Location)
            )  );
         end if;
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension "
            &  Get_Dimension (Value)
            &  " at "
            &  Image (Value.Location)
            &  " is incompatible with "
            &  Dimension_Image (Context, Context.Dimension)
            &  " expected in the operation at "
            &  Image (Location)
         )  );
   end Greater_Logical_Term;

end Parsers.FCL.Code.Orders.Numerics.Reals;
