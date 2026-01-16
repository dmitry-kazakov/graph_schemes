--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Reals                                    Spring, 2005       --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Intuitionistic;         use Fuzzy.Intuitionistic;

with Fuzzy.Feature.Domain_Float_Handle;
with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Parsers.FCL.Code.Subsets.Integers;
with Parsers.FCL.Code.Subsets.Intuitionistic;
with Parsers.FCL.Code.Subsets.Ranges.Reals;
with Parsers.FCL.Code.Subsets.Singletons.Reals;
with Units.Base;

package body Parsers.FCL.Code.Subsets.Reals is
   use Fuzzy.Feature.Domain_Float_Handle;
   use Measure_Edit;
   use Variable_Edit;

   function To_Set (Left : Constant_Value'Class)
      return Variable_Measure is
   begin
      if Left in Set'Class then
         return Set'Class (Left).Value;
      elsif Left in Integers.Set'Class then
         return To_Variable_Measure (Integers.Set'Class (Left).Value);
      elsif Left in Singletons.Singleton'Class then
         return
            Set'Class
            (  Singletons.Reals.To_Set
               (  Singletons.Reals.To_Singleton (Left)
            )  ) .Value;
      elsif Left in Ranges.Numeric_Range'Class then
         return
            To_Variable_Measure (Ranges.Reals.To_Range (Left).Value);
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Real subset is expected at "
            &  Image (Left.Location)
         )  );
      end if;
   end To_Set;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term is
   begin
      return Equal_Logical_Term (To_Set (Context, Value));
   end Equal_Logical_Term;

   function Get_Set (Tree : Node'Class) return Set'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      declare
         Result : constant Lattice'Class := Get_Lattice (Context, Tree);
      begin
         return Set'(Result.Location, To_Set (Result));
      end;
   end Get_Set;

   function Get_Value_Or_Set (Tree : Node'Class) return Set'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      declare
         Result : constant Constant_Value'Class :=
                     Get_Comparable (Context, Tree);
      begin
         if Result in Set'Class then
            return Set'Class (Result);
         elsif Result in Integers.Set'Class then
            return
               Set'
               (  Result.Location,
                  To_Variable_Measure
                  (  Integers.Set'Class (Result).Value
               )  );
         elsif Result in Singletons.Singleton'Class then
            return
               Set'Class
               (  Singletons.Reals.To_Set
                  (  Singletons.Reals.To_Singleton (Result)
               )  );
         elsif Result in Ranges.Numeric_Range'Class then
            return
               Set'
               (  Result.Location,
                  To_Variable_Measure
                  (  Ranges.Reals.To_Range (Result).Value
               )  );
         elsif Result in Orders.Numerics.Integers.Int'Class then
            return
               Set'
               (  Result.Location,
                  To_Variable_Measure
                  (  Domain_Float
                     (  Orders.Numerics.Integers.Int'Class
                        (  Result
                        ) .Value
               )  )  );
         elsif Result in Orders.Numerics.Reals.Real'Class then
            return
               Set'
               (  Result.Location,
                  To_Variable_Measure
                  (  Orders.Numerics.Reals.Real'Class (Result).Value
               )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Real value or subset is expected at "
               &  Image (Result.Location)
            )  );
         end if;
      end;
   end Get_Value_Or_Set;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is
      Result : Fuzzy.Intuitionistic.Set := To_Set (Context, Left);
   begin
      Fuzzy.And_At (Result.Possibility, Right.Possibility);
      Fuzzy.And_At (Result.Necessity,   Right.Necessity);
      return
         Subsets.Intuitionistic.Set'
         (  Result.Cardinality,
            Location & Left.Location,
            Result
         );
   end Cut;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Confidence
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Location & Left.Location,
            Left.Value and Right
         );
   end Cut;

   function Get_Preference (Left : Set) return Preference is
   begin
      return Real_Set_Preference;
   end Get_Preference;

   function Get_Dimension (Left : Variable_Measure) return String is
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

   function Get_Unit_Text (Item : Set; Mode : Code_Set) return String is
   begin
      if (  Item.Value.SI = Units.Base.Unitless
         and then
            Item.Value.Offset = 0.0
         )
      then
         return "";
      else
         return
         (  " ["
         &  Image
            (  Measure'(Item.Value.SI, 1.0, Item.Value.Offset),
               Mode
            )
         &  "]"
         );
      end if;
   end Get_Unit_Text;

   function Image
            (  Feature : Feature_Handle;
               Item    : Set;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Item.Value.Gain) & Get_Unit_Text (Item, Mode);
   end Image;

   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Set;
               Inversed : Boolean
            )  return Logic.Logical'Class is
      use Code.Orders.Numerics;
      use Code.Orders.Numerics.Integers;
      use Code.Orders.Numerics.Reals;
   begin
      if Inversed then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Location & Right.Location,
               Is_In (Right.Value, To_Set (Left))
            );
      else
         if Left in Int'Class then
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In
                  (  To_Measure (Domain_Float (Int'Class (Left).Value)),
                     Right.Value
               )  );
         elsif Left in Real'Class then
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In
                  (  Real'Class (Left).Value,
                     Right.Value
               )  );
         else
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In (To_Set (Left), Right.Value)
               );
         end if;
      end if;
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Real value or subset"
            &  Get_Unit_Text (Right, Messaging_Parameters.Mode)
            &  " is expected at "
            &  Image (Left.Location)
         )  );
   end Is_Subset;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      Second : Singletons.Reals.Real_Singleton renames
                  Singletons.Reals.To_Singleton (Right);
      Result : Set := Left;
   begin
      Result.Location := Left.Location & Right.Location;
      Append (Result.Value, Second.Value.Value, Second.Value.Level);
      return Result;
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension "
            &  Dimension_Image (Context, Left.Value.SI)
            &  " at "
            &  Image (Left.Location)
            &  " is incompatible with "
            &  Dimension_Image (Context, Second.Value.Value.SI)
            &  " as found at "
            &  Image (Right.Location)
         )  );
      when Data_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "The value at "
            &  Image (Left.Location)
            &  " must precede one at "
            &  Image (Right.Location)
         )  );
   end Join;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      Other : Variable_Measure renames To_Set (Right);
   begin
      if Left.Value.SI = Other.SI then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Right.Location & Location,
               Left.Value = Other
            );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible dimensions "
            &  Get_Dimension (Left.Value)
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

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Set
            )  return Lattice'Class is
   begin
      return Set'(Left.Location & Location, not Left.Value);
   end Logical_Not;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Left.Location & Right.Location & Location,
            Left.Value and To_Set (Right)
         );
   end Logical_And;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Left.Location & Right.Location & Location,
            Left.Value or To_Set (Right)
         );
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Left.Location & Right.Location & Location,
            Left.Value xor To_Set (Right)
         );
   end Logical_Xor;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Measure
            )  return Constant_Value'Class is
   begin
      return Set'(Left.Location & Location, Left.Value * Right);
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Numeric error in scaling at "
            &  Image (Left.Location)
            &  " by the scale at "
            &  Image (Location)
         )  );
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Unit error in scaling at "
            &  Image (Left.Location)
            &  " by the scale at "
            &  Image (Location)
         )  );
   end Set_Dimension;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term is
   begin
      return Subset_Logical_Term (To_Set (Context, Value));
   end Subset_Logical_Term;

   function To_Set
            (  Context : Resolution_Context;
               Left    : Set
            )  return Standard.Fuzzy.Intuitionistic.Set is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Unexpected real subset found at " & Image (Left.Location)
         );
      elsif Is_Domain_Float (Context.Expected.Feature) then
         return
            Classify_To_Set
            (  Classify
               (  Context.Expected.Feature,
                  Left.Value
            )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Real subset found, whereas one of '"
            &  Get_Name (Context.Expected.Feature)
            &  "' (declared at "
            &  Image (Context.Expected.Location)
            &  ") is expected at "
            &  Image (Left.Location)
         )  );
      end if;
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Dimension ["
            &  Get_Scale_Text
               (  Context.Expected.Feature,
                  Messaging_Parameters
               )
            &  "] of '"
            &  Get_Name (Context.Expected.Feature)
            &  "' (declared at "
            &  Image (Context.Expected.Location)
            &  ") is incompatible with ["
            &  Image
               (  Measure'(Left.Value.SI, 1.0, 0.0),
                  Messaging_Parameters.Mode
               )
            &  "] as found at "
            &  Image (Left.Location)
         )  );
   end To_Set;

end Parsers.FCL.Code.Subsets.Reals;
