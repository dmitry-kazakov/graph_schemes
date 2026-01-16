--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Integers                                 Spring, 2005       --
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
with Parsers.FCL.Code.Orders;      use Parsers.FCL.Code.Orders;

with Fuzzy.Feature.Domain_Float_Handle;
with Fuzzy.Feature.Domain_Integer_Handle;
with Fuzzy.Feature.Integer_Handle;
with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Parsers.FCL.Code.Subsets.Singletons.Integers;
with Parsers.FCL.Code.Subsets.Singletons.Reals;
with Parsers.FCL.Code.Subsets.Intuitionistic;
with Parsers.FCL.Code.Subsets.Reals;
with Parsers.FCL.Code.Subsets.Ranges.Integers;

package body Parsers.FCL.Code.Subsets.Integers is
   use Variable_Measures;
   use Variable_Edit;
   use Fuzzy.Feature.Domain_Float_Handle;
   use Fuzzy.Feature.Domain_Integer_Handle;

   function To_Set (Left : Constant_Value'Class) return Variable is
   begin
      if Left in Set'Class then
         return Set'Class (Left).Value;
      elsif Left in Singletons.Integers.Integer_Singleton'Class then
         return
            Set'Class
            (  Singletons.Integers.To_Set
               (  Singletons.Integers.Integer_Singleton'Class (Left)
            )  ) .Value;
      elsif Left in Ranges.Integers.Integer_Range'Class then
         return
            Set'Class
            (  Ranges.Integers.To_Set
               (  Ranges.Integers.Integer_Range'Class (Left)
            )  ) .Value;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer subset is expected at "
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
      return Integer_Set_Preference;
   end Get_Preference;

   function Image
            (  Feature : Feature_Handle;
               Item    : Set;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Item.Value);
   end Image;

   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Set;
               Inversed : Boolean
            )  return Logic.Logical'Class is
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
                  (  Domain_Float (Int'Class (Left).Value),
                     Right.Value
               )  );
         elsif Left in Real'Class then
            return
               Reals.Is_Subset
               (  Location,
                  Context,
                  Left,
                  Reals.Set'
                  (  Left.Location,
                     To_Variable_Measure (Right.Value)
                  ),
                  False
               );
         else
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In (To_Set (Left), Right.Value)
               );
         end if;
      end if;
   end Is_Subset;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      use Singletons.Integers;
      use Singletons.Reals;
   begin
      if Right in Real_Singleton'Class then
         return
            Reals.Join
            (  Location,
               Context,
               Reals.Set'
               (  Left.Location,
                  To_Variable_Measure (Left.Value)
               ),
               Right
            );
      elsif Right in Integer_Singleton'Class then
         declare
            Second : Integer_Singleton'Class renames
                        Integer_Singleton'Class (Right);
            Result : Set := Left;
         begin
            Result.Location := Left.Location & Right.Location;
            Append
            (  Result.Value,
               Domain_Float (Second.Value),
               Second.Level
            );
            return Result;
         end;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Integer singleton is expected at "
         &  Image (Right.Location)
      )  );
   exception
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
   begin
      return
         Logic.Intuitionistic.Truth_Value'
         (  Left.Location & Right.Location & Location,
            Left.Value = To_Set (Right)
         );
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
      return
         Subsets.Reals.Set'
         (  Left.Location & Location,
            Left.Value * Right
         );
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
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Unexpected integer subset found at "
            &  Image (Left.Location)
         )  );
      elsif Is_Domain_Integer (Context.Expected.Feature) then
         declare
            From   : constant Domain_Integer :=
                        Integer_Handle.Get_From
                        (  Context.Expected.Feature
                        );
            Level  : Fuzzy_Boolean;
            Result : Fuzzy.Intuitionistic.Set
                        (Get_Cardinality (Context.Expected.Feature));
         begin
            for Index in 1..Result.Cardinality loop
               Level :=
                  Is_In
                  (  Domain_Float (From) + Domain_Float (Index - 1),
                     Left.Value
                  );
               Result.Possibility (Index) := Level.Possibility;
               Result.Necessity   (Index) := Level.Necessity;
            end loop;
            return Result;
         end;
      elsif Is_Domain_Float (Context.Expected.Feature) then
         return
            Classify_To_Set
            (  Classify
               (  Context.Expected.Feature,
                  To_Variable_Measure (Left.Value)
            )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Integer subset found, whereas one of '"
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
               &  ") is incompatible with [1] as found at "
               &  Image (Left.Location)
            )  );
   end To_Set;

end Parsers.FCL.Code.Subsets.Integers;
