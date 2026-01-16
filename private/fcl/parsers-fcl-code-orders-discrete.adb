--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Discrete                                 Summer, 2005       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Integer_Intervals;          use Integer_Intervals;
with Parsers.FCL.Code.Logic;     use Parsers.FCL.Code.Logic;

with Parsers.FCL.Code.Subsets.Intuitionistic;
with Parsers.FCL.Code.Logic.Plain;

package body Parsers.FCL.Code.Orders.Discrete is

   function To_Point
            (  Context : Resolution_Context;
               Left    : Constant_Value'Class
            )  return Positive is
   begin
      if Left in Point'Class then
         return Point'Class (Left).Value;
      else
         if Is_Valid (Context.Expected.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Domain value name of '"
               &  Get_Name (Context.Expected.Feature)
               &  "' (declared at "
               &  Image (Context.Expected.Location)
               &  ") is expected at "
               &  Image (Left.Location)
            )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Domain value name is expected at "
               &  Image (Left.Location)
            )  );
         end if;
      end if;
   end To_Point;

   function Get_Point
            (  Context : Resolution_Context;
               Term    : Identifier'Class
            )  return Point is
   begin
      if Term.Malformed then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Malformed name '"
            &  Term.Value
            &  "' found at "
            &  Image (Term.Location)
         )  );
      end if;
      if Is_Valid (Context.Expected.Feature) then
         begin
            return
            (  Term.Location,
               Value (Term.Value, Context.Expected.Feature).From
            );
         exception
            when others =>
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "'"
                  &  Term.Value
                  &  "' is not a domain value of '"
                  &  Get_Name (Context.Expected.Feature)
                  &  "' (declared at "
                  &  Image (Context.Expected.Location)
                  &  ") as expected at "
                  &  Image (Term.Location)
               )  );
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Unrecognized name '"
            &  Term.Value
            &  "' found at "
            &  Image (Term.Location)
         )  );
      end if;
   end Get_Point;

   function Image
            (  Feature : Feature_Handle;
               Item    : Point;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Feature, Interval'(Item.Value, Item.Value));
   end Image;

--     function Cut
--              (  Location : Parsers.Multiline_Source.Location;
--                 Context  : Resolution_Context;
--                 Left     : Point;
--                 Right    : Fuzzy_Boolean
--              )  return Lattice'Class is
--        Cardinality : constant Positive :=
--           Get_Cardinality (Context.Expected.Feature);
--        Result : Subsets.Intuitionistic.Set :=
--           (  Cardinality,
--              Location & Left.Location,
--              (  Cardinality,
--                 (others => Confidence'First),
--                 (others => Confidence'First)
--           )  );
--     begin
--        Result.Value.Possibility (Left.Value) := Right.Possibility;
--        Result.Value.Necessity   (Left.Value) := Right.Necessity;
--        return Result;
--     end Cut;
--  
--     function Cut
--              (  Location : Parsers.Multiline_Source.Location;
--                 Context  : Resolution_Context;
--                 Left     : Point;
--                 Right    : Confidence
--              )  return Lattice'Class is
--        Result : Subsets.Plain.Set :=
--           (  Get_Cardinality (Context.Expected.Feature),
--              Location & Left.Location,
--              (others => Confidence'First)
--           );
--     begin
--        Result.Value (Left.Value) := Right;
--        return Result;
--     end Cut;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      if Left.Value = To_Point (Context, Right) then
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'Last
            );
      else
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'First
            );
      end if;
   end EQ;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term is
      Cardinality : constant Positive :=
                       Get_Cardinality (Context.Expected.Feature);
      Result : Fuzzy.Intuitionistic.Set :=
                       (  Cardinality,
                          (others => Confidence'First),
                          (others => Confidence'First)
                       );
   begin
      Result.Possibility (Value.Value) := Confidence'Last;
      Result.Necessity   (Value.Value) := Confidence'Last;
      return Equal_Logical_Term (Result);
   end Equal_Logical_Term;

   function Get_Preference (Left : Point) return Preference is
   begin
      return Domain_Point_Preference;
   end Get_Preference;
   
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term is
      Cardinality : constant Positive :=
                       Get_Cardinality (Context.Expected.Feature);
      Result : Fuzzy.Intuitionistic.Set :=
                       (  Cardinality,
                          (others => Confidence'First),
                          (others => Confidence'First)
                       );
   begin
      for Index in Value.Value..Cardinality loop
         Result.Possibility (Index) := Confidence'Last;
         Result.Necessity   (Index) := Confidence'Last;
      end loop;
      return Subset_Logical_Term (Result);
   end Greater_Or_Equal_Logical_Term;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term is
      Cardinality : constant Positive :=
                       Get_Cardinality (Context.Expected.Feature);
      Result : Fuzzy.Intuitionistic.Set :=
                       (  Cardinality,
                          (others => Confidence'First),
                          (others => Confidence'First)
                       );
   begin
      for Index in Value.Value + 1..Cardinality loop
         Result.Possibility (Index) := Confidence'Last;
         Result.Necessity   (Index) := Confidence'Last;
      end loop;
      return Subset_Logical_Term (Result);
   end Greater_Logical_Term;

   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class
            )  return Logical'Class is
   begin
      if Left.Value > To_Point (Context, Right) then
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'Last
            );
      else
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'First
            );
      end if;
   end GT;
   
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class
            )  return Logical'Class is
   begin
      if Left.Value >= To_Point (Context, Right) then
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'Last
            );
      else
         return
            Logic.Plain.Truth_Value'
            (  Location & Left.Location & Right.Location,
               Confidence'First
            );
      end if;
   end GE;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Logic.Plain.Truth_Value;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Domain value name of '"
         &  Get_Name (Context.Expected.Feature)
         &  "' (declared at "
         &  Image (Context.Expected.Location)
         &  ") as found at "
         &  Image (Left.Location)
         &  " cannot have dimension as found at "
         &  Image (Location)
      )  );
      return Result;
   end Set_Dimension;

   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class is
      From : Positive;
      To   : Positive := To_Point (Context, Right);
   begin
      if Inversed then
         if Left.Value < To then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "The lower range bound at "
               &  Image (Left.Location)
               &  " is greater than the upper one at "
               &  Image (Right.Location)
            )  );
         end if;
         From := To;
         To   := Left.Value;
      else
         if Left.Value > To then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "The lower range bound at "
               &  Image (Left.Location)
               &  " is greater than the upper one at "
               &  Image (Right.Location)
            )  );
         end if;
         From := Left.Value;
      end if;
      declare
         Cardinality : constant Positive :=
            Get_Cardinality (Context.Expected.Feature);
         Result : Subsets.Intuitionistic.Set :=
            (  Cardinality,
               Location & Left.Location & Right.Location,
               (  Cardinality,
                  (others => Confidence'First),
                  (others => Confidence'First)
            )  );
      begin
         for Index in From..To loop
            Result.Value.Possibility (Index) := Confidence'Last;
            Result.Value.Necessity   (Index) := Confidence'Last;
         end loop;
         return Result;
      end;
   end Stretch;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term is
      Cardinality : constant Positive :=
                       Get_Cardinality (Context.Expected.Feature);
      Result : Fuzzy.Intuitionistic.Set :=
                       (  Cardinality,
                          (others => Confidence'First),
                          (others => Confidence'First)
                       );
   begin
      Result.Possibility (Value.Value) := Confidence'Last;
      Result.Necessity   (Value.Value) := Confidence'Last;
      return Subset_Logical_Term (Result);
   end Subset_Logical_Term;

   function To_Set
            (  Context : Resolution_Context;
               Term    : Identifier'Class
            )  return Lattice'Class is
      Index : Point renames Get_Point (Context, Term);
   begin
      declare
         Cardinality : constant Positive :=
            Get_Cardinality (Context.Expected.Feature);
         Result : Subsets.Intuitionistic.Set :=
            (  Cardinality,
               Term.Location,
               (  Cardinality,
                  (others => Confidence'First),
                  (others => Confidence'First)
            )  );
      begin
         Result.Value.Possibility (Index.Value) := Confidence'Last;
         Result.Value.Necessity   (Index.Value) := Confidence'Last;
         return Result;
      end;
   end To_Set;

end Parsers.FCL.Code.Orders.Discrete;
