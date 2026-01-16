--                                                                    --
--  package Parsers.FCL.Code        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2005       --
--                                                                    --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Fuzzy;                     use Fuzzy;
with Fuzzy.Intuitionistic;      use Fuzzy.Intuitionistic;
with Parsers.FCL.Code.Logic;    use Parsers.FCL.Code.Logic;
with Parsers.FCL.Code.Orders;   use Parsers.FCL.Code.Orders;
with Parsers.FCL.Code.Subsets;  use Parsers.FCL.Code.Subsets;

with Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Handle.Edit;
with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Logic.Plain;
with Parsers.FCL.Code.Orders.Discrete;
with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Parsers.FCL.Code.Orders.Strings;
with Parsers.FCL.Code.Subsets.Singletons;
--with Ada.Text_IO;

package body Parsers.FCL.Code is
   use Fuzzy.Feature.Domain_Floats.Measure_Edit;
   use Parsers.FCL.Code.Orders.Discrete;

   function Create (Value : Fuzzy.Set) return Logical_Set_Handle is
      X : constant Logical_Set_Ptr := new Logical_Set (Value'Length);
      Result : constant Logical_Set_Handle := Ref (X);
   begin
      X.Value := Value;
      return Result;
   end Create;

   function Dimension_Image
            (  Context : Resolution_Context;
               Value   : Unit
            )  return String is
   begin
      return
      (  '['
      &  Image (Measure'(Value, 1.0, 0.0), Messaging_Parameters.Mode)
      &  ']'
      );
   end Dimension_Image;

   function Dimension_Image
            (  Context : Resolution_Context;
               Value   : Measure
            )  return String is
   begin
      return Dimension_Image (Context, Value.SI);
   end Dimension_Image;

   function Equal
            (  Left  : Logical_Set;
               Right : Object.Entity'Class;
               Flag  : Boolean := False
            )  return Boolean is
   begin
      if (  Flag
         or else
            Right not in Logical_Set'Class
         or else
            Right in Logical_Set
         )
      then
         return Left.Value = Logical_Set (Right).Value;
      else
         return Object.Equal (Right, Left, True);
      end if;
   end Equal;

   function Image
            (  Feature : Feature_Handle;
               Term    : Logical_Term
            )  return String is
      use Fuzzy.Feature.Handle.Edit;
      Cardinality : constant Positive := Get_Cardinality (Feature);
      Got         : array (Image_Type) of Boolean := (others => False);
      First       : Boolean := True;

      function Value (X, Y : Image_Type; Force : Boolean)
         return String is
         Has_X : constant Boolean := Is_Valid (Term (X));
         Has_Y : constant Boolean := Is_Valid (Term (Y));
      begin
         if (  (  (Has_X and Has_Y)
               or else
                  (Force and (Has_X or Has_Y))
               )
            and then
               not (Got (X) and Got (Y))
            )
         then
            Got (X) := True;
            Got (Y) := True;
            if Has_X then
               if Has_Y then
                  return
                     Image
                     (  Feature,
                        Classification'
                        (  Cardinality,
                           Ptr (Term (X)).Value,
                           not Ptr (Term (Y)).Value
                     )  );
               else
                  return
                     Image
                     (  Feature,
                        Classification'
                        (  Cardinality,
                           Ptr (Term (X)).Value,
                           (others => Confidence'First)
                     )  );
               end if;
            else
               return
                 Image
                  (  Feature,
                     Classification'
                     (  Cardinality,
                        (others => Confidence'Last),
                        not Ptr (Term (Y)).Value
                  )  );
            end if;
         else
            return "";
         end if;
      end Value;

      function Condition (X, Y : Image_Type; Force : Boolean)
         return String is
         Text : String renames Value (X, Y, Force);
      begin
         if Text'Length > 0 then
            if Y = Has_Not or else X = Has_Out then
               if X = Has_Out then
                  return "not " & Get_Name (Feature) & " is " & Text;
               else
                  return Get_Name (Feature) & " is " & Text;
               end if;
            else
               if X = Has_Not then
                  return Text & " not in " & Get_Name (Feature);
               else
                  return Text & " in " & Get_Name (Feature);
               end if;
            end if;
         else
            return "";
         end if;
      end Condition;

      function Item (X, Y : Image_Type; Force : Boolean)
         return String is
         Text : String renames Condition (X, Y, Force);
      begin
         if Text'Length > 0 then
            if First then
               First := False;
               return Text;
            else
               return " and " & Text;
            end if;
         else
            return "";
         end if;
      end Item;

      A : constant String := Item (Has_In,  Has_Not,     False);
      B : constant String := Item (Has_Out, Has_Not_Out, False);
      C : constant String := Item (Has_In,  Has_Out,     False);
      D : constant String := Item (Has_Not, Has_Not_Out, False);
   begin
      return A & B & C & D;
   end Image;

   function "<" (Left, Right : Preference) return Boolean is
   begin
      return Preference'Pos (Left) < Preference'Pos (Right);
   end "<";

   function Nothing (Left : Logical_Set_Handle)
      return Intervals.Logical is
      X : constant Logical_Set_Ptr := Ptr (Left);
   begin
      if X = null then
         return Intervals.Uncertain;
      elsif Possibility (X.Value) = Confidence'First then
         return Intervals.True;
      else
         return Intervals.False;
      end if;
   end Nothing;

   function Universe (Left : Logical_Set_Handle)
      return Intervals.Logical is
      X : constant Logical_Set_Ptr := Ptr (Left);
   begin
      if X = null then
         return Intervals.Uncertain;
      elsif Necessity (X.Value) = Confidence'Last then
         return Intervals.True;
      else
         return Intervals.False;
      end if;
   end Universe;

   function "=" (Left, Right : Logical_Term) return Boolean is
   begin
      for Index in Image_Type'Range loop
         if Left (Index) /= Right (Index) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   function "and" (Left, Right : Logical_Set_Handle)
      return Logical_Set_Handle is
      X : constant Logical_Set_Ptr := Ptr (Left);
      Y : constant Logical_Set_Ptr := Ptr (Right);
   begin
      if X = null then
         return Right;
      elsif Y = null then
         return Left;
      else
         declare
            Z      : constant Logical_Set_Ptr :=
                        new Logical_Set (X.Cardinality);
            Result : constant Logical_Set_Handle := Ref (Z);
         begin
            Z.Value := X.Value and Y.Value;
            return Result;
         end;
      end if;
   end "and";

   function "and" (Left, Right : Logical_Term) return Logical_Term is
   begin
      return
      (  Has_In      => Left (Has_In)     and Right (Has_In),
         Has_Out     => Left (Has_Out)    and Right (Has_Out),
         Has_Not     => Left (Has_Not)     or Right (Has_Not),
         Has_Not_Out => Left (Has_Not_Out) or Right (Has_Not_Out)
      );
   end "and";

   function "or" (Left, Right : Logical_Set_Handle)
      return Logical_Set_Handle is
      X : constant Logical_Set_Ptr := Ptr (Left);
      Y : constant Logical_Set_Ptr := Ptr (Right);
   begin
      if X = null then
         return Right;
      elsif Y = null then
         return Left;
      else
         declare
            Z      : constant Logical_Set_Ptr :=
                        new Logical_Set (X.Cardinality);
            Result : constant Logical_Set_Handle := Ref (Z);
         begin
            Z.Value := X.Value or Y.Value;
            return Result;
         end;
      end if;
   end "or";

   function "or" (Left, Right : Logical_Term) return Logical_Term is
   begin
      return
      (  Has_In      => Left (Has_In)       or Right (Has_In),
         Has_Out     => Left (Has_Out)      or Right (Has_Out),
         Has_Not     => Left (Has_Not)     and Right (Has_Not),
         Has_Not_Out => Left (Has_Not_Out) and Right (Has_Not_Out)
      );
   end "or";

   function "not" (Left : Logical_Term) return Logical_Term is
   begin
      return
      (  Has_In      => Left (Has_Not),
         Has_Out     => Left (Has_Not_Out),
         Has_Not     => Left (Has_In),
         Has_Not_Out => Left (Has_Out)
      );
   end "not";
--
-- Do_Comparable -- Evaluation of {EQ|NE}
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The result
--
   function Do_Comparable
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logical'Class is
   begin
      case Tree.Operation is
         when EQ =>
            declare
               Left   : Constant_Value'Class renames
                  Get_Comparable (Context, Tree.Operands (1).all);
               Right  : Constant_Value'Class renames
                  Get_Comparable (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical'Class
                     (  EQ (Tree.Location, Context, Right, Left)
                     );
               else
                  return
                     Logical'Class
                     (  EQ (Tree.Location, Context, Left, Right)
                     );
               end if;
            end;
         when NE =>
            declare
               Left   : Constant_Value'Class renames
                  Get_Comparable (Context, Tree.Operands (1).all);
               Right  : Constant_Value'Class renames
                  Get_Comparable (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     not Logical'Class
                         (  EQ (Tree.Location, Context, Right, Left)
                         );
               else
                  return
                     not Logical'Class
                         (  EQ (Tree.Location, Context, Right, Left)
                         );
               end if;
            end;
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Operation "
               &  Image (Tree.Operation)
               &  " is not comparison or membership test expected at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Comparable;
--
-- Do_Lattice -- Evaluation of logical and lattice operations
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The result
--
   function Do_Lattice
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Lattice'Class is
   begin
--    Ada.Text_IO.Put_Line
--    (  "Lattice:" & Image (Tree.Operation)
--    &  " at " & Image (Tree.Location)
--    &  " = " & Image (Context, Tree)
--    );
      case Tree.Operation is
         when Is_Element =>
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (1).all,
                  Tree.Operands (2).all,
                  Element_Mode
               );
         when Is_Not_Element =>
            return
               Logical_Not
               (  Tree.Location,
                  Subset_Test
                  (  Tree.Location,
                     Context,
                     Tree.Operands (1).all,
                     Tree.Operands (2).all,
                     Element_Mode
               )  );
         when Has_Element =>
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (2).all,
                  Tree.Operands (1).all,
                  Element_Mode
               );
         when Has_Not_Element =>
            return
               Logical_Not
               (  Tree.Location,
                  Subset_Test
                  (  Tree.Location,
                     Context,
                     Tree.Operands (2).all,
                     Tree.Operands (1).all,
                     Element_Mode
               )  );
         when Is_In_Subset =>
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (1).all,
                  Tree.Operands (2).all,
                  Universal_Mode
               );
         when Is_Superset =>
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (2).all,
                  Tree.Operands (1).all,
                  Subset_Mode
               );
         when Is_Not_Superset =>
            return
               Logical_Not
               (  Tree.Location,
                  Subset_Test
                  (  Tree.Location,
                     Context,
                     Tree.Operands (2).all,
                     Tree.Operands (1).all,
                     Subset_Mode
               )  );
         when Has_Superset =>
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (1).all,
                  Tree.Operands (2).all,
                  Subset_Mode
               );
         when Has_Not_Superset =>
            return
               Logical_Not
               (  Tree.Location,
                  Subset_Test
                  (  Tree.Location,
                     Context,
                     Tree.Operands (1).all,
                     Tree.Operands (2).all,
                     Subset_Mode
               )  );
         when Is_Subset =>
            if Tree.Operands (2).all in Expression'Class then
               declare
                  Right : Expression'Class renames
                     Expression'Class (Tree.Operands (2).all);
               begin
                  if (  Right.Operation = Not_In
                     and then
                        Right.Operands'Length = 1
                     )
                  then
                     Right.Operation := Lattice_Not;
                  end if;
               end;
            end if;
            return
               Subset_Test
               (  Tree.Location,
                  Context,
                  Tree.Operands (1).all,
                  Tree.Operands (2).all,
                  Universal_Mode
               );
         when Not_In =>
            Raise_Exception
            (  Syntax_Error'Identity,
               "Misplaced 'not in' found at " & Image (Tree.Location)
            );
         when Lattice_And =>
            declare
               Left  : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_And (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_And (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Lattice_Or =>
            declare
               Left  : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_Or (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_Or (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Lattice_Xor =>
            declare
               Left  : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_Xor (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_Xor (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Lattice_Not =>
            return
               Logical_Not
               (  Tree.Location,
                  Get_Lattice (Context, Tree.Operands (1).all)
               );
         when Logical_And =>
            declare
               Left  : Lattice'Class renames
                  Get_Logical (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Logical (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_And (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_And (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Logical_Or =>
            declare
               Left  : Lattice'Class renames
                  Get_Logical (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Logical (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_Or (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_Or (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Logical_Not =>
            return
               Logical_Not
               (  Tree.Location,
                  Get_Logical (Context, Tree.Operands (1).all)
               );
         when Intersection =>
            declare
               Left  : Lattice'Class renames
                  Get_Set (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Set (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_And (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_And (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Union =>
            declare
               Left  : Lattice'Class renames
                  Get_Set (Context, Tree.Operands (1).all);
               Right : Lattice'Class renames
                  Get_Set (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Logical_Or (Tree.Location, Context, Right, Left);
               else
                  return
                     Logical_Or (Tree.Location, Context, Left, Right);
               end if;
            end;
         when Colon =>
            case Tree.Operands'Length is
               when 2 =>
                  declare
                     Left  : Plain.Truth_Value'Class renames
                                Plain.Get_Confidence
                                (  Context,
                                   Tree.Operands (1).all
                                );
                     Right : Plain.Truth_Value'Class renames
                                Plain.Get_Confidence
                                (  Context,
                                   Tree.Operands (2).all
                                );
                  begin
                     return
                        Logic.Intuitionistic.Truth_Value'
                        (  (  Left.Location
                           &  Right.Location
                           &  Tree.Location
                           ),
                           Fuzzy_Boolean'
                           (  Possibility => Left.Value,
                              Necessity   => Right.Value
                        )  );
                  end;
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Too many colon-separated items in square "
                     &  "brackets at "
                     &  Image (Tree.Location)
                  )  );
            end case;
         when Membership =>
            declare
               Left  : Lattice'Class renames
                  Get_Lattice (Context, Tree.Operands (1).all);
               Right : Logical'Class renames
                  Logic.To_Logical
                  (  Get_Logical (Context, Tree.Operands (2).all)
                  );
               use Logic.Intuitionistic;
            begin
               if Right in Plain.Truth_Value'Class then
                  return
                     Cut
                     (  Tree.Location,
                        Context,
                        Left,
                        Plain.Truth_Value'Class (Right).Value
                     );
               else
                  return
                     Cut
                     (  Tree.Location,
                        Context,
                        Left,
                        Truth_Value'Class (Right).Value
                     );
               end if;
            end;
         when Ellipsis =>
            declare
               Left  : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (1).all);
               Right : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Stretch
                     (  Tree.Location,
                        Context,
                        Right,
                        Left,
                        True
                     );
               else
                  return
                     Stretch
                     (  Tree.Location,
                        Context,
                        Left,
                        Right,
                        False
                     );
               end if;
            end;
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Operation "
               &  Image (Tree.Operation)
               &  " is not logical, set-theoretic, range or"
               &  " membership test expected at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Lattice;

   function Do_Bracket
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class is
   begin
      case Tree.Operands'Length is
         when 1 => -- Order bracket
            return Get_Comparable (Context, Tree.Operands (1).all);
         when 2 => -- Membership function point
            return
               Singletons.Get_Singleton
               (  Tree.Location,
                  Context,
                  Tree.Operands (1).all,
                  Tree.Operands (2).all
               );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Too many items in brackets at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Bracket;

   function Do_Component
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class is
   begin
      if Tree.Operands'Length /= 2 then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Too many components in the name at "
            &  Image (Get_Location (Tree))
         )  );
      elsif Tree.Operands (1).all in Identifier'Class then
         declare
            Name : Identifier'Class renames
                       Identifier'Class (Tree.Operands (1).all);
         begin
            if Name.Malformed then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Malformed name '"
                  &  Name.Value
                  &  "' found at "
                  &  Image (Name.Location)
               )  );
            end if;
            if Context.Features /= null then
               declare
                  Nested : Resolution_Context;
               begin
                  Nested.Expected :=
                     Find (Context.Features.all, Name.Value);
                  Nested.Location := Name.Location;
                  Nested.Inversed := Context.Inversed;
                  if Tree.Operands (2).all in Identifier'Class then
                     return
                        Discrete.Get_Point
                        (  Nested,
                           Identifier'Class (Tree.Operands (2).all)
                        );
                  else
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Identifier is expected at "
                        &  Image (Get_Location (Tree.Operands (2).all))
                     )  );
                  end if;
               exception
                  when End_Error =>
                     null;
               end;
            end if;
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unrecognized name '"
               &  Name.Value
               &  "' found at "
               &  Image (Name.Location)
            )  );
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Identifier is expected at "
            &  Image (Get_Location (Tree.Operands (1).all))
         )  );
      end if;
   end Do_Component;

   function Do_Dimension
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class is
   begin
      case Tree.Operands'Length is
         when 1 =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Too less items in square brackets at "
               &  Image (Tree.Location)
            )  );
         when 2 => -- Dimension x [y]
            return
               Set_Dimension
               (  Tree.Location,
                  Context,
                  Get_Comparable (Context, Tree.Operands (1).all),
                  Numerics.Reals.To_Measure
                  (  Numerics.Get_Numeric
                     (  Context,
                        Tree.Operands (2).all
               )  )  );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Too many items in square brackets at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Dimension;

   function Do_Interval
            (  Context : Resolution_Context;
               Tree    : Expression'Class;
               Mode    : Subsetting_Mode
            )  return Lattice'Class is
   begin
      case Tree.Operands'Length is
         when 1 =>
            if Mode = Subset_Mode then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Two items are expected in square brackets at "
                  &  Image (Tree.Location)
               )  );
            end if;
            if (  Tree.Operands (1).all in Expression'Class
               and then
                  (  Expression'Class (Tree.Operands (1).all).Operation
                  =  Colon
               )  )
            then -- Intuinistic truth value
               return Get_Lattice (Context, Tree.Operands (1).all);
            end if;
         when 2 =>
            if Mode = Element_Mode then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Colon ':' is expected in square brackets at "
                  &  Image (Tree.Location)
               )  );
            end if;
            declare
               use Numerics;
               Left : Orders.Numerics.Numeric'Class renames
                  Get_Numeric (Context, Tree.Operands (1).all);
               Right : Orders.Numerics.Numeric'Class renames
                  Get_Numeric (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return
                     Stretch
                     (  Tree.Location,
                        Context,
                        Right,
                        Left,
                        True
                     );
               else
                  return
                     Stretch
                     (  Tree.Location,
                        Context,
                        Left,
                        Right,
                        False
                     );
               end if;
            end;
         when others =>
            null;
      end case;
      case Mode is
         when Element_Mode =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Colon ':' is expected in square brackets at "
               &  Image (Tree.Location)
            )  );
         when Subset_Mode =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Two items are expected in square brackets at "
               &  Image (Tree.Location)
            )  );
         when Universal_Mode =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Two items or colon ':' are expected in "
               &  "square brackets at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Interval;

   function Do_Index
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class is
   begin
      case Tree.Operands'Length is
         when 3 => -- Joining a singleton
            return
               Join
               (  Tree.Location,
                  Context,
                  Get_Comparable (Context, Tree.Operands (1).all),
                  Singletons.Get_Singleton
                  (  Tree.Location,
                     Context,
                     Tree.Operands (2).all,
                     Tree.Operands (3).all
               )  );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Invalid singleton value at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Index;

   function Get_Comparable
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Constant_Value'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames
                      Expression'Class (Tree);
         begin
            case Node.Operation is
               when EQ | NE =>
                  return Do_Comparable (Context, Node);
               when LE | LT | GE | GT =>
                  return Do_Ordered (Context, Node);
               when Concatenate =>
                  return Strings.Do_Text (Context, Node);
               when Lattice_And  | Lattice_Or       |
                    Lattice_Xor  | Colon            |
                    Logical_And  | Logical_Or       |
                    Logical_Not  | Lattice_Not      |
                    Membership   | Ellipsis         |
                    Is_Subset    | Is_In_Subset     |
                    Is_Superset  | Is_Not_Superset  |
                    Has_Superset | Has_Not_Superset |
                    Is_Element   | Is_Not_Element   |
                    Union        | Intersection     |
                    Has_Element  | Has_Not_Element  | Not_In =>
                  return Do_Lattice (Context, Node);
               when Arithmetic =>
                  return Numerics.Do_Numeric (Context, Node);
               when Component =>
                  return Do_Component (Context, Node);
               when Left_Bracket =>
                  return Do_Bracket (Context, Node);
               when Left_Dimension =>
                  return Do_Dimension (Context, Node);
               when Left_Interval =>
                  return Do_Interval (Context, Node, Universal_Mode);
               when Left_Index =>
                  return Do_Index (Context, Node);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Operation "
                     &  Image (Node.Operation)
                     &  " is not operation expected at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Numeric_Literal'Class then
         return Numerics.To_Numeric (Numeric_Literal'Class (Tree));
      elsif Tree in String_Literal'Class then
         return Strings.To_Text (String_Literal'Class (Tree));
      elsif Tree in Character_Literal'Class then
         return Strings.To_Text (Character_Literal'Class (Tree));
      elsif Tree in Identifier'Class then
         return Get_Point (Context, Identifier'Class (Tree));
      elsif Tree in Truth_Literal'Class then
         return Logic.Intuitionistic.
                To_Truth_Value (Truth_Literal'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Operand is expected at "
            &  Image (Term'Class (Tree).Location)
         )  );
      end if;
   end Get_Comparable;

   function Get_Comparable (Tree : Node'Class)
      return Constant_Value'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      return Get_Comparable (Context, Tree);
   end Get_Comparable;

   function Get_Lattice
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames Expression'Class (Tree);
         begin
            case Node.Operation is
               when EQ | NE =>
                  return Do_Comparable (Context, Node);
               when Left_Index =>
                  return Lattice'Class (Do_Index (Context, Node));
               when Logical_And  | Logical_Or       |
                    Lattice_And  | Lattice_Or       | Lattice_Xor |
                    Lattice_Not  | Logical_Not      |
                    Is_Subset    | Is_In_Subset     |
                    Is_Superset  | Is_Not_Superset  |
                    Is_Element   | Is_Not_Element   |
                    Has_Superset | Has_Not_Superset |
                    Has_Element  | Has_Not_Element  |
                    Membership   | Ellipsis         |
                    Intersection | Union            |
                    Colon        | Not_In =>
                  return Do_Lattice (Context, Node);
               when Left_Bracket =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Bracket (Context, Node);
                  begin
                     if Result in Lattice'Class then
                        return Lattice'Class (Result);
                     end if;
                  end;
               when Left_Dimension =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Dimension (Context, Node);
                  begin
                     if Result in Lattice'Class then
                        return Lattice'Class (Result);
                     end if;
                  end;
               when Left_Interval =>
                  return Do_Interval (Context, Node, Universal_Mode);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Operation "
                     &  Image (Node.Operation)
                     &  " is not comparison or logical or lattice"
                     &  " operator expected at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Numeric_Literal'Class then
         declare
            use Numerics;
            Value : Orders.Numerics.Numeric'Class renames
               To_Numeric (Numeric_Literal'Class (Tree));
         begin
            declare
               Level : Confidence;
            begin
               if Value in Parsers.FCL.Code.Orders.Numerics.
                           Integers.Int'Class then
                  Level :=
                     To_Confidence
                     (  Parsers.FCL.Code.Orders.Numerics.
                        Integers.Int'Class (Value).Value
                     );
               elsif Value in Reals.Real'class then
                  Level :=
                     To_Confidence (Reals.Real'Class (Value).Value);
               else
                  raise Data_Error;
               end if;
               return Plain.Truth_Value'(Value.Location, Level);
            exception
               when Data_Error =>
                  null;
            end;
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Truth value [0,1] is expected at "
               &  Image (Value.Location)
            )  );
         end;
      elsif Tree in Identifier'Class then
         return To_Set (Context, Identifier'Class (Tree));
      elsif Tree in Truth_Literal'Class then
         return Logic.Intuitionistic.
                To_Truth_Value (Truth_Literal'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Truth value or subset is expected at "
         &  Image (Get_Location (Tree))
      )  );
   end Get_Lattice;

   function Get_Logical
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames Expression'Class (Tree);
         begin
            case Node.Operation is
               when EQ | NE =>
                  return Do_Comparable (Context, Node);
               when Lattice_And  | Lattice_Or       | Lattice_Xor |
                    Lattice_Not  | Logical_Not      |
                    Logical_And  | Logical_Or       |
                    Is_Subset    | Is_In_Subset     | Membership  |
                    Is_Superset  | Is_Not_Superset  |
                    Has_Superset | Has_Not_Superset |
                    Is_Element   | Is_Not_Element   |
                    Has_Element  | Has_Not_Element  |
                    Colon        | Not_In =>
                  return Do_Lattice (Context, Node);
               when Left_Bracket =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Bracket (Context, Node);
                  begin
                     if Result in Lattice'Class then
                        return Lattice'Class (Result);
                     end if;
                  end;
               when Left_Dimension =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Dimension (Context, Node);
                  begin
                     if Result in Lattice'Class then
                        return Lattice'Class (Result);
                     end if;
                  end;
               when Left_Interval =>
                  return Do_Interval (Context, Node, Element_Mode);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Operation "
                     &  Image (Node.Operation)
                     &  " is not comparison or logical operator"
                     &  " expected at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Numeric_Literal'Class then
         declare
            use Numerics;
            Value : Orders.Numerics.Numeric'Class renames
                    To_Numeric (Numeric_Literal'Class (Tree));
         begin
            declare
               Level : Confidence;
            begin
               if Value in Parsers.FCL.Code.Orders.Numerics.
                           Integers.Int'Class then
                  Level :=
                     To_Confidence
                     (  Parsers.FCL.Code.Orders.Numerics.
                        Integers.Int'Class (Value).Value
                     );
               elsif Value in Reals.Real'class then
                  Level :=
                     To_Confidence (Reals.Real'Class (Value).Value);
               else
                  raise Data_Error;
               end if;
               return Plain.Truth_Value'(Value.Location, Level);
            exception
               when Data_Error =>
                  null;
            end;
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Truth value [0,1] is expected at "
               &  Image (Value.Location)
            )  );
         end;
      elsif Tree in Truth_Literal'Class then
         return Logic.Intuitionistic.
                To_Truth_Value (Truth_Literal'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         "Truth value is expected at " & Image (Get_Location (Tree))
      );
   end Get_Logical;

   function Get_Set
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames Expression'Class (Tree);
         begin
            case Node.Operation is
               when EQ | NE =>
                  return Do_Comparable (Context, Node);
               when Left_Index =>
                  return Lattice'Class (Do_Index (Context, Node));
               when Lattice_And  | Lattice_Or  | Lattice_Xor |
                    Lattice_Not  | Membership  | Ellipsis    |
                    Intersection | Union       | Colon =>
                  return Do_Lattice (Context, Node);
               when Left_Bracket =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Bracket (Context, Node);
                  begin
                     if Result in Lattice'Class then
                        return Lattice'Class (Result);
                     end if;
                  end;
               when Left_Interval =>
                  return Do_Interval (Context, Node, Subset_Mode);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Operation "
                     &  Image (Node.Operation)
                     &  " is not lattice operator expected at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Identifier'Class then
         return To_Set (Context, Identifier'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         "A subset is expected at " & Image (Get_Location (Tree))
      );
   end Get_Set;

   function Image
            (  Context : Resolution_Context;
               Item    : Expression'Class;
               Mode    : Units.Code_Set := UTF8_Set
            )  return String is
   begin
      return Image (Context.Expected.Feature, Item, Mode);
   end Image;

   function Is_Lower (Left : Logical_Term) return Boolean is
      use Intervals;
   begin
      return
      (  Nothing (Left (Has_In))       /= False
      and then
         Universe (Left (Has_Not))     /= False
      and then
         Nothing (Left (Has_Out))      /= False
      and then
         Universe (Left (Has_Not_Out)) /= False
      );
   end Is_Lower;

   function Is_Upper (Left : Logical_Term) return Boolean is
      use Intervals;
   begin
      return
      (  Universe (Left (Has_In))     /= False
      and then
         Nothing (Left (Has_Not))     /= False
      and then
         Universe (Left (Has_Out))    /= False
      and then
         Nothing (Left (Has_Not_Out)) /= False
      );
   end Is_Upper;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Missing operation at "
         &  Image (Location)
      )  );
      return Logic.Plain.Truth_Value'(Logical with 0.0); -- Requred
   end Join;

   function Equal_Logical_Term (Value : Fuzzy.Intuitionistic.Set)
      return Logical_Term is
   begin
      return
      (  Has_In      => Create (Value.Possibility),
         Has_Not     => Create (not Value.Necessity),
         Has_Out     => Create (not Value.Necessity),
         Has_Not_Out => Null_Handle
      );
   end Equal_Logical_Term;

   function Subset_Logical_Term (Value : Fuzzy.Intuitionistic.Set)
      return Logical_Term is
   begin
      return
      (  Has_In      => Create (Value.Possibility),
         Has_Not     => Create (not Value.Necessity),
         Has_Out     => Null_Handle,
         Has_Not_Out => Null_Handle
      );
   end Subset_Logical_Term;

end Parsers.FCL.Code;
