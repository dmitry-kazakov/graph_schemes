--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Predicates                 Luebeck            --
--  Implementation                                 Spring, 2005       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Integer_Intervals;          use Integer_Intervals;
with Strings_Edit;               use Strings_Edit;

with Fuzzy.Feature.Domain_Float_Handle;
use  Fuzzy.Feature.Domain_Float_Handle;

package body Parsers.FCL.Code.Predicates is
   use Conjunctions;
   use Disjunctions;

   type Comparison_Result is (Less, Equal, Greater);
--
-- And_At, Not_At, Or_At, Xor_At -- Logical operators
--
--    Left    - Accumulator, expression
--    Feature - Feature of the logical term
--    Image   - Of the feature to be set
--    Value   - Of the image
--
-- These procedures are called to evaluate:
--
--    Left and Term
--    Left or  Term
--    Left xor Term
--
-- Where Term has the  form  of  any  of  the  images  Has_In,  Has_Out,
-- Has_Not, Has_Not_Out. The following table defines the translations of
-- the operations into images:
--
-- 1. Feature  is Value. This is a classification of Feature as a subset
--    of Value
--       Has_In  := Value
--       Has_Not := not Value
--
-- 2. Value is Feature. This is an estimation of Feature as  a  superset
--    of Value
--       Has_In  := Value
--       Has_Out := not Value
--
-- 3. Feature is not Value <=> not (Feature is Value)
--       Has_Not := Value
--       Has_In  := not Value
--
-- 4. not Feature is Value
--       Has_Out     := Value
--       Has_Not_Out := not Value
--
-- 5. not Feature is not Value <=> not (not Feature is Value)
--       Has_Not_Out := Value
--       Has_Out     := not Value
--
-- 6. Value is not Feature <=> not (Value is Feature)
--       Has_Out := Value
--       Has_In  := not Value
--
-- 7. not Value is Feature
--       Has_Not     := Value
--       Has_Not_Out := not Value
--
-- 8. not Value is not Feature <=> not (not Value is Feature)
--       Has_Not_Out := Value
--       Has_Not     := not Value
--
-- 9. Feature = Value <=> Feature is Value and Value is Feature
--
-- De Morgan's rule not (A is B) <=> A is not B
--
-- When Value is an intuitionistic set, then in above Value is its upper
-- set and not Value is the complement of its lower set.
--
-- And_At, Or_At, Xor_At -- Logical operators
--
--    Left  - Accumulator, expression
--    Right - Operand, expression
--
   procedure And_At (Left : in out Logical_Form; Right : Logical_Form);
   procedure Or_At  (Left : in out Logical_Form; Right : Logical_Form);
   procedure Xor_At (Left : in out Logical_Form; Right : Logical_Form);
--
-- not -- Logical inversion
--
--    Left - To invert
--
-- Returns :
--
--    not Left
--
   function "not" (Left : Logical_Form) return Logical_Form;
--
-- Do_Predicate -- Evaluate a logical operation on predicates
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The logical form of the result predicate
--
   function Do_Predicate
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logical_Form;

   function Compare (Left, Right : Logical_Set_Handle)
      return Comparison_Result;
   function Compare (Left, Right : Logical_Term)
      return Comparison_Result;
   function Compare (Left, Right : Logical_Conjunction)
      return Comparison_Result;
   pragma Inline (Compare);

   function Compare (Left, Right : Logical_Set_Handle)
      return Comparison_Result is
      X : constant Logical_Set_Ptr := Ptr (Left);
      Y : constant Logical_Set_Ptr := Ptr (Right);
      J : Integer;
   begin
      if X = null then
         if Y = null then
            return Equal;
         else
            return Less;
         end if;
      elsif Y = null then
         return Greater;
      elsif X.Value'Length /= Y.Value'Length then
         if X.Value'Length < Y.Value'Length then
            return Less;
         else
            return Greater;
         end if;
      else
         J := Y.Value'First;
         for I in X.Value'Range loop
            if X.Value (I) /= Y.Value (J) then
               if X.Value (I) < Y.Value (J) then
                  return Less;
               else
                  return Greater;
               end if;
            end if;
            J := J + 1;
         end loop;
         return Equal;
      end if;
   end Compare;

   function Compare (Left, Right : Logical_Term)
      return Comparison_Result is
   begin
      case Compare (Left (Has_In), Right (Has_In)) is
         when Less    => return Less;
         when Equal   => null;
         when Greater => return Greater;
      end case;
      case Compare (Left (Has_Not), Right (Has_Not)) is
         when Less    => return Greater;
         when Equal   => null;
         when Greater => return Less;
      end case;
      case Compare (Left (Has_Out), Right (Has_Out)) is
         when Less    => return Less;
         when Equal   => null;
         when Greater => return Greater;
      end case;
      case Compare (Left (Has_Not_Out), Right (Has_Not_Out)) is
         when Less    => return Greater;
         when Equal   => null;
         when Greater => return Less;
      end case;
      return Equal;
   end Compare;

   function Compare (Left, Right : Logical_Conjunction)
      return Comparison_Result is
   begin
      if Get_Size (Left) /= Get_Size (Right) then
         if Get_Size (Left) < Get_Size (Right) then
            return Less;
         else
            return Greater;
         end if;
      end if;
      for Index in 1..Get_Size (Left) loop
         if Get_Key (Left, Index) /= Get_Key (Right, Index) then
            if Get_Key (Left, Index) < Get_Key (Right, Index) then
               return Less;
            else
               return Greater;
            end if;
         end if;
         case Compare (Get (Left, Index), Get (Right, Index)) is
            when Less    => return Less;
            when Equal   => null;
            when Greater => return Greater;
         end case;
      end loop;
      return Equal;
   end Compare;

   function "=" (Left, Right : Logical_Conjunction) return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "<" (Left, Right : Logical_Conjunction) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function Get_Size (Form : Logical_Form) return Natural is
   begin
      return Get_Size (Form.Value);
   end Get_Size;

   function Get_Size
            (  Form     : Logical_Form;
               Junction : Positive
            )  return Natural is
   begin
      return Get_Size (Get (Form.Value, Junction));
   end Get_Size;

   function Get_Feature
            (  Form     : Logical_Form;
               Junction : Positive;
               Term     : Positive
            )  return Feature_Handle is
   begin
      return Get_Key (Get (Form.Value, Junction), Term);
   end Get_Feature;

   function Get_Term
            (  Form     : Logical_Form;
               Junction : Positive;
               Term     : Positive
            )  return Logical_Term is
   begin
      return Get (Get (Form.Value, Junction), Term);
   end Get_Term;

   generic
      This_Form : Form_Type;
      with function Is_Lower (Left : Logical_Term) return Boolean;
      with function Is_Upper (Left : Logical_Term) return Boolean;
      with function "and" (Left, Right : Logical_Term)
         return Logical_Term;
      with function "or"  (Left, Right : Logical_Term)
         return Logical_Term;
      with function ">=" (Left, Right : Logical_Term) return Boolean;
   package Lattice is
      procedure Or_At
                (  Left  : in out Logical_Form;
                   Right : Logical_Form
                );
      function Normalize (Disjunction : Logical_Form)
         return Logical_Form;
      function "not" (Left : Logical_Form) return Logical_Form;
   end Lattice;

   function ">=" (Left, Right : Logical_Term) return Boolean is
   begin
      case Compare (Left, Right) is
         when Less            => return False;
         when Equal | Greater => return True;
      end case;
   end ">=";

   function "<=" (Left, Right : Logical_Term) return Boolean is
   begin
      case Compare (Left, Right) is
         when Greater      => return False;
         when Equal | Less => return True;
      end case;
   end "<=";

   package body Lattice is separate;
   package DNFs is
      new Lattice (DNF, Is_Lower, Is_Upper, "and", "or", ">=");
   package CNFs is
      new Lattice (CNF, Is_Upper, Is_Lower, "or", "and", "<=");

   procedure And_At (Left : in out Logical_Form; Right : Logical_Form)
      renames CNFs.Or_At;

   procedure Or_At (Left : in out Logical_Form; Right : Logical_Form)
      renames DNFs.Or_At;

   function "not" (Left : Logical_Form) return Logical_Form is
   begin
      if Left.Form = CNF then
         return DNFs."not" (Left);
      else
         return CNFs."not" (Left);
      end if;
   end "not";

   procedure Xor_At (Left : in out Logical_Form; Right : Logical_Form) is
      Not_Left  : Logical_Form := not Left;
      Not_Right : constant Logical_Form := not Right;
   begin
      And_At (Left, Not_Right);
      And_At (Not_Left, Right);
      Or_At  (Left, Not_Left);
   end Xor_At;

   function Normalize (Left : Logical_Form) return Logical_Form is
   begin
      if Left.Form = DNF then
         return Left;
      else
         return DNFs.Normalize (Left);
      end if;
   end Normalize;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form is
      Result : Logical_Form;
   begin
      Result.Form     := Left.Form;
      Result.Value    := Left.Value;
      Result.Location := Left.Location & Location & Right.Location;
      And_At (Result, Right);
      return Result;
   end Logical_And;

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form
            )  return Logical_Form is
      Result : Logical_Form := not Left;
   begin
      Result.Location := Left.Location & Location;
      return not Left;
   end Logical_Not;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form is
      Result : Logical_Form;
   begin
      Result.Form     := Left.Form;
      Result.Value    := Left.Value;
      Result.Location := Left.Location & Location & Right.Location;
      Or_At (Result, Right);
      return Result;
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form is
      Result : Logical_Form;
   begin
      Result.Form     := Left.Form;
      Result.Value    := Left.Value;
      Result.Location := Left.Location & Location & Right.Location;
      Xor_At (Result, Right);
      return Result;
   end Logical_Xor;

   type Identifier_Ptr is access constant Identifier'Class;
   for Identifier_Ptr'Storage_Pool use Node_Ptr'Storage_Pool;
   type Alternative is record
      Variable : Variable_Descriptor;
      Name     : Identifier_Ptr;
      Index    : Integer;
      Inversed : Boolean := False;
   end record;
   type Alternatives_List is array (Positive range <>) of Alternative;

   procedure Set_Scale (Context : in out Resolution_Context) is
   begin
      if Is_Domain_Float (Context.Expected.Feature) then
         Context.Dimension := Get_Scale (Context.Expected.Feature);
      else
         Context.Dimension := Np;
      end if;
   end Set_Scale;

   procedure Do_Context
             (  Context  : in out Resolution_Context;
                Tree     : Expression'Class;
                Resolved : out Positive
             )  is
      List     : Alternatives_List (1..Tree.Operands'Length);
      Count    : Natural := 0;
      Inversed : Boolean;

      procedure Down (Tree : Node'Class; Index : Positive) is
      begin
         if Tree in Identifier'Class then
            declare
               Name : Identifier'Class renames Identifier'Class (Tree);
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
               List (Count + 1).Variable :=
                  Find (Context.Features.all, Name.Value);
               Count := Count + 1;
               List (Count).Name     := Name'Unchecked_Access;
               List (Count).Inversed := Inversed;
               List (Count).Index    := Index;
            exception
               when End_Error =>
                  null;
            end;
         elsif Tree in Expression'Class then
            declare
               Node : Expression'Class renames Expression'Class (Tree);
            begin
               case Node.Operation is
                  when Logical_Not | Lattice_Not =>
                     Inversed := not Inversed;
                     Down (Node.Operands (1).all, Index);
                  when Not_In =>
                     if Node.Operands'Length = 1 then
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "Misplaced 'not in' found at "
                           &  Image (Node.Location)
                        )  );
                     end if;
                  when Left_Bracket =>
                     if Node.Operands'Length = 1 then
                        Down (Node.Operands (1).all, Index);
                     end if;
                  when others =>
                     null;
               end case;
            end;
         end if;
      end Down;
   begin
      for Index in Tree.Operands'Range loop
         Inversed := False;
         Down (Tree.Operands (Index).all, Index);
      end loop;
      case Count is
         when 0 => -- Nothing resolved
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "No variable found among the operands "
               &  "of the operation at "
               &  Image (Tree.Location)
            )  );
         when 1 => -- There is only one
            Context.Expected := List (1).Variable;
            Context.Location := List (1).Name.Location;
            Context.Inversed := List (1).Inversed;
            Resolved         := List (1).Index;
            Set_Scale (Context);
         when others =>
            declare
               Conflicting  : array (1..Count) of Positive;
               Accepted     : Natural := 0;
               Domain_Value : Interval;
            begin
               for Current in 1..Count loop
                  begin
                     for Other in 1..Count loop
                        if Other /= Current then
                           Domain_Value :=
                              Value
                              (  List (Other).Name.Value,
                                 List (Current).Variable.Feature
                              );
                        end if;
                     end loop;
                     Accepted := Accepted + 1;
                     Conflicting (Accepted) := Current;
                  exception
                     when End_Error =>
                        null;
                  end;
               end loop;
               if Accepted > 1 then
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "More than one variable appears in "
                     &  "the operands of the operation at "
                     &  Image (Tree.Location)
                  )  );
               elsif Accepted = 0 then
                  Accepted := Count - 1; -- Take any, messages later
               end if;
               Context.Expected := List (Accepted).Variable;
               Context.Location := List (Accepted).Name.Location;
               Context.Inversed := List (Accepted).Inversed;
               Resolved         := List (Accepted).Index;
               Set_Scale (Context);
            end;
      end case;
   end Do_Context;

   function Do_Predicate
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logical_Form is
   begin
      case Tree.Operation is
         when Lattice_And | Logical_And =>
            return
               Logical_And
               (  Tree.Location,
                  Get_Predicate (Context, Tree.Operands (1).all),
                  Get_Predicate (Context, Tree.Operands (2).all)
               );
         when Lattice_Or | Logical_Or =>
            return
               Logical_Or
               (  Tree.Location,
                  Get_Predicate (Context, Tree.Operands (1).all),
                  Get_Predicate (Context, Tree.Operands (2).all)
               );
         when Lattice_Xor =>
            return
               Logical_Xor
               (  Tree.Location,
                  Get_Predicate (Context, Tree.Operands (1).all),
                  Get_Predicate (Context, Tree.Operands (2).all)
               );
         when Logical_Not | Lattice_Not  =>
            return
               Logical_Not
               (  Tree.Location,
                  Get_Predicate (Context, Tree.Operands (1).all)
               );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Logical operator is expected at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Predicate;

   function Do_Bracket
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logical_Form is
   begin
      case Tree.Operands'Length is
         when 1 => -- Order bracket
            return Get_Predicate (Context, Tree.Operands (1).all);
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Too many items in brackets at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Bracket;

   function To_Predicate
            (  Context  : Resolution_Context;
               Value    : Logical_Term;
               Swap     : Boolean;
               Inverse  : Boolean;
               Location : Parsers.Multiline_Source.Location
            )  return Logical_Form is
      Result      : Logical_Form;
      Conjunction : Logical_Conjunction;
      Term        : Logical_Term := Value;
   begin
      Result.Location := Location;
      if Swap then
         if Context.Inversed xor Inverse then
            Term := -- V in not X
               (  Has_In      => Value (Has_Out),
                  Has_Not     => Value (Has_In),
                  Has_Out     => Value (Has_Not_Out),
                  Has_Not_Out => Value (Has_Not)
               );
         else
            Term := -- V in X
               (  Has_In      => Value (Has_In),
                  Has_Not     => Value (Has_Out),
                  Has_Out     => Value (Has_Not),
                  Has_Not_Out => Value (Has_Not_Out)
               );
         end if;
      else
         if Context.Inversed then
            if Inverse then
               Term := -- not X in not V
                  (  Has_In      => Value (Has_Not_Out),
                     Has_Not     => Value (Has_Out),
                     Has_Out     => Value (Has_Not),
                     Has_Not_Out => Value (Has_In)
                  );
            else
               Term := -- not X in V
                  (  Has_In      => Value (Has_Out),
                     Has_Not     => Value (Has_Not_Out),
                     Has_Out     => Value (Has_In),
                     Has_Not_Out => Value (Has_Not)
                  );
            end if;
         else
            if Inverse then
               Term := -- X in not V
                  (  Has_In      => Value (Has_Not),
                     Has_Not     => Value (Has_In),
                     Has_Out     => Value (Has_Not_Out),
                     Has_Not_Out => Value (Has_Out)
                  );
            else
               Term := -- X in V
                  Value;
            end if;
         end if;
      end if;
      if not Is_Lower (Term) then
         Add (Conjunction, Context.Expected.Feature, Term);
         Add (Result.Value, Conjunction);
      end if;
      return Result;
   end To_Predicate;

   function Get_Predicate
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Logical_Form is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames Expression'Class (Tree);
         begin
            case Node.Operation is
               when EQ | NE =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => False,
                           Inverse  => Node.Operation = NE,
                           Location => Get_Location (Node),
                           Value    =>
                              Equal_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Is_Subset | Is_In_Subset =>
                  if Node.Operands (2).all in Expression'Class then
                     declare
                        Right : Expression'Class renames
                           Expression'Class (Node.Operands (2).all);
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
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => Index /= 1,
                           Inverse  => False,
                           Location => Get_Location (Node),
                           Value    =>
                              Subset_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Has_Superset | Is_Element  =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => Index /= 1,
                           Inverse  => False,
                           Location => Get_Location (Node),
                           Value    =>
                              Subset_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Has_Not_Superset | Is_Not_Element =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => Index /= 1,
                           Inverse  => True,
                           Location => Get_Location (Node),
                           Value    =>
                              Subset_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Is_Superset | Has_Element =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => Index = 1,
                           Inverse  => False,
                           Location => Get_Location (Node),
                           Value    =>
                              Subset_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Is_Not_Superset | Has_Not_Element =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     return
                        To_Predicate
                        (  Context  => Frame,
                           Swap     => Index = 1,
                           Inverse  => True,
                           Location => Get_Location (Node),
                           Value    =>
                              Subset_Logical_Term
                              (  Node.Location,
                                 Frame,
                                 Get_Comparable
                                 (  Frame,
                                    Node.Operands (3 - Index).all
                        )     )  );
                  end;
               when Not_In =>
                  if Node.Operands'Length = 2 then
                     declare
                        Frame : Resolution_Context := Context;
                        Index : Positive;
                     begin
                        Do_Context (Frame, Node, Index);
                        return
                           To_Predicate
                           (  Context  => Frame,
                              Swap     => Index /= 1,
                              Inverse  => True,
                              Location => Get_Location (Node),
                              Value    =>
                                 Subset_Logical_Term
                                 (  Node.Location,
                                    Frame,
                                    Get_Comparable
                                    (  Frame,
                                       Node.Operands (3 - Index).all
                           )     )  );
                     end;
                  else
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Misplaced 'not in' found at "
                        &  Image (Node.Location)
                     )  );
                  end if;
               when GE | LT | LE | GT =>
                  declare
                     Frame : Resolution_Context := Context;
                     Index : Positive;
                  begin
                     Do_Context (Frame, Node, Index);
                     if (  (Index = 1 and then Node.Operation in GE..LT)
                        or else
                           (Index = 2 and then Node.Operation in LE..GT)
                        )
                     then
                        return
                           To_Predicate
                           (  Context  => Frame,
                              Swap     => False,
                              Location => Get_Location (Node),
                              Inverse  =>
                                 (  Node.Operation = LT
                                 or else
                                    Node.Operation = GT
                                 ),
                              Value    =>
                                 Greater_Or_Equal_Logical_Term
                                 (  Node.Location,
                                    Frame,
                                    Get_Comparable
                                    (  Frame,
                                       Node.Operands (3 - Index).all
                           )     )  );
                     else
                        return
                           To_Predicate
                           (  Context  => Frame,
                              Swap     => False,
                              Location => Get_Location (Node),
                              Inverse  =>
                                 (  Node.Operation = GE
                                 or else
                                    Node.Operation = LE
                                 ),
                              Value    =>
                                 Greater_Logical_Term
                                 (  Node.Location,
                                    Frame,
                                    Get_Comparable
                                    (  Frame,
                                       Node.Operands (3 - Index).all
                           )     )  );
                     end if;
                  end;
               when Lattice_Not | Logical_Not |
                    Logical_And | Logical_Or  |
                    Lattice_And | Lattice_Or  | Lattice_Xor =>
                  return Do_Predicate (Context, Node);
               when Left_Bracket =>
                  return Do_Bracket (Context, Node);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Operation "
                     &  Image (Node.Operation)
                     &  " is not a relational, logical, "
                     &  " membership test expected operation at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Logical expression term is expected at "
            &  Image (Term'Class (Tree).Location)
         )  );
      end if;
   end Get_Predicate;

   function Get_Predicate
            (  Features : Dictionary;
               Tree     : Node'Class
            )  return Logical_Form is
      Context : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      return Get_Predicate (Context, Tree);
   end Get_Predicate;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Conjunction : Logical_Conjunction;
                Operation   : String
             )  is
      First : Boolean := True;
   begin
      for Index in 1..Get_Size (Conjunction) loop
         if First then
            First := False;
         else
            Put (Destination, Pointer, Operation);
         end if;
         Put
         (  Destination,
            Pointer,
            Image
            (  Get_Key (Conjunction, Index),
               Get (Conjunction, Index)
         )  );
      end loop;
   end Put;

   procedure Put
             (  Destination   : in out String;
                Pointer       : in out Integer;
                Value         : Logical_Form;
                Or_Operation  : String;
                And_Operation : String
             )  is
      First : Boolean := True;
   begin
      for Index in 1..Get_Size (Value) loop
         if First then
            First := False;
         else
            Put (Destination, Pointer, Or_Operation);
         end if;
         Put (Destination, Pointer, "(");
         Put
         (  Destination,
            Pointer,
            Get (Value.Value, Index),
            And_Operation
         );
         Put (Destination, Pointer, ")");
      end loop;
   end Put;

   function Image
            (  Feature : Feature_Handle;
               Item    : Logical_Form;
               Mode    : Code_Set
            )  return String is
      Length : Positive := 1024;
   begin
      if Get_Size (Item.Value) = 0 then
         return "nothing";
      end if;
      loop
         declare
            Destination : String (1..Length);
            Pointer     : Integer := Destination'First;
         begin
            if Item.Form = DNF then
               Put (Destination, Pointer, Item, " or ", " and ");
            else
               Put (Destination, Pointer, Item, " and ", " or ");
            end if;
            return Destination (Destination'First..Pointer - 1);
         exception
            when Layout_Error =>
               Length := Length * 2;
         end;
      end loop;
   end Image;

end Parsers.FCL.Code.Predicates;
