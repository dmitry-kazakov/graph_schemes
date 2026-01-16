--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Predicates                 Luebeck            --
--  Interface                                      Spring, 2005       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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
--
--  This  package  is  used  for conversion of a logical expression with
--  and, or, xor, not operations into a disjunctive normal form: 
--
--     (a and b and c) or (d and e) ...
--
--  The  terms of the form can be inversed using not-operator. They also
--  may repeat.
--
with Generic_Map;
with Generic_Set;

package Parsers.FCL.Code.Predicates is
--
-- Logical_Form -- A logical expression
--
-- The expression is represented as either  conjunctive  or  disjunctive
-- normal  form.  It  can  be  converted  to  the  disjunctive from when
-- necessary. 
--
   type Logical_Form is new Term with private;
--
-- Get_Feature -- Get the list of features
--
--    Form     - The logical form
--    Junction - The junction number: 1..Get_Size (Form)
--    Term     - The term number: 1..Get_Size (Form, Junction)
--
-- Returns :
--
--    The feature used in the junction
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Feature
            (  Form     : Logical_Form;
               Junction : Positive;
               Term     : Positive
            )  return Feature_Handle;
--
-- Get_Predicate -- Evaluate a predicate
--
--    Context / Features - The name resolution context
--    Tree               - The expression
--
-- Returns :
--
--    The logical form of the predicate
--
   function Get_Predicate
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Logical_Form;
   function Get_Predicate
            (  Features : Dictionary;
               Tree     : Node'Class
            )  return Logical_Form;
--
-- Get_Size -- Get number of junctions in the form
--
--    Form - The logical form
--
-- Returns :
--
--    The number, for example 3 for (A and B) or (C and D) or E
--
   function Get_Size (Form : Logical_Form) return Natural;
--
-- Get_Size -- Get number terms in a junctions in the form
--
--    Form     - The logical form
--    Junction - The junction number: 1..Get_Size (Form)
--
-- Returns :
--
--    The number terms in the junction
--
   function Get_Size (Form : Logical_Form; Junction : Positive)
      return Natural;
--
-- Get_Term -- Get term
--
--    Form     - The logical form
--    Junction - The junction number: 1..Get_Size (Form)
--    Term     - The term number: 1..Get_Size (Form, Junction)
--
-- Returns :
--
--    The term
--
-- Exceptions :
--
--    Constraint_Error - Wrong feature
--
   function Get_Term
            (  Form     : Logical_Form;
               Junction : Positive;
               Term     : Positive
            )  return Logical_Term;
--
-- Logical operations
--
   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form;
   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form
            )  return Logical_Form;
   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form;
   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Logical_Form;
               Right    : Logical_Form
            )  return Logical_Form;
--
-- Normalize -- Convert to a disjunctive form
--
--    Left - The expression to convert
--
-- Returns :
--
--    A DNF equivalent of Left
--
   function Normalize (Left : Logical_Form) return Logical_Form;
--
-- Image -- Overrides Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Logical_Form;
               Mode    : Code_Set
            )  return String;
 
private

   package Conjunctions is
      new Generic_Map
          (  Key_Type    => Feature_Handle,
             Object_Type => Logical_Term
          );
--
-- Logical_Conjunction -- Conjunction of logical terms
--
   subtype Logical_Conjunction is Conjunctions.Map;
   
   Null_Conjunction : Logical_Conjunction;

   function "=" (Left, Right : Logical_Conjunction) return Boolean;
   function "<" (Left, Right : Logical_Conjunction) return Boolean;
   
   package Disjunctions is
      new Generic_Set (Logical_Conjunction, Null_Conjunction);
--
-- Form_Type -- The type of the form
--
-- Conjunctive form is and-or, disjunctive is or-and form.
--
   type Form_Type is (CNF, DNF);
--
-- Logical_Form -- A disjunctive or conjunctive form
--
--    Form  - The type of
--
   type Logical_Form is new Term with record
      Form  : Form_Type := DNF;
      Value : Disjunctions.Set;
   end record;

end Parsers.FCL.Code.Predicates;
