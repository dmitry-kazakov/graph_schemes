--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code                            Luebeck            --
--  Interface                                      Winter, 2005       --
--                                                                    --
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

with Confidence_Factors;  use Confidence_Factors;
with Fuzzy.Feature;       use Fuzzy.Feature;
with Fuzzy.Logic;         use Fuzzy.Logic;
with Units;               use Units;

with Fuzzy.Intuitionistic;
with Intervals;
with Object.Handle;

package Parsers.FCL.Code is
   use Variables_Lists;

   type Dictionary_Ptr is access constant Dictionary;
--
-- Resolution_Context -- The context of name resolution
--
--    Features  - The variables list (a pointer to)
--    Expected  - The expected variable
--    Dimension - Its dimension
--    Location  - Of a reference to the expected variable
--    Inversed  - "not" before the variable
--
   type Resolution_Context is record
      Features  : Dictionary_Ptr;
      Expected  : Variable_Descriptor;
      Dimension : Measure := Np;
      Location  : Parsers.Multiline_Source.Location;
      Inversed  : Boolean := False;
   end record;
--
-- Image -- Conversion of an expression to string
--
--    Context - Parsing context
--    Item    - The expression
--    Mode    - The character set
--
-- Returns :
--
--    String representation of the expression
--
   function Image
            (  Context : Resolution_Context;
               Item    : Expression'Class;
               Mode    : Units.Code_Set := UTF8_Set
            )  return String;
--
-- Logical_Set
--
   type Logical_Set (Cardinality : Positive) is
      new Object.Entity with
   record
      Value : Fuzzy.Set (1..Cardinality);
   end record;
   function Equal
            (  Left  : Logical_Set;
               Right : Object.Entity'Class;
               Flag  : Boolean := False
            )  return Boolean;
   type Logical_Set_Ptr is access Logical_Set'Class;
   package Logical_Set_Handles is
      new Object.Handle
          (  Object_Type     => Logical_Set,
             Object_Type_Ptr => Logical_Set_Ptr
          );
   subtype Logical_Set_Handle is Logical_Set_Handles.Handle;
   function Create (Value : Fuzzy.Set) return Logical_Set_Handle;
--
-- Nothing -- Empty set
--
--    Left - A handle to
--
-- Returns :
--
--    True      - Left is a handle to empty set
--    Uncertain - Left is invalid
--    False     - Otherwise
--
   function Nothing (Left : Logical_Set_Handle)
      return Intervals.Logical;
--
-- Universe -- Universe set
--
--    Left - A handle to
--
-- Returns :
--
--    True      - Left is a handle to the universal set
--    Uncertain - Left is invalid
--    False     - Otherwise
--
   function Universe (Left : Logical_Set_Handle)
      return Intervals.Logical;
   pragma Inline (Create, Nothing, Universe);
   function "and" (Left, Right : Logical_Set_Handle)
      return Logical_Set_Handle;
   function "or"  (Left, Right : Logical_Set_Handle)
      return Logical_Set_Handle;
   use Logical_Set_Handles;
--
-- Logical_Term -- A term of the expression
--
   type Logical_Term is array (Image_Type) of Logical_Set_Handle;

   function "="   (Left, Right : Logical_Term) return Boolean;
   function "and" (Left, Right : Logical_Term) return Logical_Term;
   function "or"  (Left, Right : Logical_Term) return Logical_Term;
   function "not" (Left : Logical_Term) return Logical_Term;
   pragma Inline ("=", "and", "or", "not");
   function Is_Lower (Left : Logical_Term) return Boolean;
   function Is_Upper (Left : Logical_Term) return Boolean;
--
-- Image -- Conversion to string
--
--    Feature - The feature of the logical term
--    Term    - The logical term value
--
-- Returns :
--
--    A string representation of Term
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or feature
--
   function Image
            (  Feature : Feature_Handle;
               Term    : Logical_Term
            )  return String;

   type Identifier_Ptr is access constant Identifier'Class;
   for Identifier_Ptr'Storage_Pool use Node_Ptr'Storage_Pool;
   type Literal_Ptr is access constant Literal'Class;
   for Literal_Ptr'Storage_Pool use Node_Ptr'Storage_Pool;
--
-- Constant_Value -- Evaluated expression values
--
   type Constant_Value is abstract new Term with null record;
--
-- Preference -- Type implementation preference
--
-- Types  derived  from   Constant_Value   are   ordered   to   simplify
-- implementation  of  double-dispatch.  All   dyadic   operations   are
-- dispatching in the first parameter. They re-dispatch to an equivalent
-- operation  with  commuted  parameters when the preference of the left
-- parameter is less than one of the second.
--
   type Preference is
        (  Integer_Preference,
           Integer_Singleton_Preference,
           Integer_Range_Preference,
           Integer_Set_Preference,
           Domain_Point_Preference,
           Fuzzy_Set_Preference,
           Real_Preference,
           Real_Singleton_Preference,
           Real_Range_Preference,
           Real_Set_Preference,
           Literal_String_Preference,
           Result_String_Preference,
           Fuzzy_Logical_Preference,
           Intuitionistic_Logical_Preference,
           Intuitionistic_Set_Preference
        );
--
-- < -- Preference's order
--
   function "<" (Left, Right : Preference) return Boolean;
--
-- Get_Preference -- Get type preference
--
--    Left - A constant value
--
-- Returns :
--
--    The preference
--
   function Get_Preference (Left : Constant_Value)
      return Preference is abstract;
--
-- Do_Bracket -- Evaluate a bracket expression
--
--    Context - The name resolution context
--    Tree    - The expression (brackets)
--
-- Returns :
--
--    The result
--
   function Do_Bracket
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class;
--
-- Get_Comparable -- Evaluate an expression with comparable result
--
--    Context - The name resolution context
--    Tree    - The expression
--
-- Returns :
--
--    The result
--
   function Get_Comparable
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Constant_Value'Class;
--
-- Get_Comparable -- Evaluate an expression with comparable result
--
--    Tree - The expression
--
-- The function creates a context as necessary. Then  it  evaluates  the
-- expression and returns its comparable equivalent.
--
-- Returns :
--
--    The result
--
   function Get_Comparable (Tree : Node'Class)
      return Constant_Value'Class;
--
-- Do_Dimension -- Evaluate a dimension assignment expression
--
--    Context - The name resolution context
--    Tree    - The expression: <value> [<dimension>]
--
-- Returns :
--
--    The result
--
   function Do_Dimension
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Constant_Value'Class;
--
-- Operations on constant values
--
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is abstract;
   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value;
               Right    : Measure
            )  return Constant_Value'Class is abstract;
--
-- Equal_Logical_Term            -- Convert value to a logical term
-- Greater_Or_Equal_Logical_Term
-- Greater_Logical_Term
-- Subset_Logical_Term
--
--    Location - Of the operation
--    Context  - The name resolution context
--    Value    - The value to convert
--
-- Returns :
--
--    The logical term
--
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Constant_Value
            )  return Logical_Term is abstract;
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Constant_Value
            )  return Logical_Term is abstract;
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Constant_Value
            )  return Logical_Term is abstract;
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Constant_Value
            )  return Logical_Term is abstract;
--
-- Lattice -- Abstract lattice
--
   type Lattice is abstract new Constant_Value with null record;
--
-- Lattice operations
--
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Lattice;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is abstract;
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Lattice;
               Right    : Confidence
            )  return Lattice'Class is abstract;
   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Lattice;
               Right    : Lattice'Class
            )  return Lattice'Class is abstract;
   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Lattice
            )  return Lattice'Class is abstract;
   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Lattice;
               Right    : Lattice'Class
            )  return Lattice'Class is abstract;
   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Lattice;
               Right    : Lattice'Class
            )  return Lattice'Class is abstract;
--
-- Get_Lattice -- Evaluation of a lattice value
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The result is a set or logical value
--
   function Get_Lattice
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class;
--
-- Get_Logical -- Evaluation of a logical value
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The result is a logical value
--
   function Get_Logical
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class;
--
-- Get_Set -- Evaluation of a set value
--
--    Context - The name resolution context
--    Tree    - The operation
--
-- Returns :
--
--    The result is a set or logical value
--
   function Get_Set
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Lattice'Class;
--
-- Dimension_Image -- Dimension specification
--
--    Context - The messaging parameters of
--    Value   - A unit or any of the dimension to show
--
-- Returns :
--
--    A string describing the dimension
--
   function Dimension_Image
            (  Context : Resolution_Context;
               Value   : Unit
            )  return String;
   function Dimension_Image
            (  Context : Resolution_Context;
               Value   : Measure
            )  return String;
--
-- Comparable -- Comparable values
--
   type Comparable is abstract new Constant_Value with null record;
--
-- Equal_Logical_Term  -- Convert set value to a logical term
-- Subset_Logical_Term
--
--    Value - The value to convert (intuitionistic set)
--
-- Returns :
--
--    The images corresponding to the value as a logical term
--
   function Equal_Logical_Term (Value : Fuzzy.Intuitionistic.Set)
      return Logical_Term;
   function Subset_Logical_Term (Value : Fuzzy.Intuitionistic.Set)
      return Logical_Term;
--
-- Subsetting_Mode -- Operation mode
--
   type Subsetting_Mode is (Subset_Mode, Element_Mode, Universal_Mode);

end Parsers.FCL.Code;
