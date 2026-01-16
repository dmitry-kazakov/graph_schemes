--                                                                    --
--  package Parsers.FCL             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2005       --
--                                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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
--  This package provides a FCL expression parser. The result of parsing
--  is stored in a parsing tree allocated on a stack pool.
--
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Fuzzy.Feature.Handle;      use Fuzzy.Feature.Handle;
with Parsers.Multiline_Source;  use Parsers.Multiline_Source;
with Strings_Edit.UTF8;         use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;    use Strings_Edit.UTF8.Maps;

with Ada.Unchecked_Deallocation;
with Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Domain_Integers;
with Fuzzy.Logic;
with Parsers.Generic_Token.Segmented_Lexer;
with Stack_Storage;
with Strings_Edit;
with Tables.UTF8_Names;
with Units;

package Parsers.FCL is
   use Fuzzy.Feature.Domain_Floats.Float_Measures;
   use Fuzzy.Feature.Domain_Integers;

   function Is_Blank (Value : UTF8_Code_Point) return Boolean;
   function Is_Identifier_Body (Value : UTF8_Code_Point) return Boolean;
   function Is_Unit_Symbol (Value : UTF8_Code_Point) return Boolean;

   Max_Cardinality  : constant := 100_000;
   Blank_Characters : constant Unicode_Set := To_Set (Is_Blank'Access);
--
-- Get -- A keyword from the source
--
--    Code - To parse
--    Text - The keyword (case insensitive)
--
-- Returns :
--
--    True if Text was recognized and skipped in Code
--
   function Get (Code : access Source'Class; Text : String)
      return Boolean;
--
-- Get -- A name from the source
--
--    Code   - To parse
--    Object - The object's name (used in error messages)
--
-- Returns :
--
--    The name
--
   function Get (Code : access Source'Class; Object : String)
      return String;
--
-- Operations -- All the operations supported
--
   type Operations is
        (     -- Operators according to ARM 4.5
           Logical_And, Logical_Or,              -- Logical
           Lattice_And, Lattice_Or, Lattice_Xor, -- Lattice
           EQ, NE, GE, LT, LE, GT,               -- Relational
           Intersection, Union,               -- Set-theoretic
           Is_Subset,                         -- Membership test
           Is_Superset,                       -- Superset
           Is_Not_Superset,                   -- Not a superset
           Is_Element,                        -- Member test
           Is_Not_Element,                    -- Not a member test
           Has_Superset,                      -- Commuted superset
           Has_Not_Superset,                  -- Commuted not a superset
           Has_Element,                       -- Commuted member test
           Has_Not_Element,                   -- Inversed
           Concatenate, Add, Sub,             -- Binary adding
           Plus, Minus,                       -- Unary adding
           Mul, Div,                          -- Multiplying
           Pow_0, Pow_1, Pow_2, Pow_3, Pow_4, -- Postfix powers
           Pow_5, Pow_6, Pow_7, Pow_8, Pow_9,
           Pow_Minus, Pow_Plus,
           Postfix_Pow,
           Pow, Abs_Value,           -- Highest precedence
           Logical_Not, Lattice_Not,
              -- Hard-wired operators
           Ellipsis,                 -- Range ".."
           Component,                -- Component extraction "."
           Membership,               -- Alpha cut ":"
              -- Brackets
           Left_Bracket,  Right_Bracket,  -- Brackets ()
           Left_Interval, Right_Interval, -- Brackets []
           Left_Index,                    -- Brackets f()
           Left_Dimension,                -- Brackets f[]
              -- Commas and ligatures
           Comma, Colon,                  -- ",", ":"
              -- Inverses
           Add_Inv, Mul_Inv,              -- 0-x, 1/x
              -- Keywords
           Reserved,                      -- "then", "else"
           Is_In_Subset,                  -- ["is" +] "in"
           Not_In,                        -- "not" + "in"
           Not_Subset,                    -- Infix "not"
           Member                         -- "in"
        );
   subtype Relational  is Operations range EQ..GT;
   subtype Additive    is Operations range Concatenate..Sub;
   subtype Unary       is Operations range Plus..Minus;
   subtype Multiplying is Operations range Mul..Div;
   subtype Postfix     is Operations range Pow_0..Pow_Plus;
   subtype Arithmetic  is Operations range Add..Abs_Value;
--
-- "and" -- Checks operation associations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Returns :
--
--     True if Left is compatible with Right
--
   function "and" (Left, Right : Operations) return Boolean;
--
-- Image -- Readable notation of the operation
--
--     Action - The operation
--
-- Returns :
--
--     Text representation
--
   function Image (Action : Operations) return String;
--
-- Is_Commutative -- Commutative operations
--
--     Left  - The operation on the left
--     Right - The operation on the right
--
-- Commutative groups:
--
--     {.}, {|}
--
-- Though  A.B  is  not commutative, it makes sense to treat it as if it
-- were commutative to parse A.B.C as "."(A,B,C).
--
-- Returns :
--
--     True if Left and Right are from a group
--
   function Is_Commutative (Left, Right : Operations) return Boolean;
--
-- Is_Inverse -- Of a group
--
--     Operation - To be tested
--
-- Returns :
--
--     True if - or /
--
   function Is_Inverse (Operation : Operations) return Boolean;
--
-- Group_Inverse -- Of a group
--
--     Operation - An operation of either {+, -} or {*, /}
--
-- Returns :
--
--     Add_Inv for + or -
--     Mul_Inv for * or /
--
   function Group_Inverse (Operation : Operations) return Operations;
--
-- Priorities -- The levels of association
--
   type Priorities is mod 13;
--
-- Parsing  tree.  To  make  it  efficient  the  nodes  of  the tree are
-- allocated  on  a  stack.  The stack is provided by a stack pool. This
-- allows  to  remove the whole tree by deallocating its first allocated
-- node or any other pool object allocated before it. Tree_Pool  is  the
-- stack storage pool used for this.
--
   Tree_Pool : Stack_Storage.Pool (2048, 128);
--
-- Node -- Of a parsing tree
--
   type Node is abstract tagged null record;
--
-- Get_Location -- Get expression source location
--
--    Item - The node
--
-- Returns :
--
--    The source location of the whole expression
--
   function Get_Location (Item : Node'Class)
      return Parsers.Multiline_Source.Location;
--
-- Image -- To be used for tree output
--
--  [ Feature ] - The expected feature (context)
--    Item      - The node
--    Mode      - The code set
--
-- Returns :
--
--    The string representation of the node
--
   function Image
            (  Feature : Feature_Handle;
               Item    : Node;
               Mode    : Units.Code_Set
            )  return String is abstract;
   function Image
            (  Item : Node'Class;
               Mode : Units.Code_Set
            )  return String;
--
-- Node_Ptr -- Pointer to a node, class-wide, Tree_Pool specific
--
   type Node_Ptr is access Node'Class;
   for Node_Ptr'Storage_Pool use Tree_Pool;
   procedure Free is
      new Standard.Ada.Unchecked_Deallocation (Node'Class, Node_Ptr);
   --
   -- Mark -- Marks the pool state for quick tree removal
   --
   type Mark is new Node with null record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Mark;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Term -- Expression term, abstract base type
   --
   type Term is abstract new Node with record
      Location : Parsers.Multiline_Source.Location;
   end record;
   --
   -- Literal -- Expression literal, abstract base type
   --
   type Literal (Length : Natural) is abstract new Term with record
      Value : aliased String (1..Length);
   end record;
   --
   -- Numeric_Literal -- A numeric literal, abstract base type
   --
   -- The  field  Malformed is set to true to indicate a syntax error in
   -- the  literal, which was detected and corrected. The field Exponent
   -- is set to Integer'First or Integer'Last  when  the  exponent  part
   -- cannot be represented (it is too big). The  field  Value  contains
   -- the mantissa, which is always whole.
   --
   subtype Number_Base is Integer range 2..16;
   type Numeric_Literal is abstract new Literal with record
      Malformed : Boolean     := False;
      Base      : Number_Base := 10;
      Dimension : Measure     := Np;
      Exponent  : Integer;
   end record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Numeric_Literal;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Integer_Literal -- Represents integer literals
   --
   type Integer_Literal is new Numeric_Literal with null record;
   --
   -- Real_Literal -- Represents real literals
   --
   type Real_Literal is new Numeric_Literal with null record;
   --
   -- String_Literal -- Represents string literals
   --
   type String_Literal is new Literal with null record;
   function Image
            (  Feature : Feature_Handle;
               Item    : String_Literal;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Character_Literal -- Represents character literals
   --
   type Character_Literal is new Term with record
      Value : UTF8_Code_Point;
   end record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Character_Literal;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Power_Literal -- Power expression literal
   --
   type Power_Literal is new Term with record
      Value : Domain_Integer;
   end record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Power_Literal;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Truth_Literal -- Represents truth value literals
   --
   type Truth_Literal is new Term with record
      Value : Fuzzy.Logic.Fuzzy_Boolean;
   end record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Truth_Literal;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Identifier -- Represents identifiers
   --
   type Identifier is new Literal with record
      Malformed : Boolean := False;
   end record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Identifier;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Missing operand -- Represents an assumed operand
   --
   type Missing_Operand is new Term with null record;
   function Image
            (  Feature : Feature_Handle;
               Item    : Missing_Operand;
               Mode    : Units.Code_Set
            )  return String;
   --
   -- Expression -- Non-terminal node
   --
   type Argument_List is array (Positive range <>) of Node_Ptr;
   type Expression (Count : Positive) is new Node with record
      Operation : Operations;
      Location  : Parsers.Multiline_Source.Location;
      Operands  : Argument_List (1..Count);
   end record;
--
-- Image -- Conversion of an expression to string
--
--    Feature - The feature to which the expression value belongs
--    Item    - The expression
--    Mode    - The character set
--
-- Returns :
--
--    String representation of the expression
--
   function Image
            (  Feature : Feature_Handle;
               Item    : Expression;
               Mode    : Units.Code_Set := Units.UTF8_Set
            )  return String;
--
-- Tokens -- The lexical tokens
--
   package Tokens is
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Node_Ptr,
             Priority_Type  => Priorities,
             Sources        => Code
          );
   use Tokens;
--
-- Check_Spelling -- Of a name, no checks
--
   procedure Check_Spelling (Name : String);
--
-- Check_Matched -- Check if no broken keyword matched
--
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
--
-- Token_Tables -- Case-insensitive tables of tokens
--
   package Token_Tables is
      new Tokens.Vocabulary.UTF8_Names (Blanks => Blank_Characters);
--
-- The tables of prefix, infix and postfix operations
--
   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;
--
-- Variable_Type -- The types of the variables supported
--
   type Variable_Type is
        (  Discrete_Var,
           Float_Var,
           Integer_Var,
           Real_Var
        );
--
-- Variable_Tables -- Case-insensitive tables of tokens
--
   type Variable_Descriptor (Kind_Of : Variable_Type := Integer_Var) is
   record
      Feature  : Feature_Handle;
      Location : Parsers.Multiline_Source.Location;
      case Kind_Of is
         when Real_Var =>
            Scale      : Unbounded_String;
            Fuzzify_At : Parsers.Multiline_Source.Location;
         when others =>
            null;
      end case;
   end record;
   package Variables_Tables is new Tables (Variable_Descriptor);
   package Variables_Lists is
      new Variables_Tables.UTF8_Names (Blanks => Blank_Characters);
--
-- Lexers -- Table driven lexers
--
   package Lexers is new Tokens.Segmented_Lexer;
--
-- Expression -- The lexer using our tables
--
   type FCL_Expression is
      new Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
--
-- Call -- Evaluates an operator
--
   function Call
            (  Context   : access FCL_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Enclose -- Evaluates an expression in brackets
--
   function Enclose
            (  Context : access FCL_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token;
--
-- Get_Blank -- Overrides Parsers.Generic_Lexer...
--
   procedure Get_Blank
             (  Context : in out FCL_Expression;
                Code    : in out Source'Class;
                Got_It  : out Boolean
             );
--
-- Get_Operand -- Recognizes an operand (float number)
--
   procedure Get_Operand
             (  Context  : in out FCL_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             );
--
-- On_Missing_Operation -- To deal with "and", "or" etc
--
   procedure On_Missing_Operation
             (  Context  : in out FCL_Expression;
                Code     : in out Source'Class;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                                  Lexical_Token;
                Got_It   : out Boolean
             );
--
-- On_Premodifier -- Overrides the default handling of modifiers
--
   procedure On_Postmodifier
             (  Context   : in out FCL_Expression;
                Code      : in out Source'Class;
                Operation : in out Tokens.Operation_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             );
   procedure On_Postmodifier
             (  Context   : in out FCL_Expression;
                Code      : in out Source'Class;
                Argument  : in out Tokens.Argument_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             );
--
-- Parsing_Mode -- The code set used for unit input
--
   Parsing_Mode : constant Units.Code_Set := Units.UTF8_Set;
--
-- Messaging_Parameters -- The messages output parameters
--
   Messaging_Parameters : constant Fuzzy.Feature.Output_Parameters :=
      (  Abs_Small   =>-Strings_Edit.MaxSmall,
         Base        => 10,
         Default     => Fuzzy.Logic.Certain_True,
         Mode        => Parsing_Mode,
         Rel_Small   => Strings_Edit.MaxSmall,
         Put_Plus    => False,
         Put_Units   => False,
         Quote_Units => False,
         Use_Derived => True,
         Use_SI      => False
      );

private
   pragma Inline (Check_Matched);
   pragma Inline (Is_Blank);
   pragma Inline (Is_Identifier_Body);
   pragma Inline (Is_Unit_Symbol);

   Superscript_0     : constant UTF8_Code_Point := 16#2070#;
   Superscript_1     : constant UTF8_Code_Point := 16#00B9#;
   Superscript_2     : constant UTF8_Code_Point := 16#00B2#;
   Superscript_3     : constant UTF8_Code_Point := 16#00B3#;
   Superscript_4     : constant UTF8_Code_Point := 16#2074#;
   Superscript_5     : constant UTF8_Code_Point := 16#2075#;
   Superscript_6     : constant UTF8_Code_Point := 16#2076#;
   Superscript_7     : constant UTF8_Code_Point := 16#2077#;
   Superscript_8     : constant UTF8_Code_Point := 16#2078#;
   Superscript_9     : constant UTF8_Code_Point := 16#2079#;
   Superscript_Plus  : constant UTF8_Code_Point := 16#207A#;
   Superscript_Minus : constant UTF8_Code_Point := 16#207B#;

end Parsers.FCL;
