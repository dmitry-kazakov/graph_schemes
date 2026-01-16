--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Strings                                  Winter, 2005       --
--  Interface                                                         --
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

package Parsers.FCL.Code.Orders.Strings is
--
-- Text -- Abstract string type
--
   type Text is abstract new Ordered with null record;
   function Get_Value (Tree : Text) return String is abstract;
--
-- Do_Text -- Evaluate a string operation
--
   function Do_Text
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Text'Class;
--
-- Operations -- Overrides Parsers.FCL.Code...
--
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term;
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term;
   overriding
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Text;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- Operations -- Override Parsers.FCL.Orders...
--
   overriding
   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Text;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class;
--
-- Get_Text -- Evaluation of a string expression
--
   function Get_Text
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Text'Class;
--
-- To_String -- Conversion of literals
--
   function To_String (Left : Constant_Value'Class) return String;
   function To_String (Tree : Literal'Class)    return String;
--
-- To_Text -- Conversion of literals
--
--    Node - A literal node
--
-- Returns :
--
--    The text object
--
   function To_Text (Node : String_Literal'Class) return Text'Class;
   function To_Text (Node : Character_Literal'Class) return Text'Class;
--
-- String operations
--   
   function Concatenate
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Text;
               Right    : Text'Class
            )  return Text'Class is abstract;
--
-- Literal_Text
--
   type Literal_Text is new Text with record
      Value : Literal_Ptr;
   end record;
   overriding
   function Get_Preference (Left : Literal_Text) return Preference;
   overriding
   function Concatenate
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Literal_Text;
               Right    : Text'Class
            )  return Text'Class;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function Get_Value (Tree : Literal_Text) return String;
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Literal_Text;
               Mode    : Code_Set
            )  return String;
--
-- Result_Text
--
   type Result_Text (Length : Natural) is new Text with record
      Value : String (1..Length);
   end record;
   overriding
   function Get_Preference (Left : Result_Text) return Preference;
   overriding
   function Concatenate
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Result_Text;
               Right    : Text'Class
            )  return Text'Class;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function Get_Value (Tree : Result_Text) return String;
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Result_Text;
               Mode    : Code_Set
            )  return String;

end Parsers.FCL.Code.Orders.Strings;
