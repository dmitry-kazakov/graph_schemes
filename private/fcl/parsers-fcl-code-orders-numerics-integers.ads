--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numeric.Integers                         Winter, 2005       --
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

with Generic_Map;

package Parsers.FCL.Code.Orders.Numerics.Integers is
--
-- Int -- Integer values
--
   type Int is new Numeric with record
      Value : Domain_Integer;
   end record;
--
-- Get_Integer -- Evaluate an expression as an integer
--
--    Tree - The expression
--
-- Returns :
--
--    The result
--
   function Get_Integer (Tree : Node'Class) return Int'Class;
--
-- To_Integer -- Getting a value from
--
--    Value - The value to be converted
--
-- Returns :
--
--    The value
--
   function To_Integer (Value : Constant_Value'Class)
      return Domain_Integer;
--
-- Operations -- Override Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Int;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Get_Preference (Left : Int) return Preference;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term;
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Int
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- Operations -- Override Parsers.FCL.Orders...
--
   overriding
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Int;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class;
--
-- Operations -- Override Parsers.FCL.Orders.Numerics...
--
   overriding
   function Abs_Value
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class;
   overriding
   function Add
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Div
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Div_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Minus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class;
   overriding
   function Mul
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Plus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int
            )  return Numeric'Class;
   overriding
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Domain_Integer
            )  return Numeric'Class;
   overriding
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Pow_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Sub
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Sub_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Int;
               Right    : Numeric'Class
            )  return Numeric'Class;
--
-- Rule_Numbers_Set -- Sets of rule numbers
--
   type Rule_Number is new Domain_Integer;
   package Rules_Maps is new Generic_Map (Rule_Number, Location);

end Parsers.FCL.Code.Orders.Numerics.Integers;
