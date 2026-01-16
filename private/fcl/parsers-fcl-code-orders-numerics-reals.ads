--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numeric.Reals                            Winter, 2005       --
--  Interface                                                         --
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

package Parsers.FCL.Code.Orders.Numerics.Reals is
--
-- Real -- Real valued expressions
--
   type Real is new Numeric with record
      Value : Measure;
   end record;
--
-- To_Measure -- Getting a value from
--
--  [ Context ] - The context (determines the expected dimension)
--    Value     - The value to be converted
--
-- Returns :
--
--    The value in the expected units
--
   function To_Measure
            (  Context : Resolution_Context;
               Value   : Constant_Value'Class
            )  return Measure;
   function To_Measure (Left : Constant_Value'Class) return Measure;
--
-- To_Real -- Taking the expression term from an identifier
--
--    Context - The context
--    Name    - The identifier
--
-- Returns :
--
--    The value corresponding to Name treated as a dimension unit
--
   function To_Real
            (  Context : Resolution_Context;
               Name    : Identifier'Class
            )  return Real;
--
-- To_SI -- Getting a value from
--
--  [ Location ] - Of the source value
--    Value      - The value to be converted
--
-- Returns :
--
--    The value in SI units
--
   function To_SI
            (  Location : Parsers.Multiline_Source.Location;
               Value    : Measure
            )  return Domain_Float;
   function To_SI (Value : Real) return Domain_Float;
--
-- Operations -- Override Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Real;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Get_Preference (Left : Real) return Preference;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term;
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Real
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- Operations -- Override Parsers.FCL.Code.Orders...
--
   overriding
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class;
--
-- Operations -- Override Parsers.FCL.Orders.Numerics...
--
   overriding
   function Abs_Value
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class;
   overriding
   function Add
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Div
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Div_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Minus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class;
   overriding
   function Mul
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Domain_Integer
            )  return Numeric'Class;
   overriding
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Pow_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Plus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real
            )  return Numeric'Class;
   overriding
   function Sub
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;
   overriding
   function Sub_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real;
               Right    : Numeric'Class
            )  return Numeric'Class;

end Parsers.FCL.Code.Orders.Numerics.Reals;
