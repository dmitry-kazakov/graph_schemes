--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numerics                                 Winter, 2005       --
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

with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;

with Ada.Numerics.Generic_Elementary_Functions;

package Parsers.FCL.Code.Orders.Numerics is
   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Domain_Float);
   use Elementary_Functions;
--
-- Numeric -- A numeric expression result
--
   type Numeric is abstract new Ordered with null record;
--
-- Do_Numeric -- Evaluate a numeric operation
--
   function Do_Numeric
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Numeric'Class;
--
-- Get_Numeric -- Evaluate a numeric expression
--
--    Context - The name resolution context
--    Tree    - The expression
--
-- Returns :
--
--    The numeric value of the expression
--
   function Get_Numeric
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Numeric'Class;
--
-- Get_Numeric -- Evaluate an expression with numeric result
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
   function Get_Numeric (Tree : Node'Class) return Numeric'Class;
--
-- Numeric operations
--
   function Abs_Value
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric
            )  return Numeric'Class is abstract;
   function Add
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Div
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Div_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Mul
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Minus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric
            )  return Numeric'Class is abstract;
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Domain_Integer
            )  return Numeric'Class is abstract;
   function Pow
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Pow_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Plus
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric
            )  return Numeric'Class is abstract;
   function Sub
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
   function Sub_Inv
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric;
               Right    : Numeric'Class
            )  return Numeric'Class is abstract;
--
-- Subset_Logical_Term -- Implements Parsers.FCL.Code...
--
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Numeric
            )  return Logical_Term;
--
-- To_Numeric -- From a literal
--
--    Literal - A numeric literal
--
-- When  literal  is dimensioned and the unit differs from 1 SI then the
-- result is Numerics.Reals.Real. Otherwise it is Numerics.Integers.Int.
--
-- Returns :
--
--    The corresponding value
--
   function To_Numeric (Literal : Numeric_Literal'Class)
      return Numeric'Class;
   function To_Numeric (Literal : Power_Literal'Class)
      return Numeric'Class;

end Parsers.FCL.Code.Orders.Numerics;
