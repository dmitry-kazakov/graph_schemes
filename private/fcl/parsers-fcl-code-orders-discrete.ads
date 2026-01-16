--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Discrete                                 Summer, 2005       --
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

package Parsers.FCL.Code.Orders.Discrete is
--
-- Point -- One discrete domain point
--
   type Point is new Ordered with record
      Value : Positive;
   end record;
--
-- Get_Point -- A feature domain point
--
--    Context - The context
--    Name    - The identifier
--
-- Returns :
--
--    The feature domain point
--
   function Get_Point
            (  Context : Resolution_Context;
               Term    : Identifier'Class
            )  return Point;
--
-- To_Set -- A feature domain point as a set
--
--    Context - The context
--    Name    - The identifier
--
-- Returns :
--
--    The feature domain point singleton set
--
   function To_Set
            (  Context : Resolution_Context;
               Term    : Identifier'Class
            )  return Lattice'Class;
--
-- Image -- Overrides Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Point;
               Mode    : Code_Set
            )  return String;
--
-- Operation -- Override Parsers.FCL.Code...
--
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term;
   overriding
   function Get_Preference (Left : Point) return Preference;
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Measure
            )  return Constant_Value'Class;
   overriding
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Point
            )  return Logical_Term;
--
-- Operation -- Override Parsers.FCL.Code.Orders...
--
   overriding
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class
            )  return Logic.Logical'Class;
   overriding
   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Point;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class;

end Parsers.FCL.Code.Orders.Discrete;
