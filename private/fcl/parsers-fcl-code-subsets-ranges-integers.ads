--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Ranges.Integers                          Summer, 2005       --
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

with Fuzzy.Feature.Domain_Integers;

package Parsers.FCL.Code.Subsets.Ranges.Integers is
   use Fuzzy.Feature.Domain_Integers.Integer_Intervals;
--
-- Integer_Range -- Expressions of integer ranges
--
   type Integer_Range is new Numeric_Range with record
      Value : Interval;
   end record;
--
-- Get_Range -- Evaluate an expression as an integer range
--
--    Tree - The expression
--
-- Returns :
--
--    The result
--
   function Get_Range (Tree : Node'Class) return Integer_Range'Class;
--
-- Image -- Override Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Integer_Range;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Get_Preference (Left : Integer_Range) return Preference;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Range;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Range;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- Operations -- Override Parsers.FCL.Code.Subsets...
--
   overriding
   function To_Set
            (  Context : Resolution_Context;
               Left    : Integer_Range
            )  return Fuzzy.Intuitionistic.Set;
--
-- To_Set -- Override Paraser.FCL.Code.Subset.Ranges...
--
   overriding
   function To_Set (Left : Integer_Range) return Subset'Class;

end Parsers.FCL.Code.Subsets.Ranges.Integers;
