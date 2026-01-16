--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Logic                      Luebeck            --
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

with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;

package Parsers.FCL.Code.Logic is
--
-- Logical -- Logical values
--
   type Logical is abstract new Lattice with null record;
--
-- To_Logical -- Conversion
--
--    Left - The value to convert
--
-- Returns :
--
--    The corresponding logical value
--
   function To_Logical (Left : Constant_Value'Class)
      return Logical'Class;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class;
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Confidence
            )  return Lattice'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term;
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Measure
            )  return Constant_Value'Class;
   overriding
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term;
--
-- To_Confidence -- Conversion to
--
--    Left - A number
--
-- Returns :
--
--    The corresponding confidence factor
--
-- Exceptions :
--
--    Data_Error - Out of range
--    Unit_Error - Dimensioned argument
--
   function To_Confidence (Value : Domain_Integer) return Confidence;
   function To_Confidence (Value : Domain_Float  ) return Confidence;
   function To_Confidence (Value : Measure       ) return Confidence;
--
-- not -- Complement
--
--    Left - The argument
--
-- Returns :
--
--    Logical complement of Left
--
   function "not" (Left : Logical) return Logical is abstract;

end Parsers.FCL.Code.Logic;
