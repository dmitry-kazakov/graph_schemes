--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets                    Luebeck            --
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

with Parsers.FCL.Code.Logic;

package Parsers.FCL.Code.Subsets is
--
-- Subset -- Sets of values
--
   type Subset is abstract new Lattice with null record;
--
-- Classifiy_To_Set -- Classification to set conversion
--
--    Set - A classification (A in Bs)
--
-- Returns :
--
--    The corresponding set (Bs has A)
--
   function Classify_To_Set (Set : Fuzzy.Intuitionistic.Classification)
      return Fuzzy.Intuitionistic.Set;
--
-- Get_Set -- Conversion a value to an intuitionistic set
--
--    Context - The resolution context
--    Left    - The value to convert to
--
-- Returns :
--
--    The set
--
   function Get_Set
            (  Context : Resolution_Context;
               Left    : Constant_Value'Class
            )  return Fuzzy.Intuitionistic.Set;
--
-- Is_Subset -- Subset test operation
--
--    Location - Of the operation
--    Context  - The resolution context
--    Left     - The left operand
--    Right    - The right operand
--    Inversed - Right in Left, if true
--
-- Returns :
--
--    Left in Right
--
   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Subset;
               Inversed : Boolean
            )  return Logic.Logical'Class is abstract;
--
-- To_Set -- Conversion to an intuitionistic set
--
--    Context - The resolution context
--    Left    - The value to convert to
--
-- Returns :
--
--    The set
--
   function To_Set
            (  Feature : Resolution_Context;
               Left    : Subset
            )  return Fuzzy.Intuitionistic.Set is abstract;
--
-- Subset_Test -- Evaluation of
--
--    Location - Of the operation
--    Context  - The resolution context
--    Left     - The left operand
--    Right    - The right operand
--
-- Returns :
--
--    The result
--
   function Subset_Test
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Node'Class;
               Right    : Node'Class;
               Mode     : Subsetting_Mode
            )  return Logic.Logical'Class;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Subset
            )  return Logical_Term;
   overriding
   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Subset
            )  return Logical_Term;

   pragma Inline (Classify_To_Set);

end Parsers.FCL.Code.Subsets;
