--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Intuitionistic                           Spring, 2005       --
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

package Parsers.FCL.Code.Subsets.Intuitionistic is
--
-- Set -- Expressions of intuitionistic sets
--
   type Set (Cardinality : Positive) is new Subset with record
      Value : Fuzzy.Intuitionistic.Set (Cardinality);
   end record;
--
-- Image -- Override Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Set;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
-- 
   overriding
   function Get_Preference (Left : Set) return Preference;
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class;
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Confidence
            )  return Lattice'Class;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Measure
            )  return Constant_Value'Class;
   overriding
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term;
--
-- Operations -- Override Parsers.FCL.Code.Subsets...
-- 
   overriding
   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Set
            )  return Lattice'Class;
   overriding
   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Set;
               Inversed : Boolean
            )  return Logic.Logical'Class;
--
-- Operations -- Override Parsers.FCL.Code.Subsets...
-- 
   overriding
   function To_Set
            (  Context : Resolution_Context;
               Left    : Set
            )  return Fuzzy.Intuitionistic.Set;

end Parsers.FCL.Code.Subsets.Intuitionistic;
