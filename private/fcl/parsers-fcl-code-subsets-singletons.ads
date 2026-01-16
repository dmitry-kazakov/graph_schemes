--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Singletons                               Spring, 2005       --
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

package Parsers.FCL.Code.Subsets.Singletons is
--
-- Singleton -- A value with confidence level
--
   type Singleton is abstract new Subset with null record;
--
-- Get_Singleton -- Evaluate a singleton expression
--
--    Location - Of the singleton
--    Context  - The name resolution context
--    Left     - The value expression
--    Right    - The level expression
--
-- Returns :
--
--    The corresponding singleton
--
   function Get_Singleton
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Node'Class;
               Right    : Node'Class
            )  return Singleton'Class;
--
-- Get_Singleton -- Evaluate a singleton expression
--
--    Location - Of the singleton
--    Context  - The name resolution context
--    Tree     - The value expression
--
-- Returns :
--
--    The corresponding singleton
--
   function Get_Singleton
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Tree     : Node'Class
            )  return Singleton'Class;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class;
   overriding
   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Singleton
            )  return Logical_Term;
   overriding
   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Singleton
            )  return Lattice'Class;
   overriding
   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Singleton
            )  return Logical_Term;
--
-- Operations -- Overrides Parsers.FCL.Code.Subsets..
--
   overriding
   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Singleton;
               Inversed : Boolean
            )  return Logic.Logical'Class;
   overriding
   function To_Set
            (  Context  : Resolution_Context;
               Left     : Singleton
            )  return Fuzzy.Intuitionistic.Set;
--
-- To_Set -- Conversion to a set
--
--    Left - The singleton
--
-- Returns :
--
--    The corresponding set
--
   function To_Set (Left : Singleton) return Subset'Class is abstract;

end Parsers.FCL.Code.Subsets.Singletons;
