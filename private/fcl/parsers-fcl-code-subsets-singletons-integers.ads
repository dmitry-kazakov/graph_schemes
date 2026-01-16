--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Singletons.Integers                      Summer, 2005       --
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

with Parsers.FCL.Code.Subsets.Singletons.Reals;

package Parsers.FCL.Code.Subsets.Singletons.Integers is
--
-- Integer_Singleton -- A real value with confidence level
--
   type Integer_Singleton is new Singleton with record
      Value : Domain_Integer;
      Level : Confidence;
   end record;
--
-- Operations -- Overrides Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Integer_Singleton;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Confidence
            )  return Lattice'Class;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Get_Preference (Left : Integer_Singleton) return Preference;
   overriding
   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- To_Real_Singleton -- Conversion
--
--    Left - Integer singleton
--
-- Returns :
--
--    The corresponding real singleton
--
   function To_Real_Singleton (Left : Integer_Singleton)
      return Reals.Real_Singleton;
--
-- Operations -- Override Parsers.FCL.Code.Orders.Singletons...
--
   overriding
   function To_Set (Left : Integer_Singleton) return Subset'Class;

end Parsers.FCL.Code.Subsets.Singletons.Integers;
