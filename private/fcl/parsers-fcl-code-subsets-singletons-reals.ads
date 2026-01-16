--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Singletons.Reals                         Spring, 2005       --
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

package Parsers.FCL.Code.Subsets.Singletons.Reals is
   use Domain_Floats.Variable_Measures;
--
-- Real_Singleton -- A real value with confidence level
--
   type Real_Singleton is new Singleton with record
      Value : Pair_Measure;
   end record;
--
-- Image -- Overrides Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Real_Singleton;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
--
   overriding
   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Confidence
            )  return Lattice'Class;
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Get_Preference (Left : Real_Singleton) return Preference;
   overriding
   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Measure
            )  return Constant_Value'Class;
--
-- Operations -- Override Parsers.FCL.Code.Orders.Singletons...
--
   overriding
   function To_Set (Left : Real_Singleton) return Subset'Class;
--
-- To_Singleton -- Conversion
--
--    Left - The operand
--
-- Numeric  values  and  integer  singletons  can  be  treated  as  real
-- singletons. For  other  arguments  an  appropriate  syntax  error  is
-- propagated.
--
-- Returns :
--
--    The singleton corresponding to Left
--
   function To_Singleton (Left : Constant_Value'Class)
      return Real_Singleton;

end Parsers.FCL.Code.Subsets.Singletons.Reals;
