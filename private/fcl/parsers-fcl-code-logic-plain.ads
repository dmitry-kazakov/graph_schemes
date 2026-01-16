--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Logic.                     Luebeck            --
--        Plain                                    Summer, 2005       --
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

package Parsers.FCL.Code.Logic.Plain is
--
-- Truth_Value -- Logical values
--   
   type Truth_Value is new Logical with record
      Value : Confidence;
   end record;
--
-- Get_Confidence -- Get logical value
--
--    Context - The name resolution context
--    Tree    - The expression
--
-- Returns :
--
--    The result
--
   function Get_Confidence
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Truth_Value'Class;
--
-- Operations -- Override Parsers.FCL...
--
   overriding
   function Image
            (  Feature : Feature_Handle;
               Item    : Truth_Value;
               Mode    : Code_Set
            )  return String;
--
-- Operations -- Override Parsers.FCL.Code...
-- 
   overriding
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class;
   overriding
   function Get_Preference (Left : Truth_Value) return Preference;
   overriding
   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Truth_Value
            )  return Lattice'Class;
   overriding
   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class;
   overriding
   function "not" (Left : Truth_Value) return Truth_Value;

end Parsers.FCL.Code.Logic.Plain;
