--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders                     Luebeck            --
--  Interface                                      Winter, 2005       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

package Parsers.FCL.Code.Orders is
--
-- Ordered -- Ordered expression result
--
   type Ordered is abstract new Comparable with null record;
--
-- Do_Ordered -- Evaluate a relational operation
--
   function Do_Ordered
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logic.Logical'Class;
--
-- Get_Ordered -- Evaluate an expression as ordered
--
   function Get_Ordered
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Ordered'Class;
--
-- Operations defined on the ordered values
--
   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Ordered;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is abstract;
   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Ordered;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is abstract;
   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Ordered;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class is abstract;

end Parsers.FCL.Code.Orders;
