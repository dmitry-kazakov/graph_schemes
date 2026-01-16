--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Domain_Floats                 Luebeck            --
--  Instantiation                                  Autumn, 2005       --
--                                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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
--
--  This  package  defines standard float domain. By default it is based
--  on  the standard Float type. If it does not satisfy the requirements
--  this is the place to change it.
--
with Fuzzy.Feature.Generic_Domain_Float;
with Fuzzy.Index_Map;

package Fuzzy.Feature.Domain_Floats is
   new Fuzzy.Feature.Generic_Domain_Float
       (  Domain_Number  => Float,
          Suffix         => "Float",
          Interval_Index => Fuzzy.Index_Map.Index,
          Interval_Map   => Fuzzy.Index_Map.Map,
          To_Confidence  => Fuzzy.Index_Map.To_Confidence
       );
