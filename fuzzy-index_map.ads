--                                                                    --
--  package Fuzzy.Index_Map         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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
--  This  package  provides  an  interval  index  to  confidence  factor
--  mapping, which can be used for instantiation of Fuzzy.Number. 
--
package Fuzzy.Index_Map is
   pragma Preelaborate (Fuzzy.Index_Map);
   type Index is range 1..10;
   type Map is array (Index) of Confidence;
   To_Confidence : constant Map :=
      (  1 => 0.20,
         2 => 0.39,
         3 => 0.54,
         4 => 0.67,
         5 => 0.77,
         6 => 0.85,
         7 => 0.91,
         8 => 0.95,
         9 => 0.98,
        10 => Confidence'Last
      );
end Fuzzy.Index_Map;
