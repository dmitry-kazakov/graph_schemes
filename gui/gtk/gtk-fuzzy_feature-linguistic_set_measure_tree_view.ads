--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Linguistc_Set_Measure_Tree_View          Summer, 2007       --
--  Instantiation                 Last revision :  22:14 29 Jan 2012  --
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

with Gtk.Fuzzy_Feature.Linguistic_Set_Domain;
with Gtk.Fuzzy_Feature.Linguistic_Set_Tree_View;
with Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View;

package Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Tree_View is
   new Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View
       (  Fuzzy_Measures                => Fuzzy_Measures,
          Derived_Measures              => Derived_Measures,
          Irregular_Measures            => Irregular_Measures,
          Float_Edit                    => Float_Edit,
          Measure_Edit                  => Measure_UTF8_Edit,
          Domain                        => Linguistic_Set_Domain,
          Tree_View                     => Linguistic_Set_Tree_View,
          Fuzzy_Measure_Linguistics     => Variable_Measures,
          Fuzzy_Measure_Linguistic_Sets => Variable_Measure_Sets
      );
