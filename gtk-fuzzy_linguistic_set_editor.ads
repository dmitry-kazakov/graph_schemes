--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Linguistic_Set_Editor             Luebeck            --
--  Instantiation                                  Summer, 2007       --
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

with Gtk.Generic_Fuzzy_Linguistic_Set_Editor;
with Gtk.Fuzzy_Linguistic_Set_Domain;
with Gtk.Fuzzy_Linguistic_Set_Tree_View;
with Gtk.Fuzzy_Linguistic_Set_Zoom_Panel;

package Gtk.Fuzzy_Linguistic_Set_Editor is
   new Gtk.Generic_Fuzzy_Linguistic_Set_Editor
       (  Domain     => Gtk.Fuzzy_Linguistic_Set_Domain,
          Zoom_Panel => Gtk.Fuzzy_Linguistic_Set_Zoom_Panel,
          Tree_View  => Gtk.Fuzzy_Linguistic_Set_Tree_View
       );
