--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Renamings               Luebeck            --
--  Interface                                      Summer, 2009       --
--                                                                    --
--                                Last revision :  11:45 29 May 2020  --
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

with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers.References;  use Gtk.Handlers.References;

private package Gtk.Fuzzy_Catalogue.Renamings is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Renamings);
--
-- Renaming_Record -- Box of renaming
--
   type Renaming_Record (<>) is
      new Gtk_Item_Box_Record with private;
   type Renaming is access all Renaming_Record'Class;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Renaming_Record);
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--    Name    - The initial Name
--
   procedure Gtk_New
             (  Item    : out Renaming;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Name    : Item_Name
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--    Name - The initial Name
--
   procedure Initialize
             (  Item : not null access Renaming_Record'Class;
                Name : Item_Name
             );
private
   type Callbacks is array (1..3) of Handler_Reference;
   type Renaming_Record is new Gtk_Item_Box_Record with record
      Name     : Gtk_GEntry;
      Hint     : Gtk_Box;
      Handlers : Callbacks;
   end record;

   procedure Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Renaming
             );

   package Renaming_Handlers is
      new Gtk.Handlers.User_Callback (Gtk_Widget_Record, Renaming);

end Gtk.Fuzzy_Catalogue.Renamings;
