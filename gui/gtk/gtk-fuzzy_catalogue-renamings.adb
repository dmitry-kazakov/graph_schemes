--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Renamings              Luebeck            --
--  Implementation                                 Summer, 2009       --
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

with Fuzzy.Persistence;  use Fuzzy.Persistence;
with GLib.Messages;      use GLib.Messages;
with Gtk.Table;          use Gtk.Table;
with Gtk.Widget.Styles;  use Gtk.Widget.Styles;
with Name_Tables;        use Name_Tables;
with Persistent.Handle;  use Persistent.Handle;
with Strings_Edit;       use Strings_Edit;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Renamings is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Renamings." & Name;
   end Where;

   procedure Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Renaming
             )  is
   begin
      Delete (Item);
   end Changed;

   procedure Commit (Item : not null access Renaming_Record) is
      Raw_Name : constant UTF8_String := Get_Text (Item.Name);
   begin
      if Raw_Name'Length = 0 then -- Wrong name
         Error (Item, Style_Get (Item.Browser, "empty-name-error"));
         Set_Hint (Item.Hint, Item.Browser, Erroneous, True);
         return;
      end if;
      begin
         Check_Name (Raw_Name);
      exception
         when Constraint_Error => -- Illegal name
            Error (Item, Style_Get (Item.Browser, "name-error"));
            Set_Hint (Item.Hint, Item.Browser, Erroneous, True);
            return;
      end;
      declare
         New_Name : constant UTF8_String :=
                       Name_Maps.Canonize (Raw_Name);
         View     : constant Gtk_Persistent_Storage_Items_View :=
                       Get_Current (Item.Browser).
                          List.all'Unchecked_Access;
         Selected : Selection := Get_Selection (View);
         Storage  : Storage_Handle := Get_Storage (View);
         Folder   : constant Deposit_Handle :=
                       Get_Directory_Object (View);
         Old_Name : constant UTF8_String :=
                       UTF8_String
                       (  Get_Name
                          (  View,
                             Selected (Selected'First)
                       )  );
      begin
         if Selected'Length /= 1 then
            Delete (Item);
         end if;
         begin
            if Is_Prefix
               (  Fuzzy.Feature.Feature_Class,
                  Get_Class (Storage, Old_Name, Folder)
               )
            then
               Set_Name (Get (Storage, Old_Name, Folder), New_Name);
            end if;
         exception
            when Ada.IO_Exceptions.Use_Error =>
               Error
               (  Item,
                  Style_Get (Item.Browser, "unsupported-error")
               );
               Set_Hint (Item.Hint, Item.Browser, Erroneous, True);
               return;
         end;
         Rename
         (  Storage    => Storage,
            Old_Name   => Old_Name,
            Old_Parent => Folder,
            New_Name   => New_Name,
            New_Parent => Folder
         );
         Renamed
         (  View,
            Selected (Selected'First),
            Item_Name (New_Name)
         );
         Delete (Item);
      exception
         when Reason : Ada.IO_Exceptions.Data_Error =>
            Error
            (  Item,
               (  Style_Get
                  (  Item.Browser,
                     "inconsistent-storage-error"
                  )
               &  Exception_Message (Reason)
            )  );
            Set_Hint (Item.Hint, Item.Browser, Erroneous, True);
         when Ada.IO_Exceptions.Name_Error =>
            Error_Duplicated
            (  Item    => Item,
               Name    => New_Name,
               Storage => Storage,
               Parent  => Folder
            );
      end;
   exception
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
   end Commit;

   procedure Gtk_New
             (  Item    : out Renaming;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Name    : Item_Name
             )  is
   begin
      Item := new Renaming_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Renamings.Initialize (Item, Name);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item : not null access Renaming_Record'Class;
                Name : Item_Name
             )  is
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
      Grid  : Gtk_Table;
      Label : Gtk_Label;
   begin
      Initialize (Item, "renaming box", True);
      Gtk_New (Grid, 1, 3, False);
      Set_Col_Spacings (Grid, Column_Spacing);
      Set_Row_Spacings (Grid, Row_Spacing);

      Gtk_New (Label, Style_Get (Item.Browser, "new-name-label"));
      Attach
      (  Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.Name);
      Set_Text (Item.Name, UTF8_String (Name));
      Attach
      (  Grid,
         Item.Name,
         1, 2,
         0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Hint);
      Attach
      (  Grid,
         Item.Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );

      Set
      (  Item.Handlers (1),
         Renaming_Handlers.Connect
         (  Get_Current (Item.Browser).List,
            "selection-changed",
            Renaming_Handlers.To_Marshaller (Changed'Access),
            Item.all'Access
      )  );
      Set
      (  Item.Handlers (2),
         Renaming_Handlers.Connect
         (  Get_Current (Item.Browser).List,
            "directory-changed",
            Renaming_Handlers.To_Marshaller (Changed'Access),
            Item.all'Access
      )  );
      Set
      (  Item.Handlers (3),
         Renaming_Handlers.Connect
         (  Item.Browser.List_Tabs,
            "switch-page",
            Renaming_Handlers.To_Marshaller (Changed'Access),
            Item.all'Access
      )  );
      Pack_Start (Item, Grid, False, False);
      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-rename-label"),
         Style_Get (Item.Browser, "tab-rename-icon"),
         Item
      );
   end Initialize;

end Gtk.Fuzzy_Catalogue.Renamings;
