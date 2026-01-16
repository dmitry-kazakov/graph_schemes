--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Delimiter_Frame                          Winter, 2008       --
--  Implementation                                                    --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with GLib.Messages;           use GLib.Messages;
with GLib.Properties;         use GLib.Properties;
with Gtk.Alignment;           use Gtk.Alignment;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Widget.Styles;       use Gtk.Widget.Styles;

with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Delimiter_Frame is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Delimiter_Frame." & Name;
   end Where;

   function Get
            (  Widget : not null access
                  Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record'Class
            )  return String is
   begin
      if Get_Active (Widget.Space_Button) then
         Set_Hint
         (  Widget.Delimiter_Hint,
            Widget.Browser,
            Checked,
            True
         );
         if Widget.Can_Space then
            return " ";
         else
            return "";
         end if;
      elsif Get_Active (Widget.Semicolon_Button) then
         Set_Hint
         (  Widget.Delimiter_Hint,
            Widget.Browser,
            Checked,
            True
         );
         return ";";
      elsif Get_Active (Widget.Tabulator_Button) then
         Set_Hint
         (  Widget.Delimiter_Hint,
            Widget.Browser,
            Checked,
            True
         );
         return "" & HT;
      end if;
      declare
         Delimiter : constant String :=
                        Get_Text (Widget.Delimiter_Edit);
      begin
         if Delimiter'Length = 0 then
            Set_Hint
            (  Widget.Delimiter_Hint,
               Widget.Browser,
               Erroneous,
               True
            );
            raise Data_Error;
         else
            Set_Hint
            (  Widget.Delimiter_Hint,
               Widget.Browser,
               Checked,
               True
            );
            return Delimiter;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
         return "";
   end Get;

   procedure Gtk_New
             (  Widget    : out Gtk_Fuzzy_Catalogue_Deilimiter_Frame;
                Can_Space : Boolean;
                Title     : String;
                Browser   : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Widget := new Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record
                    (  Browser.all'Unchecked_Access
                    );
      begin
         Initialize (Widget, Can_Space, Title);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record'Class;
                Can_Space : Boolean;
                Title     : String
             )  is
      Column_Spacing : constant GUInt :=
                          Style_Get (Widget.Browser, "column-spacing");
      Delimiter_Box  : Gtk_Box;
      Box            : Gtk_Box;
      Buttons        : Gtk_Box;
      Align          : Gtk_Alignment;
   begin
      Gtk.Frame.Initialize (Widget, Style_Get (Widget.Browser, Title));
      Gtk_New_HBox (Box);
      Set_Border_Width (Box, Column_Spacing);
      Add (Widget, Box);

      Gtk_New_VBox (Buttons);
      Pack_Start (Box, Buttons, False, False);
         if Can_Space then
            Gtk_New
            (  Widget.Space_Button,
               Widget.Space_Button,
               Style_Get (Widget.Browser, "space-delimiter-label")
            );
         else
            Gtk_New
            (  Widget.Space_Button,
               Widget.Space_Button,
               Style_Get (Widget.Browser, "blank-delimiter-label")
            );
         end if;
         Widget.Can_Space := Can_Space;
         Pack_Start (Buttons, Widget.Space_Button, False, False);
         Gtk_New
         (  Widget.Tabulator_Button,
            Widget.Space_Button,
            Style_Get (Widget.Browser, "tabulator-delimiter-label")
         );
         Pack_Start (Buttons, Widget.Tabulator_Button, False, False);
         Gtk_New
         (  Widget.Semicolon_Button,
            Widget.Space_Button,
            Style_Get (Widget.Browser, "semicolon-delimiter-label")
         );
         Pack_Start (Buttons, Widget.Semicolon_Button, False, False);
         Gtk_New
         (  Widget.Other_Button,
            Widget.Space_Button,
            Style_Get (Widget.Browser, "other-delimiter-label")
         );
         Pack_Start (Buttons, Widget.Other_Button, False, False);

      Gtk_New (Align, 0.5, 1.0, 0.2, 0.2);
      Pack_Start (Box, Align, True, True);
      Gtk_New_HBox (Delimiter_Box);
      Set_Spacing (Delimiter_Box, GInt (Column_Spacing));
      Add (Align, Delimiter_Box);

      Gtk_New (Widget.Delimiter_Edit);
      Widget.Delimiter_Edit.Set_Width_Chars (8);
      if (  Find_Property (Widget.Delimiter_Edit, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Widget.Delimiter_Edit,
            Build ("max-width-chars"),
            GInt'(8)
         );
      end if;
      Set_Sensitive (Widget.Delimiter_Edit, False);
      Pack_Start (Delimiter_Box, Widget.Delimiter_Edit, True, True);
      Gtk_New_HBox (Widget.Delimiter_Hint);
      Set_Hint (Widget.Delimiter_Hint, Widget.Browser, Checked, True);
      Pack_Start (Delimiter_Box, Widget.Delimiter_Hint, False, False);

      Handlers.Connect
      (  Widget.Other_Button,
         "toggled",
         Toggled'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Semicolon_Button,
         "toggled",
         Toggled'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Space_Button,
         "toggled",
         Toggled'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Tabulator_Button,
         "toggled",
         Toggled'Access,
         Widget.all'Access
      );
   end Initialize;

   procedure Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Parent : Gtk_Fuzzy_Catalogue_Deilimiter_Frame
             )  is
   begin
      if Get_Active (Parent.Other_Button) then
         Set_Sensitive (Parent.Delimiter_Edit, True);
         if Get_Text (Parent.Delimiter_Edit)'Length > 0 then
            Set_Hint
            (  Parent.Delimiter_Hint,
               Parent.Browser,
               Checked,
               True
            );
         else
            Set_Hint
            (  Parent.Delimiter_Hint,
               Parent.Browser,
               Erroneous,
               True
            );
         end if;
      else
         Set_Sensitive (Parent.Delimiter_Edit, False);
         Set_Hint
         (  Parent.Delimiter_Hint,
            Parent.Browser,
            Checked,
            True
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Toggled")
         )  );
   end Toggled;

end Gtk.Fuzzy_Catalogue.Delimiter_Frame;
