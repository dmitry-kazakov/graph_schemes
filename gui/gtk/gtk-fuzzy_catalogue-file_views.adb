--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.File_Views             Luebeck            --
--  Implementation                                 Winter, 2009       --
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

with Ada.Characters.Latin_1;       use Ada.Characters.Latin_1;
with Ada.Directories;              use Ada.Directories;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with GLib.Messages;                use GLib.Messages;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Text_Iter;                use Gtk.Text_Iter;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;
with Pango.Font;                   use Pango.Font;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Strings_Edit.UTF8;            use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Handling;   use Strings_Edit.UTF8.Handling;

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.File_Views is
   use Edit_Buttons;
   use Redo_Buttons;
   use Save_Buttons;
   use Undo_Buttons;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  File_View_Record'Class,
             File_View
          );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.File_Views." & Name;
   end Where;

   function Cancel
            (  Item : not null access File_View_Record
            )  return Boolean is
   begin
      return
      (  not Get_Modified (Item.Buffer)
      or else
         (  Gtk_Response_OK
         =  Query
            (  Item.Browser,
               Style_Get (Item.Browser, "modified-query-title"),
               Style_Get (Item.Browser, "query-icon"),
               Style_Get (Item.Browser, "query-icon-size"),
               Style_Get (Item.Browser, "modified-query")
      )  )  );
   end Cancel;

   procedure Changed
             (  Object : access GObject_Record'Class;
                Item   : File_View
             )  is
   begin
      Set_Sensitive (Item.Redo, Can_Redo (Item.Buffer));
      Set_Sensitive (Item.Undo, Can_Undo (Item.Buffer));
      Set_Sensitive (Item.Save, Get_Modified (Item.Buffer));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed")
         )  );
   end Changed;

   function Check_Encoding (Item : not null access File_View_Record)
      return Boolean is
      Code     : GUniChar;
      Current  : Gtk_Text_Iter;
      Start    : Gtk_Text_Iter;
      Location : Gtk_Text_Iter;
      Flag     : Boolean;
      Error    : Boolean := False;
   begin
      if Item.Mode = UTF8_Set then
         return True;
      end if;
      Get_Start_Iter (Item.Buffer, Current);
      while not Is_End (Current) loop
         Code := Get_Char (Current);
         if (  Code > 255
            or else
               (Code > 127 and then Item.Mode = ASCII_Set)
            )
         then
            if Item.Error_Tag = null then
               declare
                  Color : Gdk_Color;
               begin
                  Item.Error_Tag :=
                     Create_Tag (Item.Buffer, "Encoding error");
                  Color :=
                     Style_Get
                     (  Item.Browser,
                        "error-code-foregound-color",
                        Def_Code_Error_Color
                     );
                  Set_Property
                  (  Item.Error_Tag,
                     Foreground_Gdk_Property,
                     Color
                  );
               end;
            end if;
            Copy (Current, Start);
            Forward_Char (Current, Flag);
            Apply_Tag (Item.Buffer, Item.Error_Tag, Start, Current);
            if not Error then
               Error := True;
               Copy (Start, Location);
            end if;
         else
            Forward_Char (Current, Flag);
         end if;
         if Ends_Line (Current) then
            Forward_Line (Current, Flag);
         end if;
      end loop;
      if Error then
         if Scroll_To_Iter
            (  Item.View,
               Location,
               0.25,
               False,
               0.0,
               0.0
            )
         then
            null;
         end if;
         return False;
      else
         return True;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Check_Encoding")
         )  );
         return False;
   end Check_Encoding;

   procedure Completed (Item : not null access File_View_Record) is
   begin
      Replace
      (  Item.Browser.Files.Open,
         Build_Filename
         (  Get_Text (Item.Directory),
            Get_Text (Item.Name)
         ),
         Item.all'Unchecked_Access
      );
      End_Not_Undoable_Action (Item.Buffer);
      Set_Modified (Item.Buffer, False);
      Show_Error (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Completed")
         )  );
   end Completed;

   procedure Edit
             (  Object : access GObject_Record'Class;
                Item   : File_View
             )  is
   begin
      if Item.Edit /= null then
         Remove (Item.Tool_Box, Item.Edit);
         Item.Edit := null;
      end if;

      Gtk_New (Item.Undo);
      Pack_Start (Item.Tool_Box, Item.Undo, False, False);
      Set_Sensitive (Item.Undo, False);
      Show (Item.Undo);

      Gtk_New (Item.Redo);
      Pack_Start (Item.Tool_Box, Item.Redo, False, False);
      Set_Sensitive (Item.Redo, False);
      Show (Item.Redo);

      Gtk_New (Item.Save);
      Pack_Start (Item.Save_Button_Box, Item.Save, False, False);
      Set_Sensitive (Item.Save, False);
      Show (Item.Save);

      Buffer_Handlers.Connect
      (  Item.Buffer,
         "changed",
         Changed'Access,
         Item.all'Unchecked_Access
      );
      Buffer_Handlers.Connect
      (  Item.Redo,
         "clicked",
         Redo'Access,
         Item.all'Unchecked_Access
      );
      Buffer_Handlers.Connect
      (  Item.Save,
         "clicked",
         Save'Access,
         Item.all'Unchecked_Access
      );
      Buffer_Handlers.Connect
      (  Item.Undo,
         "clicked",
         Undo'Access,
         Item.all'Unchecked_Access
      );
      Set_Editable (Item.View, True);
      Set_Editable (Item.Name, True);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Edit")
         )  );
   end Edit;

   procedure Finalize (Item : not null access File_View_Record) is
   begin
      if Item.Browser.Files /= null then
         for Index in 1..GetSize (Item.Browser.Files.Open) loop
            if (  GetTag (Item.Browser.Files.Open, Index)
               =  Item.all'Unchecked_Access
               )
            then
               Delete (Item.Browser.Files.Open, Index);
               exit;
            end if;
         end loop;
      end if;
      Finalize (Gtk_Item_Box_Record (Item.all)'Unchecked_Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   procedure Gtk_New
             (  Item      : out File_View;
                File_Name : String;
                Mode      : Code_Set;
                New_File  : Boolean;
                Language  : Gtk_Source_Language;
                Browser   : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Gtk_New
      (  Item             => Item,
         File_Name        => File_Name,
         Mode             => Mode,
         New_File         => New_File,
         Language         => Language,
         Line_From_No     => 1,
         Position_From_No => 1,
         Line_To_No       => 0,
         Position_To_No   => 0,
         Browser          => Browser
      );
   end Gtk_New;

   procedure Gtk_New
             (  Item        : out File_View;
                File_Name   : String;
                Mode        : Code_Set;
                New_File    : Boolean;
                Language    : Gtk_Source_Language;
                Line_No     : Positive;
                Position_No : Positive;
                Browser     : not null access
                              Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Gtk_New
      (  Item             => Item,
         File_Name        => File_Name,
         Mode             => Mode,
         New_File         => New_File,
         Language         => Language,
         Line_From_No     => Line_No,
         Position_From_No => Position_No,
         Line_To_No       => Line_No,
         Position_To_No   => Position_No - 1,
         Browser          => Browser
      );
   end Gtk_New;

   procedure Gtk_New
             (  Item             : out File_View;
                File_Name        : String;
                Mode             : Code_Set;
                New_File         : Boolean;
                Language         : Gtk_Source_Language;
                Line_From_No     : Positive;
                Position_From_No : Positive;
                Line_To_No       : Natural;
                Position_To_No   : Natural;
                Browser          : not null access
                                   Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      if IsIn (Browser.Files.Open, File_Name) then
         -- The file is already viewed
         declare
            View : constant Gtk_Item_Box :=
                            Find (Browser.Files.Open, File_Name);
            Start, Stop : Gtk_Text_Iter;
         begin
            if View.all in File_View_Record'Class then
               Item :=
                  File_View_Record'Class (View.all)'Unchecked_Access;
               Get_Start_Iter (Item.Buffer, Start);
               Get_End_Iter   (Item.Buffer, Stop);
               Remove_Tag_By_Name (Item.Buffer, "Left",   Start, Stop);
               Remove_Tag_By_Name (Item.Buffer, "Middle", Start, Stop);
               Remove_Tag_By_Name (Item.Buffer, "Right",  Start, Stop);
               Item.Marked := False;
               Set_Item (Browser, View);
               if Line_From_No <= Line_To_No then
                  Item.Line_From_No     := Line_From_No;
                  Item.Line_To_No       := Line_To_No;
                  Item.Position_From_No := Position_From_No;
                  Item.Position_To_No   := Position_To_No;
                  Show_Error (Item);
               end if;
               return;
            end if;
         end;
      end if;
      Item := new File_View_Record (Browser.all'Unchecked_Access, Mode);
      begin
         if Line_From_No <= Line_To_No then
            Item.Line_From_No     := Line_From_No;
            Item.Line_To_No       := Line_To_No;
            Item.Position_From_No := Position_From_No;
            Item.Position_To_No   := Position_To_No;
         else
            Item.Marked := True;
         end if;
         Initialize (Item, File_Name, New_File, Language);
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
             (  Item      : not null access File_View_Record'Class;
                File_Name : UTF8_String;
                New_File  : Boolean;
                Language  : Gtk_Source_Language
             )  is
      Frame  : Gtk_Frame;
      Bar    : Gtk_VSeparator;
      Scroll : Gtk_Scrolled_Window;
      Font   : Pango_Font_Description :=
               From_String (Style_Get (Item.Browser, "text-view-font"));
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
   begin
      Initialize (Item, "file view box", False);
      Item.New_File := New_File;

      if Language = null then
         Gtk_New (Item.Buffer);
      else
         Gtk_New (Item.Buffer, Language);
      end if;
      Begin_Not_Undoable_Action (Item.Buffer);
      Gtk_New (Item.View, Item.Buffer);
      Unref (Item.Buffer);

      Set_Right_Margin_Position (Item.View, 72);
      Set_Show_Right_Margin (Item.View, True);
      Set_Show_Line_Numbers (Item.View, True);
      Set_Highlight_Current_Line (Item.View, True);
      Set_Highlight_Matching_Brackets (Item.Buffer, True);
      Set_Highlight_Syntax (Item.Buffer, True);

      Modify_Font (Item.View, Font);
      Free (Font);
      Set_Editable (Item.View, False);
      Gtk_New (Scroll);
      Add (Scroll, Item.View);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Scroll);
      Pack_Start (Item, Frame, True, True);

      Gtk_New_HBox (Item.Tool_Box);
      Pack_Start (Item, Item.Tool_Box, False, False);
      Set_Spacing (Item.Tool_Box, GInt (Button_Spacing));

      if not New_File then
         Gtk_New (Item.Edit);
         Pack_Start (Item.Tool_Box, Item.Edit, False, False);
         Buffer_Handlers.Connect
         (  Item.Edit,
            "clicked",
            Edit'Access,
            Item.all'Unchecked_Access
         );
      end if;

      Gtk_New (Item.Cursor);
      Pack_End (Item.Tool_Box, Item.Cursor, False, False);

      Gtk_New_VSeparator (Bar);
      Pack_End (Item.Tool_Box, Bar, False, False);

      Gtk_New_HBox (Item.Save_Button_Box);
      Pack_End (Item.Tool_Box, Item.Save_Button_Box, False, False);

      Gtk_New (Item.Name);
      Set_Text (Item.Name, Get_Basename (File_Name));
      Pack_End (Item.Tool_Box, Item.Name, False, False);
      Set_Editable (Item.Name, New_File);

      Gtk_New (Item.Directory, Get_Dirname (File_Name));
      Item.Directory.Set_Halign (Align_End);
      Item.Directory.Set_Valign (Align_Center);
--    Set_Alignment (Item.Directory, 1.0, 0.5);
      Pack_End (Item.Tool_Box, Item.Directory);

      Show_All (Item);

      if New_File then
         Edit (Item, Item.all'Unchecked_Access);
      end if;

      Buffer_Handlers.Connect
      (  Item.Buffer,
         "mark-set",
         Mark_Set'Access,
         Item.all'Unchecked_Access
      );
      Add_Item
      (  Item.Browser,
         Get_Basename (File_Name),
         Style_Get (Item.Browser, "tab-file-icon"),
         Item
      );
      if not New_File then
         Append (Item.File_Name, File_Name);
         Start_Servicing (Item);
      end if;
   end Initialize;

   procedure Mark_Set
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Item   : File_View
             )  is
      Iter     : Gtk_Text_Iter;
      Line     : Integer;
      Position : Integer;
   begin
      Get_Text_Iter (Nth (Params, 1), Iter);
      Line := Integer (Get_Line (Iter)) + 1;
      Position := Integer (Get_Offset (Iter));
      Set_Line_Offset (Iter, 0);
      Position := Position - Integer (Get_Offset (Iter)) + 1;
      Set_Text
      (  Item.Cursor,
         (  Strings_Edit.Integers.Image (Line)
         &  ":"
         &  Strings_Edit.Integers.Image (Position)
      )  );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Mark_Set")
         )  );
   end Mark_Set;

   procedure Redo
             (  Object : access GObject_Record'Class;
                Item   : File_View
             )  is
   begin
      Redo (Item.Buffer);
      Set_Sensitive (Item.Redo, Can_Redo (Item.Buffer));
      Set_Sensitive (Item.Undo, Can_Undo (Item.Buffer));
      Set_Sensitive (Item.Save, Get_Modified (Item.Buffer));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Redo")
         )  );
   end Redo;

   procedure Write
             (  Item : not null access File_View_Record;
                Name : UTF8_String
             )  is
      use Ada.Text_IO;
      File : File_Type;
   begin
      begin
         Create (File, Out_File, Name);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Error
            (  Item,
               (  Style_Get (Item.Browser, "no-file-error-begin")
               &  To_String (Name)
               &  Style_Get (Item.Browser, "no-file-error-end")
            )  );
            return;
         when Reason : Ada.IO_Exceptions.Use_Error =>
            Error
            (  Item,
               (  Style_Get (Item.Browser, "open-file-error-begin")
               &  To_String (Name)
               &  Style_Get (Item.Browser, "open-file-error-end")
               &  Exception_Message (Reason)
            )  );
            return;
      end;
      declare
         Start : Gtk_Text_Iter;
         Stop  : Gtk_Text_Iter;
         Flag  : Boolean := False;
      begin
         Get_Start_Iter (Item.Buffer, Start);
         case Item.Mode is
            when ASCII_Set | Latin1_Set =>
               while not Is_End (Start) loop
                  if Ends_Line (Start) then
                     --
                     -- This is a GTK bug fix, which handles empty lines
                     -- improperly
                     --
                     New_Line (File);
                  else
                     Copy (Start, Stop);
                     Forward_To_Line_End (Stop, Flag);
                     Put_Line
                     (  File,
                        To_String
                        (  Get_Text (Item.Buffer, Start, Stop),
                           '?'
                     )  );
                  end if;
                  Forward_Line (Start, Flag);
               end loop;
            when UTF8_Set =>
               while not Is_End (Start) loop
                  if Ends_Line (Start) then
                     --
                     -- This is a GTK bug fix, which handles empty lines
                     -- improperly
                     --
                     New_Line (File);
                  else
                     Copy (Start, Stop);
                     Forward_To_Line_End (Stop, Flag);
                     Put_Line
                     (  File,
                        Get_Text (Item.Buffer, Start, Stop)
                     );
                  end if;
                  Forward_Line (Start, Flag);
               end loop;
         end case;
         Close (File);
         if Item.Save /= null then
            Set_Sensitive (Item.Save, False);
         end if;
         Set_Modified  (Item.Buffer, False);
      exception
         when Reason : Ada.IO_Exceptions.Data_Error =>
            Error
            (  Item,
               (  Style_Get (Item.Browser, "file-error-begin")
               &  To_String (Name)
               &  Style_Get (Item.Browser, "file-error-end")
               &  Exception_Message (Reason)
            )  );
            Close (File);
         when others =>
            Close (File);
            raise;
      end;
   end Write;

   procedure Save
             (  Object : access GObject_Record'Class;
                Item   : File_View
             )  is
      use Ada.Text_IO;
      Wait      : Wait_Cursor (Item);
      File_Name : constant String :=
                     Build_Filename
                     (  Get_Text (Item.Directory),
                        Get_Text (Item.Name)
                     );
      Backup : constant String := File_Name & ".bak";
   begin
      Clean (Item);
      if not Check_Encoding (Item) then
         Error (Item, Style_Get (Item.Browser, "encoding-error"));
         return;
      end if;
      if (  File_Test (File_Name, File_Test_Exists)
         and then
            File_Test (File_Name, File_Test_Is_Regular)
         )  then
         begin
            Copy_File
            (  Source_Name => File_Name,
               Target_Name => Backup
            );
         exception
            when Ada.IO_Exceptions.Name_Error =>
               Error
               (  Item,
                  (  Style_Get (Item.Browser, "no-file-error-begin")
                  &  File_Name
                  &  Style_Get (Item.Browser, "no-file-error-end")
               )  );
               return;
            when Reason : Ada.IO_Exceptions.Use_Error =>
               Error
               (  Item,
                  (  Style_Get (Item.Browser, "copy-file-error-begin")
                  &  File_Name
                  &  Style_Get (Item.Browser, "copy-file-error-middle")
                  &  Backup
                  &  Style_Get (Item.Browser, "copy-file-error-end")
                  &  Exception_Message (Reason)
               )  );
               return;
         end;
      end if;
      if (  Item.New_File
         and then
            File_Test (File_Name, File_Test_Exists)
         and then
            (  Gtk_Response_OK
            /= Query
               (  Item.Browser,
                  Style_Get (Item.Browser, "overwrite-query-title"),
                  Style_Get (Item.Browser, "query-icon"),
                  Style_Get (Item.Browser, "query-icon-size"),
                  (  Style_Get (Item.Browser, "overwrite-query-begin")
                  &  File_Name
                  &  Style_Get (Item.Browser, "overwrite-query-end")
         )  )  )  )
      then
         return;
      end if;
      Write (Item, File_Name);
      Item.New_File := False;
      Changed
      (  Item.Browser.Files.Store,
         Item_Path (Get_Text (Item.Directory))
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save")
         )  );
   end Save;

   procedure Service (Item : not null access File_View_Record) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, To_String (Item.File_Name));
      loop
         Get_Line
         (  File,
            Item.Input_Buffer
            (  Item.Input_Last + 1
            .. Item.Input_Buffer'Last
            ),
            Item.Input_Last
         );
         if Item.Input_Last < Item.Input_Buffer'Last then
            Item.Input_Last := Item.Input_Last + 1;
            Item.Input_Buffer (Item.Input_Last) := LF;
         end if;
         if Item.Input_Last = Item.Input_Buffer'Last then
            Service_Update (Item);
            Item.Input_Last := 0;
         end if;
      end loop;
   exception
      when Ada.IO_Exceptions.End_Error =>
         Service_Update (Item);
         Close (File);
      when others =>
         Close (File);
         raise;
   end Service;

   procedure Show_Error (Item : not null access File_View_Record) is
      Count : constant Natural :=
                       Natural (Get_Line_Count (Item.Buffer));
   begin
      if Count < Item.Line_From_No or else Item.Marked then
         return;
      end if;
      declare
         Result : Boolean;
         Color  : Gdk_Color;
         Start  : Gtk_Text_Iter;
         Stop   : Gtk_Text_Iter;
      begin
         -- Marking error location
         Get_Iter_At_Line_Index
         (  Item.Buffer,
            Stop,
            GInt (Item.Line_From_No) - 1,
            GInt (Item.Position_From_No) - 1
         );
         if Scroll_To_Iter (Item.View, Stop, 0.25, False, 0.0, 0.0) then
            null;
         end if;
         if Item.Left_Tag = null then
            Item.Left_Tag := Create_Tag (Item.Buffer, "Left");
         end if;
         Get_Iter_At_Line
         (  Item.Buffer,
            Start,
            GInt (Item.Line_From_No) - 1
         );
         Color :=
            Style_Get
            (  Item.Browser,
               "scanned-text-backgound-color",
               Def_Scanned_Color
            );
         Set_Property (Item.Left_Tag, Background_Gdk_Property, Color);
         Apply_Tag (Item.Buffer, Item.Left_Tag, Start, Stop);
         if (  Item.Line_From_No < Item.Line_To_No
            or else
               (  Item.Line_From_No = Item.Line_To_No
               and then
                  Item.Position_From_No <= Item.Position_To_No
            )  )
         then
            Copy (Dest => Start, Source => Stop);
            if Item.Middle_Tag = null then
               Item.Middle_Tag := Create_Tag (Item.Buffer, "Middle");
            end if;
            Get_Iter_At_Line_Index
            (  Item.Buffer,
               Stop,
               GInt (Item.Line_To_No) - 1,
               GInt (Item.Position_To_No)
            );
            Color :=
               Style_Get
               (  Item.Browser,
                  "error-text-color",
                  Def_Error_Color
               );
            Set_Property
            (  Item.Middle_Tag,
               Foreground_Gdk_Property,
               Color
            );
            Apply_Tag (Item.Buffer, Item.Middle_Tag, Start, Stop);
            Copy (Dest => Start, Source => Stop);
         end if;
         Forward_To_Line_End (Stop, Result);
         if Item.Right_Tag = null then
            Item.Right_Tag := Create_Tag (Item.Buffer, "Right");
         end if;
         Color :=
            Style_Get
            (  Item.Browser,
               "unscanned-text-backgound-color",
               Def_Unscanned_Color
            );
         Set_Property (Item.Right_Tag, Background_Gdk_Property, Color);
         Apply_Tag (Item.Buffer, Item.Right_Tag, Start, Stop);
         Item.Marked := Count > Item.Line_From_No;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Show_Error")
         )  );
   end Show_Error;

   procedure Undo
             (  Object : access GObject_Record'Class;
                Item   : File_View
             )  is
   begin
      Undo (Item.Buffer);
      Set_Sensitive (Item.Redo, Can_Redo (Item.Buffer));
      Set_Sensitive (Item.Undo, Can_Undo (Item.Buffer));
      Set_Sensitive (Item.Save, Get_Modified (Item.Buffer));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Undo")
         )  );
   end Undo;

   procedure Updated (Item : not null access File_View_Record) is
      End_Iter : Gtk_Text_Iter;
   begin
      case Item.Mode is
         when UTF8_Set =>
            declare
               From : Integer := 1;
               procedure Add (To : Integer) is
               begin
                  if To >= From then
                     Get_End_Iter (Item.Buffer, End_Iter);
                     Insert_Alt
                     (  Item.Buffer,
                        End_Iter,
                        Item.Input_Buffer (From..To)
                     );
                  end if;
               end Add;
               Index : Integer := 1;
            begin
               while Index < Item.Input_Last loop
                  begin
                     Skip (Item.Input_Buffer, Index);
                  exception
                     when Data_Error =>
                        Add (Index - 1);
                        Get_End_Iter (Item.Buffer, End_Iter);
                        Insert_Alt
                        (  Item.Buffer,
                           End_Iter,
                           Strings_Edit.UTF8.Image (16#FFFD#)
                        );
                        Index := Index + 1;
                        From  := Index;
                  end;
               end loop;
               Add (Item.Input_Last);
            end;
         when ASCII_Set | Latin1_Set =>
            Get_End_Iter (Item.Buffer, End_Iter);
            Insert_Alt
            (  Item.Buffer,
               End_Iter,
               To_UTF8 (Item.Input_Buffer (1..Item.Input_Last))
            );
      end case;
   exception
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
   end Updated;

end Gtk.Fuzzy_Catalogue.File_Views;
