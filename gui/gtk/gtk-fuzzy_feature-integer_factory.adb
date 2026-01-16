--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Integer_Factory                          Winter, 2007       --
--  Implementation                                                    --
--                                Last revision :  09:45 08 Oct 2016  --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Fuzzy.Feature.Integer_Handle;  use Fuzzy.Feature.Integer_Handle;
with Fuzzy.Gtk_Icon_Factory;        use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;                 use GLib.Messages;
with GLib.Properties;               use GLib.Properties;
with GLib.Properties.Creation;      use GLib.Properties.Creation;
with GLib.Types;                    use GLib.Types;
with GtkAda.Types;                  use GtkAda.Types;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Missed;                    use Gtk.Missed;
with Gtk.Widget;                    use Gtk.Widget;
with Gtk.Widget.Styles;             use Gtk.Widget.Styles;
with Strings_Edit;                  use Strings_Edit;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with Strings_Edit.UTF8.Maps;        use Strings_Edit.UTF8.Maps;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;
with Name_Tables;

package body Gtk.Fuzzy_Feature.Integer_Factory is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Integer_Factory." & Name;
   end Where;

   procedure Changed
             (  Edit   : access Gtk_Entry_Record'Class;
                Widget : Gtk_Integer_Factory
             )  is
   begin
      Widget.Modified := True;
   end Changed;

   function Create
            (  Widget : not null access Gtk_Integer_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      From, To : Integer;
   begin
      Get (Widget, From, To);
      return Create (Name, From, To);
   end Create;

   function Edited
            (  Widget : not null access Gtk_Integer_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   procedure Get
             (  Widget : access Gtk_Integer_Factory_Record;
                From   : out Integer;
                To     : out Integer
             )  is
   begin
      begin
         declare
            Text    : constant UTF8_String :=
                               Get_Text (Widget.From_Entry);
            Pointer : Integer := Text'First;
         begin
            Get (Text, Pointer, Name_Tables.Blanks);
            Get (Text, Pointer, From);
            Get (Text, Pointer, Name_Tables.Blanks);
            if Pointer < Text'Length then
               raise Ada.IO_Exceptions.Data_Error;
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "missing-from-error");
            when Constraint_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "overflow-from-error");
            when Ada.IO_Exceptions.Data_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "non-numeric-from-error");
         end;
      exception
         when others =>
            Set_Hint
            (  Widget.From_Hint,
               Widget,
               Erroneous,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.To_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            raise;
      end;
      begin
         declare
            Text    : constant UTF8_String :=
                               Get_Text (Widget.To_Entry);
            Pointer : Integer := Text'First;
         begin
            Get (Text, Pointer, Name_Tables.Blanks);
            Get (Text, Pointer, To);
            Get (Text, Pointer, Name_Tables.Blanks);
            if Pointer < Text'Length then
               raise Data_Error;
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "missing-to-error");
            when Constraint_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "overflow-to-error");
            when Ada.IO_Exceptions.Data_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "non-numeric-to-error");
         end;
      exception
         when others =>
            Set_Hint
            (  Widget.From_Hint,
               Widget,
               Checked,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.To_Hint,
               Widget,
               Erroneous,
               Is_Editable (Widget)
            );
            raise;
      end;
      if From > To then
         Set_Hint
         (  Widget.From_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         Set_Hint
         (  Widget.To_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise Constraint_Error with Style_Get (Widget, "bounds-error");
      end if;
      Set_Hint
      (  Widget.From_Hint,
         Widget,
         Checked,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.To_Hint,
         Widget,
         Checked,
         Is_Editable (Widget)
      );
   end Get;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Integer_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "bounds-error",
               Nick    => "Wrong interval",
               Blurb   => "From is greater than to message",
               Default => (  "The field from cannot exceed "
                          &  "to"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-from-error",
               Nick    => "Missing from",
               Blurb   => "No integer in the from field message",
               Default => (  "The field from must contain "
                          &  "an integer number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-to-error",
               Nick    => "Missing to",
               Blurb   => "No integer in the to field message",
               Default => (  "The field to must contain "
                          &  "an integer number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-from-error",
               Nick    => "Overlowed from",
               Blurb   => "The field to is out of range message",
               Default => (  "The field from contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-to-error",
               Nick    => "Overlowed to",
               Blurb   => "The field to is out of range message",
               Default => (  "The field from contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-from-error",
               Nick    => "Wrong from",
               Blurb   => "No number in the field to message",
               Default => (  "The field from must contain "
                          &  "an integer number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-to-error",
               Nick    => "Wrong to",
               Blurb   => "No number in the field to message",
               Default => (  "The field to must contain "
                          &  "an integer number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "from-label",
               Nick    => "From",
               Blurb   => "The from label",
               Default => "From"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "to-label",
               Nick    => "To",
               Blurb   => "The to label",
               Default => "To"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget   : out Gtk_Integer_Factory;
                Feature  : Feature_Handle := No_Feature;
                Editable : Boolean        := True
             )  is
   begin
      if Feature.Is_Valid and then not Is_Integer (Feature) then
         raise Constraint_Error with "Not an integer feature";
      end if;
      Widget := new Gtk_Integer_Factory_Record;
      begin
         Initialize (Widget, Feature, Editable);
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
             (  Widget   : not null access
                           Gtk_Integer_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 2, 3, False);
      Gtk_New (Widget.From_Label);
      Widget.From_Label.Set_Halign (Align_End);
      Widget.From_Label.Set_Valign (Align_Center);
--    Set_Alignment (Widget.From_Label, 1.0, 0.5);
      Gtk_New (Widget.To_Label);
      Widget.To_Label.Set_Halign (Align_End);
      Widget.To_Label.Set_Valign (Align_Center);
--    Set_Alignment (Widget.To_Label, 1.0, 0.5);
      Gtk_New (Widget.From_Entry);
      Widget.From_Entry.Set_Width_Chars  (10);
      if Find_Property (Widget.From_Entry, "max-width-chars") /= null
      then
         Set_Property
         (  Widget.From_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Gtk_New (Widget.To_Entry);
      Widget.To_Entry.Set_Width_Chars (10);
      if Find_Property (Widget.To_Entry, "max-width-chars") /= null then
         Set_Property
         (  Widget.To_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Gtk_New_HBox (Widget.From_Hint);
      Gtk_New_HBox (Widget.To_Hint);
      if Is_Integer (Feature) then
         Set_Text (Widget.From_Entry, Image (Get_From (Feature)));
         Set_Text (Widget.To_Entry,   Image (Get_To   (Feature)));
      end if;
      Set_Homogeneous (Widget, False);
      Attach
      (  Widget,
         Widget.From_Label,
         0, 1, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Widget,
         Widget.To_Label,
         0, 1, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Widget,
         Widget.From_Entry,
         1, 2, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Widget,
         Widget.To_Entry,
         1, 2, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Widget,
         Widget.From_Hint,
         2, 3, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Widget,
         Widget.To_Hint,
         2, 3, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Handlers.Connect
      (  Widget,
         "style-updated",
         Handlers.To_Marshaller (Style_Updated'Access)
      );
      Entry_Handlers.Connect
      (  Widget.From_Entry,
         "changed",
         Entry_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Entry_Handlers.Connect
      (  Widget.To_Entry,
         "changed",
         Entry_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Set_Sensitive (Widget.From_Entry, Editable);
      Set_Sensitive (Widget.To_Entry,   Editable);
      Style_Updated (Widget);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Integer_Factory_Record
            )  return Boolean is
   begin
      return Is_Sensitive (Widget.From_Entry);
   end Is_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Integer_Factory_Record
             )  is
      From, To : Integer;
   begin
      Get (Widget, From, To);
   end Verify;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Integer_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         Set_Sensitive (Widget.From_Entry, Editable);
         Set_Sensitive (Widget.To_Entry,   Editable);
      end if;
   end Set_Editable;

   procedure Style_Updated
             (  Widget : access Gtk_Integer_Factory_Record'Class
             )  is
   begin
      Set_Text (Widget.From_Label, Style_Get (Widget, "from-label"));
      Set_Text (Widget.To_Label,   Style_Get (Widget, "to-label"));
      Set_Hint
      (  Widget.From_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.To_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
   end Style_Updated;

end Gtk.Fuzzy_Feature.Integer_Factory;
