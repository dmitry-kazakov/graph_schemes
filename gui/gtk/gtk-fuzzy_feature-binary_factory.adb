--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Binary_Factory            Luebeck            --
--  Implementation                                 Summer 2009        --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Deposit_Handles;           use Deposit_Handles;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Abstract_Browser;      use Gtk.Abstract_Browser;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Persistent;                use Persistent;
with Persistent.Handle;         use Persistent.Handle;
with Strings_Edit.Integers;     use Strings_Edit.Integers;

with Fuzzy.Feature.Binary.Mutually_Independent;
with Fuzzy.Feature.Binary;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Binary_Factory is
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Binary_Factory." & Name;
   end Where;

   function Log2 (Value : Positive) return Positive is
      Count  : Integer  := Value - 1;
      Result : Positive := 1;
   begin
      loop
         Count := Count / 2;
         exit when Count = 0;
         Result := Result + 1;
      end loop;
      return Result;
   end Log2;

   procedure Get
             (  Widget       : not null access
                               Gtk_Binary_Factory_Record;
                Feature      : out Feature_Handle;
                Independent  : out Boolean;
                Bit_Position : out Natural
             )  is
      Position : constant GInt := Get_Active (Widget.Bit_Combo);
   begin
      Feature := Widget.Get_Feature;
      if Position < 0 then
         Set_Hint
         (  Widget.Bit_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise Constraint_Error with Style_Get (Widget, "no-bit-error");
      end if;
      Independent  := Widget.Independent.Get_Active;
      Bit_Position := Natural (Position);
   end Get;

   function Get_Feature
            (  Widget : not null access Gtk_Binary_Factory_Record
            )  return Feature_Handle is
      Path   : constant Item_Path := Get_Path (Widget.Feature_Entry);
      Store  : Storage_Handle;
      Object : Deposit_Handle;
   begin
      if Path'Length = 0 then
         raise Constraint_Error with
               Style_Get (Widget, "missing-feature-error");
      end if;
      Check
      (  Widget.Feature_Entry.Get_Picker,
         Path,
         Get_Constraint (Widget.Feature_Entry),
         Store,
         Object
      );
      if (  not Store.Is_Valid
         or else
            not Object.Is_Valid
         or else
            Object.Ptr.all not in Feature_Object'Class
         )
      then
         raise Constraint_Error with
              Style_Get (Widget, "wrong-feature-error");
      end if;
      return To_Feature_Handle (Object);
   exception
      when Picking_Error =>
         Set_Hint
         (  Widget.Feature_Hint,
            Widget,
            Conflicting,
            Is_Editable (Widget)
         );
         raise;
      when others =>
         Set_Hint
         (  Widget.Feature_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise;
   end Get_Feature;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             )  is
   begin
      Factory.Modified := True;
      if Factory.Bit_Combo.Get_Active >= 0 then
         Set_Hint
         (  Factory.Bit_Hint,
            Factory,
            Checked,
            Is_Editable (Factory)
         );
      end if;
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

   function Create
            (  Widget : not null access Gtk_Binary_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Feature      : Feature_Handle;
      Independent  : Boolean;
      Bit_Position : Natural;
   begin
      Widget.Get (Feature, Independent, Bit_Position);
      if Independent then
         return
            Fuzzy.Feature.Binary.Mutually_Independent.Create
            (  Name,
               Feature,
               Bit_Position
            );
      else
         return
            Fuzzy.Feature.Binary.Create
            (  Name,
               Feature,
               Bit_Position
            );
      end if;
   end Create;

   function Edited
            (  Widget : not null access Gtk_Binary_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Binary_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "base-feature-label",
               Nick    => "Feature",
               Blurb   => "Base feature label",
               Default => "Base feature"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "bit-label",
               Nick    => "Bit",
               Blurb   => "The bit position label",
               Default => "Base's bit"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "independent-label",
               Nick    => "Independent",
               Blurb   => "Independent bit label",
               Default => "Independent bit"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-feature-error",
               Nick    => "No feature",
               Blurb   => "No feature message",
               Default => (  "A base feature must be specified. The "
                          &  "feature value is to be determined from "
                          &  "bits of positions of the base feature "
                          &  "domain values"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "no-bit-error",
               Nick    => "No bit error",
               Blurb   => "No bit selected error",
               Default => "No bit selected"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "wrong-feature-error",
               Nick    => "Wrong feature",
               Blurb   => "Wrong feature message",
               Default => "The path does not specify a feature"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget      : out Gtk_Binary_Factory;
                Picker      : not null access
                              Gtk_Object_Picker_Record'Class;
                Constraint  : Picker_Constraint;
                Feature     : Feature_Handle := No_Feature;
                Editable    : Boolean        := True;
                Independent : Boolean        := False
             )  is
   begin
      if (  Feature.Is_Valid
         and then
            not Fuzzy.Feature.Binary.Is_Binary (Feature)
         and then
            not Fuzzy.Feature.Binary.Mutually_Independent.Is_Bit
                (  Feature
         )      )
      then
         raise Constraint_Error with "Not a binary feature";
      end if;
      Widget := new Gtk_Binary_Factory_Record;
      begin
         Initialize
         (  Widget,
            Picker,
            Constraint,
            Feature,
            Editable,
            Independent
         );
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
             (  Widget      : not null access
                              Gtk_Binary_Factory_Record'Class;
                Picker      : not null access
                              Gtk_Object_Picker_Record'Class;
                Constraint  : Picker_Constraint;
                Feature     : Feature_Handle;
                Editable    : Boolean;
                Independent : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 2, 5, False);
      Widget.Set_Homogeneous (False);
         -- Labels
      Gtk_New (Widget.Feature_Label);
      Widget.Feature_Label.Set_Halign (Align_End);
      Widget.Feature_Label.Set_Valign (Align_Center);
--    Widget.Feature_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Bit_Label);
      Widget.Bit_Label.Set_Halign (Align_End);
      Widget.Bit_Label.Set_Valign (Align_Center);
--    Widget.Bit_Label.Set_Alignment (1.0, 0.5);
         -- Hints
      Gtk_New_HBox (Widget.Feature_Hint);
      Gtk_New_HBox (Widget.Bit_Hint);
         -- Entries
      Gtk_New
      (  Widget   => Widget.Feature_Entry,
         Name     => "source feature",
         Hint     => Widget.Feature_Hint,
         Picker   => Picker,
         Editable => Editable
      );
      Gtk_New (Widget.Bit_Combo);
      Gtk_New (Widget.Independent);
      Widget.Independent.Set_Alignment (1.0, 0.5);
      if Fuzzy.Feature.Binary.Mutually_Independent.Is_Bit (Feature) then
         Set_Feature
         (  Widget,
            Fuzzy.Feature.Binary.Get_Source (Feature),
            True
         );
         Widget.Bit_Combo.Set_Active
         (  GInt (Fuzzy.Feature.Binary.Get_Bit_Position (Feature))
         );
         Widget.Independent.Set_Active (True);
      elsif Fuzzy.Feature.Binary.Is_Binary (Feature) then
         Set_Feature
         (  Widget,
            Fuzzy.Feature.Binary.Get_Source (Feature),
            True
         );
         Widget.Bit_Combo.Set_Active
         (  GInt (Fuzzy.Feature.Binary.Get_Bit_Position (Feature))
         );
      else
         Widget.Independent.Set_Active (Independent);
         begin
            Set_Feature (Widget, Get_Feature (Widget), False);
         exception
            when others =>
               null;
         end;
      end if;
         -- Row 1
      Widget.Attach
      (  Widget.Feature_Label,
         0, 1, 0, 1,
         Xoptions => Gtk.Enums.Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Feature_Entry,
         1, 4, 0, 1,
         Xoptions => Gtk.Enums.Fill or Expand,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Feature_Hint,
         4, 5, 0, 1,
         Xoptions => Gtk.Enums.Fill,
         Yoptions => Shrink
      );
         -- Row 2
      Widget.Attach
      (  Widget.Bit_Label,
         0, 1, 1, 2,
         Xoptions => Gtk.Enums.Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Bit_Combo,
         1, 2, 1, 2,
         Xoptions => Gtk.Enums.Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Bit_Hint,
         2, 3, 1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Independent,
         3, 4, 1, 2,
         Xoptions => Gtk.Enums.Fill or Expand,
         Yoptions => Shrink
      );

      Handlers.Connect
      (  Widget,
         "style-updated",
         Handlers.To_Marshaller (Style_Updated'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Get_Entry (Widget.Feature_Entry),
         "changed",
         Handlers.To_Marshaller (Set_Feature'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Bit_Combo,
         "changed",
         Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Independent,
         "toggled",
         Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Combine (Constraint, Get_Constraint (Widget.Feature_Entry));
      Widget.Feature_Entry.Set_Sensitive (Editable);
      Widget.Bit_Combo.Set_Sensitive (Editable);
      Widget.Independent.Set_Sensitive (Editable);
      Style_Updated (Widget, Widget.all'Access);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Binary_Factory_Record
            )  return Boolean is
   begin
      return Widget.Feature_Entry.Is_Editable;
   end Is_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Binary_Factory_Record
             )  is
      Feature      : Feature_Handle;
      Independent  : Boolean;
      Bit_Position : Natural;
   begin
      Get (Widget, Feature, Independent, Bit_Position);
   end Verify;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Binary_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         Set_Sensitive (Widget.Feature_Entry, Editable);
         Set_Sensitive (Widget.Bit_Combo,     Editable);
         Set_Sensitive (Widget.Independent,   Editable);
      end if;
   end Set_Editable;

   procedure Set_Feature
             (  Factory     : not null access Gtk_Binary_Factory_Record;
                Feature     : Feature_Handle;
                Change_Path : Boolean
             )  is
   begin
      Remove_All (Factory.Bit_Combo);
      if Change_Path then
         Set_Path
         (  Factory.Feature_Entry,
            Get_Path
            (  Get_Picker (Factory.Feature_Entry),
               To_Deposit_Handle (Feature)
         )  );
      end if;
      Append_Text (Factory.Bit_Combo, "0 (MSB)");
      for Index in 1..Log2 (Get_Cardinality (Feature)) - 1 loop
         Append_Text (Factory.Bit_Combo, Image (Index));
      end loop;
      Set_Size_Request (Factory.Bit_Combo);
   end Set_Feature;

   procedure Set_Feature
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             )  is
   begin
      Set_Feature (Factory, Get_Feature (Factory), True);
      Changed (Widget, Factory);
   exception
      when others =>
         null;
   end Set_Feature;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             )  is
   begin
      Set_Text
      (  Factory.Feature_Label,
         Style_Get (Factory, "base-feature-label")
      );
      Set_Text (Factory.Bit_Label, Style_Get (Factory, "bit-label"));
      Set_Label
      (  Factory.Independent,
         Style_Get (Factory, "independent-label")
      );
      Set_Hint
      (  Factory.Feature_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
      Set_Hint (Factory.Bit_Hint, Factory, None, Is_Editable (Factory));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

end Gtk.Fuzzy_Feature.Binary_Factory;
