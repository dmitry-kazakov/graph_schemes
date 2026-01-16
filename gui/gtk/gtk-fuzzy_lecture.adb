--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Lecture                           Luebeck            --
--  Implementation                                 Summer, 2006       --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Gtk_Icon_Factory;       use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;                use GLib.Messages;
with GLib.Object;                  use GLib.Object;
with GLib.Properties;              use GLib.Properties;
with GLib.Properties.Creation;     use GLib.Properties.Creation;
with GLib.Properties.Icon_Size;    use GLib.Properties.Icon_Size;
with GLib.Types;                   use GLib.Types;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Cell_Renderer;            use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Fuzzy;      use Gtk.Cell_Renderer_Fuzzy;
with Gtk.Cell_Renderer_PixBuf;     use Gtk.Cell_Renderer_PixBuf;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.Fuzzy_Feature;            use Gtk.Fuzzy_Feature;
with Gtk.Image;                    use Gtk.Image;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;

with Ada.Unchecked_Deallocation;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Lecture is
   use Fuzzy.Feature.Handle;
   use Fuzzy.Feature.Handle.Edit;
   use Gtk.Tree_Model;
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Lecture." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Input_Parameters'Class,
             Input_Parameters_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Output_Parameters'Class,
             Output_Parameters_Ptr
          );

   procedure Added
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
      Data : Added_Data (Observer'Access);
   begin
      Request (Data);
   end Added;

   procedure Changed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             )  is
   begin
      null;
   end Changed;

   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             )  is
   begin
      null;
   end Deleted;

   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
      Data : Deleted_Data (Observer'Access);
   begin
      Request (Data);
   end Deleted;

   procedure Finalize (Observer : in out Fuzzy_Lecture_Observer) is
   begin
      Free (Observer.Input);
      Free (Observer.Output);
      Finalize (Lecture_Observer (Observer));
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

   function Get_Iter
            (  Widget     : not null access Gtk_Fuzzy_Lecture_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter is
   begin
      return Get_Iter (Widget.Model, Example, Complement);
   end Get_Iter;

   function Get_Lesson
            (  Widget : not null access Gtk_Fuzzy_Lecture_Record
            )  return Lecture_Handle is
      Result : Lecture_Handle;
   begin
      if Widget.Model /= null then
         Result := Get_Lesson (Widget.Model);
      end if;
      return Result;
   end Get_Lesson;

   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Lecture_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.View;
   end Get_Tree_View;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Scrolled_Window.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            GLib.Properties.Icon_Size.Property.Gnew_Enum
            (  Name  => "example-type-icon-size",
               Nick  => "Example type icon size",
               Blurb =>
                  (  "Size of the icon shown in the title of the "
                  &  "example type column"
                  ),
               Default =>
                  Gtk_Icon_Size_Enum'Val (Icon_Size_Small_Toolbar)
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "example-no-icon",
               Nick    => "Example no. icon",
               Default => Number_Icon,
               Blurb   =>
                  (  "The stock ID of the icon shown in the title "
                  &  "of the example number column"
         )  )     );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            GLib.Properties.Icon_Size.Property.Gnew_Enum
            (  Name  => "example-no-icon-size",
               Nick  => "Example no. icon size",
               Blurb =>
                  (  "Size of the icon shown in the title of the "
                  &  "example number column"
                  ),
               Default =>
                  Gtk_Icon_Size_Enum'Val (Icon_Size_Small_Toolbar)
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "example-type-icon",
               Nick    => "Example type icon",
               Default => Confirm_Icon,
               Blurb   =>
                  (  "The stock ID of the icon shown in the title "
                  &  "of the example type column"
         )  )     );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "feature-title-box-spacing",
               Nick    => "Feature title spacing",
               Blurb   => "Spacing in the feature title boxes",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Lecture) is
   begin
      Widget := new Gtk_Fuzzy_Lecture_Record;
      begin
         Gtk.Fuzzy_Lecture.Initialize (Widget);
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
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record'Class
             )  is
      Column         : Gtk_Tree_View_Column;
      Text_Renderer  : Gtk_Cell_Renderer_Text;
      Image_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Column_No      : Gint;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Scrolled_Window.Initialize (Widget);
      Widget.Set_Policy (Policy_Automatic, Policy_Automatic);

      Gtk_New (Widget.View);
      Widget.Add (Widget.View);
      Widget.View.Set_Rules_Hint (True);
      Gtk_New (Column);
      Gtk_New (Image_Renderer);
      Column.Pack_Start (Image_Renderer, True);
      Add_Stock_Attribute (Column, Image_Renderer, 0);
      Column_No := Append_Column (Widget.View, Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (0);

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Set_Property
      (  Text_Renderer,
         Cell_Background_Property,
         "light grey"
      );
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute (Text_Renderer, "text", 1);
      Column_No := Append_Column (Widget.View, Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (1);

      Style_Updated (Widget.View, Widget.all'Unchecked_Access);

      Handlers.Connect
      (  Widget.View,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
   end Initialize;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults
             )  is

      procedure Add_Value_Column (Feature : Feature_Handle) is
         Column    : Gtk_Tree_View_Column;
         Renderer  : Gtk_Cell_Renderer_Fuzzy;
         Column_No : GInt;
      begin
         Gtk_New (Column);
         Gtk_New (Renderer, Create (Feature, Input, Output));
         Column.Pack_Start (Renderer, True);
         Column_No := Append_Column (Widget.View, Column) - 1;
         Add_Attribute
         (  Column,
            Renderer,
            "classification-value",
            Column_No
         );
         Column.Set_Resizable (True);
         declare
            Title : Gtk_VBox;
            Name  : Gtk_HBox;
            Image : Gtk_Image;
            Label : Gtk_Label;
         begin
            Gtk_New_VBox (Title);
            Gtk_New_HBox (Name);
            Title.Pack_Start (Name, False, False);

            Gtk_New
            (  Image,
               Get_Feature_Icon (Get_Class (Feature)),
               Gtk.Enums.Icon_Size_Menu
            );
            Name.Pack_Start (Image, False, False);
            Gtk_New (Label, Get_Name (Feature));
            Name.Pack_Start (Label, True, True);

            if (  not Output.Put_Units
               and then
                  Ptr (Feature).all in Domain_Feature_Object'Class
               )
            then
               declare
                  Frame : Gtk_Frame;
                  Scale : constant UTF8_String :=
                                   Get_Scale_Text
                                   (  Domain_Feature_Object'Class
                                      (  Ptr (Feature).all
                                      ),
                                      Output
                                   );
               begin
                  if Scale'Length > 0 then
                     Gtk_New (Frame);
                     Title.Pack_Start (Frame, True, True);
                     Gtk_New (Label);
                     Add (Frame, Label);
                     Label.Set_Text (Scale);
                  end if;
               end;
            end if;
            Title.Show_All;
            Column.Set_Widget (Title);
            declare
               Parent    : constant Gtk_Widget := Get_Parent (Title);
               Alignment : Gtk_Alignment;
            begin
               if Parent.all in Gtk_Alignment_Record'Class then
                  Alignment :=
                     Gtk_Alignment_Record'Class
                     (  Parent.all
                     ) 'Unchecked_Access;
                  Set (Alignment, 0.0, 0.0, 1.0, 1.0);
               end if;
            end;
         end;
         Column.Set_Sort_Column_Id (Column_No);
      end Add_Value_Column;

      Input_Ptr  : Input_Parameters_Ptr;
      Output_Ptr : Output_Parameters_Ptr;

   begin
      if Widget.Model /= null then
         Widget.View.Set_Model (Null_Gtk_Tree_Model);
         Widget.Model := null;
      end if;
      declare -- Removing columns
         Column : Gtk_Tree_View_Column := Get_Column (Widget.View, 2);
         Result : GInt;
      begin
         while Column /= null loop
            Result := Remove_Column (Widget.View, Column);
            Column := Get_Column (Widget.View, 2);
         end loop;
      end;
      if Is_Valid (Lesson) then
         Input_Ptr  := new Input_Parameters'Class'(Input);
         Output_Ptr := new Output_Parameters'Class'(Output);
         Invalidate (Widget.Observer);
         Gtk_New (Widget.Model, Lesson);
         for Index in 1..Get_Features_Number (Lesson) loop
            Add_Value_Column (Get_Feature (Lesson, Index));
         end loop;
         Widget.View.Set_Model (To_Interface (Widget.Model));
         Unref (Widget.Model);
         Set
         (  Widget.Observer,
            new Fuzzy_Lecture_Observer
                (  Widget.all'Unchecked_Access,
                   Ptr (Lesson)
         )      );
         declare
            Observer : Fuzzy_Lecture_Observer renames
               Fuzzy_Lecture_Observer (Ptr (Widget.Observer).all);
         begin
            Observer.Input  := Input_Ptr;
            Observer.Output := Output_Ptr;
            Input_Ptr  := null;
            Output_Ptr := null;
         end;
      else
         Invalidate (Widget.Observer);
      end if;
      if Widget.View /= null then
         declare
            Dummy  : GInt; -- Sizing tree view to at leas 600x500
            Height : GInt;
            Width  : GInt;
         begin
            Widget.View.Columns_Autosize;
            Widget.View.Get_Preferred_Height (Dummy, Height);
            Widget.View.Get_Preferred_Width  (Dummy, Width);
            Widget.View.Set_Size_Request
            (  GInt'Min (Width,  600),
               GInt'Min (Height, 500)
            );
         end;
      end if;
   exception
      when others =>
         Free (Input_Ptr);
         Free (Output_Ptr);
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle;
                Size   : Positive;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Put
      (  Widget => Widget,
         Lesson => Lesson,
         Input  => Input,
         Output => Output
      );
   end Put;

   procedure Renamed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             )  is
      Data : Renamed_Data
             (  Observer'Access,
                Feature.Self,
                New_Name'Length
             );
   begin
      Data.Name := New_Name;
      Request (Data);
   end Renamed;

   procedure Service (Data : in out Added_Data) is
   begin
      Put
      (  Data.Observer.Widget,
         Data.Observer.Widget.Get_Lesson,
         Data.Observer.Input.all,
         Data.Observer.Output.all
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service added")
         )  );
   end Service;

   procedure Service (Data : in out Deleted_Data) is
   begin
      Put
      (  Data.Observer.Widget,
         Data.Observer.Widget.Get_Lesson,
         Data.Observer.Input.all,
         Data.Observer.Output.all
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service deleted")
         )  );
   end Service;

   procedure Service (Data : in out Renamed_Data) is
      Observer : Fuzzy_Lecture_Observer renames Data.Observer.all;

      procedure Set_Name (Position : GInt) is
         use Widget_List;
         Widget : Gtk_Widget :=
                  Observer.Widget.View.Get_Column (Position).Get_Widget;
         List   : Glist;
         Item   : Glist;
      begin
         if Widget = null then
            return;
         end if;
         if Widget.all not in Gtk_VBox_Record'Class then
            return;
         end if;
         List :=
            Get_Children
            (  Gtk_VBox_Record'Class (Widget.all)'Unchecked_Access
            );
         if List = Null_List then
            return;
         end if;
         Widget := Get_Data (List);
         Free (List);
         if Widget = null then
            return;
         end if;
         if Widget.all not in Gtk_HBox_Record'Class then
            return;
         end if;
         List :=
            Get_Children
            (  Gtk_HBox_Record'Class (Widget.all
            ) 'Unchecked_Access);
         if List = Null_List then
            return;
         end if;
         Item := Next (List);
         if Item = Null_List then
            Free (List);
            return;
         end if;
         Widget := Get_Data (Item);
         Free (List);
         if Widget = null then
            return;
         end if;
         if Widget.all not in Gtk_Label_Record'Class then
            return;
         end if;
         Set_Text
         (  Gtk_Label_Record'Class (Widget.all)'Unchecked_Access,
            Data.Name
         );
      end Set_Name;
   begin
      Set_Name
      (  GInt
         (  Get_Feature_Column (Observer.Widget.Model, Data.Feature.all)
         -  1
      )  );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service Renamed")
         )  );
   end Service;

   procedure Set_Visible
             (  Widget     : not null access Gtk_Fuzzy_Lecture_Record;
                Example    : Positive;
                Complement : Boolean;
                Feature    : Feature_Handle := No_Feature
             )  is
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Lesson : Lecture_Handle;
   begin
      Lesson := Widget.Get_Lesson;
      Row    := Widget.Get_Iter (Example, Complement);
      Path   := Get_Path (To_Interface (Widget.Model), Row);
      if Feature.Is_Valid and then Is_Valid (Lesson) then
         for Index in 1..Get_Features_Number (Lesson) loop
            if Get_Feature (Lesson, Index) = Feature then
               Scroll_To_Cell
               (  Widget.View,
                  Path,
                  Get_Column (Widget.View, GInt (Index + 1)),
                  True,
                  0.5,
                  0.5
               );
               Path_Free (Path);
               return;
            end if;
         end loop;
      end if;
      Widget.View.Scroll_To_Cell (Path, null, True, 0.5, 0.5);
      Path_Free (Path);
   exception
      when Constraint_Error =>
         null;
   end Set_Visible;

   procedure Show
             (  Lesson    : Lecture_Handle;
                Title     : UTF8_String             := "";
                Button    : UTF8_String             := "_OK";
                Parent    : Gtk_Window              := null;
                Input     : Input_Parameters'Class  := Input_Defaults;
                Output    : Output_Parameters'Class := Output_Defaults;
                Flags     : Gtk_Dialog_Flags        := Modal
             )  is
      Dialog : Gtk_Dialog;
      View   : Gtk_Fuzzy_Lecture;
   begin
      Gtk_New (View);
      begin
         Put (View, Lesson, Input, Output);
      exception
         when others =>
            Unref (View);
            raise;
      end;
      Gtk_New (Dialog, Title, Parent, Flags);
      Dialog.Get_Content_Area.Pack_Start (View);
      Show_All (View);
      if Button'Length /= 0 then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Label    => Button,
            Response => Gtk_Response_OK
         );
      end if;
      if Gtk_Response_OK = Run (Dialog) then
         null;
      end if;
      GLib.Object.Checked_Destroy (Dialog);
   end Show;

   procedure Show
             (  Lesson   : Lecture_Handle;
                Size     : Positive;
                Title    : UTF8_String             := "";
                Button   : UTF8_String             := "_OK";
                Parent   : Gtk_Window              := null;
                Input    : Input_Parameters'Class  := Input_Defaults;
                Output   : Output_Parameters'Class := Output_Defaults;
                Flags    : Gtk_Dialog_Flags        := Modal
             )  is
   begin
      Show
      (  Lesson   => Lesson,
         Title    => Title,
         Button   => Button,
         Parent   => Parent,
         Input    => Input,
         Output   => Output,
         Flags    => Flags
      );
   end Show;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Lecture : Gtk_Fuzzy_Lecture
             )  is
      Spacing : constant GUInt :=
                Style_Get (Lecture, "feature-title-box-spacing");
      Column  : Gtk_Tree_View_Column;
      Image   : Gtk_Image;
      Title   : Gtk_Widget;
   begin
      Gtk_New
      (  Image,
         Style_Get (Lecture, "example-type-icon"),
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Lecture, "example-type-icon-size")
      )  );
      Set_Widget (Lecture.View.Get_Column (0), Image);
      Image.Show;
      Gtk_New
      (  Image,
         Style_Get (Lecture, "example-no-icon"),
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Lecture, "example-no-icon-size")
      )  );
      Set_Widget (Lecture.View.Get_Column (1), Image);
      Image.Show;
      Lecture.View.Get_Column (0).Set_Spacing (GInt (Spacing));
      Lecture.View.Get_Column (1).Set_Spacing (GInt (Spacing));
      for Index in 2..GInt'Last loop
         Column := Lecture.View.Get_Column (Index);
         exit when Column = null;
         Title := Get_Widget (Column);
         if Title /= null and then Title.all in Gtk_VBox_Record'Class
         then
            declare
               use Widget_List;
               Title_Box : constant Gtk_VBox :=
                  Gtk_VBox_Record'Class (Title.all)'Unchecked_Access;
               List : Glist := Get_Children (Title_Box);
               Item : Gtk_Widget;
            begin
               Title_Box.Set_Spacing (GInt (Spacing));
               Title_Box.Set_Border_Width (Spacing);
               if List /= Null_List then
                  Item := Get_Data (List);
                  if (  Item /= null
                     and then
                        Item.all in Gtk_HBox_Record'Class
                     )
                  then
                     Set_Spacing
                     (  Gtk_HBox_Record'Class (Item.all)'Access,
                        GInt (Spacing)
                     );
                  end if;
               end if;
               Free (List);
            end;
         end if;
      end loop;
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

end Gtk.Fuzzy_Lecture;
