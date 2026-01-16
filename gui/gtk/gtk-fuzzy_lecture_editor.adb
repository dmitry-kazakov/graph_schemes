--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Lecture_Editor                    Luebeck            --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Lecture;                use Fuzzy.Lecture;
with GLib.Object;                  use GLib.Object;
with GLib.Messages;                use GLib.Messages;
with GLib.Properties;              use GLib.Properties;
with GLib.Properties.Creation;     use GLib.Properties.Creation;
with GLib.Properties.Icon_Size;    use GLib.Properties.Icon_Size;
with GLib.Types;                   use GLib.Types;
with GLib.Values;                  use GLib.Values;
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Cell_Renderer_PixBuf;     use Gtk.Cell_Renderer_PixBuf;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.Fuzzy_Feature;            use Gtk.Fuzzy_Feature;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Separator;                use Gtk.Separator;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;

with Ada.Unchecked_Deallocation;
with GLib.Values.Fuzzy;
with GLib.Object.Checked_Destroy;
with Interfaces.C.Strings;
with Strings_Edit.Integers;

package body Gtk.Fuzzy_Lecture_Editor is
   use Fuzzy.Feature.Handle.Edit;
   use Fuzzy.Feature.Handle;
   use Fuzzy.Gtk_Icon_Factory;
   use Gtk.Box;
   use Gtk.Enums;
   use Gtk.Missed;
   use Gtk.Tree_Model;
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
             (  0 => Interfaces.C.Strings.
                     New_String ("modified-changed"),
                1 => Interfaces.C.Strings.
                     New_String ("features-selection-changed")
             );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Lecture_Editor." & Name;
   end Where;

   procedure Add
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Example : constant Positive :=
                         Natural (N_Children (Editor.Model) / 2) + 1;
   begin
      Add_Example (Editor.Model, Example);
      declare
         Path : constant Gtk_Tree_Path :=
                Get_Path
                (  Editor.Model,
                   Get_Iter (Editor.Model, Example, True)
                );
      begin
         Scroll_To_Cell
         (  Editor.View,
            Path,
            Editor.View.Get_Column (0),
            False,
            0.0,
            0.0
         );
         Path_Free (Path);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add")
         )  );
   end Add;

   procedure Add_Random
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Example : constant Positive :=
                         Natural (N_Children (Editor.Model) / 2) + 1;
   begin
      Add_Random_Singleton_Example (Editor.Model, Example);
      declare
         Path : constant Gtk_Tree_Path :=
                Get_Path
                (  Editor.Model,
                   Get_Iter (Editor.Model, Example, True)
                );
      begin
         Scroll_To_Cell
         (  Editor.View,
            Path,
            Editor.View.Get_Column (0),
            False,
            0.0,
            0.0
         );
         Path_Free (Path);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Random")
         )  );
   end Add_Random;

   procedure Added_Column
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      No : constant Natural := Get_Inserted_Column (Editor.Model);
   begin
      Ref (Editor.Model);
      Editor.View.Set_Model (Null_Gtk_Tree_Model);
      if No > 0 then
         Reset_Columns (Editor, No);
      end if;
      Editor.View.Set_Model (To_Interface (Editor.Model));
      Unref (Editor.Model);
      Set_Sensitive
      (  Editor.Add,
         Editor.Model.Get_Features_Number > 0
      );
      Set_Sensitive
      (  Editor.Random,
         Editor.Model.Get_Features_Number > 0
      );
   end Added_Column;

   procedure Add_Feature
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             )  is
      Lesson : Lecture_Handle;
   begin
      if not Widget.Model.Is_In (Feature) then
         Add_Column
         (  Widget.Model,
            Feature,
            Widget.Model.Get_Features_Number + 1
         );
         Style_Updated (Widget, Widget.all'Unchecked_Access);
      end if;
   end Add_Feature;

   procedure Add_Value_Column
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             )  is
      Column   : Gtk_Tree_View_Column;
      Title    : Column_Title;
      Renderer : Cell_Renderer;
   begin
      Gtk_New (Column);
      Renderer := new Cell_Renderer_Record;
      Initialize
      (  Renderer,
         Create (Feature, Widget.Input.all, Widget.Output.all),
         Gtk.Cell_Renderer_Fuzzy.Get_Type,
         GLib.Values.Fuzzy.GType_Set
      );
      Renderer.Set_Editable_Undefined (True);
      Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
      Column.Pack_Start (Renderer, True);
      Renderer.Column := Widget.View.Append_Column (Column) - 2;
      Add_Attribute
      (  Column,
         Renderer,
         "classification-value",
         Renderer.Column
      );
      Column.Set_Resizable (True);
      Renderer_Handlers.Connect
      (  Renderer,
         "commit",
         Commit'Access,
         Widget.all'Unchecked_Access
      );
      Gtk_New (Title, Feature, Widget.Output.all);
      Column.Set_Widget (Title);
      declare
         Parent    : constant Gtk_Widget := Get_Parent (Title);
         Alignment : Gtk_Alignment;
      begin
         if Parent.all in Gtk_Alignment_Record'Class then
            Alignment :=
               Gtk_Alignment_Record'Class (Parent.all)'Unchecked_Access;
            Alignment.Set (0.0, 0.0, 1.0, 1.0);
         end if;
      end;
      Title.Show_All;
      Widget.Titles.Put (Renderer.Column, Title);
      Column_Handlers.Connect
      (  Title,
         "clicked",
         Column_Selected'Access,
         Widget.all'Unchecked_Access
      );
   end Add_Value_Column;

   procedure Browsing_Selection
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Row_Set_Ptr
             )  is
      use Row_Sets;
      Indices : constant GInt_Array := Get_Indices (Path);
   begin
      Data.Add (Indices (Indices'First));
   end Browsing_Selection;

   procedure Column_Selected
             (  Column : access Column_Title_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      if not Editor.Action then
         Editor.Action := True;
         declare
            This  : Gtk_Tree_View_Column;
            Title : Column_Title;
         begin
            for Index in 2..GInt'Last loop
               This := Editor.View.Get_Column (Index);
               exit when This = null;
               Title := Editor.Titles.Get (Index - 1);
               if Title /= Column and then Title.Get_Active then
                  Title.Set_Active (False);
               end if;
            end loop;
            Features_Selection_Changed (Editor);
         exception
            when Error : others =>
               Log
               (  Fuzzy_ML_Domain,
                  Log_Level_Critical,
                  (  "Fault: "
                  &  Exception_Information (Error)
                  &  Where ("Column_Selected")
               )  );
         end;
         Editor.Action := False;
      end if;
   end Column_Selected;

   procedure Commit
             (  Cell   : access Cell_Renderer_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Row   : constant Gtk_Tree_Iter :=
              Get_Iter_From_String
              (  To_Interface (Editor.Model),
                 Cell.Get_Path
              );
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value := Get (Cell);
         Editor.Model.Put (Row, Cell.Column, Value);
         Unset (Value);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   procedure Copy
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Copy_Rows (Editor.Model, Editor.Get_Selection);
      if Editor.Model.Get_Examples_Number > 0 then
         declare
            Path : constant Gtk_Tree_Path :=
                   Editor.Model.Get_Path
                   (  Editor.Model.Get_Iter
                      (  Editor.Model.Get_Examples_Number,
                         True
                   )  );
         begin
            Editor.View.Scroll_To_Cell
            (  Path,
               Editor.View.Get_Column (0),
               False,
               0.0,
               0.0
            );
            Path_Free (Path);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Copy")
         )  );
   end Copy;

   procedure Delete
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Editor.Model.Delete_Rows (Editor.Get_Selection);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete")
         )  );
   end Delete;

   procedure Deleted_Column
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      No : constant Natural := Editor.Model.Get_Deleted_Column;
   begin
      Ref (Editor.Model);
      Editor.View.Set_Model (Null_Gtk_Tree_Model);
      if No > 0 then
         Reset_Columns (Editor, No);
      end if;
      Editor.View.Set_Model (To_Interface (Editor.Model));
      Unref (Editor.Model);
      Editor.Add.Set_Sensitive
      (  Editor.Model.Get_Features_Number > 0
      );
      Editor.Random.Set_Sensitive
      (  Editor.Model.Get_Features_Number > 0
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Delete_Column")
         )  );
   end Deleted_Column;

   procedure Delete_Feature
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             )  is
   begin
      Widget.Model.Delete_Column
      (  Widget.Model.Get_Column (Feature)
      );
   exception
      when Constraint_Error =>
         null;
   end Delete_Feature;

   procedure Destroyed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
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
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Column_Titles.Unbounded_Array,
                Column_Title_Array_Ptr
             );
   begin
      Free (Editor.Input);
      Free (Editor.Output);
      Free (Editor.Titles);
   end Destroyed;

   procedure Example_No_Func
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Path    : constant Gtk_Tree_Path := Get_Path (Model, Iter);
      Indices : constant GInt_Array := Get_Indices (Path);
   begin
      if Indices (Indices'First) mod 2 = 0 then
         Set_Property
         (  Cell,
            Text_Property,
            Strings_Edit.Integers.Image
            (  Integer (Indices (Indices'First) / 2) + 1
         )  );
      else
         Set_Property (Cell, Text_Property, "");
      end if;
      Path_Free (Path);
   end Example_No_Func;

   procedure Features_Selection_Changed
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             )  is
   begin
      Handlers.Emit_By_Name (Widget, "features-selection-changed");
   end Features_Selection_Changed;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Box.Get_VBox_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name,
            Signals      => Signals
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "spacing",
               Nick    => "spacing",
               Blurb   => "Spacing in the widget",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "button-spacing",
               Nick    => "Button spacing",
               Blurb   => "Spacing in the button box",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
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

   function Get_Selected_Feature
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Feature_Handle is
      Column : Gtk_Tree_View_Column;
   begin
      for Index in 2..GInt'Last loop
         Column := Widget.View.Get_Column (Index);
         exit when Column = null;
         if Widget.Titles.Get (Index - 1).Get_Active then
            return Widget.Model.Get_Feature (Positive (Index - 1));
         end if;
      end loop;
      return No_Feature;
   end Get_Selected_Feature;

   function Get_Selection
            (  Editor : not null access
                        Gtk_Fuzzy_Lecture_Editor_Record'Class
            )  return Row_Sets.Set is
      Data : aliased Row_Sets.Set;
   begin
      Foreach.Selected_Foreach
      (  Editor.View.Get_Selection,
         Browsing_Selection'Access,
         Data'Unchecked_Access
      );
      return Data;
   end Get_Selection;

   function Get_Selection
            (  Editor : not null access
                        Gtk_Fuzzy_Lecture_Editor_Record'Class
            )  return GInt_Array is
      use Row_Sets;
      Data : aliased Row_Sets.Set;
   begin
      Foreach.Selected_Foreach
      (  Editor.View.Get_Selection,
         Browsing_Selection'Access,
         Data'Unchecked_Access
      );
      declare
         Result : GInt_Array (1..Data.Get_Size);
      begin
         for Index in Result'Range loop
            Result (Index) := Data.Get (Positive (Index));
         end loop;
         return Result;
      end;
   end Get_Selection;

   function Get_Buttons_Box
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.Buttons;
   end Get_Buttons_Box;

   procedure Get_Examples
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : in out Lecture_Handle;
                Viewer : Indicator_Handle
             )  is
   begin
      Widget.Get_Examples (Lesson, Viewer.Ptr);
   end Get_Examples;

   procedure Get_Examples
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
   begin
      Widget.Model.Get (Lesson, Viewer);
   end Get_Examples;

   function Get_Modified
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Boolean is
   begin
      return Widget.Model.Can_Undo;
   end Get_Modified;

   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.View;
   end Get_Tree_View;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Lecture_Editor) is
   begin
      Widget := new Gtk_Fuzzy_Lecture_Editor_Record;
      begin
         Gtk.Fuzzy_Lecture_Editor.Initialize (Widget);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Lecture_Editor)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Column_Title;
                Feature : Feature_Handle;
                Output  : Output_Parameters'Class
             )  is
   begin
      Widget := new Column_Title_Record;
      begin
         Initialize (Widget, Feature, Output);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Column_Title)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record'Class
             )  is
      Frame          : Gtk_Frame;
      Separator      : Gtk_Separator;
      Scroll         : Gtk_Scrolled_Window;
      Column         : Gtk_Tree_View_Column;
      Text_Renderer  : Gtk_Cell_Renderer_Text;
      Image_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Column_No      : Gint;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize_VBox (Widget);
      Widget.Titles := new Column_Titles.Unbounded_Array;
      Gtk_New_HBox (Widget.Buttons);
      Widget.Pack_Start (Widget.Buttons, False, False);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Frame.Add (Scroll);
      Widget.Pack_Start (Frame);

      Gtk_New (Widget.View);
      Set_Mode (Widget.View.Get_Selection, Selection_Multiple);
      Scroll.Add (Widget.View);
      Widget.View.Set_Rules_Hint (True);
      Gtk_New (Column);
      Gtk_New (Image_Renderer);
      Column.Pack_Start (Image_Renderer, True);
      Add_Stock_Attribute (Column, Image_Renderer, 0);
      Column_No := Widget.View.Append_Column (Column);
      Column.Set_Resizable (True);

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Set_Property
      (  Text_Renderer,
         Cell_Background_Property,
         "light grey"
      );
      Column.Pack_Start (Text_Renderer, True);
      Example_Cell_Data.Set_Cell_Data_Func
      (  Column,
         Text_Renderer,
         Example_No_Func'Access,
         Widget.all'Unchecked_Access
      );
      Column_No := Widget.View.Append_Column (Column);
      Column.Set_Resizable (True);

      Gtk_New (Widget.Undo);
      Widget.Undo.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Undo, False, False);

      Gtk_New (Widget.Redo);
      Widget.Redo.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Redo, False, False);

      Gtk_New_VSeparator (Separator);
      Widget.Buttons.Pack_Start (Separator, False, False);

      Gtk_New (Widget.Add);
      Widget.Add.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Add, False, False);

      Gtk_New (Widget.Random);
      Widget.Random.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Random, False, False);

      Gtk_New (Widget.Delete);
      Widget.Delete.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Delete, False, False);

      Gtk_New (Widget.Copy);
      Widget.Copy.Set_Sensitive (False);
      Widget.Buttons.Pack_Start (Widget.Copy, False, False);

      Handlers.Connect
      (  Widget.Add,
         "clicked",
         Add'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.Copy,
         "clicked",
         Copy'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.Delete,
         "clicked",
         Delete'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.Random,
         "clicked",
         Add_Random'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.Redo,
         "clicked",
         Redo'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.View.Get_Selection,
         "changed",
         Selection_Changed'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.Undo,
         "clicked",
         Undo'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget,
         "destroy",
         Destroyed'Access,
         Widget.all'Unchecked_Access
      );
      Handlers.Connect
      (  Widget.View,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      Style_Updated (Widget.View, Widget.all'Access);
   end Initialize;

   procedure Initialize
             (  Widget  : not null access Column_Title_Record'Class;
                Feature : Feature_Handle;
                Output  : Output_Parameters'Class
             )  is
      Alignment : Gtk_Alignment;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Gtk.Toggle_Button.Initialize (Widget);
      Widget.Set_Relief (Relief_Half);

      Gtk_New (Widget.Grid, 2, 2, False);
      Widget.Add (Widget.Grid);

      Gtk_New
      (  Widget.Feature_Image,
         Get_Feature_Icon (Feature.Get_Class),
         Gtk.Enums.Icon_Size_Menu
      );
      Widget.Grid.Attach
      (  Widget.Feature_Image,
         0, 1,
         0, 1,
         XOptions => Shrink,
         YOptions => Expand or Fill
      );

      Gtk_New (Widget.Feature_Label, Get_Name (Feature));
      Gtk_New (Alignment, 0.0, 0.0, 0.0, 0.0);
      Alignment.Add (Widget.Feature_Label);
      Widget.Grid.Attach
      (  Alignment,
         1, 2,
         0, 1,
         XOptions => Expand or Fill,
         YOptions => Expand or Fill
      );

      if (  not Output.Put_Units
         and then
            Feature.Ptr.all in Domain_Feature_Object'Class
         )
      then
         declare
            Frame : Gtk_Frame;
            Scale : constant UTF8_String :=
                    Get_Scale_Text
                    (  Domain_Feature_Object'Class (Feature.Ptr.all),
                       Output
                    );
         begin
            if Scale'Length > 0 then
               Gtk_New (Frame);
               Gtk_New (Widget.Scale_Label);
               Frame.Add (Widget.Scale_Label);
               Widget.Scale_Label.Set_Text (Scale);
               Widget.Grid.Attach
               (  Frame,
                  0, 2,
                  1, 2,
                  XOptions => Expand or Fill,
                  YOptions => Shrink
               );
            end if;
         end;
      end if;
   end Initialize;

   function Has_Feature
            (  Widget  : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
               Feature : Feature_Handle
            )  return Boolean is
   begin
      return
         Widget.Model /= null and then Widget.Model.Is_In (Feature);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Has_Feature")
         )  );
         return False;
   end Has_Feature;

   procedure Modified_Changed
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             )  is
   begin
      Handlers.Emit_By_Name (Widget, "modified-changed");
   end Modified_Changed;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
   begin
      Viewer.Reset (Get_Examples_Number (Lesson));
      if Widget.Model /= null then
         Widget.View.Set_Model (Null_Gtk_Tree_Model);
         Widget.Model := null;
      end if;
      declare -- Removing columns
         Column : Gtk_Tree_View_Column := Widget.View.Get_Column (2);
         Result : GInt;
      begin
         while Column /= null loop
            Result := Widget.View.Remove_Column (Column);
            Column := Widget.View.Get_Column (2);
         end loop;
      end;
      declare
         Feature : Feature_Handle;
      begin
         Gtk_New (Widget.Model, Lesson, Viewer);
         if Is_Valid (Lesson) then
            for Index in 1..Get_Features_Number (Lesson) loop
               Widget.Add_Value_Column (Lesson.Get_Feature (Index));
            end loop;
            Widget.Add.Set_Sensitive (   True);
            Widget.Random.Set_Sensitive (True);
         else
            Widget.Add.Set_Sensitive (   False);
            Widget.Random.Set_Sensitive (False);
         end if;
         Widget.View.Set_Model (To_Interface (Widget.Model));
         Unref (Widget.Model);
         Handlers.Connect
         (  Widget.Model,
            "column-deleted",
            Deleted_Column'Access,
            Widget.all'Unchecked_Access
         );
         Handlers.Connect
         (  Widget.Model,
            "column-inserted",
            Added_Column'Access,
            Widget.all'Unchecked_Access
         );
         Handlers.Connect
         (  Widget.Model,
            "redo-changed",
            Redo_Changed'Access,
            Widget.all'Unchecked_Access
         );
         Handlers.Connect
         (  Widget.Model,
            "undo-changed",
            Undo_Changed'Access,
            Widget.all'Unchecked_Access
         );
      exception
         when others =>
            Unref (Widget.Model);
            raise;
      end;
      Style_Updated (Widget, Widget.all'Unchecked_Access);
      Unselect_All (Widget.View.Get_Selection);
      Widget.Copy.Set_Sensitive (False);
      Widget.Delete.Set_Sensitive (False);
      Widget.Redo.Set_Sensitive (False);
      Widget.Undo.Set_Sensitive (False);
      declare
         Size : Gtk_Requisition;
      begin
         Widget.View.Columns_Autosize;    -- Size columns
         Widget.View.Size_Request (Size); -- Query the integral size
         Widget.View.Set_Size_Request     -- Set new size
         (  GInt'Min (Size.Width,  600),
            GInt'Min (Size.Height, 500)
         );
      end;
   exception
      when others =>
         Widget.Model := null;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults;
                Viewer : Indicator_Handle
             )  is
   begin
      Widget.Put (Lesson, Input, Output, Viewer.Ptr);
   end Put;

   procedure Redo
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Editor.Model.Redo;
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

   procedure Redo_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Editor.Redo.Set_Sensitive (Editor.Model.Can_Redo);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Redo_Changed")
         )  );
   end Redo_Changed;

   procedure Reset_Columns
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                From   : Positive
             )  is
      Column  : Gtk_Tree_View_Column;
      Deleted : constant GInt := GInt (From) + 1;
      Count   : GInt;
   begin
      loop
         Column := Widget.View.Get_Column (Deleted);
         exit when Column = null;
         Count := Widget.View.Remove_Column (Column);
      end loop;
      for Index in From..Widget.Model.Get_Features_Number loop
         Widget.Add_Value_Column
         (  Widget.Model.Get_Feature (Index)
         );
      end loop;
   end Reset_Columns;

   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Selected : constant GInt :=
                 Count_Selected_Rows (Editor.View.Get_Selection);
   begin
      Set_Sensitive (Editor.Copy,   Selected > 0);
      Set_Sensitive (Editor.Delete, Selected > 0);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selection_Changed")
         )  );
   end Selection_Changed;

   procedure Set_Stored
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             )  is
   begin
      Set_Stored (Widget.Model);
   end Set_Stored;

   procedure Style_Updated
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
      Spacing : constant GUInt :=
                Style_Get (Editor, "feature-title-box-spacing");
      Column  : Gtk_Tree_View_Column;
      Image   : Gtk_Image;
   begin
      Editor.Set_Spacing (Style_Get (Editor, "spacing"));
      Editor.Buttons.Set_Spacing
      (  Style_Get (Editor, "button-spacing")
      );
      Gtk_New
      (  Image,
         Style_Get (Editor, "example-type-icon"),
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Editor, "example-type-icon-size")
      )  );
      Editor.View.Get_Column (0).Set_Widget (Image);
      Image.Show;
      Gtk_New
      (  Image,
         Style_Get (Editor, "example-no-icon"),
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Editor, "example-no-icon-size")
      )  );
      Editor.View.Get_Column (1).Set_Widget (Image);
      Show (Image);
      Editor.View.Get_Column (0).Set_Spacing (GInt (Spacing));
      Editor.View.Get_Column (1).Set_Spacing (GInt (Spacing));
      for Index in 2..GInt'Last loop
         Column := Editor.View.Get_Column (Index);
         exit when Column = null;
         declare
            Title : constant Column_Title :=
                             Editor.Titles.Get (Index - 1);
         begin
            Title.Grid.Set_Col_Spacings (Spacing);
            Title.Grid.Set_Row_Spacings (Spacing);
         end;
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

   procedure Undo
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Undo (Editor.Model);
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

   procedure Undo_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             )  is
   begin
      Editor.Undo.Set_Sensitive (Editor.Model.Can_Undo);
      Modified_Changed (Editor);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Undo_Changed")
         )  );
   end Undo_Changed;

end Gtk.Fuzzy_Lecture_Editor;
