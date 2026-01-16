--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Widgets                            Luebeck            --
--  Implementation                                 Spring, 2003       --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Confidence_Factors;            use Confidence_Factors;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Feature.Handle.Tables;   use Fuzzy.Feature.Handle.Tables;
with Fuzzy.Graph.Handle;            use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Handle.Edit;       use Fuzzy.Graph.Handle.Edit;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Text_IO;  use Fuzzy.Lecture.Handle.Text_IO;
with GLib;                          use GLib;
with GLib.Values;                   use GLib.Values;
with GNAT.Exception_Actions;        use GNAT.Exception_Actions;
with Gtk.Activity_Indicator;        use Gtk.Activity_Indicator;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Button;                    use Gtk.Button;
with Gtk.Cell_Renderer;             use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;        use Gtk.Cell_Renderer_Text;
with Gtk.Fuzzy_Lecture;             use Gtk.Fuzzy_Lecture;
with Gtk.Fuzzy_Lecture_Diff;        use Gtk.Fuzzy_Lecture_Diff;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Fuzzy_Catalogue;           use Gtk.Fuzzy_Catalogue;
with Gtk.Fuzzy_Graph;               use Gtk.Fuzzy_Graph;
with Gtk.Fuzzy_Feature.Factory;     use Gtk.Fuzzy_Feature.Factory;
with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Store;                use Gtk.List_Store;
with Gtk.Main.Router;               use Gtk.Main.Router;
with Gtk.Widget;                    use Gtk.Widget;
with Gtk.Window;                    use Gtk.Window;
with Gtk.Paned;                     use Gtk.Paned;
with Gtk.Scrolled_Window;           use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                use Gtk.Tree_Model;
with Gtk.Tree_View;                 use Gtk.Tree_View;
with Gtk.Tree_View_Column;          use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;            use Gtk.Tree_Selection;
with Gtk.Tree_Store;                use Gtk.Tree_Store;
with Gtk.Widget.Styles.Store;       use Gtk.Widget.Styles.Store;
with Strings_Edit.UTF8.Maps;        use Strings_Edit.UTF8.Maps;
with Test_Tone_Feature;             use Test_Tone_Feature;
with Test_Day_Of_Week_Feature;      use Test_Day_Of_Week_Feature;

with Gtk.Cell_Renderer_Fuzzy.Feature_Value;
use  Gtk.Cell_Renderer_Fuzzy.Feature_Value;

with Ada.Unchecked_Conversion;
with Fuzzy.Intuitionistic;
with GLib.Values.Feature_Value;
with GNAT.Traceback.Symbolic;
with Gtk.Cell_Renderer.Abstract_Renderer;
with Gtk.Missed;
with Name_Tables;
with System;
--
-- Remove the following line if you do not use the GNAT Ada compiler.
--
with Gtk.Main.Router.GNAT_Stack;

procedure Test_Graph_Schemes_Gtk_Widgets is

   File_Name : constant String := "test_gtk_widgets.css";

   type Local_Callback is access function return Gtk_Widget;

   function Call is
      new Ada.Unchecked_Conversion (System.Address, Local_Callback);

   Window         : Gtk_Window;
   Pane           : Gtk_HPaned;
   Test_Widget    : Gtk_Widget;
   Box            : Gtk_Box;
   Get_CSS_Button : Gtk_Button;

   type Local_Button_Handler is access
      procedure (Widget : access Gtk_Button_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Button_Handler,
             Cb_Gtk_Button_Void
          );
   type Local_Selection_Handler is access
      procedure (Selection : access Gtk_Tree_Selection_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Selection_Handler,
             Cb_Gtk_Tree_Selection_Void
          );
   type Local_Commit_Handler is access procedure
        (  Cell : not null access Gtk.Cell_Renderer.Abstract_Renderer.
                  Gtk_Abstract_Renderer_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Commit_Handler,
             Gtk.Cell_Renderer.Abstract_Renderer.Commit_Callback
          );

   procedure On_Get_CSS (Button : access Gtk_Button_Record'Class) is
      File : File_Type;
   begin
      if Test_Widget /= null then
         Create (File, Out_File, File_Name);
         Put_Styles (File, Test_Widget);
         Close (File);
         Set_Label
         (  Get_CSS_Button,
            File_Name & " has been written. Press to rewrite"
         );
      end if;
   end On_Get_CSS;

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class
             )  is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Value : GValue;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter and then not Has_Child (Model, Iter) then
         Get_Value (Model, Iter, 1, Value);
         if Test_Widget /= null then
            Remove (Pane, Test_Widget);
            Test_Widget := null;
         end if;
         Test_Widget := Call (Get_Address (Value)).all;
         if Test_Widget /= null then
            Pane.Add2 (Test_Widget);
            Show_All (Test_Widget);
         end if;
         Unset (Value);
      end if;
   exception
      when Error : others =>
         Gtk.Main.Router.Trace (Error);
   end On_Selection;
--
-- Individual tests
--
   Factory : Gtk_Fuzzy_Feature_Factory;
   Store   : Gtk_List_Store;

   procedure Test_1_On_Verify
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Factory.Verify;
      Say ("OK");
   exception
      when Error : others =>
         Say (Exception_Message (Error));
   end Test_1_On_Verify;

   function Test_1 return Gtk_Widget is
      Box     : Gtk_VBox;
      Button  : Gtk_Button;
   begin
      Gtk_New_VBox (Box);
      Gtk_New (Factory);
      Gtk_New (Button);
      Set_Label (Button, "Verify");
      Box.Pack_Start (Factory);
      Box.Pack_Start (Button, False, False);
      Button.On_Clicked (+Test_1_On_Verify'Access);
      return Box.all'Access;
   end Test_1;

   function Test_2 return Gtk_Widget is
      View     : Gtk_Fuzzy_Lecture;
      Result   : Lecture_Handle;
      Features : constant Feature_Array :=
      (  Create_Discrete ("L-CORE", "high, mid, low"),
         Create_Discrete ("L-SURF", "high, mid, low"),
         Create_Discrete ("L-O2", "excellent, good, fair, poor"),
         Create_Discrete ("L-BP", "high, mid, low"),
         Create_Discrete ("SURF-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("CORE-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("BP-STBL", "stable, mod-stable, unstable"),
         Create_Float    ("COMFORT", 5, 0.0, 20.0),
         Create_Discrete ("ADM-DECS", "I, S, A")
      );
   begin
      Gtk_New (View);
      Result :=
         Read
         (  File_Name => "../fuzzy_ml-examples/example4.txt",
            Features  => Features
         );
      Put (View, Result);
      return View.all'Access;
   end Test_2;

   function Test_3 return Gtk_Widget is
      Features : constant Feature_Array :=
      (  Create_Discrete ("L-CORE", "high, mid, low"),
         Create_Discrete ("L-SURF", "high, mid, low"),
         Create_Discrete ("L-O2", "excellent, good, fair, poor"),
         Create_Discrete ("L-BP", "high, mid, low"),
         Create_Discrete ("SURF-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("CORE-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("BP-STBL", "stable, mod-stable, unstable"),
         Create_Float    ("COMFORT", 5, 0.0, 20.0),
         Create_Discrete ("ADM-DECS", "I, S, A")
      );
   begin
      Show
      (  Read
         (  File_Name => "../fuzzy_ml-examples/example4.txt",
            Features  => Features
         ),
         Parent => Window
      );
      return null;
   end Test_3;

   function Test_4 return Gtk_Widget is
      View      : Gtk_Fuzzy_Lecture_Diff;
      Result    : Lecture_Handle;
      Reference : Lecture_Handle;
      Features  : constant Feature_Array :=
      (  Create_Discrete ("L-CORE", "high, mid, low"),
         Create_Discrete ("L-SURF", "high, mid, low"),
         Create_Discrete ("L-O2", "excellent, good, fair, poor"),
         Create_Discrete ("L-BP", "high, mid, low"),
         Create_Discrete ("SURF-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("CORE-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("BP-STBL", "stable, mod-stable, unstable"),
         Create_Float    ("COMFORT", 5, 0.0, 20.0),
         Create_Discrete ("ADM-DECS", "I, S, A")
      );
   begin
      Gtk_New (View);
      Result :=
         Read
         (  File_Name => "../fuzzy_ml-examples/example4.txt",
            Features  => Features
         );
      Copy (Reference, Result);
      Put
      (  Reference,
         1,
         Features (Features'First),
         Value ("low", Features (Features'First))
      );
      Put (View, Reference, Result);
      return View.all'Access;
   end Test_4;

   function Test_5 return Gtk_Widget is
      Result    : Lecture_Handle;
      Reference : Lecture_Handle;
      Features  : constant Feature_Array :=
      (  Create_Discrete ("L-CORE", "high, mid, low"),
         Create_Discrete ("L-SURF", "high, mid, low"),
         Create_Discrete ("L-O2", "excellent, good, fair, poor"),
         Create_Discrete ("L-BP", "high, mid, low"),
         Create_Discrete ("SURF-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("CORE-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("BP-STBL", "stable, mod-stable, unstable"),
         Create_Float    ("COMFORT", 5, 0.0, 20.0),
         Create_Discrete ("ADM-DECS", "I, S, A")
      );
   begin
      Result :=
         Read
         (  File_Name => "../fuzzy_ml-examples/example4.txt",
            Features  => Features
         );
      Copy (Reference, Result);
      Put
      (  Reference,
         1,
         Features (Features'First),
         Value ("low", Features (Features'First))
      );
      Show (Reference, Result, Parent => Window);
      return null;
   end Test_5;

   function Test_6 return Gtk_Widget is
      View      : Gtk_Tree_View;
      Renderer  : Gtk_Cell_Renderer_Fuzzy_Feature_Value;
      Column    : Gtk_Tree_View_Column;
      Row       : Gtk_Tree_Iter;
      Column_No : Gint;
      Value     : GValue;
      use GLib.Values.Feature_Value;
   begin
      Gtk_New (Store, (0 => GType_Feature_Value));

      Store.Append (Row);
      Init (Value, GType_Feature_Value);
      Set
      (  Value,
         Create_Float ("Float", 5, 0.0, 20.0),
         Fuzzy.Set'
         (  1    => Confidence'Last,
            2    => 0.5,
            3..5 => Confidence'First
      )  );
      Set_Value (Store, Row, 0, Value);

      Store.Append (Row);
      Set
      (  Value,
         Create_Discrete ("L-CORE", "high, mid, low"),
         Fuzzy.Intuitionistic.Set'
         (  Cardinality => 3,
            Possibility =>
               (  1 => Confidence'Last,
                  2 => 0.5,
                  3 => Confidence'First
               ),
            Necessity =>
               (  1 => 0.5,
                  2 => Confidence'First,
                  3 => Confidence'First
      )  )     );
      Set_Value (Store, Row, 0, Value);

      Store.Append (Row);
      Set
      (  Value,
         Create_Isosceles_Trapezoids ("Traps", 6, -10.0, 10.0, 1.0),
         Fuzzy.Intuitionistic.Classification'
         (  Cardinality => 6,
            Possibility =>
               (  1 => Confidence'Last,
                  2 => 0.7,
                  3 => Confidence'First,
                  4 => Confidence'Last,
                  5 => 0.8,
                  6 => Confidence'Last
               ),
            Necessity =>
               (  1 => Confidence'Last,
                  2 => 0.5,
                  3 => Confidence'First,
                  4 => 0.3,
                  5 => 0.8,
                  6 => Confidence'Last
      )  )     );
      Set_Value (Store, Row, 0, Value);
      Unset (Value);

      Gtk_New (View, Store);
      Unref (Store);
      Gtk_New (Column);
      Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "feature-value", 0);
      Column_No := Append_Column (View, Column);
      Column.Set_Title ("Any value");
      Column.Set_Resizable (True);
      return View.all'Access;
   end Test_6;

   procedure Test_7_Edit_0
             (  Cell : not null access
                       Gtk.Cell_Renderer.Abstract_Renderer.
                           Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter  :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record'Class
            (  Cell.all
            ) .Get;
         Set_Value (Store, Row, 0, Value);
         Unset (Value);
      end if;
   end Test_7_Edit_0;

   function Test_7 return Gtk_Widget is
      View      : Gtk_Tree_View;
      Renderer  : Gtk_Cell_Renderer_Fuzzy_Feature_Value;
      Column    : Gtk_Tree_View_Column;
      Row       : Gtk_Tree_Iter;
      Column_No : Gint;
      Value     : GValue;
      use GLib.Values.Feature_Value;
   begin
      Gtk_New (Store, (0 => GType_Feature_Value));

      Store.Append (Row);

      Store.Append (Row);
      Init (Value, GType_Feature_Value);
      Set
      (  Value,
         Create_Float ("Float", 5, 0.0, 20.0),
         Fuzzy.Set'
         (  1    => Confidence'Last,
            2    => 0.5,
            3..5 => Confidence'First
      )  );
      Set_Value (Store, Row, 0, Value);

      Store.Append (Row);
      Set
      (  Value,
         Create_Discrete ("L-CORE", "high, mid, low"),
         Fuzzy.Intuitionistic.Set'
         (  Cardinality => 3,
            Possibility =>
               (  1 => Confidence'Last,
                  2 => 0.5,
                  3 => Confidence'First
               ),
            Necessity =>
               (  1 => 0.5,
                  2 => Confidence'First,
                  3 => Confidence'First
      )  )     );
      Set_Value (Store, Row, 0, Value);

      Store.Append (Row);
      Set
      (  Value,
         Create_Isosceles_Trapezoids ("Traps", 6, -10.0, 10.0, 1.0),
         Fuzzy.Intuitionistic.Classification'
         (  Cardinality => 6,
            Possibility =>
               (  1 => Confidence'Last,
                  2 => 0.7,
                  3 => Confidence'First,
                  4 => Confidence'Last,
                  5 => 0.8,
                  6 => Confidence'Last
               ),
            Necessity =>
               (  1 => Confidence'Last,
                  2 => 0.5,
                  3 => Confidence'First,
                  4 => 0.3,
                  5 => 0.8,
                  6 => Confidence'Last
      )  )     );
      Set_Value (Store, Row, 0, Value);
      Unset (Value);

      Gtk_New (View, Store);
      Unref (Store);

      Gtk_New (Column);
      Gtk_New (Renderer);
      Renderer.Set_Editable_Undefined (True);
      Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "feature-value", 0);
      Column_No := Append_Column (View, Column);
      Column.Set_Title ("Any value");
      Column.Set_Resizable (True);
      Renderer.On_Commit (+Test_7_Edit_0'Access);
      return View.all'Access;
   end Test_7;

   function Test_8 return Gtk_Widget is
      Pane      : Gtk_Paned;
      Box       : Gtk_VBox;
      Catalogue : Gtk_Fuzzy_Catalogue;
   begin
      Gtk_New_VPaned (Pane);
      Gtk_New_VBox (Box);
      Pane.Add2 (Box);
      Gtk_New (Catalogue, Box);
      Pane.Add1 (Catalogue);
      return Pane.all'Access;
   end Test_8;

   function Test_9 return Gtk_Widget is
      Machine : Gtk_Activity_Indicator;
   begin
      Gtk_New (Machine);
      return Machine.all'Access;
   end Test_9;

   function Test_10 return Gtk_Widget is
      Graph    : Gtk_Fuzzy_Graph;
      Lesson   : Lecture_Handle;
      X1       : constant Feature_Handle :=
                          Create_Discrete
                          (  "color",
                             (  "Red, Green, Blue, Yellow, Pink, Cyan, "
                             &  "Brown, Black, White"
                          )  );
      X2       : constant Feature_Handle :=
                          Create_Day_Of_Week_Feature ("day");
      Classes  : constant Feature_Handle :=
                          Create_Tone_Feature ("tone");
      Node     : Node_Handle;
      Features : Table;
   begin
      Features.Add ("color", X1);
      Features.Add ("day",   X2);
      Features.Add ("tone",  Classes);
      Node :=
         Value
         (  (  "color"
            &  "(  Red..Blue, Pink => "
            &  "      day "
            &  "      (  MO, TH..SU => tone = C..E; "
            &  "         WE         => tone = A"
            &  "      );"
            &  "   White =>"
            &  "      day "
            &  "      (  WE, TH, FR => tone = C,D; "
            &  "         SU         => tone = G"
            &  "      )"
            &  ")"
            ),
            Features
         );
      Gtk_New (Graph);
      Graph.Put (Node);
      return Graph.all'Access;
   end Test_10;

   function Test_11 return Gtk_Widget is
      Graph    : Gtk_Fuzzy_Graph;
      Lesson   : Lecture_Handle;
      X        : constant Feature_Handle :=
                          Create_Discrete
                          (  "color",
                             (  "Red, Green, Blue, Yellow, Pink, Cyan, "
                             &  "Brown, Black, White"
                          )  );
      Classes  : constant Feature_Handle :=
                          Create_Tone_Feature ("tone");
      Node     : Node_Handle;
      Features : Table;
   begin
      Features.Add ("color", X);
      Features.Add ("tone",  Classes);
      Node := Value ("color (Red => tone = C)", Features);
      Gtk_New (Graph);
      Graph.Put (Node);
      return Graph.all'Access;
   end Test_11;

   procedure Program_Error_Tracer
             (  Occurence : Exception_Occurrence
             )  is
      use GNAT.Traceback, GNAT.Traceback.Symbolic;
   begin
      Put_Line
      (  "Traced Program_Error "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Program_Error_Tracer;

   type Exception_Tracer_Ptr is access
      procedure (Occurence : Exception_Occurrence);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Exception_Tracer_Ptr,
             Exception_Action
          );
begin
   Register_Id_Action
   (  Program_Error'Identity,
      +Program_Error_Tracer'Access
   );
   Gtk.Main.Init;
   --
   -- The following lines are meant for debugging under GNAT. They cause
   -- stack  tracing upon errors in the libraries specified. Remove them
   -- if you are using another compiler.
   --
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Gtk");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GObject");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GtkAda+");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Fuzzy");

   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Gtk.Window.Set_Default_Size (Window, 600, 400);
   Window.Set_Title ("Test fuzzy ML GTK+ widgets");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Border_Width (10);
   Gtk_New_HPaned (Pane);

   Name_Tables.Name_Body := Name_Tables.Name_Body or To_Set ("-");
   -- The list of tests
   declare
      Scroll : Gtk_Scrolled_Window;
      View   : Gtk_Tree_View;
      List   : Gtk_Tree_Store;
      Row    : Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Ptr    : GValue;

      procedure Add_Test (Name : String; Test : System.Address) is
      begin
         Append (List, Row, Parent);
         Set (List, Row, 0, Name);
         Set_Address (Ptr, Test);
         Set_Value (List, Row, 1, Ptr);
      end Add_Test;
   begin
      Init (Ptr, GType_Pointer);
      Gtk_New (View);
      Gtk_New (List, (GType_String, GType_Pointer));
      -- Creating the list of tests
         -- Feature
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Features");
            Add_Test ("Factory", Test_1'Address);
            Add_Test
            (  "Gtk_Cell_Renderer_Fuzzy_Feature_Value",
               Test_6'Address
            );
            Add_Test
            (  "Gtk_Cell_Renderer_Fuzzy_Feature_Value (editor)",
               Test_7'Address
            );
         -- Training set
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Training sets");
            Add_Test ("Gtk_Fuzzy_Lecture",           Test_2'Address);
            Add_Test ("Gtk.Fuzzy_Lecture.Show",      Test_3'Address);
            Add_Test ("Gtk_Fuzzy_Lecture_Diff",      Test_4'Address);
            Add_Test ("Gtk.Fuzzy_Lecture_Diff.Show", Test_5'Address);
         -- Catalogues
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Catalogues");
            Add_Test ("Catalogue", Test_8'Address);
         -- Activity
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Indicators");
            Add_Test ("Activity", Test_9'Address);
         -- Graphs
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Classifiers");
            Add_Test ("Graphs (discrete features)", Test_10'Address);
            Add_Test ("Graphs (one path)", Test_11'Address);
         -- Done with the list
      Unset (Ptr);
      View.Set_Rules_Hint (True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
         Column_No := Append_Column (View, Column);
         Column.Set_Title ("Test");
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);
      end;
      Set_Mode (View.Get_Selection, Selection_Single);
      View.Set_Model (To_Interface (List));
      View.Expand_All;
      View.Get_Selection.On_Changed (+On_Selection'Access);
      Gtk_New (Scroll);
      Scroll.Add (View);
      Pane.Add1 (Scroll);
      declare
         Label : Gtk_Label;
      begin
         Gtk_New (Label, "select a test on the left");
         Test_Widget := Label.all'Unchecked_Access;
         Pane.Add2 (Label);
      end;
      declare
         Size : Gtk_Requisition;
      begin
         View.Columns_Autosize;   -- Size columns
         Size_Request (View, Size); -- Query the integral size
         Set_Size_Request                -- Set new size
         (  View,
            GInt'Min (Size.Width,  600),
            GInt'Min (Size.Height, 600)
         );
      end;
   end;
   Gtk_New_VBox (Box);
   Gtk_New (Get_CSS_Button);
   Set_Label (Get_CSS_Button, "Take CSS file from");
   Box.Pack_Start (Pane);
   Box.Pack_Start (Get_CSS_Button, False, False);
   Get_CSS_Button.On_Clicked (+On_Get_CSS'Access);
   Window.Add (Box);
   Box.Show_All;
   Window.Show;
   Gtk.Main.Main;

exception
   when Error : others =>
      Gtk.Main.Router.Trace (Error);
end Test_Graph_Schemes_Gtk_Widgets;
