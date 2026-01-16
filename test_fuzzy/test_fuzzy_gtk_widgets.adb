--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Fuzzy_Gtk_Widgets                      Luebeck            --
--  Implementation                                 Spring, 2007       --
--                                                                    --
--                                Last revision :  08:55 08 Apr 2022  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Text_IO;                    use Ada.Text_IO;
with Confidence_Factors;             use Confidence_Factors;
with Fuzzy.Abstract_Edit.Handle;     use Fuzzy.Abstract_Edit.Handle;
with Fuzzy.Abstract_Edit.Named;      use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Logic;                    use Fuzzy.Logic;
with Fuzzy_Linguistics;              use Fuzzy_Linguistics;
with Fuzzy_Linguistic_Sets;          use Fuzzy_Linguistic_Sets;
with Gdk.Event;                      use Gdk.Event;
with GLib;                           use GLib;
with GLib.Properties;                use GLib.Properties;
with GLib.Values;                    use GLib.Values;
with GLib.Values.Confidence_Factors; use GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Logic;        use GLib.Values.Fuzzy.Logic;
with GNAT.Exception_Actions;         use GNAT.Exception_Actions;
with GtkAda.Style;                   use GtkAda.Style;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Button;                     use Gtk.Button;
with Gtk.Cell_Renderer;              use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Fuzzy;        use Gtk.Cell_Renderer_Fuzzy;
with Gtk.Cell_Renderer_Text;         use Gtk.Cell_Renderer_Text;
with Gtk.Fuzzy_Boolean;              use Gtk.Fuzzy_Boolean;
with Gtk.Fuzzy_Set;                  use Gtk.Fuzzy_Set;
with Gtk.Fuzzy_Set_Entry;            use Gtk.Fuzzy_Set_Entry;
with Gtk.List_Store;                 use Gtk.List_Store;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Paned;                      use Gtk.Paned;
with Gtk.Window;                     use Gtk.Window;
with Gtk.Widget;                     use Gtk.Widget;
with Gtk.Widget.Styles.Store;        use Gtk.Widget.Styles.Store;
with Gtk.Separator;                  use Gtk.Separator;
with Gtk.Table;                      use Gtk.Table;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Selection;             use Gtk.Tree_Selection;
with Gtk.Tree_Store;                 use Gtk.Tree_Store;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.UTF8.Handling;     use Strings_Edit.UTF8.Handling;

with Ada.Unchecked_Conversion;
with Fuzzy.Intuitionistic;
with GLib.Values.Fuzzy;
with GLib.Values.Fuzzy.Intuitionistic;
with GNAT.Traceback.Symbolic;

with Gtk.Cell_Renderer.Abstract_Renderer;
with Gtk.Fuzzy_Boolean_Drawing;
with Gtk.Fuzzy_Linguistic_Set_Editor;
with Gtk.Fuzzy_Linguistic_Set_Measure_Editor;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Main.Router;
with Gtk.Style_Provider;

with Gdk.Color;
with System;
with Units.Constants;

with Gtk.Cell_Renderer_Fuzzy_Boolean;
use  Gtk.Cell_Renderer_Fuzzy_Boolean;
--
-- Remove the following line if you do not use the GNAT Ada compiler.
--
with Gtk.Main.Router.GNAT_Stack;
with Fuzzy_Linguistic_Set_Measures;

procedure Test_Fuzzy_Gtk_Widgets is

   File_Name    : constant String := "test_gtk_widgets.css-file";
   Default_Name : constant String := "test_fuzzy_gtk_widgets.css";

   type Local_Callback is access function return Gtk_Widget;

   function Call is
      new Ada.Unchecked_Conversion (System.Address, Local_Callback);

   Window         : Gtk_Window;
   Pane           : Gtk_HPaned;
   Test_Widget    : Gtk_Widget;
   Box            : Gtk_Box;
   Get_CSS_Button : Gtk_Button;
   Store          : Gtk_List_Store;

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
        (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
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
         Get_CSS_Button.Set_Label
         (  File_Name & " has been written. Press to rewrite"
         );
      end if;
   exception
      when Error : others =>
         Put_Line
         (  "On_Get_CSS fault: "
         &  Exception_Information (Error)
         );
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
         Pane.Pack2 (Test_Widget, True, True);
         Test_Widget.Show_All;
         Unset (Value);
      end if;
   exception
      when Error : others =>
         Put_Line
         (  "On_Selection fault: "
         &  Exception_Information (Error)
         );
   end On_Selection;
--
-- Individual tests
--
   function Test_1 return Gtk_Widget is
      Table  : Gtk_Table;
      Viewer : Gtk_Fuzzy_Boolean;
   begin
      Gtk_New (Table, 4, 4, False);
      Gtk_New (Viewer, (0.0, Confidence'Last));
      Table.Attach (Viewer, 0, 1, 0, 1);

      Gtk_New (Viewer, (0.0, 0.6));
      Table.Attach (Viewer, 0, 1, 1, 2);

      Gtk_New (Viewer, (0.0, 0.3));
      Table.Attach (Viewer, 0, 1, 2, 3);

      Gtk_New (Viewer, (0.0, 0.0));
      Table.Attach (Viewer, 0, 1, 3, 4);
      ------
      Gtk_New (Viewer, (0.3, Confidence'Last));
      Table.Attach (Viewer, 1, 2, 0, 1);

      Gtk_New (Viewer, (0.3, 0.6));
      Table.Attach (Viewer, 1, 2, 1, 2);

      Gtk_New (Viewer, (0.3, 0.3));
      Table.Attach (Viewer, 1, 2, 2, 3);

      Gtk_New (Viewer, (0.3, 0.0));
      Table.Attach (Viewer, 1, 2, 3, 4);
      ------
      Gtk_New (Viewer, (0.6, Confidence'Last));
      Table.Attach (Viewer, 2, 3, 0, 1);

      Gtk_New (Viewer, (0.6, 0.6));
      Table.Attach (Viewer, 2, 3, 1, 2);

      Gtk_New (Viewer, (0.6, 0.3));
      Table.Attach (Viewer, 2, 3, 2, 3);

      Gtk_New (Viewer, (0.6, 0.0));
      Table.Attach (Viewer, 2, 3, 3, 4);
      ------
      Gtk_New (Viewer, (Confidence'Last, Confidence'Last));
      Table.Attach (Viewer, 3, 4, 0, 1);

      Gtk_New (Viewer, (Confidence'Last, 0.6));
      Table.Attach (Viewer, 3, 4, 1, 2);

      Gtk_New (Viewer, (Confidence'Last, 0.3));
      Table.Attach (Viewer, 3, 4, 2, 3);

      Gtk_New (Viewer, (Confidence'Last, 0.0));
      Table.Attach (Viewer, 3, 4, 3, 4);

      return Table.all'Access;
   end Test_1;

   function Test_2 return Gtk_Widget is
      Table  : Gtk_Table;
      Viewer : Gtk_Fuzzy_Boolean;
   begin
      Gtk_New (Table, 4, 4, False);

      Gtk_New (Viewer, (0.0, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 0, 1, 0, 1);

      Gtk_New (Viewer, (0.0, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 0, 1, 1, 2);

      Gtk_New (Viewer, (0.0, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 0, 1, 2, 3);

      Gtk_New (Viewer, (0.0, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 0, 1, 3, 4);
      ------
      Gtk_New (Viewer, (0.3, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 1, 2, 0, 1);

      Gtk_New (Viewer, (0.3, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 1, 2, 1, 2);

      Gtk_New (Viewer, (0.3, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 1, 2, 2, 3);

      Gtk_New (Viewer, (0.3, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 1, 2, 3, 4);
      ------
      Gtk_New (Viewer, (0.6, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 2, 3, 0, 1);

      Gtk_New (Viewer, (0.6, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 2, 3, 1, 2);

      Gtk_New (Viewer, (0.6, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 2, 3, 2, 3);

      Gtk_New (Viewer, (0.6, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 2, 3, 3, 4);
      ------
      Gtk_New (Viewer, (Confidence'Last, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 3, 4, 0, 1);

      Gtk_New (Viewer, (Confidence'Last, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 3, 4, 1, 2);

      Gtk_New (Viewer, (Confidence'Last, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 3, 4, 2, 3);

      Gtk_New (Viewer, (Confidence'Last, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Bullet);
      Table.Attach (Viewer, 3, 4, 3, 4);

      return Table.all'Access;
   end Test_2;

   function Test_3 return Gtk_Widget is
      Table  : Gtk_Table;
      Viewer : Gtk_Fuzzy_Boolean;
   begin
      Gtk_New (Table, 4, 4, False);

      Gtk_New (Viewer, (0.0, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 0, 1, 0, 1);

      Gtk_New (Viewer, (0.0, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 0, 1, 1, 2);

      Gtk_New (Viewer, (0.0, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 0, 1, 2, 3);

      Gtk_New (Viewer, (0.0, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 0, 1, 3, 4);
      ------
      Gtk_New (Viewer, (0.3, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 1, 2, 0, 1);

      Gtk_New (Viewer, (0.3, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 1, 2, 1, 2);

      Gtk_New (Viewer, (0.3, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 1, 2, 2, 3);

      Gtk_New (Viewer, (0.3, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 1, 2, 3, 4);
      ------
      Gtk_New (Viewer, (0.6, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 2, 3, 0, 1);

      Gtk_New (Viewer, (0.6, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 2, 3, 1, 2);

      Gtk_New (Viewer, (0.6, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 2, 3, 2, 3);

      Gtk_New (Viewer, (0.6, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 2, 3, 3, 4);
      ------
      Gtk_New (Viewer, (Confidence'Last, Confidence'Last));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 3, 4, 0, 1);

      Gtk_New (Viewer, (Confidence'Last, 0.6));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 3, 4, 1, 2);

      Gtk_New (Viewer, (Confidence'Last, 0.3));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 3, 4, 2, 3);

      Gtk_New (Viewer, (Confidence'Last, 0.0));
      Viewer.Set_Shape (Gtk.Fuzzy_Boolean_Drawing.Text);
      Table.Attach (Viewer, 3, 4, 3, 4);

      return Table.all'Access;
   end Test_3;

   function Test_4 return Gtk_Widget is
      Table  : Gtk_Table;
      Viewer : Gtk_Fuzzy_Boolean;
   begin
      Gtk_New (Table, 4, 4, False);
      Table.Set_Name ("test-4");

      Gtk_New (Viewer, (0.0, Confidence'Last));
      Table.Attach (Viewer, 0, 1, 0, 1);

      Gtk_New (Viewer, (0.0, 0.6));
      Table.Attach (Viewer, 0, 1, 1, 2);

      Gtk_New (Viewer, (0.0, 0.3));
      Table.Attach (Viewer, 0, 1, 2, 3);

      Gtk_New (Viewer, (0.0, 0.0));
      Table.Attach (Viewer, 0, 1, 3, 4);
      ------
      Gtk_New (Viewer, (0.3, Confidence'Last));
      Table.Attach (Viewer, 1, 2, 0, 1);

      Gtk_New (Viewer, (0.3, 0.6));
      Table.Attach (Viewer, 1, 2, 1, 2);

      Gtk_New (Viewer, (0.3, 0.3));
      Table.Attach (Viewer, 1, 2, 2, 3);

      Gtk_New (Viewer, (0.3, 0.0));
      Table.Attach (Viewer, 1, 2, 3, 4);
      ------
      Gtk_New (Viewer, (0.6, Confidence'Last));
      Table.Attach (Viewer, 2, 3, 0, 1);

      Gtk_New (Viewer, (0.6, 0.6));
      Table.Attach (Viewer, 2, 3, 1, 2);

      Gtk_New (Viewer, (0.6, 0.3));
      Table.Attach (Viewer, 2, 3, 2, 3);

      Gtk_New (Viewer, (0.6, 0.0));
      Table.Attach (Viewer, 2, 3, 3, 4);
      ------
      Gtk_New (Viewer, (Confidence'Last, Confidence'Last));
      Table.Attach (Viewer, 3, 4, 0, 1);

      Gtk_New (Viewer, (Confidence'Last, 0.6));
      Table.Attach (Viewer, 3, 4, 1, 2);

      Gtk_New (Viewer, (Confidence'Last, 0.3));
      Table.Attach (Viewer, 3, 4, 2, 3);

      Gtk_New (Viewer, (Confidence'Last, 0.0));
      Table.Attach (Viewer, 3, 4, 3, 4);

      return Table.all'Access;
   end Test_4;

   procedure Test_5_Edit_0
             (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
                       Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class (Cell.all).Get;
         Set_Value (Store, Row, 0, Value);
         Unset (Value);
      end if;
   end Test_5_Edit_0;

   procedure Test_5_Edit_1
             (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
                       Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter  :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class (Cell.all).Get;
         Set_Value (Store, Row, 1, Value);
         Unset (Value);
      end if;
   end Test_5_Edit_1;

   function Test_5 return Gtk_Widget is
      Table  : Gtk_Tree_View;
      Row    : Gtk_Tree_Iter := Null_Iter;
      First  : GValue;
      Second : GValue;
   begin
      Gtk_New (Table);
      Set_Rules_Hint (Table, True);
      Gtk_New (Store, (GType_Fuzzy_Boolean, GType_Confidence));
      Init (First,  GType_Fuzzy_Boolean);
      Init (Second, GType_Confidence);
      for I in 1..5 loop
         for J in 1..5 loop
            Store.Append (Row);
            Set
            (  First,
               (  Possibility =>
                     Confidence (Float (Confidence'Last) / Float (I)),
                  Necessity =>
                     Confidence (Float (Confidence'Last) / Float (J))
            )  );
            Set
            (  Second,
               Confidence (Float (Confidence'Last) / Float (I))
            );
            Store.Set_Value (Row, 0, First);
            Store.Set_Value (Row, 1, Second);
         end loop;
      end loop;
      Unset (First);
      Unset (Second);
      declare
         Column    : Gtk_Tree_View_Column;
         Renderer  : Gtk_Cell_Renderer_Fuzzy_Boolean;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Renderer);
         Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
         Column.Pack_Start (Renderer, True);
         Column.Add_Attribute (Renderer, "fuzzy-boolean-value", 0);
         Column_No := Table.Append_Column (Column);
         Column.Set_Title ("Logical");
         Column.Set_Resizable (True);
         Renderer.On_Commit (+Test_5_Edit_0'Access);

         Gtk_New (Column);
         Gtk_New (Renderer);
         Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
         Column.Pack_Start (Renderer, True);
         Column.Add_Attribute (Renderer, "confidence-value", 1);
         Column_No := Table.Append_Column (Column);
         Column.Set_Title ("Confidence");
         Column.Set_Resizable (True);
         Renderer.On_Commit (+Test_5_Edit_1'Access);
      end;
      Table.Set_Model (To_Interface (Store));
      return Table.all'Access;
   end Test_5;

   function Test_6 return Gtk_Widget is
      Result : Gtk_Fuzzy_Set;
      Domain : Domain_Description;
   begin
      Add (Domain, "Red",   1);
      Add (Domain, "Green", 2);
      Add (Domain, "Blue",  3);
      Gtk_New (Result, Domain, Fuzzy.Set'(Confidence'Last, 0.5, 0.0));
      return Result.all'Access;
   end Test_6;

   function Test_7 return Gtk_Widget is
      Result : Gtk_Fuzzy_Set;
      Domain : Domain_Description;
      Value  : Fuzzy.Intuitionistic.Classification (12);
      Index  : Integer := 1;
   begin
      Add (Domain, "Red",     1);
      Add (Domain, "Green",   2);
      Add (Domain, "Blue",    3);
      Add (Domain, "Black",   4);
      Add (Domain, "Brown",   5);
      Add (Domain, "Yellow",  6);
      Add (Domain, "Pink",    7);
      Add (Domain, "Cyan",    8);
      Add (Domain, "Magenta", 9);
      Add (Domain, "Orange", 10);
      Add (Domain, "Purple", 11);
      Add (Domain, "White",  12);
      for I in 1..4 loop
         for J in 1..3 loop
            Value.Possibility (Index) :=
               Confidence (Float (Confidence'Last) / Float (I));
            Value.Necessity (Index) :=
               Confidence (Float (Confidence'Last) / Float (J));
            Index := Index + 1;
         end loop;
      end loop;
      Gtk_New (Result, Domain, Value);
      Set_Editable (Result, True);
      return Result.all'Access;
   end Test_7;

   function Test_8 return Gtk_Widget is
      Result : Gtk_HBox;
      Edit   : Gtk_Fuzzy_Set;
      Domain : Domain_Description;
      Value  : Fuzzy.Intuitionistic.Classification (12);
      Index  : Integer := 1;
   begin
      Gtk_New_HBox (Result);
      Add (Domain, "Red",     1);
      Add (Domain, "Green",   2);
      Add (Domain, "Blue",    3);
      Add (Domain, "Black",   4);
      Add (Domain, "Brown",   5);
      Add (Domain, "Yellow",  6);
      Add (Domain, "Pink",    7);
      Add (Domain, "Cyan",    8);
      Add (Domain, "Magenta", 9);
      Add (Domain, "Orange", 10);
      Add (Domain, "Purple", 11);
      Add (Domain, "White",  12);
      for I in 1..4 loop
         for J in 1..3 loop
            Value.Possibility (Index) :=
               Confidence (Float (Confidence'Last) / Float (I));
            Value.Necessity (Index) :=
               Confidence (Float (Confidence'Last) / Float (J));
            Index := Index + 1;
         end loop;
      end loop;
      Gtk_New (Edit, Domain, Value);
      Set_Editable (Edit, True);
      Result.Pack_Start (Edit);

      Gtk_New (Edit, Domain, Value.Possibility);
      Set_Editable (Edit, True);

      Set_Name (Result, "test-8");
      Result.Pack_Start (Edit);
      return Result.all'Access;
   end Test_8;

   function Create_Linguistic_Set (Factor : Float := 1.0)
      return Linguistic_Set is
      Domain : Linguistic_Set;
   begin
      declare
         Triangle : Variable;
         From     : Float := -10.0;
         Step     : constant Float := 5.0;
      begin
         for Index in 1..4 loop
            Erase (Triangle);
            Append (Triangle, From,              Confidence'First);
            Append (Triangle, From + Step,       Confidence'Last );
            Append (Triangle, From + 2.0 * Step, Confidence'First);
            From := From + Step;
            Add
            (  Domain,
               "Triangle_" & Image (Index),
               Triangle * Factor
            );
         end loop;
      end;
      declare
         Rock : Variable;
      begin
         Append (Rock, -10.0, Confidence'First);
         Append (Rock,  -5.0, 0.2);
         Append (Rock,   0.0, 0.7);
         Append (Rock,   1.0, Confidence'Last);
         Append (Rock,   5.0, 0.5);
         Append (Rock,   7.0, 0.3);
         Add (Domain, "Rock", Rock * Factor);
      end;
      declare
         Shoulder : Variable;
      begin
         Append (Shoulder,  0.0, Confidence'Last);
         Append (Shoulder, 10.0, Confidence'First);
         Add (Domain, "Shoulder", Shoulder * Factor);
      end;
      declare
         Rectangle : Variable;
      begin
         Append (Rectangle, -10.0, Confidence'First);
         Append (Rectangle, -10.0, Confidence'Last);
         Append (Rectangle,  10.0, Confidence'Last);
         Append (Rectangle,  10.0, Confidence'First);
         Add (Domain, "Rectangle", Rectangle * Factor);
      end;
      declare
         Trapezoid : Variable;
      begin
         Append (Trapezoid, -10.0, Confidence'First);
         Append (Trapezoid,   0.0, Confidence'Last);
         Append (Trapezoid,  10.0, Confidence'Last);
         Append (Trapezoid,  15.0, Confidence'First);
         Add (Domain, "Trapezoid", Trapezoid * Factor);
      end;
      declare
         Singleton : Variable;
      begin
         Append (Singleton, 10.0, Confidence'First);
         Append (Singleton, 10.0, Confidence'Last);
         Append (Singleton, 10.0, Confidence'First);
         Add (Domain, "Singleton", Singleton * Factor);
      end;
      declare
         Pulse : Variable;
      begin
         Append (Pulse, 0.0, 0.75);
         Append (Pulse, 0.0, Confidence'Last);
         Append (Pulse, 0.0, 0.25);
         Append (Pulse, 0.0, 0.5);
         Add (Domain, "Pulse", Pulse * Factor);
      end;
      return Domain;
   end Create_Linguistic_Set;

   function Test_9 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor.Domain_Of;
      Result : Gtk_Fuzzy_Linguistic_Set_Domain;
   begin
      Gtk_New (Result, Create_Linguistic_Set (1000.0));
      Set_X_Scroll (Result, False);
      Set_Y_Scroll (Result, False);
      Set_Domain_Note (Result, "km/h");
      return Result.all'Access;
   end Test_9;

   function Test_10 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor.Domain_Of;
      use Gtk.Fuzzy_Linguistic_Set_Editor.Zoom_Panel_Of;
      Result : Gtk_Box;
      Set    : Gtk_Fuzzy_Linguistic_Set_Domain;
      Zoom   : Gtk_Fuzzy_Linguistic_Set_Zoom_Panel;
   begin
      Gtk_New_HBox (Result);
      Gtk_New (Set, Create_Linguistic_Set);
      Gtk_New (Zoom, Set);
      Set_Domain_Note (Set, "km/h");
      Result.Pack_Start (Set);
      Result.Pack_Start (Zoom, False);
      return Result.all'Access;
   end Test_10;

   function Test_11 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor.Zoom_Panel_Of;
      use Gtk.Fuzzy_Linguistic_Set_Editor.Tree_View_Of;
      Result : Gtk_VPaned;
      Box    : Gtk_HBox;
      Zoom   : Gtk_Fuzzy_Linguistic_Set_Zoom_Panel;
      Tree   : Gtk_Fuzzy_Linguistic_Set_Tree_View;
   begin
      Gtk_New_HPaned (Result);
      Gtk_New_HBox (Box);
      Gtk_New (Tree, Create_Linguistic_Set);
      Gtk_New (Zoom, Tree.Get_Domain_View);
      Result.Add1 (Tree);
      Box.Pack_Start (Tree.Get_Domain_View);
      Box.Pack_Start (Zoom, False);
      Result.Add2 (Box);
      return Result.all'Access;
   end Test_11;

   function Test_12 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor.Domain_Of;
      use Gtk.Fuzzy_Linguistic_Set_Editor.Zoom_Panel_Of;
      use Gtk.Fuzzy_Linguistic_Set_Editor.Tree_View_Of;
      Result  : Gtk_VPaned;
      Table   : Gtk_Table;
      Buttons : Gtk_VBox;
      Tree    : Gtk_Fuzzy_Linguistic_Set_Tree_View;
      Line    : Gtk_Separator;
   begin
      Gtk_New_HPaned (Result);
      Gtk_New (Tree, Create_Linguistic_Set);
      Tree.Set_Editable (True);
      Result.Add1 (Tree);

      Gtk_New (Table, 4, 4, False);
      Table.Attach
      (  Get_Y_Move_Bar (Tree),
         1, 2, 0, 1,
         XOptions => 0
      );
      Table.Attach
      (  Get_Zoom_Y_Scale (Tree.Get_Domain_View),
         1, 2, 1, 2,
         XOptions => 0
      );
      Table.Attach (Tree.Get_Domain_View, 2, 4, 0, 2);
      Table.Attach
      (  Get_Zoom_X_Scale (Tree.Get_Domain_View),
         2, 3, 2, 3,
         YOptions => 0
      );
      Table.Attach
      (  Get_X_Move_Bar (Tree),
         3, 4, 2, 3,
         YOptions => 0
      );
      Gtk_New_VBox (Buttons);
      Buttons.Set_Spacing (3);
         Buttons.Pack_Start (Tree.Get_Undo_Button,   False, False);
         Buttons.Pack_Start (Tree.Get_Redo_Button,   False, False);
         Buttons.Pack_Start (Tree.Get_New_Button,    False, False);
         Buttons.Pack_Start (Tree.Get_Copy_Button,   False, False);
         Buttons.Pack_Start (Tree.Get_Add_Button,    False, False);
         Buttons.Pack_Start (Tree.Get_Purge_Button,  False, False);
         Buttons.Pack_Start (Tree.Get_Remove_Button, False, False);
         Buttons.Pack_Start (Tree.Get_Find_Button,   False, False);
         Buttons.Pack_Start (Tree.Get_Up_Button,     False, False);
         Buttons.Pack_Start (Tree.Get_Down_Button,   False, False);
      Table.Attach (Buttons, 0, 1, 0, 4, XOptions => 0, YOptions => 0);
      Gtk_New_HBox (Buttons);
      Buttons.Set_Spacing (3);
         Buttons.Pack_Start
         (  Get_Zoom_In_Button (Tree.Get_Domain_View),
            False,
            False
         );
         Buttons.Pack_Start
         (  Get_Zoom_100_Button (Tree.Get_Domain_View),
            False,
            False
         );
         Buttons.Pack_Start
         (  Get_Zoom_Out_Button (Tree.Get_Domain_View),
            False,
            False
         );
         Gtk_New_Vseparator (Line);
         Buttons.Pack_Start (Line, False, False);
         Buttons.Pack_Start
         (  Get_Zoom_Undo_Button (Tree.Get_Domain_View),
            False,
            False
         );
         Buttons.Pack_Start
         (  Get_Zoom_Redo_Button (Tree.Get_Domain_View),
            False,
            False
         );
         Buttons.Pack_Start
         (  Get_Zoom_Fit_Button (Tree.Get_Domain_View),
            False,
            False
         );
      Table.Attach (Buttons, 1, 4, 3, 4, XOptions => 0, YOptions => 0);

      Result.Add2 (Table);
      return Result.all'Access;
   end Test_12;

   function Test_13 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Editor;
   begin
      Gtk_New
      (  Result,
         Create_Linguistic_Set,
         Orientation_Horizontal
      );
      return Result.all'Access;
   end Test_13;

   function Test_14 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Editor;
   begin
      Gtk_New
      (  Result,
         Create_Linguistic_Set,
         Orientation_Vertical
      );
      return Result.all'Access;
   end Test_14;

   function Test_15 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Editor;
   begin
      Gtk_New
      (  Result,
         Create_Linguistic_Set,
         Orientation_Horizontal,
         False
      );
      return Result.all'Access;
   end Test_15;

   function Test_16 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Editor;
   begin
      Gtk_New
      (  Result,
         Create_Linguistic_Set,
         Orientation_Vertical,
         False
      );
      return Result.all'Access;
   end Test_16;

   function Test_17 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Measure_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
      Set    : constant Linguistic_Set := Create_Linguistic_Set;
   begin
      Gtk_New
      (  Result,
         (Units.Constants.Pressure, Set, 0.0),-- Compiler bug, should be
-- (Units.Constants.Pressure, Create_Linguistic_Set, 0.0),
         "kPa",
         Orientation_Horizontal,
         False
      );
      Result.Set_Editable (True);
      return Result.all'Access;
   end Test_17;

   function Test_18 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Measure_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
   begin
      Gtk_New
      (  Result,
         (Units.Constants.Pressure, Create_Linguistic_Set, 0.0),
         "kPa",
         Orientation_Vertical,
         False
      );
      Result.Set_Editable (True);
      return Result.all'Access;
   end Test_18;

   function Test_19 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Measure_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
   begin
      Gtk_New
      (  Result,
         (Units.Constants.Pressure, Create_Linguistic_Set, 0.0),
         "kPa",
         Orientation_Horizontal
      );
      Result.Set_Editable (False);
      return Result.all'Access;
   end Test_19;

   function Test_20 return Gtk_Widget is
      use Gtk.Fuzzy_Linguistic_Set_Measure_Editor;
      Result : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
   begin
      Gtk_New
      (  Result,
         (Units.Constants.Pressure, Create_Linguistic_Set, 0.0),
         "kPa",
         Orientation_Vertical
      );
      Result.Set_Editable (False);
      return Result.all'Access;
   end Test_20;

   procedure Test_21_Edit_0
             (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
                       Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Record'Class (Cell.all).Get;
         Store.Set_Value (Row, 0, Value);
         Unset (Value);
      end if;
   end Test_21_Edit_0;

   procedure Test_21_Edit_1
             (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
                       Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Record'Class (Cell.all).Get;
         Store.Set_Value (Row, 1, Value);
         Unset (Value);
      end if;
   end Test_21_Edit_1;

   procedure Test_21_Edit_2
             (  Cell : access Gtk.Cell_Renderer.Abstract_Renderer.
                       Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       Store.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value :=
            Gtk_Cell_Renderer_Fuzzy_Record'Class (Cell.all).Get;
         Store.Set_Value (Row, 2, Value);
         Unset (Value);
      end if;
   end Test_21_Edit_2;

   function Test_21 return Gtk_Widget is
      View   : Gtk_Tree_View;
      Row    : Gtk_Tree_Iter := Null_Iter;
      Value  : GValue;
      Data   : constant Handle := Ref (new Domain_Description);
      Domain : Domain_Description renames
               Domain_Description (Ptr (Data).all);
   begin
      Gtk_New
      (  Store,
         (  GLib.Values.Fuzzy.GType_Set,
            GLib.Values.Fuzzy.Intuitionistic.GType_Set,
            GLib.Values.Fuzzy.Intuitionistic.GType_Classification,
            GType_String
      )  );
      Add (Domain, "Red",   1);
      Add (Domain, "Green", 2);
      Add (Domain, "Blue",  3);

      Store.Append (Row);
      Init (Value, GLib.Values.Fuzzy.GType_Set);
      GLib.Values.Fuzzy.Set
      (  Value,
         (Confidence'First, Confidence'Last, 0.3)
      );
      Store.Set_Value (Row, 0, Value);
      Unset (Value);

      Init (Value, GLib.Values.Fuzzy.Intuitionistic.GType_Set);
      GLib.Values.Fuzzy.Intuitionistic.Set
      (  Value,
         Fuzzy.Intuitionistic.Set'
         (  Cardinality => 3,
            Possibility => (Confidence'First, Confidence'Last, 0.3),
            Necessity   => (Confidence'First, 0.5, 0.3)
      )  );
      Store.Set_Value (Row, 1, Value);
      Unset (Value);

      Init
      (  Value,
         GLib.Values.Fuzzy.Intuitionistic.GType_Classification
      );
      GLib.Values.Fuzzy.Intuitionistic.Set
      (  Value,
         Fuzzy.Intuitionistic.Classification'
         (  Cardinality => 3,
            Possibility => (Confidence'First, Confidence'Last, 0.3),
            Necessity   => (Confidence'First, 0.5, 0.3)
      )  );
      Store.Set_Value (Row, 2, Value);
      Unset (Value);

      Store.Set (Row, 3, "X1=");

      Store.Append (Row);

      Store.Append (Row);
      Init (Value, GLib.Values.Fuzzy.GType_Set);
      GLib.Values.Fuzzy.Set
      (  Value,
         (Confidence'First, 0.7, 0.5)
      );
      Store.Set_Value (Row, 0, Value);
      Unset (Value);

      Init (Value, GLib.Values.Fuzzy.Intuitionistic.GType_Set);
      GLib.Values.Fuzzy.Intuitionistic.Set
      (  Value,
         Fuzzy.Intuitionistic.Set'
         (  Cardinality => 3,
            Possibility => (Confidence'First, 0.7, 0.5),
            Necessity   => (Confidence'First, 0.3, 0.3)
      )  );
      Store.Set_Value (Row, 1, Value);
      Unset (Value);

      Init
      (  Value,
         GLib.Values.Fuzzy.Intuitionistic.GType_Classification
      );
      GLib.Values.Fuzzy.Intuitionistic.Set
      (  Value,
         Fuzzy.Intuitionistic.Classification'
         (  Cardinality => 3,
            Possibility => (Confidence'First, 0.7, 0.5),
            Necessity   => (Confidence'First, 0.3, 0.3)
      )  );
      Store.Set_Value (Row, 2, Value);
      Unset (Value);

      Store.Set (Row, 3, "X2=");

      Gtk_New (View, Store);
      Unref (Store);
      View.Set_Rules_Hint (True);

      declare
         Column    : Gtk_Tree_View_Column;
         Renderer  : Gtk_Cell_Renderer_Fuzzy;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Renderer, Create (Data));
         Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
         Set_Editable_Undefined (Renderer, True);
         Set_Property (Renderer, Build ("xalign"), GFloat (0.0));
         Column.Pack_Start (Renderer, True);
         Column.Add_Attribute (Renderer, "set-value", 0);
         Column_No := Append_Column (View, Column);
         Column.Set_Title ("Fuzzy set");
         Column.Set_Resizable (True);
         Renderer.On_Commit (+Test_21_Edit_0'Access);

         Gtk_New (Column);
         Gtk_New (Renderer, Create (Data));
         Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
         Set_Editable_Undefined (Renderer, True);
         Set_Property (Renderer, Build ("xalign"), GFloat (0.0));
         Column.Pack_Start (Renderer, True);
         Column.Add_Attribute (Renderer, "intuitionistic-set-value", 1);
         Column_No := Append_Column (View, Column);
         Column.Set_Title ("Intuitionistic set");
         Column.Set_Resizable (True);
         Renderer.On_Commit (+Test_21_Edit_1'Access);

         Gtk_New (Column);
         Gtk_New (Renderer, Create (Data));
         Renderer.Set_Mode (Cell_Renderer_Mode_Editable);
         Set_Editable_Undefined (Renderer, True);
         Set_Property (Renderer, Build ("xalign"), GFloat (0.0));
         Column.Pack_Start (Renderer, True);
         Column.Add_Attribute (Renderer, "classification-value", 2);
         Column.Add_Attribute (Renderer, "prefix-text", 3);
         Column_No := Append_Column (View, Column);
         Column.Set_Title ("Classification");
         Column.Set_Resizable (True);
         Renderer.On_Commit (+Test_21_Edit_2'Access);
      end;
      return View.all'Access;
   end Test_21;

   function Test_22 return Gtk_Widget is
      Result : Gtk_Vbox;
      Edit   : Gtk_Fuzzy_Set_Entry;
      Data   : constant Handle := Ref (new Domain_Description);
      Domain : Domain_Description
                  renames Domain_Description (Ptr (Data).all);
      Value  : Fuzzy.Intuitionistic.Classification (12);
      Index  : Integer := 1;
   begin
      Add (Domain, "Red",     1);
      Add (Domain, "Green",   2);
      Add (Domain, "Blue",    3);
      Add (Domain, "Black",   4);
      Add (Domain, "Brown",   5);
      Add (Domain, "Yellow",  6);
      Add (Domain, "Pink",    7);
      Add (Domain, "Cyan",    8);
      Add (Domain, "Magenta", 9);
      Add (Domain, "Orange", 10);
      Add (Domain, "Purple", 11);
      Add (Domain, "White",  12);
      for I in 1..4 loop
         for J in 1..3 loop
            Value.Possibility (Index) :=
               Confidence (Float (Confidence'Last) / Float (I));
            Value.Necessity (Index) :=
               Confidence (Float (Confidence'Last) / Float (J));
            Index := Index + 1;
         end loop;
      end loop;
      Gtk_New (Edit, Create (Data), Value);
      Edit.Set_Name ("test-22");
      Gtk_New_VBox (Result);
      Result.Pack_Start (Edit, Expand => False, Fill => False);
      return Result.all'Access;
   end Test_22;

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

   if File_Test (Default_Name, File_Test_Exists) then
      Load_Css_File
      (  Default_Name,
         Put_Line'Access,
         Gtk.Style_Provider.Priority_Application
      );
   end if;

   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Default_Size (600, 400);
   Window.Set_Title ("Test fuzzy GTK+ widgets");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Border_Width (10);
   Gtk_New_HPaned (Pane);
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
         List.Append (Row, Parent);
         Gtk.Missed.Set (List, Row, 0, Name);
         Set_Address (Ptr, Test);
         Set_Value (List, Row, 1, Ptr);
      end Add_Test;
   begin
      Init (Ptr, GType_Pointer);
      Gtk_New (View);
      Gtk_New (List, (GType_String, GType_Pointer));
      -- Creating the list of tests
         -- Fuzzy_Boolean
         List.Append (Parent, Null_Iter);
         List.Set (Parent, 0, "Fuzzy logical values");
            Add_Test ("Gtk_Fuzzy_Boolean bar",       Test_1'Address);
            Add_Test ("Gtk_Fuzzy_Boolean bullet",    Test_2'Address);
            Add_Test ("Gtk_Fuzzy_Boolean text",      Test_3'Address);
            Add_Test ("Gtk_Fuzzy_Boolean CSS style", Test_4'Address);
            Add_Test
            (  "Gtk_Cell_Renderer_Fuzzy_Boolean",
               Test_5'Address
            );
         -- Fuzzy_Set
         List.Append (Parent, Null_Iter);
         List.Set
         (  Parent,
            0,
            "Fuzzy sets, intuitionistic fuzzy sets, classifications"
         );
            Add_Test ("Gtk_Fuzzy_Set", Test_6'Address);
            Add_Test
            (  "Gtk_Fuzzy_Set editable classification",
               Test_7'Address
            );
            Add_Test
            (  "Gtk_Fuzzy_Set CSS style editable classification",
               Test_8'Address
            );
            Add_Test ("Gtk_Fuzzy_Set_Entry",     Test_22'Address);
            Add_Test ("Gtk_Cell_Renderer_Fuzzy", Test_21'Address);
         -- Linguistic_Set
         List.Append (Parent, Null_Iter);
         List.Set (Parent, 0, "Linguistic sets");
            Add_Test ("Gtk_Linguistic_Set",        Test_9'Address);
            Add_Test ("Gtk_Linguistic_Set (zoom)", Test_10'Address);
            Add_Test ("Gtk_Linguistic_Set (view)", Test_11'Address);
            Add_Test ("Gtk_Linguistic_Set (edit)", Test_12'Address);
            Add_Test
            (  "Gtk_Linguistic_Set_Editor (horizontal layout)",
               Test_13'Address
            );
            Add_Test
            (  "Gtk_Linguistic_Set_Editor (vertical layout)",
               Test_14'Address
            );
            Add_Test
            (  "Gtk_Linguistic_Set_Viewer (horizontal layout)",
               Test_15'Address
            );
            Add_Test
            (  "Gtk_Linguistic_Set_Viewer (vertical layout)",
               Test_16'Address
            );
         -- Measure_Linguistic_Set
         List.Append (Parent, Null_Iter);
         List.Set (Parent, 0, "Dimensioned linguistic sets");
            Add_Test
            (  "Gtk_Measure_Linguistic_Set_Editor (horizontal layout)",
               Test_17'Address
            );
            Add_Test
            (  "Gtk_Measure_Linguistic_Set_Editor (vertical layout)",
               Test_18'Address
            );
            Add_Test
            (  "Gtk_Measure_Linguistic_Set_Viewer (horizontal layout)",
               Test_19'Address
            );
            Add_Test
            (  "Gtk_Measure_Linguistic_Set_Viewer (vertical layout)",
               Test_20'Address
            );
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
         Set_Sort_Column_Id (Column, 0);
      end;
      Get_Selection (View).Set_Mode (Selection_Single);
      View.Set_Model (To_Interface (List));
      View.Expand_All;
      Get_Selection (View).On_Changed (+On_Selection'Access);
      Gtk_New (Scroll);
      Scroll.Add (View);
      Pane.Add1 (Scroll);
      declare
         Size : Gtk_Requisition;
      begin
         Columns_Autosize (View);   -- Size columns
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
   Get_CSS_Button.Set_Label ("Take CSS file from");
   Box.Pack_Start (Pane);
   Box.Pack_Start (Get_CSS_Button, False, False);
   Get_CSS_Button.On_Clicked (+On_Get_CSS'Access);
   Window.Add (Box);
   Window.Show_All;
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_Fuzzy_Gtk_Widgets;
