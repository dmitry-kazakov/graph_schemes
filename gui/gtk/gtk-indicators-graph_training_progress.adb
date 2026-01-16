--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.                             Luebeck            --
--        Graph_Training_Progress                  Spring, 2006       --
--  Implementation                                                    --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Confidence_Factors.Edit;   use Confidence_Factors.Edit;
with Fuzzy.Feature.Handle;      use Fuzzy.Feature.Handle;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Lecture;             use Fuzzy.Lecture;
with Fuzzy.Samples_Lists;       use Fuzzy.Samples_Lists;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GLib.Values;               use GLib.Values;
with Gtk.Cell_Renderer_Fixed;   use Gtk.Cell_Renderer_Fixed;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Strings_Edit;              use Strings_Edit;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Strings_Edit.Floats;       use Strings_Edit.Floats;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Indicators.Graph_Training_Progress is
   use Feature_Statistics_Maps;

   Graph_Progress_Class_Record : aliased Ada_GObject_Class :=
                                         Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Indicators.Graph_Training_Progress." & Name;
   end Where;

   function Get_List (List : Examples_List) return String is
   begin
      if Is_Empty (List) then
         return "";
      else
         declare
            Text     : String (1..512);
            Index    : Examples_List_Position := First;
            Interval : Examples_Range;
            Pointer  : Integer := Text'First;
         begin
            loop
               Interval := Get (List, Index);
               if Pointer > Text'First then
                  Put (Text, Pointer, ", ");
               end if;
               Put (Text, Pointer, Integer (Interval.From));
               if Interval.From /= Interval.To then
                  Put (Text, Pointer, "..");
                  Put (Text, Pointer, Integer (Interval.To));
               end if;
               Index := Next (List, Index);
               exit when not Is_In (List, Index);
               if Pointer > Text'Last - 32 then
                  Put (Text, Pointer, ",...");
                  exit;
               end if;
            end loop;
            return Text (1..Pointer - 1);
         exception
            when Constraint_Error | Ada.IO_Exceptions.Layout_Error =>
               return Text (1..Pointer - 1);
         end;
      end if;
   end Get_List;

   procedure Do_Initialize
             (  Widget  : access
                   Gtk_Graph_Training_Progress_Record'Class;
                Button  : Gtk_Button;
                Spacing : Gtk_Size;
                State   : Gtk_Training_State
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      if Button = null then
         Initialize
         (  Widget          => Widget,
            Button_Position => (1, 2, 1, 2),
            Bar_Position    => (0, 1, 1, 2),
            Timed           => True,
            Size            => (2, 2),
            Spacing         => Spacing,
            Homogeneous     => False,
            No_Viewer       => True
         );
      else
         Initialize
         (  Widget          => Widget,
            Button          => Button,
            Button_Position => (1, 2, 1, 2),
            Bar_Position    => (0, 1, 1, 2),
            Timed           => True,
            Size            => (2, 2),
            Spacing         => Spacing,
            Homogeneous     => False,
            No_Viewer       => True
         );
      end if;
      if State = null then
         Gtk_New (Widget.Content);
         Attach (Widget, Widget.Content, 0, 2, 0, 1);
      else
         Widget.Content := State;
      end if;

      Set_Viewer
      (  Widget,
         Ref (new Training_Viewer (Widget, Get_Bar (Widget)))
      );

      Handlers.Connect
      (  Widget,
         "style-updated",
         Handlers.To_Marshaller (Style_Updated'Access)
      );
      Style_Updated (Widget);
   end Do_Initialize;

   procedure Draw (Viewer : in out Training_Viewer) is
   begin
      Draw (Timed_Bar (Viewer));
      if Viewer.Context = null then
         return;
      end if;
      declare
         Content : Gtk_Training_State renames Viewer.Widget.Content;
         Feature : Feature_Handle;
         Index   : Integer;
         Context : Graph_Training_Data'Class renames Viewer.Context.all;
         Int     : GValue;
         Real    : GValue;
         Text    : GValue;
         Row     : Gtk_Tree_Iter := Null_Iter;
      begin
         Ref (Content.Features_Store);  -- While doing changes
         Set_Model (Content.Features_View, Null_Gtk_Tree_Model);
         Init (Int,  GType_Int);
         Init (Text, GType_String);
         Init (Real, GType_Double);
         Content.Example_Value.Set_Text (Image (Context.Example));
         Content.Total_Value.Set_Text
         (  Image (Context.Lesson.Get_Examples_Number)
         );
         Content.Threshold_Value.Set_Text (Image (Context.Threshold));
         Content.Example_Cut_Value.Set_Text
         (  Image (Context.Example_Cut)
         );
         Content.Equivalence_Value.Set_Text
         (  Image (Context.Equivalence)
         );
         Content.Rotations_Value.Set_Text (Image (Context.Rotations));
         Content.RotationsPerSec_Value.Set_Text
         (  Image
            (  Float (Context.Rotations) / Float (Clock - Get (Viewer)),
               AbsSmall => -2
         )  );
         for Index in 1..Get_Size (Context.Statistics) loop
            declare
               Feature : constant Feature_Handle :=
                         Get (Context.Statistics, Index).Ptr.Feature;
            begin
               if (  Feature.Is_Valid
                  and then
                     not Content.Map.Is_In (Feature.Get_ID)
                  )
               then
                  Content.Features_Store.Append (Row);
                  Set_Int (Int, GInt (Get_ID (Feature)));
                  Set_Value (Content.Features_Store, Row, 0, Int);
                  Set_String (Text, Get_Name (Feature));
                  Set_Value (Content.Features_Store, Row, 1, Text);
                  Content.Map.Add (Feature.Get_ID, Feature);
               end if;
            end;
         end loop;
         Row := Get_Iter_First (Content.Features_Store);
         while Row /= Null_Iter loop
            Feature :=
               Content.Map.Get
               (  Feature_ID (Get_Int (Content.Features_Store, Row, 0))
               );
            Index := Find (Context.Statistics, Ptr (Feature));
            if Index > 0 then
               declare
                  Statistics : Feature_Statistics'Class renames
                      Ptr (Get (Context.Statistics, Index)).all;
               begin
                  Set_Double (Real, GDouble (Statistics.Up));
                  Set_Value (Content.Features_Store, Row, 2, Real);
                  if Statistics.Up > 0 or else Statistics.Down > 0 then
                     Set_Double
                     (  Real,
                        GDouble
                        (  Statistics.Depth
                        /  (  Float (Statistics.Up)
                           +  Float (Statistics.Down)
                     )  )  );
                     Set_Value (Content.Features_Store, Row, 4, Real);
                  end if;
                  Set_Double (Real, GDouble (Statistics.Down));
                  Set_Value (Content.Features_Store, Row, 3, Real);
                  Set_Double (Real, GDouble (Statistics.Order));
                  Set_Value (Content.Features_Store, Row, 5, Real);
                  Set_String (Text, Get_List (Statistics.Erroneous));
                  Set_Value (Content.Features_Store, Row, 6, Text);
               end;
            else
               Remove (Content.Features_Store, Row);
            end if;
            Next (Content.Features_Store, Row);
         end loop;
         for Image in Image_Type loop
            if Context.Example > 0 then
               Set_Double
               (  Real,
                  (  GDouble (Context.Profiling.Data (Image).Total_Time)
                  *  1_000.0
                  /  GDouble (Context.Example)
               )  );
               Set_Value
               (  Content.Images_Store,
                  Content.Images_Iter (Image),
                  1,
                  Real
               );
            end if;
            Set_Double
            (  Real,
               (  GDouble (Context.Profiling.Data (Image).Current_Time)
               *  1_000.0
            )  );
            Set_Value
            (  Content.Images_Store,
               Content.Images_Iter (Image),
               2,
               Real
            );
         end loop;
         Unset (Text);
         Unset (Int);
         Unset (Real);
         Set_Model
         (  Content.Features_View,
            To_Interface (Content.Features_Store)
         );
         Unref (Content.Features_Store);
      exception
         when Error : others =>
            Unset (Text);
            Unset (Int);
            Unset (Real);
            Set_Model
            (  Content.Features_View,
               To_Interface (Content.Features_Store)
            );
            Unref (Content.Features_Store);
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Draw")
            )  );
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            "Fault: " & Exception_Information (Error) & Where ("Draw")
         );
   end Draw;

   function Get_Training_State
            (  Widget : not null access
                        Gtk_Graph_Training_Progress_Record'Class
            )  return Gtk_Training_State is
   begin
      return Widget.Content;
   end Get_Training_State;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Indicators.Progress.Get_Type,
            Class_Record => Graph_Progress_Class_Record'Access,
            Type_Name    => Graph_Progress_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "example-label",
               Nick    => "Example",
               Default => "Example",
               Blurb   => (  "The label of the current training "
                          &  "example number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "example-cut-label",
               Nick    => "Example cut",
               Default => "Estimated N(r|r)",
               Blurb   => (  "The label of the current estimation "
                          &  "of N(r|r)"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "total-examples-label",
               Nick    => "Total examples",
               Default => "Total examples",
               Blurb   => (  "The label of the total training examples "
                          &  "number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "rotations-label",
               Nick    => "Rotations",
               Default => "Rotations",
               Blurb   => (  "The label of the node rotations "
                          &  "number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "threshold-label",
               Nick    => "Threshold",
               Default => "Threshold",
               Blurb   => "The label of the threshold parameter"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "equivalence-label",
               Nick    => "Equivalence",
               Default => "Equivalence",
               Blurb   => "The label of the equivalence parameter "
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "rotations-per-second-label",
               Nick    => "Rotations per second",
               Default => "Per second",
               Blurb   => (  "The label of the node rotations "
                          &  "number per second"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "feature-column-title",
               Nick    => "Feature",
               Default => "Feature",
               Blurb   => "The title of the feature name column"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "image-column-title",
               Nick    => "Image",
               Default => "Image",
               Blurb   => "The title of the image name column"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "image-mean-time-title",
               Nick    => "Mean time",
               Default => "Mean time, ms",
               Blurb   => "The title of the image mean time column"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "image-time-title",
               Nick    => "Time",
               Default => "Time, ms",
               Blurb   => "The title of the image time column"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "rotations-up-column-title",
               Nick    => "Upward feature rotations",
               Default => "Up",
               Blurb   => (  "The title of the upward feature "
                          &  "rotations column"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "rotations-down-column-title",
               Nick    => "Downward feature rotations",
               Default => "Down",
               Blurb   => (  "The title of the downward feature "
                          &  "rotations column"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "depth-column-title",
               Nick    => "Feature rotation depth",
               Default => "At",
               Blurb   => (  "The title of the feature rotations "
                          &  "depth column"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "order-column-title",
               Nick    => "Feature order",
               Default => "Order",
               Blurb   => "The title of the feature order column"
         )  );
         Install_Style_Property
         (  Class_Ref (Graph_Progress_Class_Record.The_Type),
            Gnew_String
            (  Name    => "inconsistent-column-title",
               Nick    => "Inconsistent examples",
               Default => "Erroneous",
               Blurb   => (  "The title of the examples inconsistent "
                          &  "in the feature"
         )  )             );
      end if;
      return Graph_Progress_Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget  : out Gtk_Graph_Training_Progress;
                Button  : not null access Gtk_Button_Record'Class;
                Spacing : Gtk_Size := (3, 3);
                State   : Gtk_Training_State := null
             )  is
   begin
      Widget := new Gtk_Graph_Training_Progress_Record;
      begin
         Initialize (Widget, Button, Spacing, State);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Graph_Training_Progress)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Graph_Training_Progress;
                Spacing : Gtk_Size := (3, 3);
                State   : Gtk_Training_State := null
             )  is
   begin
      Widget := new Gtk_Graph_Training_Progress_Record;
      begin
         Initialize (Widget, Spacing, State);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Graph_Training_Progress)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New (Widget : out Gtk_Training_State) is
   begin
      Widget := new Gtk_Training_State_Record;
      begin
         Gtk.Indicators.Graph_Training_Progress.Initialize (Widget);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Training_State)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Gtk_Training_State_Record'Class
             )  is
      procedure Do_Label
                (  Label     : out Gtk_Label;
                   Position  : Gtk_Rectangle;
                   Alignment : Gtk_Justification;
                   XOptions  : Gtk_Attach_Options := Fill
                )  is
      begin
         Gtk_New (Label);
         case Alignment is
            when Justify_Left =>
               Label.Set_Halign (Align_Start);
               Label.Set_Valign (Align_Center);
--             Label.Set_Alignment (0.0, 0.5);
            when Justify_Center =>
               Label.Set_Halign (Align_Center);
               Label.Set_Valign (Align_Center);
--             Label.Set_Alignment (0.5, 0.5);
            when Justify_Right =>
               Label.Set_Halign (Align_End);
               Label.Set_Valign (Align_Center);
--             Label.Set_Alignment (1.0, 0.5);
            when Justify_Fill =>
               null;
         end case;
         if Alignment = Justify_Left then
            Widget.Attach
            (  Label,
               Position.Left,
               Position.Right,
               Position.Top,
               Position.Bottom,
               XOptions => XOptions,
               YOptions => Shrink
            );
         else
            Widget.Attach
            (  Label,
               Position.Left,
               Position.Right,
               Position.Top,
               Position.Bottom,
               XOptions => XOptions,
               YOptions => Shrink
            );
         end if;
         Label.Show;
      end Do_Label;
      Column    : Gtk_Tree_View_Column;
      Text      : Gtk_Cell_Renderer_Text;
      Number    : Gtk_Cell_Renderer_Fixed;
      Column_No : Gint;
      Scroll    : Gtk_Scrolled_Window;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize (Widget, 4, 8, False);
         -- Features statistics
      Gtk_New
      (  Widget.Features_Store,
         (  GType_Int,
            GType_String,
            GType_Double,
            GType_Double,
            GType_Double,
            GType_Double,
            GType_String
      )  );
      Gtk_New (Widget.Features_View, Widget.Features_Store);
      Set_Rules_Hint (Widget.Features_View, True);
      Widget.Features_Store.Unref;

      Gtk_New (Column);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Visible (False);

      Gtk_New (Column);
      Gtk_New (Text);
      Column.Pack_Start (Text, True);
      Column.Add_Attribute (Text, "text", 1);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (1);

      Gtk_New (Column);
      Gtk_New (Number, 0);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 2);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (2);

      Gtk_New (Column);
      Gtk_New (Number, 0);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 3);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (3);

      Gtk_New (Column);
      Gtk_New (Number, 3);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 4);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (4);

      Gtk_New (Column);
      Gtk_New (Number, 3);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 5);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (5);

      Gtk_New (Column);
      Gtk_New (Text);
      Column.Pack_Start (Text, True);
      Column.Add_Attribute (Text, "text", 6);
      Column_No := Widget.Features_View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (6);

      Do_Label (Widget.Example_Label, (0, 1, 0, 1), Justify_Right);
      Do_Label (Widget.Example_Value, (1, 2, 0, 1), Justify_Left);

      Do_Label (Widget.Total_Label, (2, 3, 0, 1), Justify_Right);
      Do_Label (Widget.Total_Value, (3, 4, 0, 1), Justify_Left);

      Do_Label (Widget.Rotations_Label, (4, 5, 0, 1), Justify_Right);
      Do_Label (Widget.Rotations_Value, (5, 6, 0, 1), Justify_Left);

      Do_Label (Widget.Threshold_Label, (0, 1, 1, 2), Justify_Right);
      Do_Label (Widget.Threshold_Value, (1, 2, 1, 2), Justify_Left);

      Do_Label (Widget.Equivalence_Label, (2, 3, 1, 2), Justify_Right);
      Do_Label (Widget.Equivalence_Value, (3, 4, 1, 2), Justify_Left);

      Do_Label
      (  Widget.RotationsPerSec_Label,
         (4, 5, 1, 2),
         Justify_Right
      );
      Do_Label
      (  Widget.RotationsPerSec_Value,
         (5, 6, 1, 2),
         Justify_Left
      );

      Do_Label (Widget.Example_Cut_Label, (6, 7, 0, 1), Justify_Right);
      Do_Label
      (  Widget.Example_Cut_Value,
         (7, 8, 0, 1),
         Justify_Left,
         Fill or Expand
      );

      Gtk_New (Scroll);
      Add (Scroll, Widget.Features_View);
      Widget.Attach
      (  Scroll,
         0, 8, 3, 4,
         XOptions => Fill or Expand,
         YOptions => Fill or Expand
      );
         -- Images statistics
      Gtk_New
      (  Widget.Images_Store,
         (  GType_String,
            GType_Double,
            GType_Double
      )  );
      Widget.Images_Store.Append (Widget.Images_Iter (Has_In));
      Widget.Images_Store.Set
      (  Widget.Images_Iter (Has_In),
         0,
         "has-in"
      );
      Widget.Images_Store.Append (Widget.Images_Iter (Has_Out));
      Widget.Images_Store.Set
      (  Widget.Images_Iter (Has_Out),
         0,
         "has-out"
      );
      Widget.Images_Store.Append (Widget.Images_Iter (Has_Not));
      Widget.Images_Store.Set
      (  Widget.Images_Iter (Has_Not),
         0,
         "has-not"
      );
      Widget.Images_Store.Append (Widget.Images_Iter (Has_Not_Out));
      Widget.Images_Store.Set
      (  Widget.Images_Iter (Has_Not_Out),
         0,
         "has-not-out"
      );

      Gtk_New (Widget.Images_View, Widget.Images_Store);
      Set_Rules_Hint (Widget.Images_View, True);
      Unref (Widget.Images_Store);

      Gtk_New (Column);
      Gtk_New (Text);
      Column.Pack_Start (Text, True);
      Column.Add_Attribute (Text, "text", 0);
      Column_No := Append_Column (Widget.Images_View, Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (0);

      Gtk_New (Column);
      Gtk_New (Number, 0);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 1);
      Column_No := Append_Column (Widget.Images_View, Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (1);

      Gtk_New (Column);
      Gtk_New (Number, 0);
      Column.Pack_Start (Number, True);
      Column.Add_Attribute (Number, "value", 2);
      Column_No := Append_Column (Widget.Images_View, Column);
      Column.Set_Resizable (True);
      Column.Set_Sort_Column_Id (2);

      Widget.Attach
      (  Widget.Images_View,
         0, 8, 2, 3,
         XOptions => Fill or Expand,
         YOptions => Shrink
      );
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                             Gtk_Graph_Training_Progress_Record'Class;
                Button  : not null access Gtk_Button_Record'Class;
                Spacing : Gtk_Size;
                State   : Gtk_Training_State
             )  is
   begin
      Do_Initialize
      (  Widget  => Widget,
         Button  => Button.all'Unchecked_Access,
         Spacing => Spacing,
         State   => State
      );
   end Initialize;

   procedure Initialize
             (  Widget  : access
                             Gtk_Graph_Training_Progress_Record'Class;
                Spacing : Gtk_Size;
                State   : Gtk_Training_State
             )  is
   begin
      Do_Initialize (Widget, null, Spacing, State);
   end Initialize;

   procedure Set_Data
             (  Viewer : in out Training_Viewer;
                Data   : in out Indicator_Data'Class
             )  is
   begin
      if Data in Graph_Training_Indication_Data'Class then
         Viewer.Context :=
            Graph_Training_Indication_Data'Class
            (  Data
            ) .Data.all'Unchecked_Access;
         Request (Set_List'Access, Viewer'Access);
      end if;
   end Set_Data;

   procedure Set_Data (Viewer : in out Training_Viewer) is
   begin
      Viewer.Context := null;
   end Set_Data;

   procedure Set_List
             (  Viewer : not null access Training_Viewer'Class
             )  is
      use Feature_Statistics_Maps;
   begin
      if Viewer.Widget.Content = null then
         Gtk_New (Viewer.Widget.Content);
         Viewer.Widget.Attach
         (  Viewer.Widget.Content,
            0, 2, 0, 1,
            XOptions => Fill or Expand,
            YOptions => Fill or Expand
         );
         Viewer.Widget.Content.Show;
      else
         Viewer.Widget.Content.Features_Store.Clear;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Faulte:"
            &  Exception_Information (Error)
            &  Where ("Set_List")
         )  );
   end Set_List;

   procedure Style_Updated
             (  Widget : access Gtk_Graph_Training_Progress_Record'Class
             )  is
   begin
      Widget.Content.Example_Label.Set_Text
      (  Style_Get (Widget, "example-label")
      );
      Widget.Content.Example_Cut_Label.Set_Text
      (  Style_Get (Widget, "example-cut-label")
      );
      Widget.Content.Total_Label.Set_Text
      (  Style_Get (Widget, "total-examples-label")
      );
      Widget.Content.Rotations_Label.Set_Text
      (  Style_Get (Widget, "rotations-label")
      );
      Widget.Content.Threshold_Label.Set_Text
      (  Style_Get (Widget, "threshold-label")
      );
      Widget.Content.Equivalence_Label.Set_Text
      (  Style_Get (Widget, "equivalence-label")
      );
      Widget.Content.RotationsPerSec_Label.Set_Text
      (  Style_Get (Widget, "rotations-per-second-label")
      );
      Widget.Content.Features_View.Get_Column (0).Set_Title
      (  Style_Get (Widget, "feature-column-title")
      );
      Widget.Content.Features_View.Get_Column (1).Set_Title
      (  Style_Get (Widget, "rotations-up-column-title")
      );
      Widget.Content.Features_View.Get_Column (2).Set_Title
      (  Style_Get (Widget, "rotations-down-column-title")
      );
      Widget.Content.Features_View.Get_Column (3).Set_Title
      (  Style_Get (Widget, "depth-column-title")
      );
      Widget.Content.Features_View.Get_Column (4).Set_Title
      (  Style_Get (Widget, "order-column-title")
      );
      Widget.Content.Features_View.Get_Column (5).Set_Title
      (  Style_Get (Widget, "inconsistent-column-title")
      );
      Widget.Content.Images_View.Get_Column (0).Set_Title
      (  Style_Get (Widget, "image-column-title")
      );
      Widget.Content.Images_View.Get_Column (1).Set_Title
      (  Style_Get (Widget, "image-mean-time-title")
      );
      Widget.Content.Images_View.Get_Column (2).Set_Title
      (  Style_Get (Widget, "image-time-title")
      );
      Widget.Content.Set_Row_Spacings (Widget.Get_Row_Spacing (0));
      Widget.Content.Set_Col_Spacings (Widget.Get_Col_Spacing (0));
   end Style_Updated;

end Gtk.Indicators.Graph_Training_Progress;
