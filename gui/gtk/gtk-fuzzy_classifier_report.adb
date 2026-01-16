--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Classifier_Report                 Luebeck            --
--  Implementation                                 Winter, 2003       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Classifier;           use Fuzzy.Classifier;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with GLib.Messages;              use GLib.Messages;
with GLib.Object;                use GLib.Object;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Values;                use GLib.Values;
with GLib.Values.Fuzzy.Logic;    use GLib.Values.Fuzzy.Logic;
with GLib.Types;                 use GLib.Types;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Main.Router;            use Gtk.Main.Router;
with Gtk.Missed;                 use Gtk.Missed;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;          use Gtk.Widget.Styles;
with Indicator.Gtk_IO;           use Indicator.Gtk_IO;
with Indicator.Handle;           use Indicator.Handle;

with Ada.IO_Exceptions;
with Gtk.Cell_Renderer_Fuzzy_Boolean;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Classifier_Report is
   use Fuzzy.Gtk_Icon_Factory;
   use Gtk.Tree_View;

   Class_Record      : aliased Ada_GObject_Class := Uninitialized_Class;
   Tree_Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Classifier_Report." & Name;
   end Where;

   procedure Classify_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
      Example    : Positive;
      Complement : Boolean;
   begin
      begin
         Get_Selected_Example (Report, Example, Complement);
      exception
         when Constraint_Error =>
            Error (Report, Style_Get (Report, "no-example-message"));
            return;
      end;
      Put
      (  Report.Store,
         Classify_As_Selected
         (  Widget     => Report.Classifier,
            Lesson     => Get_Reference_Lesson (Report.Comparison.View),
            Example    => Example,
            Complement => Complement,
            Generalize => Get (Report.Comparison.Generalize)
      )  );
      Error_Reset (Report);
   exception
      when Constraint_Error =>
         Put
         (  Report.Store,
            Fuzzy.Intuitionistic.Classification'
            (  Cardinality =>
                  Get_Cardinality
                  (  Get_Classes
                     (  Get_Classifier (Report.Classifier)
                  )  ),
               Possibility => (others => Confidence'First),
               Necessity   => (others => Confidence'First)
         )  );
         Error (Report, Style_Get (Report, "no-classifier-message"));
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Classify_Selected")
         )  );
   end Classify_Selected;

   procedure Collapse
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
   begin
      Collapse_All (Report.Classifier);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Collapse")
         )  );
   end Collapse;

   function Create_Store
            (  Classes : Feature_Handle
            )  return Gtk_List_Store is
      Row    : Gtk_Tree_Iter := Null_Iter;
      Result : Gtk_List_Store;
      Domain : Domain_Description;
   begin
      Get_Domain (Classes, Domain);
      Gtk_New
      (  Result,
         (  GType_String,
            GType_Fuzzy_Boolean,
            GType_Fuzzy_Boolean
      )  );
      for Index in 1..Get_Cardinality (Classes) loop
         Append (Result, Row);
         Gtk.List_Store.Set
         (  Result,
            Row,
            0,
            Get_Name (Domain, Index)
         );
         Next (Result, Row);
      end loop;
      return Result;
   exception
      when Error : others =>
         if Result /= null then
            Unref (Result);
         end if;
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Create_Store")
         )  );
         raise;
   end Create_Store;

   procedure Error
             (  Widget  : not null access
                          Gtk_Fuzzy_Classifier_Report_Record;
                Message : UTF8_String
             )  is
   begin
      Say (Message);
   end Error;

   procedure Error_Reset
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Report_Record
             )  is
   begin
      null;
   end Error_Reset;

   procedure Expand_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
      Example    : Positive;
      Complement : Boolean;
   begin
      Get_Selected_Example (Report, Example, Complement);
      Collapse_All (Report.Classifier);
      Expand_Example
      (  Report.Classifier,
         Get_Reference_Lesson (Report.Comparison.View),
         Example,
         Complement
      );
      Error_Reset (Report);
   exception
      when Constraint_Error =>
         Error (Report, Style_Get (Report, "no-example-message"));
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Expand_Selected")
         )  );
   end Expand_Selected;

   procedure Find_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
      Progress   : Gtk_Timed_Progress_Bar;
      Example    : Positive;
      Complement : Boolean;
      Lesson     : Lecture_Handle;
   begin
      if Report.Source.View = null then
         return;
      end if;
      Lesson := Get_Lesson (Report.Source.View);
      if not
         (  Is_Valid (Lesson)
         and then
            Get_Examples_Number (Lesson) > 0
         )
      then
         return;
      end if;
      begin
         Get_Selected_Source_Example (Report, Example, Complement);
      exception
         when Constraint_Error =>
            Example    := Get_Examples_Number (Lesson);
            Complement := False;
      end;
      Gtk_New (Progress);
      Attach
      (  Report.Source.Buttons,
         Progress,
         2, 3, 0, 1,
         XOptions => Fill or Expand,
         YOptions => Shrink
      );
      Show_All (Progress);
      declare
         Bar : constant Indicator_Handle :=
                        Ref (new Timed_Bar (Progress));
      begin
         Find_Selected
         (  Widget     => Report.Classifier,
            Lesson     => Lesson,
            Example    => Example,
            Complement => Complement,
            Viewer     => Bar
         );
         Set_Visible (Report.Source.View, Example, Complement);
         Set_Selected_Source_Example (Report, Example, Complement);
         Error_Reset (Report);
      exception
         when Ada.IO_Exceptions.End_Error =>
            Error (Report, Style_Get (Report, "no-match-message"));
         when Constraint_Error =>
            Error (Report, Style_Get (Report, "no-classifier-message"));
      end;
      Remove (Report.Source.Buttons, Progress);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Find_Selected")
         )  );
   end Find_Selected;

   procedure Get_Selected_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : out Positive;
                Complement : out Boolean
             )  is
      Data  : Gtk_Tree_Model;
      Row   : Gtk_Tree_Iter;
      Value : GValue;
   begin
      Get_Selected
      (  Get_Selection (Get_Tree_View (Widget.Comparison.View)),
         Data,
         Row
      );
      if Row = Null_Iter then
         raise Constraint_Error;
      end if;
      Get_Value (Data, Row, 1, Value);
      begin
         Example := Positive (Get_Int (Value));
      exception
         when others =>
            Unset (Value);
            raise;
      end;
      Unset (Value);
      Get_Value (Data, Row, 0, Value);
      begin
         Complement := Positive_Icon /= Get_String (Value);
      exception
         when others =>
            Unset (Value);
            raise;
      end;
      Unset (Value);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Selected_Example")
         )  );
   end Get_Selected_Example;

   procedure Get_Selected_Source_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : out Positive;
                Complement : out Boolean
             )  is
      Data  : Gtk_Tree_Model;
      Row   : Gtk_Tree_Iter;
      Value : GValue;
   begin
      if Widget.Source.View = null then
         raise Constraint_Error;
      end if;
      Get_Selected
      (  Get_Selection (Get_Tree_View (Widget.Source.View)),
         Data,
         Row
      );
      if Row = Null_Iter then
         raise Constraint_Error;
      end if;
      Get_Value (Data, Row, 1, Value);
      begin
         Example := Positive (Get_Int (Value));
      exception
         when others =>
            Unset (Value);
            raise;
      end;
      Unset (Value);
      Get_Value (Data, Row, 0, Value);
      begin
         Complement := Positive_Icon /= Get_String (Value);
      exception
         when others =>
            Unset (Value);
            raise;
      end;
      Unset (Value);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Selected_Source_Example")
         )  );
   end Get_Selected_Source_Example;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Get_Type_HPaned,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Int
            (  Name    => "button-gap",
               Nick    => "Button gap",
               Minimum => 0,
               Maximum => 32767,
               Default => 3,
               Blurb   => (  "The spacing between the buttons of "
                          &  "the right pane"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "comparison-tab-title",
               Nick    => "Verification report",
               Blurb   => "The title of the comparison tab",
               Default => "Verification report"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "domain-column-title",
               Nick    => "Domain",
               Blurb   => "The title of domain values column",
               Default => "Domain"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "explained-column-title",
               Nick    => "Explained",
               Blurb   => "The title of explanation column",
               Default => "Explained"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "no-match-message",
               Nick    => "No match",
               Blurb   => "The message when nothing is matched",
               Default => "No match"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "no-classifier-message",
               Nick    => "No classifier",
               Blurb   => "The message when no classifier selected",
               Default => (  "Select a classifier part first. "
                          &  "The classifier is shown on the right "
                          &  "pane, below the classification results"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "no-example-message",
               Nick    => "No example",
               Blurb   => "The message when no example selected",
               Default => (  "Select a control example. "
                          &  "Examples are selected in the "
                          &  "control set, shown on the top of "
                          &  "the pane"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "source-tab-title",
               Nick    => "Source training set",
               Blurb   => "The title of the source set tab",
               Default => "Source training set"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "verified-column-title",
               Nick    => "Verified",
               Blurb   => "The title of verification column",
               Default => "Verified"
         )  );
--         Set_Button_Style (Classify_Button.Class);
--         Set_Button_Style (Collapse_Button.Class);
--         Set_Button_Style (Expand_Button.Class);
--         Set_Button_Style (Find_Button.Class);
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   function Get_Tree_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
            Class_Record => Tree_Class_Record'Access,
            Type_Name    => Class_Name & "TreeView"
         )
      then
         Gtk.Cell_Renderer_Fuzzy_Boolean.Install_Style_Properties
         (  Class_Peek (Tree_Class_Record.The_Type)
         );
      end if;
      return Tree_Class_Record.The_Type;
   end Get_Tree_Type;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Classifier_Report) is
   begin
      Widget := new Gtk_Fuzzy_Classifier_Report_Record;
      begin
         Gtk.Fuzzy_Classifier_Report.Initialize (Widget);
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
                         Gtk_Fuzzy_Classifier_Report_Record'Class
             )  is
      Frame    : Gtk_Frame;
      Scroller : Gtk_Scrolled_Window;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize_HPaned (Widget);
      Fuzzy.Gtk_Icon_Factory.Init;
         -- Left pane
      Gtk_New_VPaned (Widget.V_Pane);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Widget.V_Pane);
      Add1 (Widget, Frame);
         -- Top
         G_New (Widget.Report, Get_Tree_Type);
         Gtk.Tree_View.Initialize (Widget.Report);
         Set_Mode (Get_Selection (Widget.Report), Selection_Multiple);
         Set_Rules_Hint (Widget.Report, True);
         Gtk_New (Scroller);
         Add (Scroller, Widget.Report);
         Pack1 (Widget.V_Pane, Scroller, Shrink => False);
         Set_Policy (Scroller, Policy_Automatic, Policy_Automatic);
         -- Bottom
         Gtk_New (Widget.Classifier);
         Pack2 (Widget.V_Pane, Widget.Classifier, Shrink => False);
      Show_All (Frame);
         -- Right pane
      Gtk_New (Widget.Lectures);
      Set_Tab_Pos (Widget.Lectures, Pos_Left);
      Add2 (Widget, Widget.Lectures);
      declare
         Page  : Gtk_VBox;
         Frame : Gtk_Frame;
         Label : Gtk_HBox;
      begin
         Gtk_New_VBox (Page);
            -- Label
         Gtk_New_VBox (Label);
         Gtk_New (Widget.Comparison.Label);
         Set_Angle (Widget.Comparison.Label, 90.0);
         Pack_Start (Label, Widget.Comparison.Label, False, False);
            -- View
         Gtk_New (Widget.Comparison.View);
         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_In);
         Add (Frame, Widget.Comparison.View);
         Pack_Start (Page, Frame);
            -- Buttons
         Gtk_New (Widget.Comparison.Buttons, 1, 4, False);
         Pack_Start
         (  Page,
            Widget.Comparison.Buttons,
            False,
            False
         );
         Expand_Button.Gtk_New (Widget.Comparison.Expand_Selected);
         Expand_Button.Set_Sensitive
         (  Widget.Comparison.Expand_Selected,
            False
         );
         Attach
         (  Widget.Comparison.Buttons,
            Widget.Comparison.Expand_Selected,
            0, 1, 0, 1,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Handlers.Connect
         (  Widget.Comparison.Expand_Selected,
            "clicked",
            Expand_Selected'Access,
            Widget.all'Access
         );
         Collapse_Button.Gtk_New (Widget.Comparison.Collapse_All);
         Attach
         (  Widget.Comparison.Buttons,
            Widget.Comparison.Collapse_All,
            1, 2, 0, 1,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Handlers.Connect
         (  Widget.Comparison.Collapse_All,
            "clicked",
            Collapse'Access,
            Widget.all'Access
         );
         Classify_Button.Gtk_New (Widget.Comparison.Classify_Selected);
         Classify_Button.Set_Sensitive
         (  Widget.Comparison.Classify_Selected,
            False
         );
         Attach
         (  Widget.Comparison.Buttons,
            Widget.Comparison.Classify_Selected,
            2, 3, 0, 1,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Handlers.Connect
         (  Widget.Comparison.Classify_Selected,
            "clicked",
            Classify_Selected'Access,
            Widget.all'Access
         );
         Gtk_New (Widget.Comparison.Generalize, None);
         Set_Sensitive (Widget.Comparison.Generalize, False);
         Attach
         (  Widget.Comparison.Buttons,
            Widget.Comparison.Generalize,
            3, 4, 0, 1,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Append_Page (Widget.Lectures, Page, Label);
         Show_All (Label);
         Show_All (Page);
      end;
      Show_All (Widget.Lectures);
      --
      -- Creating columns
      --
      declare
         use Gtk.Cell_Renderer_Fuzzy_Boolean;
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Values    : Gtk_Cell_Renderer_Fuzzy_Boolean;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Pack_Start (Column, Text, True);
         Add_Attribute (Column, Text, "text", 0);
         Column_No := Append_Column (Widget.Report, Column);
         Set_Resizable (Column, True);

         Gtk_New (Column);
         Gtk_New (Values);
         Pack_Start (Column, Values, True);
         Add_Attribute (Column, Values, "fuzzy-boolean-value", 1);
         Column_No := Append_Column (Widget.Report, Column);
         Set_Resizable (Column, True);

         Gtk_New (Column);
         Gtk_New (Values);
         Pack_Start (Column, Values, True);
         Add_Attribute (Column, Values, "fuzzy-boolean-value", 2);
         Column_No := Append_Column (Widget.Report, Column);
         Set_Resizable (Column, True);
      end;

      Handlers.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Get_Selection (Get_Tree_View (Widget.Comparison.View)),
         "changed",
         Selection_Changed'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Classifier,
         "selection-changed",
         Selection_Changed'Access,
         Widget.all'Access
      );
      Style_Updated (Widget, Widget.all'Access);
   end Initialize;

   procedure Put
             (  Store : Gtk_List_Store;
                Value : Fuzzy.Intuitionistic.Set
             )  is
      Level : GValue;
      Row   : Gtk_Tree_Iter := Get_Iter_First (Store);
   begin
      Init (Level, GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean);
      for Index in 1..Value.Cardinality loop
         GLib.Values.Fuzzy.Logic.Set
         (  Level,
            (  Possibility => Value.Possibility (Index),
               Necessity   => Value.Necessity   (Index)
         )  );
         Set_Value (Store, Row, 1, Level);
         Next (Store, Row);
      end loop;
      Unset (Level);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Store : Gtk_List_Store;
                Value : Fuzzy.Intuitionistic.Classification
             )  is
      Level : GValue;
      Row   : Gtk_Tree_Iter := Get_Iter_First (Store);
   begin
      Init (Level, GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean);
      for Index in 1..Value.Cardinality loop
         GLib.Values.Fuzzy.Logic.Set
         (  Level,
            (  Possibility => Value.Possibility (Index),
               Necessity   => Value.Necessity   (Index)
         )  );
         Set_Value (Store, Row, 2, Level);
         Next (Store, Row);
      end loop;
      Unset (Level);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set
             )  is
      Source : Lecture_Handle;
   begin
      Put
      (  Widget     => Widget,
         Classifier => Classifier,
         Control    => Control,
         Result     => Result,
         Source     => Source,
         Report     => Report
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Source     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Put
      (  Widget    => Widget.Comparison.View,
         Reference => Control,
         Result    => Result
      );
      Widget.Store := Create_Store (Get_Classes (Classifier));
      Put (Widget.Store, Report);
      Set_Model (Widget.Report, To_Interface (Widget.Store));
      Unref (Widget.Store);
      Put (Widget.Classifier, Classifier);
      if Is_Valid (Source) then
         if Widget.Source.View = null then
            declare
               Page  : Gtk_VBox;
               Frame : Gtk_Frame;
               Label : Gtk_HBox;
            begin
               Gtk_New_VBox (Page);
                  -- Label
               Gtk_New_VBox (Label);
               Gtk_New
               (  Widget.Source.Label,
                  Style_Get (Widget, "source-tab-title")
               );
               Set_Angle (Widget.Source.Label, 90.0);
               Pack_Start (Label, Widget.Source.Label, False, False);
                  -- View
               Gtk_New (Widget.Source.View);
               Gtk_New (Frame);
               Set_Shadow_Type (Frame, Shadow_In);
               Add (Frame, Widget.Source.View);
               Pack_Start (Page, Frame, True, True);
               Handlers.Connect
               (  Get_Selection (Get_Tree_View (Widget.Source.View)),
                  "changed",
                  Selection_Changed'Access,
                  Widget.all'Access
               );
                  -- Buttons
               Gtk_New (Widget.Source.Buttons, 1, 3, False);
               Set_Col_Spacings
               (  Widget.Source.Buttons,
                  GUInt (GInt'(Style_Get (Widget, "button-gap")))
               );
               Pack_Start
               (  Page,
                  Widget.Source.Buttons,
                  False,
                  False
               );
               Collapse_Button.Gtk_New (Widget.Source.Collapse_All);
               Attach
               (  Widget.Source.Buttons,
                  Widget.Source.Collapse_All,
                  0, 1, 0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Handlers.Connect
               (  Widget.Source.Collapse_All,
                  "clicked",
                  Collapse'Access,
                  Widget.all'Access
               );
               Find_Button.Gtk_New (Widget.Source.Find_Selected);
               Find_Button.Set_Sensitive
               (  Widget.Source.Find_Selected,
                  False
               );
               Attach
               (  Widget.Source.Buttons,
                  Widget.Source.Find_Selected,
                  1, 2, 0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Handlers.Connect
               (  Widget.Source.Find_Selected,
                  "clicked",
                  Find_Selected'Access,
                  Widget.all'Access
               );

               Append_Page (Widget.Lectures, Page, Label);
               Show_All (Page);
               Show_All (Label);
            end;
         end if;
         Put (Widget.Source.View, Source);
      else
         if Widget.Source.View /= null then
            Remove_Page (Widget.Lectures, 1);
            Widget.Source.View := null;
         end if;
      end if;
      Show_All (Widget.Lectures);
      Show_All (Widget.V_Pane);
      Style_Updated (Widget, Widget.all'Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
      Examples_Selected : constant GInt :=
                          (  Count_Selected_Rows
                             (  Get_Selection
                                (  Get_Tree_View
                                   (  Report.Comparison.View
                          )  )  )  );
      Node_Selected : constant Boolean :=
                          Has_Selected (Report.Classifier);
   begin
      Classify_Button.Set_Sensitive
      (  Report.Comparison.Classify_Selected,
         Examples_Selected = 1 and then Node_Selected
      );
      Set_Sensitive
      (  Report.Comparison.Generalize,
         Examples_Selected = 1 and then Node_Selected
      );
      Expand_Button.Set_Sensitive
      (  Report.Comparison.Expand_Selected,
         Examples_Selected > 0
      );
      if Report.Source.View /= null then
         Find_Button.Set_Sensitive
         (  Report.Source.Find_Selected,
            Node_Selected
         );
      end if;
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

   procedure Set_Selected_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : Positive;
                Complement : Boolean
             )  is
   begin
      if Widget.Comparison.View = null then
         raise Constraint_Error;
      end if;
      Select_Iter
      (  Get_Selection (Get_Tree_View (Widget.Comparison.View)),
         Get_Iter (Widget.Comparison.View, Example, Complement)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Selected_Example")
         )  );
   end Set_Selected_Example;

   procedure Set_Selected_Source_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : Positive;
                Complement : Boolean
             )  is
   begin
      if Widget.Source.View = null then
         raise Constraint_Error;
      end if;
      Select_Iter
      (  Get_Selection (Get_Tree_View (Widget.Source.View)),
         Get_Iter (Widget.Source.View, Example, Complement)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Selected_Source_Example")
         )  );
   end Set_Selected_Source_Example;

   procedure Show
             (  Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Source     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             )  is
      Dialog : Gtk_Dialog;
      Widget : Gtk_Fuzzy_Classifier_Report;
   begin
      Gtk_New (Widget);
      begin
         Put
         (  Widget     => Widget,
            Classifier => Classifier,
            Control    => Control,
            Result     => Result,
            Source     => Source,
            Report     => Report
         );
      exception
         when others =>
            GLib.Object.Checked_Destroy (Widget);
            raise;
      end;
      Gtk_New (Dialog, Title, Parent, Flags);
      Dialog.Get_Content_Area.Pack_Start (Widget);
      Show_All (Widget);
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
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Show")
         )  );
   end Show;

   procedure Show
             (  Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             )  is
      Source : Lecture_Handle;
   begin
      Show
      (  Classifier => Classifier,
         Control    => Control,
         Result     => Result,
         Source     => Source,
         Report     => Report,
         Title      => Title,
         Button     => Button,
         Parent     => Parent,
         Flags      => Flags
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Show")
         )  );
   end Show;

   procedure Style_Updated
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             )  is
      use Column_List;
      Button_Spacing : constant GInt :=
                          Style_Get (Report, "button-gap");
   begin
      Set_Col_Spacings
      (  Report.Comparison.Buttons,
         GUInt (Button_Spacing)
      );
      Set_Title
      (  Get_Column (Report.Report, 0),
         Style_Get (Report, "domain-column-title")
      );
      Set_Title
      (  Get_Column (Report.Report, 1),
         Style_Get (Report, "verified-column-title")
      );
      Set_Title
      (  Get_Column (Report.Report, 2),
         Style_Get (Report, "explained-column-title")
      );
      Set_Text
      (  Report.Comparison.Label,
         Style_Get (Report, "comparison-tab-title")
      );
      if Report.Source.View /= null then
         Set_Text
         (  Report.Source.Label,
            Style_Get (Report, "source-tab-title")
         );
         Set_Col_Spacings
         (  Report.Source.Buttons,
            GUInt (Button_Spacing)
         );
      end if;
      Columns_Autosize (Report.Report);
      declare
         Size : Gtk_Requisition;
      begin
         Size_Request (Report.Report, Size);
         Set_Size_Request
         (  Report.Report,
            GInt'Min (Size.Width,  250),
            GInt'Min (Size.Height, 50)
         );
      end;
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

end Gtk.Fuzzy_Classifier_Report;
