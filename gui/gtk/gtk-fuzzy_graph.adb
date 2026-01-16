--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Graph                             Luebeck            --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Fuzzy;                      use Fuzzy;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Scheme;         use Fuzzy.Graph.Scheme;
with Fuzzy.Gtk_Icon_Factory;     use Fuzzy.Gtk_Icon_Factory;
with Integer_Intervals;          use Integer_Intervals;
with GLib;                       use GLib;
with GLib.Messages;              use GLib.Messages;
with GLib.Object;                use GLib.Object;
with GLib.Properties;            use GLib.Properties;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Types;                 use GLib.Types;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Fuzzy;    use Gtk.Cell_Renderer_Fuzzy;
with Gtk.Cell_Renderer_PixBuf;   use Gtk.Cell_Renderer_PixBuf;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Missed;                 use Gtk.Missed;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;          use Gtk.Widget.Styles;

with Gtk.Cell_Renderer_Fuzzy_Boolean;
use  Gtk.Cell_Renderer_Fuzzy_Boolean;

with Gtk.Cell_Renderer_Fuzzy.Feature_Value;
use  Gtk.Cell_Renderer_Fuzzy.Feature_Value;

with Ada.IO_Exceptions;
with Fuzzy.Lecture.General;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Graph is
   use Fuzzy.Feature.Handle.Edit;
   use Fuzzy.Lecture.Handle;

   Class_Record      : aliased Ada_GObject_Class := Uninitialized_Class;
   Tree_Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Graph." & Name;
   end Where;

   type Expansion_Data
        (  Widget  : access Gtk_Fuzzy_Graph_Record'Class;
           Context : access Classification_Parameters'Class
        )  is limited
   record
      Image : Image_Type;
   end record;

   function Classify_As_Selected
            (  Widget  : not null access Gtk_Fuzzy_Graph_Record;
               Context : not null access
                         Classification_Parameters'Class;
               Image   : Image_Type
            )  return Classification is
      Data : Gtk_Tree_Model;
      Row  : Gtk_Tree_Iter;
   begin
      Widget.View.Get_Selection.Get_Selected (Data, Row);
      if Row = Null_Iter then
         raise Constraint_Error;
      else
         return
            Classify
            (  Node    => Get_Graph (Widget.Model, Row, False),
               Context => Context,
               Image   => Image
            );
      end if;
   end Classify_As_Selected;

   function Classify_As_Selected
            (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Image      : Image_Type;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Classification is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality
                           (  Get_Classes
                              (  Get_Graph (Widget.Model)
                        )  )  );
   begin
      Select_Example (Context, Example);
      Context.Threshold  := Threshold;
      Context.Generalize := Generalize;
      return Classify_As_Selected (Widget, Context'Access, Image);
   end Classify_As_Selected;

   procedure Collapse_All
             (  Widget : not null access Gtk_Fuzzy_Graph_Record
             )  is
   begin
     Collapse_All (Widget.View);
   end Collapse_All;

   procedure Expand_Row
             (  Widget  : not null access Gtk_Fuzzy_Graph_Record'Class;
                Row     : Gtk_Tree_Iter
             )  is
      Path : constant Gtk_Tree_Path :=
                      Get_Path (To_Interface (Widget.Model), Row);
   begin
      if Expand_Row (Widget.View, Path, False) then
         null;
      end if;
      Path_Free (Path);
   end Expand_Row;

   procedure Expand_Node
             (  Data   : Expansion_Data;
                Row    : Gtk_Tree_Iter;
                Weight : Confidence
             )  is
   begin
      if Row = Null_Iter then
         return;
      end if;
      Expand_Row (Data.Widget, Row);
      case Get_Type (Data.Widget.Model, Row) is
         when Node_Row =>
            declare
               Feature : Feature_Object'Class renames
                         Ptr (Get_Feature (Data.Widget.Model, Row)).all;
               Focus   : Fuzzy.Set (1..Feature.Cardinality);
               Value   : Fuzzy.Set renames
                            Get (Feature, Data.Context, Data.Image);
               Follow  : Confidence;
               Child   : Gtk_Tree_Iter;
            begin
               for No in 0
                      .. N_Children
                         (  To_Interface (Data.Widget.Model),
                            Row
                         )  - 1
               loop
                  Child :=
                     Nth_Child
                     (  To_Interface (Data.Widget.Model),
                        Row,
                        No
                     );
                  Focus  := Get_Branch (Data.Widget.Model, Child);
                  Follow := Weight and Possibility (Focus, Value);
                  if Follow > Data.Context.Threshold then
                     declare
                        Snap : Context_Snap;
                     begin
                        Create_Constraint
                        (  Feature,
                           Data.Context.all,
                           False
                        );
                        for Index in Focus'Range loop
                           if (  Focus (Index)
                              >  Data.Context.Threshold
                              )
                           then
                              Set_Constraint
                              (  Feature,
                                 Data.Context.all,
                                 Index,
                                 True
                              );
                           end if;
                        end loop;
                        Expand_Node (Data, Child, Follow);
                     end;
                  end if;
               end loop;
            end;
         when Branch_Row =>
            Expand_Node
            (  Data,
               Children (To_Interface (Data.Widget.Model), Row),
               Weight
            );
         when Leaf_Row =>
            null;
      end case;
   end Expand_Node;

   procedure Expand_Example
             (  Widget  : not null access Gtk_Fuzzy_Graph_Record;
                Lesson  : Lecture_Handle;
                Example : Positive;
                Image   : Image_Type
             )  is
   begin
      if (  Is_Valid (Lesson)
         and then
            Example <= Get_Examples_Number (Lesson)
         and then
            Widget.Model /= null
         )
      then
         declare
            Context : aliased Classification_Parameters
                              (  Ptr (Lesson),
                                 Get_Cardinality
                                 (  Get_Classes
                                    (  Get_Graph (Widget.Model)
                              )  )  );
            Data    : aliased Expansion_Data
                              (  Widget  => Widget,
                                 Context => Context'Access
                              );
            Row     : constant Gtk_Tree_Iter :=
                      Get_Iter_First (To_Interface (Widget.Model));
         begin
            Data.Image        := Image;
            Context.Threshold := Confidence'First;
            Select_Example (Context, Example);
            Expand_Node (Data, Row, Confidence'Last);
         end;
      end if;
   end Expand_Example;

   procedure Find_Selected
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Image      : Image_Type;
                Viewer     : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Find_Selected
         (  Widget     => Widget,
            Lesson     => Lesson,
            Example    => Example,
            Complement => Complement,
            Image      => Image,
            Viewer     => Ptr (Viewer)
         );
      else
         Find_Selected
         (  Widget     => Widget,
            Lesson     => Lesson,
            Example    => Example,
            Complement => Complement,
            Image      => Image
         );
      end if;
   end Find_Selected;

   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Graph_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.View;
   end Get_Tree_View;

   function Has_Selected
            (  Widget : not null access Gtk_Fuzzy_Graph_Record
            )  return Boolean is
   begin
      return Widget.View.Get_Selection.Count_Selected_Rows = 1;
   end Has_Selected;

   procedure Next_Image
             (  Total      : Natural;
                Example_No : in out Positive;
                Negative   : in out Boolean
             )  is
      pragma Inline (Next_Image);
   begin
      if Negative then
         Example_No := Example_No + 1;
         if Example_No > Total then
            Example_No := 1;
         end if;
         Negative := False;
      else
         Negative := True;
      end if;
   end Next_Image;

   procedure Find_Selected
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Image      : Image_Type;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
      Total  : constant Natural := Get_Examples_Number (Lesson);
      Start  : Positive;
      This   : Positive;
      Contra : Boolean := Complement;
      Got_It : Boolean;
      Data   : Gtk_Tree_Model;
      Row    : Gtk_Tree_Iter;
   begin
      Widget.View.Get_Selection.Get_Selected (Data, Row);
      if Row = Null_Iter then
         raise Constraint_Error;
      end if;
      if Total = 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      if Example > Total then
         Start := Total;
      else
         Start := Example;
      end if;
      Next_Image (Total, Start, Contra);
      This := Start;
      declare
         Path    : Lecture_Handle := Fuzzy.Lecture.General.Create;
         Feature : Feature_Handle;
      begin
         Reset (Viewer.all, Total);
         while Row /= Null_Iter loop
            if Get_Type (Widget.Model, Row) = Branch_Row then
               Put
               (  Lesson  => Path,
                  Feature => Get_Feature (Widget.Model, Row),
                  Example => 1,
                  Value   => Get_Branch (Widget.Model, Row),
                  Image   => Has_In
               );
            end if;
            Row := Parent (Data, Row);
         end loop;
         loop
            --
            -- Scanning the examples of Lesson. This,  Contra  determine
            -- the  image  to check. It fits when all features from Path
            -- are possible on the image.
            --
            Got_It := True;
            for No in 1..Get_Features_Number (Path) loop
               Feature := Get_Feature (Path, No);
               Got_It :=
                  (  Confidence'First
                  <  Possibility
                     (  Get
                        (  Lesson  => Path,
                           Feature => Feature,
                           Example => 1,
                           Image   => Has_In
                        ),
                        Get
                        (  Lesson  => Lesson,
                           Feature => Feature,
                           Example => This,
                           Image   => Example_Image (Image, Contra)
                  )  )  );
               exit when not Got_It;
            end loop;
            if Got_It then
               Example    := This;
               Complement := Contra;
               Done (Viewer.all);
               return;
            end if;
            Check (Viewer.all);
            Next_Image (Total, This, Contra);
            if This = Start and then Contra /= Complement then
               Done (Viewer.all);
               raise Ada.IO_Exceptions.End_Error;
            end if;
         end loop;
      end;
   end Find_Selected;

   function Get_Tree_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
            Class_Record => Tree_Class_Record'Access,
            Type_Name    => Graph_Class_Name & "TreeView"
         )
      then
         Install_Style_Properties
         (  Class_Ref (Tree_Class_Record.The_Type)
         );
      end if;
      return Tree_Class_Record.The_Type;
   end Get_Tree_Type;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Scrolled_Window.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Graph_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "graph-column-title",
               Nick    => "Graph",
               Blurb   => "The graph column title",
               Default => "Graph"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "graph-column-spacing",
               Nick    => "Spacing",
               Blurb   => "The graph column spacing",
               Minimum => 0,
               Maximum => 32767,
               Default => 3
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Graph) is
   begin
      Widget := new Gtk_Fuzzy_Graph_Record;
      begin
         Gtk.Fuzzy_Graph.Initialize (Widget);
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
             (  Widget : not null access Gtk_Fuzzy_Graph_Record'Class
             )  is
      Column         : Gtk_Tree_View_Column;
      Image_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Value_Renderer : Gtk_Cell_Renderer_Fuzzy_Feature_Value;
      Column_No      : Gint;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Scrolled_Window.Initialize (Widget);
      Widget.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.View := new Gtk.Tree_View.Gtk_Tree_View_Record;
      G_New (Widget.View, Get_Tree_Type);
      Gtk.Tree_View.Initialize (Widget.View);
      Widget.Add (Widget.View);
      Widget.View.Set_Rules_Hint (True);
      Gtk_New (Column);

      Gtk_New (Image_Renderer);
      Column.Pack_Start (Image_Renderer, False);
      Add_Stock_Attribute (Column, Image_Renderer, 0);

      Gtk_New (Value_Renderer);
      Column.Pack_Start (Value_Renderer, True);
      Column.Add_Attribute (Value_Renderer, "feature-value", 2);
      Column.Add_Attribute (Value_Renderer, "prefix-text",   1);
      Set_Property (Value_Renderer, Xalign_Property, 0.0);

      Column_No := Widget.View.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Expand (True);
      Column.Set_Spacing (3);
      Column.Set_Sort_Column_Id (0);

      Handlers.Connect
      (  Widget.View,
         "style-set",
         Style_Updated'Access,
         Widget.all'Access
      );
      Style_Updated (Widget.View, Widget.all'Access);
   end Initialize;

   procedure Put
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Graph      : Node_Handle;
                Parameters : Output_Parameters'Class := Output_Defaults
             )  is
      Classes       : Feature_Handle;
      Columns_Count : Positive := 1;
      Column        : Gtk_Tree_View_Column;
      Column_No     : GInt;
      Renderer      : Gtk_Cell_Renderer_Fuzzy_Boolean;
   begin
      if Widget.Model /= null then
         if Is_Valid (Get_Graph (Widget.Model)) then
            -- Removing all columns but the first
            declare
               Columns : GInt :=
                  GInt
                  (  Get_Cardinality
                     (  Get_Classes
                        (  Get_Graph (Widget.Model)
                     )  )
                  +  1
                  );
            begin
               Set_Model (Widget.View, Null_Gtk_Tree_Model);
               while Columns > 2 loop
                  Columns :=
                     Widget.View.Remove_Column
                     (  Get_Column (Widget.View, Columns - 1)
                     );
               end loop;
            end;
         else
            Set_Model (Widget.View, Null_Gtk_Tree_Model);
         end if;
      end if;
      if Is_Valid (Graph) then
         Classes := Get_Classes (Graph);
         for Index in 1..Get_Cardinality (Classes) loop
            Gtk_New (Column);
            Gtk_New (Renderer);
            Column.Pack_Start (Renderer, True);
            Column_No := Widget.View.Append_Column (Column);
            Column.Add_Attribute
            (  Renderer,
               "fuzzy-boolean-value",
               Column_No + 1
            );
            Column.Set_Resizable (True);
            Column.Set_Expand (False);
            Column.Set_Sort_Column_Id (Column_No);
            Column.Set_Title
            (  Escape_Name
               (  Image (Classes, Interval'(Index, Index), Parameters)
            )  );
         end loop;
      end if;
      Gtk_New (Widget.Model, Graph);
      Set_Model (Widget.View, To_Interface (Widget.Model));
      Unref (Widget.Model);
   end Put;

   procedure Show
             (  Graph    : Node_Handle;
                Title    : UTF8_String      := "";
                Button   : UTF8_String      := "_OK";
                Parent   : Gtk_Window       := null;
                Flags    : Gtk_Dialog_Flags := Modal
             )  is
      Dialog : Gtk_Dialog;
      View   : Gtk_Fuzzy_Graph;
   begin
      Gtk_New (View);
      begin
         Put (View, Graph);
      exception
         when others =>
            GLib.Object.Checked_Destroy (View);
            raise;
      end;
      Gtk_New (Dialog, Title, Parent, Flags);
      Dialog.Get_Content_Area.Pack_Start (View);
      View.Show_All;
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

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Graph  : Gtk_Fuzzy_Graph
             )  is
   begin
      Graph.View.Get_Column (0).Set_Title
      (  Style_Get (Graph, "graph-column-title")
      );
      Graph.View.Get_Column (0).Set_Spacing
      (  Style_Get (Graph, "graph-column-spacing")
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

end Gtk.Fuzzy_Graph;
