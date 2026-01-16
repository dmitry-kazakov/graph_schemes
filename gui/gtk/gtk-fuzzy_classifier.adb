--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Classifier                        Luebeck            --
--  Implementation                                 Summer, 2006       --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Fuzzy.Classifier.Separator;  use Fuzzy.Classifier.Separator;
with Fuzzy.Feature.Handle;        use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Handle;          use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Scheme;          use Fuzzy.Graph.Scheme;
with Fuzzy.Gtk_Icon_Factory;      use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;               use GLib.Messages;
with GLib.Object;                 use GLib.Object;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with GLib.Types;                  use GLib.Types;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Image;                   use Gtk.Image;
with Gtk.Missed;                  use Gtk.Missed;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;

with Ada.IO_Exceptions;
with GtkAda.Handlers;
with GLib.Object.Checked_Destroy;
with Interfaces.C.Strings;

package body Gtk.Fuzzy_Classifier is
   use GLib;
   use Gtk.Box;
   use Gtk.Label;
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
                (  1 => Interfaces.C.Strings.
                        New_String ("selection-changed")
                );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Classifier." & Name;
   end Where;

   procedure Add
             (  Widget     : access Gtk_Fuzzy_Classifier_Record'Class;
                Classifier : Graph_Scheme_Object'Class
             )  is
      Graph   : Gtk_Fuzzy_Graph;
      Picture : Gtk_Image;
      Roots   : Root_Nodes_Array renames Classifier.Roots;
   begin
      Gtk_New (Widget.Content.Tabs);
      for Image in Roots'Range loop
         if Is_Valid (Roots (Image)) then
            Gtk_New (Graph);
            Handlers.Connect
            (  Get_Selection (Get_Tree_View (Graph)),
               "changed",
               Changed_Selection'Access,
               Widget.all'Unchecked_Access
            );
            Widget.Content.Graphs (Image).Graph := Graph;
            Put (Graph, Roots (Image));
            Gtk_New_HBox (Widget.Content.Graphs (Image).Box, False, 0);
            Gtk_New (Widget.Content.Graphs (Image).Label);
            case Image is
               when Has_In =>
                  Gtk_New (Picture, Has_In_Icon, Icon_Size_Menu);
               when Has_Not =>
                  Gtk_New (Picture, Has_Not_Icon, Icon_Size_Menu);
               when Has_Out     =>
                  Gtk_New (Picture, Has_Out_Icon, Icon_Size_Menu);
               when Has_Not_Out =>
                  Gtk_New (Picture, Has_Not_Out_Icon, Icon_Size_Menu);
            end case;
            Pack_Start
            (  Widget.Content.Graphs (Image).Box,
               Picture,
               False,
               False
            );
            Pack_Start
            (  Widget.Content.Graphs (Image).Box,
               Widget.Content.Graphs (Image).Label,
               False,
               False,
               0
            );
            Show (Picture);
            Show (Widget.Content.Graphs (Image).Label);
            Append_Page
            (  Widget.Content.Tabs,
               Graph,
               Widget.Content.Graphs (Image).Box
            );
         end if;
      end loop;
      Handlers.Connect
      (  Widget.Content.Tabs,
         "switch-page",
         Changed_Page'Access,
         Widget.all'Unchecked_Access
      );
      Pack_End (Widget, Widget.Content.Tabs);
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

   procedure Add
             (  Widget     : access Gtk_Fuzzy_Classifier_Record'Class;
                Classifier : Classifier_Handle
             )  is
   begin
      Widget.Classifier := Classifier;
      if Is_Valid (Classifier) then
         declare
            This : Classifier_Object'Class renames Ptr (Classifier).all;
         begin
            if This in Graph_Scheme_Object'Class then
               Widget.Content :=
                  (Scheme, Has_In, null, (others => (null, null, null)));
               Add (Widget, Graph_Scheme_Object'Class (This));
            elsif Is_Separator (Classifier) then
               Gtk_New (Widget.Label);
               Pack_End
               (  Widget,
                  Widget.Label,
                  Expand => False,
                  Fill   => False
               );
               Show (Widget.Label);
               Add (Widget, Get_Classifier (Classifier));
            else
               Widget.Content := (State => Unknown);
               Gtk_New (Widget.Label);
               Pack_Start (Widget, Widget.Label);
               Show (Widget.Label);
            end if;
         end;
      else
         Widget.Content := (State => Invalid);
         Gtk_New (Widget.Label);
         Pack_Start (Widget, Widget.Label);
         Show (Widget.Label);
      end if;
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

   procedure Changed_Page
             (  Object : access GObject_Record'Class;
                Params : Glib.Values.GValues;
                View   : Gtk_Fuzzy_Classifier
             )  is
      Page : GUInt := Get_UInt (Nth (Params, 2));
   begin
      if View.Content.State = Scheme then
         for Image in Image_Type'Range loop
            if View.Content.Graphs (Image).Graph /= null then
               if Page = 0 then
                  View.Content.Image := Image;
                  exit;
               else
                  Page := Page - 1;
               end if;
            end if;
         end loop;
      end if;
      GtkAda.Handlers.Widget_Callback.Emit_By_Name
      (  View,
         "selection-changed"
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Page")
         )  );
   end Changed_Page;

   procedure Changed_Selection
             (  Object : access GObject_Record'Class;
                View   : Gtk_Fuzzy_Classifier
             )  is
   begin
      GtkAda.Handlers.Widget_Callback.Emit_By_Name
      (  View,
         "selection-changed"
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Selection")
         )  );
   end Changed_Selection;

   function Classify_As_Selected
            (  Widget     : not null access
                            Gtk_Fuzzy_Classifier_Record;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is
   begin
      case Widget.Content.State is
         when Invalid | Unknown =>
            raise Constraint_Error;
         when Scheme =>
            case Widget.Content.Image is
               when Has_In | Has_Not =>
                  null;
               when Has_Out | Has_Not_Out =>
                  Context.Generalize := None;
            end case;
            return
               Classify_As_Selected
               (  Widget.Content.Graphs (Widget.Content.Image).Graph,
                  Context,
                  Example_Image (Widget.Content.Image, Complement)
               );
      end case;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Classify_As_Selected")
         )  );
         return (1, (others => 0.5), (others => 0.5));
   end Classify_As_Selected;

   function Classify_As_Selected
            (  Widget     : not null access Gtk_Fuzzy_Classifier_Record;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality
                           (  Get_Classes (Get_Classifier (Widget))
                        )  );
   begin
      Select_Example (Context, Example);
      Context.Threshold  := Threshold;
      Context.Generalize := Generalize;
      return Classify_As_Selected (Widget, Context'Access, Complement);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Classify_As_Selected")
         )  );
         return (1, (others => 0.5), (others => 0.5));
   end Classify_As_Selected;

   procedure Collapse_All
             (  Widget  : not null access Gtk_Fuzzy_Classifier_Record;
                Visible : Boolean := False
             )  is
   begin
      case Widget.Content.State is
         when Invalid | Unknown =>
            null;
         when Scheme =>
            if Visible then
               if (  Widget.Content.Graphs (Widget.Content.Image).Graph
                  /= null
                  )
               then
                  Collapse_All
                  (  Widget.Content.Graphs (Widget.Content.Image).Graph
                  );
               end if;
            else
               for Image in Image_Type'Range loop
                  if Widget.Content.Graphs (Image).Graph /= null then
                     Collapse_All (Widget.Content.Graphs (Image).Graph);
                  end if;
               end loop;
            end if;
      end case;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Collapse_All")
         )  );
   end Collapse_All;

   procedure Expand_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : Positive;
                Complement : Boolean;
                Visible    : Boolean := False
             )  is
   begin
      case Widget.Content.State is
         when Invalid | Unknown =>
            null;
         when Scheme =>
            if Visible then
               if (  Widget.Content.Graphs (Widget.Content.Image).Graph
                  /= null
                  )
               then
                  Expand_Example
                  (  Widget.Content.Graphs (Widget.Content.Image).Graph,
                     Lesson,
                     Example,
                     Example_Image (Widget.Content.Image, Complement)
                  );
               end if;
            else
               for Image in Image_Type'Range loop
                  if Widget.Content.Graphs (Image).Graph /= null then
                     Expand_Example
                     (  Widget.Content.Graphs (Image).Graph,
                        Lesson,
                        Example,
                        Example_Image (Image, Complement)
                     );
                  end if;
               end loop;
            end if;
      end case;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Expand_Example")
         )  );
   end Expand_Example;

   procedure Find_Selected
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Viewer     : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Find_Selected
         (  Widget     => Widget,
            Lesson     => Lesson,
            Example    => Example,
            Complement => Complement,
            Viewer     => Ptr (Viewer)
         );
      else
         Find_Selected
         (  Widget     => Widget,
            Lesson     => Lesson,
            Example    => Example,
            Complement => Complement
         );
      end if;
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

   procedure Find_Selected
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      case Widget.Content.State is
         when Invalid | Unknown =>
            raise Ada.IO_Exceptions.End_Error;
         when Scheme =>
            if (  Widget.Content.Graphs (Widget.Content.Image).Graph
               =  null
               )
            then
               raise Ada.IO_Exceptions.End_Error;
            end if;
            Find_Selected
            (  Widget.Content.Graphs (Widget.Content.Image).Graph,
               Lesson,
               Example,
               Complement,
               Widget.Content.Image,
               Viewer
            );
      end case;
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

   function Get_Classifier
            (  Widget : not null access Gtk_Fuzzy_Classifier_Record
            )  return Classifier_Handle is
   begin
      return Widget.Classifier;
   end Get_Classifier;

   function Get_Type return Gtk_Type is
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
            Gnew_Int
            (  Name    => "page-title-spacing",
               Nick    => "Title spacing",
               Default => 3,
               Minimum => 0,
               Maximum => 32767,
               Blurb   => (  "The gap between the page icon and"
                          &  "the page title"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "has-in-page-title",
               Nick    => "Has-in",
               Default => "in",
               Blurb   => (  "The page title of the graph deduced "
                          &  "from has-in images"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "has-not-page-title",
               Nick    => "Has-not",
               Default => "not in",
               Blurb   => (  "The page title of the graph deduced "
                          &  "from has-not images"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "has-out-page-title",
               Nick    => "Has-out",
               Default => "out",
               Blurb   => (  "The page title of the graph deduced "
                          &  "from has-out images"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "has-not-out-page-title",
               Nick    => "Has-not-out",
               Default => "not out",
               Blurb   => (  "The page title of the graph deduced "
                          &  "from has-not-out images"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "separator-classifier-label",
               Nick    => "Separator",
               Blurb   => "The label of separator classifier",
               Default => "Separator"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unsupported-classifier-label",
               Nick    => "Unsupported",
               Blurb   => "The label of an unsupported classifier",
               Default => "Unsupported"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "invalid-classifier-label",
               Nick    => "Invalid",
               Blurb   => "The label of no classifier",
               Default => "Invalid"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Classifier) is
   begin
      Widget := new Gtk_Fuzzy_Classifier_Record;
      begin
         Gtk.Fuzzy_Classifier.Initialize (Widget);
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

   function Has_Selected
            (  Widget : not null access Gtk_Fuzzy_Classifier_Record
            )  return Boolean is
   begin
      case Widget.Content.State is
         when Invalid | Unknown =>
            return False;
         when Scheme =>
            if Widget.Content.Tabs = null then
               return False;
            end if;
            declare
               Graph : Gtk_Fuzzy_Graph;
            begin
               Graph :=
                  Widget.Content.Graphs (Widget.Content.Image).Graph;
               return
               (  Graph /= null
               and then
                  Gtk.Fuzzy_Graph.Has_Selected (Graph)
               );
            end;
      end case;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Has_Selected")
         )  );
         return False;
   end Has_Selected;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Record'Class
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize_VBox (Widget);
      Init;
      Handlers.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Classifier : Classifier_Handle
             )  is
   begin
      Erase (Widget);
      Widget.Label := null;
      Add (Widget, Classifier);
      Style_Updated (Widget, Widget.all'Unchecked_Access);
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

   procedure Show
             (  Classifier : Classifier_Handle;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             )  is
      Dialog : Gtk_Dialog;
      View   : Gtk_Fuzzy_Classifier;
   begin
      Gtk_New (View);
      begin
         Put (View, Classifier);
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
                View   : Gtk_Fuzzy_Classifier
             )  is
      Content : Content_View renames View.Content;
   begin
      case Content.State is
         when Invalid =>
            Set_Text
            (  View.Label,
               Style_Get (View, "invalid-classifier-label")
            );
         when Scheme =>
            if View.Label /= null then
               Set_Text
               (  View.Label,
                  Style_Get (View, "separator-classifier-label")
               );
            end if;
            if Content.Graphs (Has_In).Label /= null then
               Set_Text
               (  Content.Graphs (Has_In).Label,
                  Style_Get (View, "has-in-page-title")
               );
            end if;
            if Content.Graphs (Has_Not).Label /= null then
               Set_Text
               (  Content.Graphs (Has_Not).Label,
                  Style_Get (View, "has-not-page-title")
               );
            end if;
            if Content.Graphs (Has_Out).Label /= null then
               Set_Text
               (  Content.Graphs (Has_Out).Label,
                  Style_Get (View, "has-out-page-title")
               );
            end if;
            if Content.Graphs (Has_Not_Out).Label /= null then
               Set_Text
               (  Content.Graphs (Has_Not_Out).Label,
                  Style_Get (View, "has-not-out-page-title")
               );
            end if;
            declare
               Spacing : constant GInt :=
                  Style_Get (View, "page-title-spacing");
            begin
               if Content.Graphs (Has_In).Box /= null then
                  Set_Spacing (Content.Graphs (Has_In).Box, Spacing);
               end if;
               if Content.Graphs (Has_Out).Box /= null then
                  Set_Spacing (Content.Graphs (Has_Out).Box, Spacing);
               end if;
               if Content.Graphs (Has_Not).Box /= null then
                  Set_Spacing (Content.Graphs (Has_Not).Box, Spacing);
               end if;
               if Content.Graphs (Has_Not_Out).Box /= null then
                  Set_Spacing
                  (  Content.Graphs (Has_Not_Out).Box,
                     Spacing
                  );
               end if;
            end;
         when Unknown =>
            Set_Text
            (  View.Label,
               Style_Get (View, "unsupported-classifier-label")
            );
      end case;
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

end Gtk.Fuzzy_Classifier;
