--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Set                               Luebeck            --
--  Implementation                                 Spring, 2007       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
with Confidence_Factors;        use Confidence_Factors;
with Fuzzy.Logic;               use Fuzzy.Logic;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with Ada.IO_Exceptions;
with GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Intuitionistic;
with GLib.Values.Fuzzy.Logic;
with Gtk.Cell_Renderer_Text;
with GLib.Object.Checked_Destroy;
with GtkAda.Handlers;
with Interfaces.C.Strings;

package body Gtk.Fuzzy_Set is
   use Gtk.Tree_View;

   Widget_Class_Record : aliased Ada_GObject_Class :=
                                 Uninitialized_Class;
   Tree_Class_Record   : aliased Ada_GObject_Class :=
                                 Uninitialized_Class;
   Step                : constant := 0.05;
--
-- Selection_State -- The variables and points of selected
--
   type Selection_Data is record
      Edit        : Gtk_Fuzzy_Set;
      Increment   : Float;
      Possibility : Boolean;
      Necessity   : Boolean;
      Modified    : Boolean := False;
   end record;
   type Selection_Data_Ptr is not null access all Selection_Data;

   package Selection_Browsing is
      new Selected_Foreach_User_Data (Selection_Data_Ptr);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Set." & Name;
   end Where;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Mode   : Content_Type
             );

   procedure Modify_Selected
             (  Widget      : not null access
                              Gtk_Fuzzy_Set_Record'Class;
                Increment   : Float;
                Possibility : Boolean;
                Necessity   : Boolean
             );

   procedure On_Selected
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Selection_Data_Ptr
             );

   procedure Commit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Set
             )  is
      Row   : constant Gtk_Tree_Iter  :=
                 Get_Iter_From_String (Widget.Store, Get_Path (Cell));
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value := Get (Cell);
         Set_Value (Widget.Store, Row, 1, Value);
         Unset (Value);
         Widget.Edited := True;
         GtkAda.Handlers.Widget_Callback.Emit_By_Name
         (  Widget,
            "changed"
         );
      end if;
   end Commit;

   Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
             (  1 => Interfaces.C.Strings.New_String ("changed")
             );

   function Get_Name
            (  Data  : Fuzzy.Abstract_Edit.User_Data'Class;
               Index : Value_Index;
               Size  : Positive := 256
            )  return String is
   begin
      declare
         Text    : String (1..Size);
         Pointer : Integer := Text'First;
      begin
         Put (Text, Pointer, Data, Index, Index);
         return Text (1..Pointer - 1);
      end;
   exception
      when Ada.IO_Exceptions.Layout_Error =>
         return Get_Name (Data, Index, (Size * 3) / 2);
   end Get_Name;

   function Create_Store
            (  Mode        : Content_Type;
               Cardinality : Natural;
               Data        : Fuzzy.Abstract_Edit.User_Data'Class
            )  return Gtk_List_Store is
      Row    : Gtk_Tree_Iter := Null_Iter;
      Result : Gtk_List_Store;
   begin
      case Mode is
         when Intuitionistic_Classification | Intuitionistic_Set =>
            Gtk_New
            (  Result,
               (  GType_String,
                  GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean
            )  );
         when Plain_Set =>
            Gtk_New
            (  Result,
               (  GType_String,
                  GLib.Values.Confidence_Factors.GType_Confidence
            )  );
      end case;
      for Index in 1..Value_Index (Cardinality) loop
         Result.Append (Row);
         Result.Set (Row, 0, Get_Name (Data, Index));
         Next (Result, Row);
      end loop;
      return Result;
   exception
      when others =>
         if Result /= null then
            Result.Unref;
         end if;
         raise;
   end Create_Store;

   function Edited
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Boolean is
   begin
      return Widget.Edited;
   end Edited;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Fuzzy.Set is
      Result : Fuzzy.Set (1..Integer (Widget.Store.N_Children));
      Level  : GValue;
      Row    : Gtk_Tree_Iter := Widget.Store.Get_Iter_First;
   begin
      if Widget.Mode /= Plain_Set then
         raise Constraint_Error;
      end if;
      for Index in Result'Range loop
         Widget.Store.Get_Value (Row, 1, Level);
         begin
            Result (Index) :=
               GLib.Values.Confidence_Factors.Get (Level);
         exception
            when others =>
               Unset (Level);
               raise;
         end;
         Unset (Level);
         Widget.Store.Next (Row);
      end loop;
      return Result;
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Fuzzy.Intuitionistic.Set is
      Result : Fuzzy.Intuitionistic.Set
                  (Positive (Widget.Store.N_Children));
      Level  : GValue;
      Value  : Fuzzy_Boolean;
      Row    : Gtk_Tree_Iter := Widget.Store.Get_Iter_First;
   begin
      if Widget.Mode /= Intuitionistic_Set then
         raise Constraint_Error;
      end if;
      for Index in 1..Result.Cardinality loop
         Widget.Store.Get_Value (Row, 1, Level);
         begin
            Value := GLib.Values.Fuzzy.Logic.Get (Level);
         exception
            when others =>
               Unset (Level);
               raise;
         end;
         Unset (Level);
         Result.Possibility (Index) := Value.Possibility;
         Result.Necessity   (Index) := Value.Necessity;
         Widget.Store.Next (Row);
      end loop;
      return Result;
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Classification is
      Result : Classification (Positive (Widget.Store.N_Children));
      Level  : GValue;
      Value  : Fuzzy_Boolean;
      Row    : Gtk_Tree_Iter := Get_Iter_First (Widget.Store);
   begin
      if Widget.Mode /= Intuitionistic_Classification then
         raise Constraint_Error;
      end if;
      for Index in 1..Result.Cardinality loop
         Widget.Store.Get_Value (Row, 1, Level);
         begin
            Value := GLib.Values.Fuzzy.Logic.Get (Level);
         exception
            when others =>
               Unset (Level);
               raise;
         end;
         Unset (Level);
         Result.Possibility (Index) := Value.Possibility;
         Result.Necessity   (Index) := Value.Necessity;
         Next (Widget.Store, Row);
      end loop;
      return Result;
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return GValue is
      Result : GValue;
   begin
      case Widget.Mode is
         when Plain_Set =>
            Init (Result, GLib.Values.Fuzzy.GType_Set);
            GLib.Values.Fuzzy.Set
            (  Result,
               Fuzzy.Set'(Get (Widget))
            );
         when Intuitionistic_Set =>
            Init (Result, GLib.Values.Fuzzy.Intuitionistic.GType_Set);
            GLib.Values.Fuzzy.Intuitionistic.Set
            (  Result,
               Fuzzy.Intuitionistic.Set'(Get (Widget))
            );
         when Intuitionistic_Classification =>
            Init
            (  Result,
               GLib.Values.Fuzzy.Intuitionistic.GType_Classification
            );
            GLib.Values.Fuzzy.Intuitionistic.Set
            (  Result,
               Classification'(Get (Widget))
            );
      end case;
      return Result;
   exception
      when others =>
         Unset (Result);
         raise;
   end Get;

   function Get_Cardinality
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Natural is
   begin
      return Natural (Widget.Store.N_Children);
   end Get_Cardinality;

   function Get_Content_Type
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Content_Type is
   begin
      return Widget.Mode;
   end Get_Content_Type;

   function Get_Name
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Index  : Positive
             )  return String is
      Data : GValue;
   begin
      if Index in 1..Widget.Get_Cardinality then
         Widget.Store.Get_Value
         (  Widget.Store.Nth_Child (Null_Iter, GInt (Index) - 1),
            0,
            Data
         );
         return Result : constant String := Get_String (Data) do
            Unset (Data);
         end return;
      else
         raise Constraint_Error;
      end if;
   end Get_Name;

   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Tree;
   end Get_Tree_View;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Scrolled_Window.Get_Type,
            Class_Record => Widget_Class_Record'Access,
            Type_Name    => Class_Name,
            Signals      => Signals
         )
      then
         Install_Style_Property
         (  Class_Ref (Widget_Class_Record.The_Type),
            Gnew_String
            (  Name    => "domain-column-title",
               Nick    => "Domain",
               Blurb   => "The title of domain values column",
               Default => "Domain"
         )  );
         Install_Style_Property
         (  Class_Ref (Widget_Class_Record.The_Type),
            Gnew_String
            (  Name    => "value-column-title",
               Nick    => "Value",
               Blurb   => "The title of values column",
               Default => "Value"
         )  );
         Gtk.Cell_Renderer_Fuzzy_Boolean.Install_Style_Properties
         (  Class_Ref (Widget_Class_Record.The_Type)
         );
      end if;
      return Widget_Class_Record.The_Type;
   end Get_Type;

   function Get_Tree_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
            Class_Record => Tree_Class_Record'Access,
            Type_Name    => Class_Name & "TreeView"
         )
      then
         Install_Style_Properties
         (  Class_Ref (Tree_Class_Record.The_Type)
         );
      end if;
      return Tree_Class_Record.The_Type;
   end Get_Tree_Type;

   function Get_Shape
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Shape is
   begin
      return Widget.Values.Get_Shape (Widget);
   end Get_Shape;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Record;
      begin
         Initialize (Widget, Data, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Fuzzy.Set)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Record;
      begin
         Initialize (Widget, Data, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Intuitionistic.Set)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Classification
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Record;
      begin
         Initialize (Widget, Data, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Intuitionistic.Classification)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : GValue
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Record;
      begin
         Initialize (Widget, Data, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (GValue)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Mode   : Content_Type
             )  is
      use Gtk.Cell_Renderer_Text;
      use Gtk.Cell_Renderer_Fuzzy_Boolean;
   begin
      G_New (Widget, Get_Type);
      Gtk.Scrolled_Window.Initialize (Widget);
      Set_Policy (Widget, Policy_Automatic, Policy_Automatic);
      Widget.Mode := Mode;

      Widget.Tree := new Gtk_Tree_View_Record;
      G_New (Widget.Tree, Get_Tree_Type);
      Gtk.Tree_View.Initialize (Widget.Tree);
      Widget.Tree.Get_Selection.Set_Mode (Selection_Multiple);

      Widget.Add (Widget.Tree);
      Widget.Tree.Set_Rules_Hint (True);
      --
      -- Creating columns
      --
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
         Column_No := Widget.Tree.Append_Column (Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Gtk_New (Widget.Values);
         Column.Pack_Start (Widget.Values, True);
         case Widget.Mode is
            when Plain_Set =>
               Column.Add_Attribute
               (  Widget.Values,
                  "confidence-value",
                  1
               );
            when Intuitionistic_Set | Intuitionistic_Classification =>
               Column.Add_Attribute
               (  Widget.Values,
                  "fuzzy-boolean-value",
                  1
               );
         end case;
         Column_No := Widget.Tree.Append_Column (Column);
         Column.Set_Resizable (True);
      end;
      --
      -- Callbacks
      --
      Widget_Callbacks.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access
      );
      Commit_Callbacks.Connect
      (  Widget.Values,
         "commit",
         Commit'Access,
         Widget.all'Access
      );
      Widget_Result_Callbacks.Connect
      (  Widget.Tree,
         "key_press_event",
         Widget_Result_Callbacks.To_Marshaller (Key_Press'Access),
         Widget.all'Access
      );
      Style_Updated (Widget);
   end Initialize;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             )  is
   begin
      Initialize (Widget, Plain_Set);
      Widget.Store := Create_Store (Widget.Mode, Value'Length, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Initialize (Widget, Intuitionistic_Set);
      Widget.Store :=
         Create_Store (Widget.Mode, Value.Cardinality, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Classification
             )  is
   begin
      Initialize (Widget, Intuitionistic_Classification);
      Widget.Store :=
         Create_Store (Widget.Mode, Value.Cardinality, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : GValue
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
      Mode : Content_Type;
   begin
      if Is_Classification (Value) then
         Mode := Intuitionistic_Classification;
      elsif Is_Set (Value) then
         Mode := Intuitionistic_Set;
      elsif GLib.Values.Fuzzy.Is_Set (Value) then
         Mode := Plain_Set;
      else
         raise Constraint_Error;
      end if;
      Initialize (Widget, Mode);
      Widget.Store :=
         Create_Store (Mode, Get_Cardinality (Value), Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Boolean is
   begin
      return Widget.Values.Get_Mode = Cell_Renderer_Mode_Editable;
   end Is_Editable;

   function Key_Press
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Edit   : Gtk_Fuzzy_Set
            )  return Boolean is
      Mode : Gdk_Modifier_Type;
   begin
      if Edit.Is_Editable then
         --
         -- Key pressed while popup window is open
         --
         case Get_Event_Type (Event) is
            when Key_Press =>
               case Get_Key_Val (Event) is
                  when GDK_Plus | GDK_Greater | GDK_KP_Add =>
                     Mode := Get_State (Event);
                     Edit.Modify_Selected
                     (  Step,
                        0 = (Mode and Control_Mask),
                        0 = (Mode and Mod1_Mask)
                     );
                     return True;
                  when GDK_Minus | GDK_Less | GDK_KP_Subtract =>
                     Mode := Get_State (Event);
                     Edit.Modify_Selected
                     ( -Step,
                        0 = (Mode and Control_Mask),
                        0 = (Mode and Mod1_Mask)
                     );
                     return True;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end if;
      return False;
   end Key_Press;

   procedure Modify_Selected
             (  Widget      : not null access
                              Gtk_Fuzzy_Set_Record'Class;
                Increment   : Float;
                Possibility : Boolean;
                Necessity   : Boolean
             )  is
      Data : aliased Selection_Data :=
                     (  Edit        => Widget.all'Access,
                        Increment   => Increment,
                        Possibility => Possibility,
                        Necessity   => Necessity,
                        Modified    => False
                     );
   begin
      Selection_Browsing.Selected_Foreach
      (  Widget.Tree.Get_Selection,
         On_Selected'Access,
         Data'Unchecked_Access
      );
      if Data.Modified then
         Widget.Edited := True;
         GtkAda.Handlers.Widget_Callback.Emit_By_Name
         (  Widget,
            "changed"
         );
      end if;
   end Modify_Selected;

   procedure On_Selected
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Selection_Data_Ptr
             )  is
      use GLib.Values.Fuzzy.Logic;
      use GLib.Values.Confidence_Factors;

      procedure Adjust (Level : in out Confidence) is
         Value : constant Float := Float (Level) + Data.Increment;
      begin
         if Value >= Float (Confidence'Last) then
            Level := Confidence'Last;
         elsif Value <= Float (Confidence'First) then
            Level := Confidence'First;
         else
            Level := Confidence (Value);
         end if;
      end Adjust;

      Value : GValue;
      Level : Fuzzy_Boolean := Certain_False;
   begin
      Get_Value (Model, Iter, 1, Value);
      if Data.Edit.Mode = Plain_Set then
         if GLib.Values.Confidence_Factors.Is_Defined (Value) then
            Level.Possibility := Get (Value);
         end if;
         Adjust (Level.Possibility);
         if not (  GLib.Values.Confidence_Factors.Is_Defined (Value)
                and then
                   Level.Possibility = Get (Value)
                )
         then
            GLib.Values.Confidence_Factors.Set
            (  Value,
               Level.Possibility
            );
            Data.Edit.Store.Set_Value (Iter, 1, Value);
            Data.Modified := True;
         end if;
      else
         Level := Get (Value);
         if Data.Possibility or else Data.Edit.Mode = Plain_Set then
            Adjust (Level.Possibility);
         end if;
         if Data.Necessity or else Data.Edit.Mode = Plain_Set then
            Adjust (Level.Necessity);
         end if;
         if not (  GLib.Values.Fuzzy.Logic.Is_Defined (Value)
                and then
                   Level = Get (Value)
                )
         then
            GLib.Values.Fuzzy.Logic.Set (Value, Level);
            Data.Edit.Store.Set_Value (Iter, 1, Value);
            Data.Modified := True;
         end if;
      end if;
      Unset (Value);
   end On_Selected;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             )  is
   begin
      Widget.Store := Create_Store (Widget.Mode, Value'Length, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Fuzzy.Set
             )  is
      Level : GValue;
      Row   : Gtk_Tree_Iter := Widget.Store.Get_Iter_First;
   begin
      if (  Widget.Mode /= Plain_Set
         or else
            Value'Length /= Widget.Get_Cardinality
         )
      then
         raise Constraint_Error;
      end if;
      Widget.Store.Ref;
      Widget.Tree.Set_Model (Null_Gtk_Tree_Model);
      Init (Level, GLib.Values.Confidence_Factors.GType_Confidence);
      for Index in Value'Range loop
         GLib.Values.Confidence_Factors.Set (Level, Value (Index));
         Widget.Store.Set_Value (Row, 1, Level);
         Widget.Store.Next (Row);
      end loop;
      Unset (Level);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Edited := False;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Widget.Store :=
         Create_Store (Widget.Mode, Value.Cardinality, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
      Level : GValue;
      Row   : Gtk_Tree_Iter := Widget.Store.Get_Iter_First;
   begin
      if (  Widget.Mode /= Intuitionistic_Set
         or else
            Value.Cardinality /= Get_Cardinality (Widget)
         )
      then
         raise Constraint_Error;
      end if;
      Widget.Store.Ref;
      Widget.Tree.Set_Model (Null_Gtk_Tree_Model);
      Init (Level, GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean);
      for Index in 1..Value.Cardinality loop
         GLib.Values.Fuzzy.Logic.Set
         (  Level,
            (  Possibility => Value.Possibility (Index),
               Necessity   => Value.Necessity   (Index)
         )  );
         Set_Value (Widget.Store, Row, 1, Level);
         Next (Widget.Store, Row);
      end loop;
      Unset (Level);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Edited := False;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Classification
             )  is
   begin
      Widget.Store :=
         Create_Store (Widget.Mode, Value.Cardinality, Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Classification
             )  is
      Level : GValue;
      Row   : Gtk_Tree_Iter := Get_Iter_First (Widget.Store);
   begin
      if (  Widget.Mode /= Intuitionistic_Classification
         or else
            Value.Cardinality /= Widget.Get_Cardinality
         )
      then
         raise Constraint_Error;
      end if;
      Widget.Store.Ref;
      Widget.Tree.Set_Model (Null_Gtk_Tree_Model);
      Init (Level, GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean);
      for Index in 1..Value.Cardinality loop
         GLib.Values.Fuzzy.Logic.Set
         (  Level,
            (  Possibility => Value.Possibility (Index),
               Necessity   => Value.Necessity   (Index)
         )  );
         Widget.Store.Set_Value (Row, 1, Level);
         Widget.Store.Next (Row);
      end loop;
      Unset (Level);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Edited := False;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : GValue
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      if Is_Classification (Value) then
         Widget.Mode := Intuitionistic_Classification;
      elsif Is_Set (Value) then
         Widget.Mode := Intuitionistic_Set;
      elsif GLib.Values.Fuzzy.Is_Set (Value) then
         Widget.Mode := Plain_Set;
      else
         raise Constraint_Error;
      end if;
      Widget.Store :=
         Create_Store (Widget.Mode, Get_Cardinality (Value), Data);
      Widget.Tree.Set_Model (To_Interface (Widget.Store));
      Widget.Store.Unref;
      Widget.Put (Value);
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : GValue
             )  is
   begin
      if GLib.Values.Fuzzy.Is_Set (Value) then
         Widget.Put (GLib.Values.Fuzzy.Get (Value));
      elsif GLib.Values.Fuzzy.Intuitionistic.Is_Set (Value) then
         Widget.Put
         (  Fuzzy.Intuitionistic.Set'
               (GLib.Values.Fuzzy.Intuitionistic.Get (Value))
         );
      elsif GLib.Values.Fuzzy.Intuitionistic.
            Is_Classification (Value) then
         Widget.Put
         (  Classification'
               (GLib.Values.Fuzzy.Intuitionistic.Get (Value))
         );
      else
         raise Constraint_Error;
      end if;
   end Put;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Fuzzy_Set_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         if Editable then
            Widget.Values.Set_Mode (Cell_Renderer_Mode_Editable);
         else
            Widget.Values.Set_Mode (Cell_Renderer_Mode_Inert);
         end if;
      end if;
   end Set_Editable;

   procedure Set_Shape
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Form   : Shape
             )  is
   begin
      Widget.Values.Set_Shape (Form);
   end Set_Shape;

   procedure Style_Updated
             (  Widget : access Gtk_Fuzzy_Set_Record'Class
             )  is
   begin
      Get_Column (Widget.Tree, 0).Set_Title
      (  Style_Get (Widget, "domain-column-title")
      );
      Get_Column (Widget.Tree, 1).Set_Title
      (  Style_Get (Widget, "value-column-title")
      );
      Widget.Tree.Columns_Autosize;
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Widget.Tree.Get_Preferred_Width  (Dummy, Width);
         Widget.Tree.Get_Preferred_Height (Dummy, Height);
         Widget.Tree.Set_Size_Request
         (  GInt'Min (Width,  600),
            GInt'Min (Height, 500)
         );
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

end Gtk.Fuzzy_Set;
