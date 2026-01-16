--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Discrete_Factory                         Winter, 2007       --
--  Implementation                                                    --
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

with Fuzzy.Feature.Discrete;     use Fuzzy.Feature.Discrete;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Types;                 use GLib.Types;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Separator;              use Gtk.Separator;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Widget.Styles;          use Gtk.Widget.Styles;
with Integer_Intervals;          use Integer_Intervals;

with Ada.IO_Exceptions;
with Fuzzy.Feature.Handle.Edit;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Discrete_Factory is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Create
            (  Widget : not null access Gtk_Discrete_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
   begin
      return Create (Name, Get_Domain (Get (Widget)).all);
   end Create;

   function Edited
            (  Widget : not null access Gtk_Discrete_Factory_Record
            )  return Boolean is
   begin
      return Edited (Widget.List);
   end Edited;

   procedure Editable_Off
             (  Widget : not null access
                         Gtk_Discrete_Factory_Record'Class
             )  is
   begin
      if Widget.Buttons /= null then
         Ref (Widget.Frame);
         Remove (Widget, Widget.Frame);
         Remove (Widget, Widget.Buttons);
         Widget.Buttons := null;
         Attach (Widget, Widget.Frame, 0, 2, 0, 1);
         Unref (Widget.Frame);
      end if;
   end Editable_Off;

   procedure Editable_On
             (  Widget : access Gtk_Discrete_Factory_Record'Class
             )  is
   begin
      if Widget.Buttons = null then
         Ref (Widget.Frame);
         Remove (Widget, Widget.Frame);
         Attach (Widget, Widget.Frame, 0, 1, 0, 1);
         Unref (Widget.Frame);
         -- Buttons box
         Gtk_New_VBox (Widget.Buttons);
         Attach
         (  Widget,
            Widget.Buttons,
            1, 2, 0, 1,
            XOptions => 0,
            YOptions => 0
         );
         Pack_Start
         (  Widget.Buttons,
            Get_New_Button (Widget.List),
            False,
            False
         );
         Pack_Start
         (  Widget.Buttons,
            Get_Copy_Button (Widget.List),
            False,
            False
         );
         Pack_Start
         (  Widget.Buttons,
            Get_Remove_Button (Widget.List),
            False,
            False
         );
         Pack_Start
         (  Widget.Buttons,
            Get_Up_Button (Widget.List),
            False,
            False
         );
         Pack_Start
         (  Widget.Buttons,
            Get_Down_Button (Widget.List),
            False,
            False
         );
         declare
            Line : Gtk_Separator;
         begin
            Gtk_New_HSeparator (Line);
            Pack_Start (Widget.Buttons, Line, False, False);
         end;
         Pack_Start
         (  Widget.Buttons,
            Get_Undo_Button (Widget.List),
            False,
            False
         );
         Pack_Start
         (  Widget.Buttons,
            Get_Redo_Button (Widget.List),
            False,
            False
         );
      end if;
   end Editable_On;

   function Get
            (  Widget : not null access Gtk_Discrete_Factory_Record
            )  return Linguistic_Set is
   begin
      if 0 = Get_Cardinality (Widget.List) then
         raise Constraint_Error with Style_Get (Widget, "empty-error");
      else
         begin
            return Widget.List.Get;
         exception
            when Constraint_Error => -- Illegal names
               raise Constraint_Error with
                     Style_Get (Widget, "name-error");
            when Ada.IO_Exceptions.Name_Error => -- Duplicated names
               Select_Duplicated (Widget.List);
               raise Constraint_Error with
                     Style_Get (Widget, "duplicate-error");
         end;
      end if;
   end Get;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Discrete_Factory_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "empty-error",
               Nick    => "Empty domain",
               Blurb   => "Empty domain message",
               Default => (  "There has to be at least one name in "
                          &  "the discrete feature domain set"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "name-error",
               Nick    => "Name error",
               Blurb   => "Name error message",
               Default => (  "The domain contains illegal names. "
                          &  "Wrong names are highlighted by a "
                          &  "different color."
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "duplicate-error",
               Nick    => "Duplicate names",
               Blurb   => "Duplicate name message",
               Default => (  "The domain contains duplicated names. "
                          &  "These names are highlighted by a "
                          &  "selection."
         )  )             );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget   : out Gtk_Discrete_Factory;
                Feature  : Feature_Handle := No_Feature;
                Editable : Boolean        := True
             )  is
   begin
      if Feature.Is_Valid and then not Is_Discrete (Feature) then
         raise Constraint_Error with "Not a discrete feature";
      end if;
      Widget := new Gtk_Discrete_Factory_Record;
      begin
         Initialize (Widget, Feature, Editable);
      exception
         when others =>
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget   : not null access
                           Gtk_Discrete_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             )  is
      List  : Linguistic_Set;
      Empty : Variable;
   begin
      if Feature.Is_Valid and then Is_Discrete (Feature) then
         for Index in 1..Get_Cardinality (Feature) loop
            Insert
            (  List,
               Index,
               Fuzzy.Feature.Handle.Edit.Image
               (  Feature,
                  Interval'(Index, Index)
               ),
               Empty
            );
         end loop;
      end if;
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 1, 2, False);
      Set_Homogeneous (Widget, False);
      -- List view
      Gtk_New (Widget.List, List);
      Set_Editable (Widget.List, Editable);
      declare
         Tree : constant Gtk_Tree_View := Get_Tree_View (Widget.List);
      begin
         Set_Visible (Get_Column (Tree, 1), False);
         Set_Visible (Get_Column (Tree, 2), False);
         Set_Visible (Get_Column (Tree, 3), False);
      end;
      Gtk_New (Widget.Frame);
      Set_Shadow_Type (Widget.Frame, Shadow_In);
      Add (Widget.Frame, Widget.List);
      if Editable then
         Attach (Widget, Widget.Frame, 0, 1, 0, 1);
         Editable_On (Widget);
      else
         Attach (Widget, Widget.Frame, 0, 2, 0, 1);
      end if;
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Discrete_Factory_Record
            )  return Boolean is
   begin
      return Is_Editable (Widget.List);
   end Is_Editable;

   procedure Set_Button_Spacing
             (  Widget  : not null access Gtk_Discrete_Factory_Record;
                Spacing : GUInt
             )  is
   begin
      if Widget.Buttons /= null then
         Set_Spacing (Widget.Buttons, GInt (Spacing));
      end if;
   end Set_Button_Spacing;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Discrete_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         if Editable then
            Editable_On (Widget);
         else
            Editable_Off (Widget);
         end if;
      end if;
   end Set_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Discrete_Factory_Record
             )  is
      List : Linguistic_Set renames Get (Widget);
   begin
      null;
   end Verify;

end Gtk.Fuzzy_Feature.Discrete_Factory;
