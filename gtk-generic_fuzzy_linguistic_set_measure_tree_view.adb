--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_           Luebeck            --
--        Measure_Tree_View                        Spring, 2007       --
--  Implementation                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Glib.Messages;           use Glib.Messages;
with Strings_Edit;            use Strings_Edit;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;
with Units.Base;              use Units.Base;

with Ada.IO_Exceptions;
with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;
with Name_Tables;

package body Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View is
   use type Fuzzy_Floats_Of.Float_Intervals_Of.Number;

   function Where (Name : String) return String is
   begin
      return
      (  " in Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View."
      &  Name
      );
   end Where;

   function Create
            (  Factory : Gtk_Fuzzy_Measure_Tree_View_Factory;
               Value   : Linguistic_Set
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View is
      Result : Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View;
   begin
      Gtk_New
      (  Result,
         (Factory.SI, Value, Factory.Offset),
         Factory.Scale
      );
      return Result.all'Access;
   end Create;

   function Edited
            (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Boolean is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Edited;
   end Edited;

   function Get
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Purge  : Boolean := True
            )  return Set_Measure is
   begin
      if (  Widget.Scale.Gain = 1.0
        and then
            Widget.Scale.Offset = Widget.Offset
         )
      then
         return
         (  Widget.Scale.SI,
            Widget.Get (Purge),
            Widget.Offset
         );
      else
         declare
            Result : Linguistic_Set;
            Set    : Linguistic_Set renames Widget.Get;
            Shift  : constant Number'Base :=
                        Widget.Scale.Offset - Widget.Offset;
            Var    : Variable;
         begin
            for Index in 1..Get_Cardinality (Set) loop
               Var := Get (Set, Index);
               if Purge then
                  Fuzzy_Measure_Linguistics.
                     Fuzzy_Linguistics.
                        Purge (Var);
               end if;
               Add
               (  Result,
                  Get_Name (Set, Index),
                  Var * Widget.Scale.Gain + Shift
               );
            end loop;
            return (Widget.Scale.SI, Result, Widget.Offset);
         end;
      end if;
   end Get;

   procedure Get
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : out Linguistic_Set;
                Scale  : out Measure;
                Purge  : Boolean := True
             )  is
   begin
      if Widget.Offset = 0.0 then
         Value := Widget.Get (Purge);
      else
         declare
            Result : Linguistic_Set;
            Set    : Linguistic_Set renames Widget.Get;
            Shift  : constant Number'Base :=
                        Widget.Scale.Offset - Widget.Offset;
            Var    : Variable;
         begin
            for Index in 1..Get_Cardinality (Set) loop
               Var := Get (Set, Index);
               if Purge then
                  Fuzzy_Measure_Linguistics.
                     Fuzzy_Linguistics.
                        Purge (Var);
               end if;
               Add
               (  Result,
                  Get_Name (Set, Index),
                  Var - Widget.Offset
               );
            end loop;
            Value := Result;
         end;
      end if;
      Scale := Widget.Scale;
   end Get;

   function Get
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Index  : Positive
            )  return Variable_Measure is
      Var : Variable renames Widget.Get (Index);
   begin
      if (  Widget.Scale.Gain = 1.0
        and then
            Widget.Scale.Offset = Widget.Offset
         )
      then
         return (Widget.Scale.SI, Var, Widget.Offset);
      else
         return
         (  Widget.Scale.SI,
            (  Var * Widget.Scale.Gain
            +  (Widget.Scale.Offset - Widget.Offset)
            ),
            Widget.Offset
         );
      end if;
   end Get;

   function Get_Cardinality
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Natural is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Cardinality;
   end Get_Cardinality;

   function Get_Domain_View
            (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return not null access
                      Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Domain_View;
   end Get_Domain_View;

   function Get_Accumulate_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Accumulate is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Accumulate_Button;
   end Get_Accumulate_Button;

   function Get_Add_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Add is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Add_Button;
   end Get_Add_Button;

   function Get_Copy_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Copy is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Copy_Button;
   end Get_Copy_Button;

   function Get_Down_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Down is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Down_Button;
   end Get_Down_Button;

   function Get_Exec_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Exec is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Exec_Button;
   end Get_Exec_Button;

   function Get_Find_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Find is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Find_Button;
   end Get_Find_Button;

   function Get_Name
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Index  : Positive
            )  return UTF8_String is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Name;
   end Get_Name;

   function Get_New_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_New is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_New_Button;
   end Get_New_Button;

   function Get_Purge_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Purge is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Purge_Button;
   end Get_Purge_Button;

   function Get_Redo_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Redo is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Redo_Button;
   end Get_Redo_Button;

   function Get_Remove_Button
            (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Remove is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Get_Remove_Button (Parent'Access);
   end Get_Remove_Button;

   function Get_Tree_View
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Tree_View is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Tree_View;
   end Get_Tree_View;

   function Get_Undo_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Undo is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Undo_Button;
   end Get_Undo_Button;

   function Get_Unit
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Unit is
   begin
      return Widget.Scale.SI;
   end Get_Unit;

   function Get_Up_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Up is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Up_Button;
   end Get_Up_Button;

   function Get_X_Move_Bar
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_X is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_X_Move_Bar;
   end Get_X_Move_Bar;

   function Get_Y_Move_Bar
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Y is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Get_Y_Move_Bar;
   end Get_Y_Move_Bar;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View;
                Value  : Set_Measure;
                Scale  : UTF8_String  := ""
             )  is
   begin
      Widget :=
         new Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
      begin
         Initialize (Widget, Value, Scale);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   function Image
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Value  : Number
            )  return UTF8_String is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Image (Value);
   end Image;

   function Get_Scale_Text
            (  Widget : not null access
               Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record'Class;
               Set    : Set_Measure;
               Scale  : UTF8_String
            )  return String is
      use Measure_Edit;
   begin
      if Scale'Length = 0 then
         Widget.Scale  := (Set.SI, 1.0, Set.Offset);
         Widget.Offset := Set.Offset;
         if (  Set.SI = Units.Base.Unitless
            and then
               Set.Offset = 0.0
            )
         then
            return "";
         else
            return Image (Widget.Scale);
         end if;
      else
         declare
            Widget_Scale : Measure;
         begin
            Widget_Scale := Value (Scale);
            if Set.SI /= Widget_Scale.SI then
               raise Unit_Error;
            end if;
            if Widget_Scale.Gain <= 0.0 then
              raise Constraint_Error;
            end if;
            Widget.Scale  := Widget_Scale;
            Widget.Offset := Set.Offset;
            return Scale;
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Ada.IO_Exceptions.Data_Error;
         end;
      end if;
   end Get_Scale_Text;

   procedure Initialize
             (  Widget : not null access
                Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record'Class;
                Value  : Set_Measure;
                Scale  : UTF8_String
             )  is
      Scale_Text : constant String :=
                      Widget.Get_Scale_Text (Value, Scale);
   begin
      if (  Widget.Scale.Offset = Value.Offset
         and then
            Widget.Scale.Gain = 1.0
         )
      then  -- No conversion needed
         Initialize (Widget, Value.Gain);
      else
         Initialize
         (  Widget,
            Get_Value_As (Value, Widget.Scale)
         );
      end if;
      Widget.Get_Domain_View.Set_Domain_Note (Scale_Text);
      Widget.Set_Domain_Column_Note (Scale_Text);
   end Initialize;

   procedure Insert
             (  Widget : not null access
                    Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable_Measure
             )  is
   begin
      Widget.Insert (Index, Name, Get_Value_As (Value, Widget.Scale));
   end Insert;

   function Is_Editable
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Boolean is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      return Parent.Is_Editable;
   end Is_Editable;

   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Value  : Variable_Measure
             )  is
   begin
      Widget.Put (Index, Get_Value_As (Value, Widget.Scale));
   end Put;

   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : Set_Measure
             )  is
   begin
      Widget.Put (Get_Value_As (Value, Widget.Scale));
   end Put;

   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : Set_Measure;
                Scale  : UTF8_String
             )  is
      Scale_Text : constant String :=
         Widget.Get_Scale_Text (Value, Scale);
   begin
      Widget.Put (Get_Value_As (Value, Widget.Scale));
      Widget.Get_Domain_View.Set_Domain_Note (Scale_Text);
      Widget.Set_Domain_Column_Note (Scale_Text);
   end Put;

   procedure Remove
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      Parent.Remove (Index);
   end Remove;

   procedure Replace
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable_Measure
             )  is
   begin
      Widget.Replace
      (  Index,
         Name,
         Get_Value_As (Value, Widget.Scale)
      );
   end Replace;

   procedure Select_Duplicated
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      Parent.Select_Duplicated;
   end Select_Duplicated;

   procedure Set_Editable
             (  Widget   : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Editable : Boolean
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      Parent.Set_Editable (Editable);
   end Set_Editable;

   procedure Set_Name
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      Parent.Set_Name (Index, Name);
   end Set_Name;

   procedure Update
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Selected : Selection
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Tree_View_Record renames
                  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
                     (Widget.all);
   begin
      Parent.Update (Selected);
   end Update;

   function Value
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Source : UTF8_String
            )  return Number is
      use Measure_Edit;
      Pointer : Integer := Source'First;
      Result  : Scaled;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, Name_Tables.Blanks );
      if Pointer <= Source'Last then
         raise Ada.IO_Exceptions.Data_Error;
      end if;
      if Result.Format in Canonic..Jumbled then
         return
            Get_Value_As (Result.Numeral * Result.Scale, Widget.Scale);
      else
         return Result.Numeral * Result.Scale.Gain;
      end if;
   end Value;

end Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View;
