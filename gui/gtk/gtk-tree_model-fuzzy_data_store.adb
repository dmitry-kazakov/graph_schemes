--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Fuzzy_Data_Store             Luebeck            --
--  Implementation                                 Autumn, 2009       --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Confidence_Factors;      use Confidence_Factors;
with Fuzzy.Feature;           use Fuzzy.Feature;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with Glib.Messages;           use Glib.Messages;
with Gtk.Tree_Model;          use Gtk.Tree_Model;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GLib.Values.Fuzzy.Intuitionistic;
with GtkAda.Types;

package body Gtk.Tree_Model.Fuzzy_Data_Store is
   use GLib.Values.Fuzzy.Intuitionistic;
   use type Fuzzy.Set;

   Fuzzy_Data_Store_Type : GType := GType_Invalid;

   Class_Record : Ada_GObject_Class;
   Signals : constant GtkAda.Types.Chars_Ptr_Array :=
             (  0 => GtkAda.Types.New_String ("redo-changed"),
                1 => GtkAda.Types.New_String ("undo-changed"),
                2 => GtkAda.Types.New_String ("column-deleted"),
                3 => GtkAda.Types.New_String ("column-inserted")
             );

   function Check
            (  Store : Gtk_Fuzzy_Data_Store_Record'Class;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   pragma Inline (Check);

   function Compose
            (  Store : Gtk_Fuzzy_Data_Store_Record'Class;
               Row   : Row_Index
            )  return Gtk_Tree_Iter;
   pragma Inline (Compose);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Classification,
             Classification_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Lecture_Rows.Unbounded_Ptr_Array,
             Lecture_Row_Ptr
          );

   function Get_Row_Index (Iter : Gtk_Tree_Iter) return Row_Index;
   pragma Inline (Get_Row_Index);

   procedure Set_Row_Index
             (  Iter : in out Gtk_Tree_Iter;
                Row  : Row_Index
             );
   pragma Inline (Set_Row_Index);

    function To_Address is
      new Ada.Unchecked_Conversion (Row_Index, System.Address);

   function To_Row_Index is
      new Ada.Unchecked_Conversion (System.Address, Row_Index);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Tree_Model.Fuzzy_Data_Store." & Name;
   end Where;

   procedure Add_Column
             (  Store   : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Feature : Feature_Handle;
                Column  : Positive;
                What    : Action_Type
             )  is
   begin
      if (  not Feature.Is_Valid
         or else
            Column > Store.Features.Get_Size + 1
         )
      then
         raise Constraint_Error;
      end if;
      for Index in 1..Store.Features.Get_Size loop
         if Store.Features.Get (Index) = Feature then
            raise Constraint_Error;
         end if;
      end loop;
      declare
         No     : Column_Index;
         Result : constant Abstract_Action_Ptr :=
                  new Delete_Column_Action;
         This   : Delete_Column_Action renames
                  Delete_Column_Action (Result.all);
      begin
         if Is_Empty (Store.Free) then
            No := Column_Index (Store.Features.Get_Size + 1);
         else
            No := Get (Store.Free, 1);
            Remove (Container => Store.Free, Index => 1);
         end if;
         This.Column := GInt (Column);
         for Column in reverse This.Column
                            .. GInt (Store.Features.Get_Size)
         loop
            Put
            (  Store.Data.Columns,
               Column + 1,
               Store.Data.Columns.Get (Column)
            );
         end loop;
         Store.Data.Columns.Put (This.Column, No);
         Store.Features.Add (No, Feature);
         Store.Complete (Result, What);
         Store.Column := Natural (This.Column);
         Handlers.Emit_By_Name (Store'Access, "column-inserted");
         Store.Column := 0;
         Complete (Store, Result, What);
      end;
   end Add_Column;

   procedure Add_Column
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Feature : Feature_Handle;
                Column  : Positive
             )  is
   begin
      Add_Column (Store.all, Feature, Column, Action_Do);
   end Add_Column;

   procedure Add_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             )  is
      No     : Row_Index :=
                  (  Row_Count'Min
                     (  Row_Index (Example) * 2,
                        Store.Rows_Count
                     )
                  +  1
                  );
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Result : constant Abstract_Action_Ptr := new Delete_Action (2);
      This   : Delete_Action renames Delete_Action (Result.all);
   begin
      for Index in 1..2 loop
         This.Rows_List (Index) := No;
         if Store.Rows_Count = 0 then
            Store.Data.Rows.Put (No, null);
         else
            Store.Data.Rows.Put
            (  Row_Index (Store.Rows_Count + 1),
               null
            );
            Store.Data.Rows.Vector
            (  No + 1
            .. Row_Index (Store.Rows_Count + 1)
            )  := Store.Data.Rows.Vector
                  (  No
                  .. Row_Index (Store.Rows_Count)
                  );
            Store.Data.Rows.Vector (No) := null;
         end if;
         Store.Rows_Count := Store.Rows_Count + 1;
         Row  := Store.Compose (No);
         Path := Store.Get_Path (Row);
         Row_Inserted (To_Interface (Store), Path, Row);
         Path_Free (Path);
         No := No + 1;
      end loop;
      Store.Complete (Result, Action_Do);
   end Add_Example;

   procedure Add_Random_Singleton_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             )  is
      No     : Row_Index :=
                  (  Row_Count'Min
                     (  Row_Index (Example) * 2,
                        Store.Rows_Count
                     )
                  +  1
                  );
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Result : constant Abstract_Action_Ptr := new Delete_Action (2);
      This   : Delete_Action renames Delete_Action (Result.all);
      Edited : Lecture_Row_Ptr :=
                  new Lecture_Rows.Unbounded_Ptr_Array;
   begin
      declare
         use Random_Numbers;
         Dice    : Generator;
         Feature : Feature_Handle;
         Cell    : Classification_Ptr;
         Index   : Positive;
      begin
         Reset (Dice);
         for Column in 1..Store.Features.Get_Size loop
            Feature := Store.Features.Get (Column);
            Cell := new Classification'
                        (  Feature.Get_Cardinality,
                           (others => Confidence'First),
                           (others => Confidence'First)
                        );
            Index := Random (Dice) mod Feature.Get_Cardinality + 1;
            Cell.Possibility (Index) := Confidence'Last;
            Cell.Necessity   (Index) := Confidence'Last;
            Edited.Put (Store.Features.Get_Key (Column), Cell);
         end loop;
      end;
      for Index in 1..2 loop
         This.Rows_List (Index) := No;
         Store.Data.Rows.Put (No, Edited);
         Edited := null;
         Store.Rows_Count := Store.Rows_Count + 1;
         Row  := Store.Compose (No);
         Path := Store.Get_Path (Row);
         Row_Inserted (To_Interface (Store), Path, Row);
         Path_Free (Path);
         No := No + 1;
      end loop;
      Store.Complete (Result, Action_Do);
   end Add_Random_Singleton_Example;

   function Can_Redo
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Boolean is
   begin
      return not Store.Data.Redo.Is_Empty;
   end Can_Redo;

   function Can_Undo
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Boolean is
   begin
      return not Store.Data.Undo.Is_Empty;
   end Can_Undo;

   function Check
            (  Store : Gtk_Fuzzy_Data_Store_Record'Class;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
      use System;
   begin
      return
      (  Iter /= Null_Iter
      and then
         Iter.User_Data2 = Store'Address
      );
   end Check;

   function Children
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if (  Parent = Null_Iter
         and then
            Store.Features.Get_Size > 0
         )
      then
         return Store.Compose (1);
      else
         return Null_Iter;
      end if;
   end Children;

   function Compose
            (  Store : Gtk_Fuzzy_Data_Store_Record'Class;
               Row   : Row_Index
            )  return Gtk_Tree_Iter is
   begin
      return
      (  Stamp      => 1,
         User_Data  => To_Address (Row),
         User_Data2 => Store'Address,
         User_Data3 => System.Null_Address
      );
   end Compose;

   procedure Complete
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record;
                Action : Abstract_Action_Ptr;
                What   : Action_Type
             )  is
      Modified : Boolean;
   begin
      case What is
         when Action_Do =>
            Modified := Store.Data.Undo.Is_Empty;
            Store.Data.Redo.Erase;
            Store.Data.Undo.Push (Ref (Action));
            Handlers.Emit_By_Name (Store'Access, "redo-changed");
            if Modified then
               Handlers.Emit_By_Name (Store'Access, "undo-changed");
            end if;
         when Action_Redo =>
            Modified := Store.Data.Undo.Is_Empty;
            Store.Data.Redo.Pop;
            Store.Data.Undo.Push (Ref (Action));
            if Store.Data.Redo.Is_Empty then
               Handlers.Emit_By_Name (Store'Access, "redo-changed");
            end if;
            if Modified then
               Handlers.Emit_By_Name (Store'Access, "undo-changed");
            end if;
         when Action_Undo =>
            Modified := Store.Data.Redo.Is_Empty;
            Store.Data.Undo.Pop;
            Store.Data.Redo.Push (Ref (Action));
            if Modified then
               Handlers.Emit_By_Name (Store'Access, "redo-changed");
            end if;
            if Store.Data.Undo.Is_Empty then
               Handlers.Emit_By_Name (Store'Access, "undo-changed");
            end if;
      end case;
   end Complete;

   procedure Copy_Rows
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Rows  : GInt_Array
             )  is
      Columns  : constant Column_Count :=
                          Column_Count (Store.Features.Get_Size);
      No       : Row_Index;
      Copied   : Row_Sets.Set;
      Inserted : Row_Sets.Set;
   begin
      for Index in Rows'Range loop
         if Rows (Index) in 0..GInt (Store.Rows_Count) - 1 then
            No := Row_Index (Rows (Index) + 1);
            Add (Copied, No);
            Add (Inserted, No);
            Add (Inserted, No + (No mod 2) * 2 - 1);
         end if;
      end loop;
      if Is_Empty (Copied) then
         return;
      end if;
      declare
         Result : constant Abstract_Action_Ptr :=
                     new Delete_Action (Get_Size (Inserted));
         This   : Delete_Action renames Delete_Action (Result.all);
      begin
         declare
            From : Lecture_Row_Ptr;
            To   : Lecture_Row_Ptr;
            Cell : Classification_Ptr;
         begin
            for Index in 1..Get_Size (Inserted) loop
               No := Inserted.Get (Index);
               if Is_In (Copied, No) then
                  From := Store.Data.Rows.Get (No);
                  if From /= null then
                     To := new Lecture_Rows.Unbounded_Ptr_Array;
                     for Column in 1..Columns loop
                        Cell := Get (From.all, Column);
                        if Cell /= null then
                           Put
                           (  To.all,
                              Column,
                              new Classification'(Cell.all)
                           );
                        end if;
                     end loop;
                     Store.Data.Rows.Put
                     (  Store.Rows_Count + Row_Index (Index),
                        To
                     );
                  end if;
               end if;
            end loop;
         end;
         declare
            Row  : Gtk_Tree_Iter;
            Path : Gtk_Tree_Path;
         begin
            for Index in 1..Get_Size (Inserted) loop
               Store.Rows_Count := Store.Rows_Count + 1;
               This.Rows_List (Index) := Store.Rows_Count;
               Row  := Store.Compose (Store.Rows_Count);
               Path := Store.Get_Path (Row);
               Row_Inserted (To_Interface (Store), Path, Row);
               Path_Free (Path);
            end loop;
         end;
         Store.Complete (Result, Action_Do);
      end;
   end Copy_Rows;

   procedure Delete_Column
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Column : Positive;
                What   : Action_Type
             )  is
      No     : constant Column_Index :=
                  Store.Data.Columns.Get (GInt (Column));
      Ptr    : Lecture_Row_Ptr;
      Result : constant Abstract_Action_Ptr :=
                  new Insert_Column_Action (Store.Rows_Count);
      This   : Insert_Column_Action renames
               Insert_Column_Action (Result.all);
   begin
      This.Column   := GInt (Column);
      This.Features := Store.Features.Get (No);
      for Row in This.Data'Range loop
         Ptr := Store.Data.Rows.Get (Row);
         if Ptr /= null then
            This.Data (Row) := Get (Ptr.all, No);
            if This.Data (Row) /= null then
               Ptr.Vector (No) := null;
            end if;
         end if;
      end loop;
      Remove (Store.Features, No);
      for Column in This.Column
                 .. GInt (Store.Features.Get_Size)
      loop
         Put
         (  Store.Data.Columns,
            Column,
            Store.Data.Columns.Get (Column + 1)
         );
      end loop;
      Add (Store.Free, No);
      Complete (Store, Result, What);
      Store.Column := Natural (This.Column);
      Handlers.Emit_By_Name (Store'Access, "column-deleted");
      Store.Column := 0;
      Complete (Store, Result, What);
   end Delete_Column;

   procedure Delete_Column
             (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
                Column : Positive
             )  is
   begin
      Delete_Column (Store.all, Column, Action_Do);
   end Delete_Column;

   procedure Delete_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             )  is
   begin
      Delete_Rows (Store, (1 => GInt (Example) * 2 - 2));
   end Delete_Example;

   procedure Delete_Rows
             (  Store : in out Gtk_Fuzzy_Data_Store_Record'Class;
                List  : Row_Index_Array;
                What  : Action_Type
             )  is
      No     : Row_Index;
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Result : constant Abstract_Action_Ptr :=
                        new Insert_Action (List'Length);
      This   : Insert_Action renames Insert_Action (Result.all);
   begin
      for Index in reverse List'Range loop
         No := List (Index);
         This.Rows_List (Index) := (No, Store.Data.Rows.Get (No));
         Store.Data.Rows.Vector (No..Store.Rows_Count - 1) :=
            Store.Data.Rows.Vector (No + 1..Store.Rows_Count);
         Store.Data.Rows.Vector (Store.Rows_Count) := null;
         Store.Rows_Count := Store.Rows_Count - 1;
         Row  := Compose (Store, No);
         Path := Store.Get_Path (Row);
         Row_Deleted (To_Interface (Store'Unchecked_Access), Path);
         Path_Free (Path);
      end loop;
      Complete (Store, Result, What);
   end Delete_Rows;

   procedure Delete_Rows
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Rows  : GInt_Array
             )  is
      No   : Row_Index;
      List : Row_Sets.Set;
   begin
      for Index in Rows'Range loop
         if Rows (Index) in 0..GInt (Store.Rows_Count) - 1 then
            No := Row_Index (Rows (Index) + 1);
            Add (List, No);
            Add (List, No + (No mod 2) * 2 - 1);
         end if;
      end loop;
      if not Is_Empty (List) then
         declare
            Rows_List : Row_Index_Array (1..Get_Size (List));
         begin
            for Index in Rows_List'Range loop
               Rows_List (Index) := Get (List, Index);
            end loop;
            Delete_Rows (Store.all, Rows_List, Action_Do);
         end;
      end if;
   end Delete_Rows;

   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Delete_Action;
                What   : Action_Type
             )  is
   begin
      Delete_Rows (Store, Action.Rows_List, What);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_It (delete action)")
         )  );
   end Do_It;

   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Delete_Column_Action;
                What   : Action_Type
             )  is
   begin
      Delete_Column (Store, Positive (Action.Column), What);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_It (delete column action)")
         )  );
   end Do_It;

   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Edit_Action;
                What   : Action_Type
             )  is
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Result : constant Abstract_Action_Ptr :=
                        new Edit_Action (Action.Row, Action.Column);
      This   : Edit_Action renames Edit_Action (Result.all);
      Edited : Lecture_Row_Ptr := Store.Data.Rows.Get (Action.Row);
   begin
      if Edited = null then
         Edited := new Lecture_Rows.Unbounded_Ptr_Array;
         Store.Data.Rows.Put (Action.Row, Edited);
      else
         This.Value := Get (Edited.all, Action.Column);
         if This.Value /= null then
            Edited.Vector (Action.Column) := null;
         end if;
      end if;
      Edited.Put (Action.Column, Action.Value);
      Action.Value := null;
      Row  := Compose (Store, Action.Row);
      Path := Get_Path (Store'Access, Row);
      Row_Changed (To_Interface (Store'Unchecked_Access), Path, Row);
      Complete (Store, Result, What);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_It (edit action)")
         )  );
   end Do_It;

   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Insert_Action;
                What   : Action_Type
             )  is
      No     : Row_Index;
      Row    : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Result : constant Abstract_Action_Ptr :=
                        new Delete_Action (Action.Rows);
      This   : Delete_Action renames Delete_Action (Result.all);
   begin
      for Index in 1..Action.Rows loop
         No := Action.Rows_List (Index).Row;
         This.Rows_List (Index) := No;
         Store.Data.Rows.Put (Store.Rows_Count + 1, null);
         Store.Data.Rows.Vector (No + 1..Store.Rows_Count + 1) :=
            Store.Data.Rows.Vector (No..Store.Rows_Count);
         Store.Data.Rows.Vector (No) :=
            Action.Rows_List (Index).Data;
         Action.Rows_List (Index).Data := null;
         Store.Rows_Count := Store.Rows_Count + 1;
      end loop;
      for Index in 1..Action.Rows loop
         Row  := Compose (Store, Action.Rows_List (Index).Row);
         Path := Get_Path (Store'Access, Row);
         Row_Inserted
         (  To_Interface (Store'Unchecked_Access),
            Path,
            Row
         );
         Path_Free (Path);
      end loop;
      Complete (Store, Result, What);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_It (insert action)")
         )  );
   end Do_It;

   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Insert_Column_Action;
                What   : Action_Type
             )  is
      Ptr : Lecture_Row_Ptr;
      No  : Column_Index;
   begin
      Add_Column
      (  Store,
         Action.Features,
         Positive (Action.Column),
         What
      );
      No := Store.Data.Columns.Get (Action.Column);
      for Row in Action.Data'Range loop
         if Action.Data (Row) /= null then
            Ptr := Store.Data.Rows.Get (Row);
            if Ptr = null then
               Ptr := new Lecture_Rows.Unbounded_Ptr_Array;
               Store.Data.Rows.Put (Row, Ptr);
            end if;
            Put (Ptr.all, No, Action.Data (Row));
            Action.Data (Row) := null;
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_It (insert column action)")
         )  );
   end Do_It;

   procedure Finalize
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Lecture_Data,
                Lecture_Data_Ptr
             );
   begin
      Free (Store.Data);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   procedure Finalize (Action : in out Edit_Action) is
   begin
      Free (Action.Value);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Edit_Action)")
         )  );
   end Finalize;

   procedure Finalize (Action : in out Insert_Action) is
   begin
      for Index in Action.Rows_List'Range loop
         Free (Action.Rows_List (Index).Data);
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Insert_Action)")
         )  );
   end Finalize;

   procedure Finalize (Action : in out Insert_Column_Action) is
   begin
      for Index in Action.Data'Range loop
         Free (Action.Data (Index));
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Insert_Column_Action)")
         )  );
   end Finalize;

   procedure Get
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : in out Lecture_Handle;
                Viewer : Indicator_Handle
             )  is
   begin
      Get (Store, Lesson, Ptr (Viewer));
   end Get;

   procedure Get
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Feature  : Feature_Handle;
      Ptr      : Lecture_Row_Ptr;
      Cell     : Classification_Ptr;
      No       : Column_Index;
      Example  : Positive := 1;
      Negative : Boolean  := False;
   begin
      Reset (Viewer.all, Natural (Store.Rows_Count));
         -- Filling the model with training set data
      for Row in 1..Store.Rows_Count loop
         Ptr := Store.Data.Rows.Get (Row);
         if Ptr /= null then
            for Column in 1..Store.Features.Get_Size loop
               No   := Store.Data.Columns.Get (GInt (Column));
               Cell := Get (Ptr.all, No);
               if Cell /= null then
                  Feature := Store.Features.Get (No);
                  if Negative then
                     Lesson.Put
                     (  Example,
                        Feature,
                        Has_Out,
                        Cell.Possibility
                     );
                     Lesson.Put
                     (  Example,
                        Feature,
                        Has_Not_Out,
                        not Cell.Necessity
                     );
                  else
                     Lesson.Put
                     (  Example,
                        Feature,
                        Has_In,
                        Cell.Possibility
                     );
                     Lesson.Put
                     (  Example,
                        Feature,
                        Has_Not,
                        not Cell.Necessity
                     );
                  end if;
               end if;
            end loop;
         end if;
         if Negative then
            Negative := False;
            Example  := Example + 1;
         else
            Negative := True;
         end if;
         Viewer.Check;
      end loop;
      Viewer.Done;
   end Get;

   function Get_Column
            (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
               Feature : Feature_Handle
            )  return Positive is
   begin
      for Index in 1..Store.Features.Get_Size loop
         if Store.Features.Get (Index) = Feature then
            declare
               No : constant Column_Index :=
                             Store.Features.Get_Key (Index);
            begin
               for Column in 1..GInt (Store.Features.Get_Size) loop
                  if Store.Data.Columns.Get (Column) = No then
                     return Positive (Column);
                  end if;
               end loop;
            end;
         end if;
      end loop;
      raise Constraint_Error;
   end Get_Column;

   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Index : Gint
            )  return GType is
   begin
      if Index < 0 then
         return GType_Invalid;
      elsif Index = 0 then
         return GType_String;
      elsif Index <= GInt (Store.Features.Get_Size) then
         return GType_Classification;
      else
         return GType_Invalid;
      end if;
   end Get_Column_Type;

   function Get_Deleted_Column
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural is
   begin
      return Store.Column;
   end Get_Deleted_Column;

   function Get_Examples_Number
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural is
   begin
      return Natural (Store.Rows_Count + 1) / 2;
   end Get_Examples_Number;

   function Get_Feature
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Column : Positive
            )  return Feature_Handle is
   begin
      return
         Get
         (  Store.Features,
            Store.Data.Columns.Get (GInt (Column))
         );
   end Get_Feature;

   function Get_Features_Number
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural is
   begin
      return Store.Features.Get_Size;
   end Get_Features_Number;

   function Get_Flags
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Tree_Model_Flags is
   begin
      return Tree_Model_Iters_Persist + Tree_Model_List_Only;
   end Get_Flags;

   function Get_Inserted_Column
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural is
   begin
      return Store.Column;
   end Get_Inserted_Column;

   function Get_Iter
            (  Store    : not null access Gtk_Fuzzy_Data_Store_Record;
               Example  : Positive;
               Positive : Boolean
            )  return Gtk_Tree_Iter is
      Row : Row_Count := Row_Count (Example) * 2 + 1;
   begin
      if Positive then
         if Row > Store.Rows_Count then
            return Store.Compose (Row);
         end if;
      else
         Row := Row + 1;
         if Row > Store.Rows_Count then
            return Store.Compose (Row);
         end if;
      end if;
      return Null_Iter;
   end Get_Iter;

   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Get_Depth (Path) = 1 then
         declare
            Indices : GInt_Array renames Get_Indices (Path);
            No      : constant GInt := Indices (Indices'First) + 1;
         begin
            if Store.Rows_Count >= Row_Count (No) then
               return Store.Compose (Row_Index (No));
            end if;
         end;
      end if;
      return Null_Iter;
   end Get_Iter;

   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return GInt is
   begin
      return GInt (Store.Features.Get_Size) + 1;
   end Get_N_Columns;

   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
   begin
      if Check (Store.all, Iter) then
         declare
            Path : Gtk_Tree_Path;
         begin
            Gtk_New (Path);
            Append_Index (Path, GInt (Get_Row_Index (Iter) - 1));
            return Path;
         end;
      else
         return Null_Gtk_Tree_Path;
      end if;
   end Get_Path;

   function Get_Row_Index (Iter : Gtk_Tree_Iter) return Row_Index is
   begin
      return To_Row_Index (Iter.User_Data);
   end Get_Row_Index;

   function Get_Type return Gtk_Type is
   begin
      if Fuzzy_Data_Store_Type = GType_Invalid then
         Fuzzy_Data_Store_Type := Register (Class_Name, Signals);
      end if;
      return Fuzzy_Data_Store_Type;
   end Get_Type;

   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : GInt;
                Value  : out GValue
             )  is
      Row  : Lecture_Row_Ptr;
      Cell : Classification_Ptr;
   begin
      if (  Column not in 0..GInt (Store.Features.Get_Size)
         or else
            not Check (Store.all, Iter)
         )
      then
         Init (Value, GType_String);
         Set_String (Value, "");
      elsif Column = 0 then
         Init (Value, GType_String);
         if Get_Row_Index (Iter) mod 2 = 1 then
            Set_String (Value, Fuzzy.Gtk_Icon_Factory.Positive_Icon);
         else
            Set_String (Value, Fuzzy.Gtk_Icon_Factory.Negative_Icon);
         end if;
      else
         Row := Store.Data.Rows.Get (Get_Row_Index (Iter));
         Init (Value, GType_Classification);
         if Row = null then
            Set_Undefined (Value);
         else
            Cell := Row.Get (Store.Data.Columns.Get (Column));
            if Cell = null then
               Set_Undefined (Value);
            else
               GLib.Values.Fuzzy.Intuitionistic.Set (Value, Cell.all);
            end if;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Get_Value")
         )  );
         raise;
   end Get_Value;

   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Data_Store;
                Lesson : Lecture_Handle;
                Viewer : Indicator_Handle
             )  is
   begin
      Gtk_New (Store, Lesson, Ptr (Viewer));
   end Gtk_New;

   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Data_Store;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
   begin
      Store := new Gtk_Fuzzy_Data_Store_Record;
      begin
         Initialize (Store, Lesson, Viewer);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return False;
   end Has_Child;

   procedure Initialize
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize (Store, Get_Type);
      Store.Data := new Lecture_Data;
      Store.Put (Lesson, Viewer);
   end Initialize;

   function Is_In
            (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
               Feature : Feature_Handle
            )  return Boolean is
   begin
      if Feature.Is_Valid then
         for Index in 1..Store.Features.Get_Size loop
            if Store.Features.Get (Index) = Feature then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Is_In;

   procedure Next
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if Check (Store.all, Iter) then
         declare
            No : constant Row_Index := Get_Row_Index (Iter) + 1;
         begin
            if Row_Count (No) <= Store.Rows_Count then
               Set_Row_Index (Iter, No);
               return;
            end if;
         end;
      end if;
      Iter := Null_Iter;
   end Next;

   function Nth_Child
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter and then N < GInt (Store.Rows_Count) then
         return Store.Compose (Row_Index (N + 1));
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   function N_Children
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
   begin
      if Iter = Null_Iter then
         return GInt (Store.Rows_Count);
      else
         return 0;
      end if;
   end N_Children;

   function Parent
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Parent;

   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if Check (Store.all, Iter) then
         declare
            No : constant Row_Index := Get_Row_Index (Iter);
         begin
            if No > 1 and then Row_Count (No) <= Store.Rows_Count then
               Set_Row_Index (Iter, No - 1);
               return;
            end if;
         end;
      end if;
      Iter := Null_Iter;
   end Previous;

   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : Indicator_Handle
             )  is
   begin
      Store.Put (Lesson, Ptr (Viewer));
   end Put;

   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Modified_Redo : constant Boolean := not Store.Data.Redo.Is_Empty;
      Modified_Undo : constant Boolean := not Store.Data.Undo.Is_Empty;
   begin
      Store.Features.Erase;
      Store.Data.Rows.Erase;
      Store.Data.Undo.Erase;
      Store.Data.Redo.Erase;
      Store.Rows_Count := 0;
      if Lesson.Is_Valid then
         declare
            Feature : Feature_Handle;
            Row     : Lecture_Row_Ptr;
            Defined : Boolean := False;
         begin
            for Index in 1..Lesson.Get_Features_Number loop
               Add
               (  Store.Features,
                  Column_Index (Index),
                  Lesson.Get_Feature (Index)
               );
               Put
               (  Store.Data.Columns,
                  GInt (Index),
                  Column_Index (Index)
               );
            end loop;
            Row := new Lecture_Rows.Unbounded_Ptr_Array;
            Reset (Viewer.all, Get_Examples_Number (Lesson));
               -- Filling the model with training set data
            for Example in 1..Get_Examples_Number (Lesson) loop
               Defined := False;
               for Column in 1..Lesson.Get_Features_Number loop
                  Feature := Lesson.Get_Feature (Column);
                  if (  Lesson.Is_Defined (Example, Feature, Has_In)
                     or else
                        Lesson.Is_Defined (Example, Feature, Has_Not)
                     )
                  then
                     Defined := True;
                     Row.Put
                     (  Column_Index (Column),
                        new Fuzzy.Intuitionistic.Classification'
                           (  Feature.Get_Cardinality,
                              Lesson.Get (Example, Feature, Has_In),
                              not Get
                                  (  Lesson,
                                     Example,
                                     Feature,
                                     Has_Not
                     )     )      );
                  end if;
               end loop;
               if Defined then
                  Store.Data.Rows.Put
                  (  Row_Index (Example * 2 - 1),
                     Row
                  );
                  Row := new Lecture_Rows.Unbounded_Ptr_Array;
                  Defined := False;
               end if;
               Store.Rows_Count := Store.Rows_Count + 1;
               for Column in 1..Lesson.Get_Features_Number loop
                  Feature := Lesson.Get_Feature (Positive (Column));
                  if (  Lesson.Is_Defined (Example, Feature, Has_Out)
                     or else
                        Lesson.Is_Defined
                        (  Example,
                           Feature,
                           Has_Not_Out
                     )  )
                  then
                     Defined := True;
                     Row.Put
                     (  Column_Index (Column),
                        new Fuzzy.Intuitionistic.Classification'
                            (  Feature.Get_Cardinality,
                               Lesson.Get (Example, Feature, Has_Out),
                               not Lesson.Get
                                   (  Example,
                                      Feature,
                                      Has_Not_Out
                     )      )      );
                  end if;
               end loop;
               if Defined then
                  Store.Data.Rows.Put
                  (  Row_Index (Example * 2),
                     Row
                  );
                  Row := new Lecture_Rows.Unbounded_Ptr_Array;
                  Defined := False;
               end if;
               Store.Rows_Count := Store.Rows_Count + 1;
               Viewer.Check;
            end loop;
            if not Defined then
               Free (Row);
            end if;
            Viewer.Done;
         end;
      end if;
      if Modified_Redo then
         Handlers.Emit_By_Name (Store, "redo-changed");
      end if;
      if Modified_Undo then
         Handlers.Emit_By_Name (Store, "undo-changed");
      end if;
   end Put;

   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Value  : GValue
             )  is
   begin
      if Row = Null_Iter then
         raise Constraint_Error;
      else
         declare
            Path   : Gtk_Tree_Path;
            Result : constant Abstract_Action_Ptr :=
                        new Edit_Action
                            (  Get_Row_Index (Row),
                               Store.Data.Columns.Get (Column)
                            );
            This   : Edit_Action renames Edit_Action (Result.all);
            Edited : Lecture_Row_Ptr := Store.Data.Rows.Get (This.Row);
         begin
            if Edited = null then
               if Is_Defined (Value) then
                  Edited := new Lecture_Rows.Unbounded_Ptr_Array;
                  Store.Data.Rows.Put (This.Row, Edited);
                  Edited.Put
                  (  This.Column,
                     new Fuzzy.Intuitionistic.
                         Classification'(Get (Value))
                  );
               end if;
            else
               This.Value := Get (Edited.all, This.Column);
               if Is_Defined (Value) then
                  Edited.Put
                  (  This.Column,
                     new Fuzzy.Intuitionistic.
                         Classification'(Get (Value))
                  );
               else
                  Edited.Vector (This.Column) := null;
               end if;
            end if;
            Path := Store.Get_Path (Row);
            Row_Changed (To_Interface (Store), Path, Row);
            Path_Free (Path);
            Store.Complete (Result, Action_Do);
         end;
      end if;
   exception
      when Error : Constraint_Error =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
         raise;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
         raise;
   end Put;

   procedure Redo
             (  Store : not null access
                        Gtk_Fuzzy_Data_Store_Record
             )  is
      This : Action_Handles.Handle;
   begin
      if not Store.Data.Redo.Is_Empty then
         This := Top (Store.Data.Redo);
         Do_It (Store.all, Ptr (This).all, Action_Redo);
      end if;
   end Redo;

   procedure Set_Row_Index
             (  Iter : in out Gtk_Tree_Iter;
                Row  : Row_Index
             )  is
   begin
      Iter.User_Data := To_Address (Row);
   end Set_Row_Index;

   procedure Set_Stored
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record
             )  is
   begin
      if not Store.Data.Undo.Is_Empty then
         Store.Data.Undo.Erase;
         Handlers.Emit_By_Name (Store, "undo-changed");
      end if;
   end Set_Stored;

   procedure Undo
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record
             )  is
      This : Action_Handles.Handle;
   begin
      if not Store.Data.Undo.Is_Empty then
         This := Top (Store.Data.Undo);
         Do_It (Store.all, Ptr (This).all, Action_Undo);
      end if;
   end Undo;

end Gtk.Tree_Model.Fuzzy_Data_Store;
