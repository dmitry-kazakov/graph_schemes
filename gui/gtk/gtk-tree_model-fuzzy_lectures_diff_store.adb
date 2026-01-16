--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Tree_Model.                            Luebeck            --
--         Fuzzy_Lectures_Diff_Store               Summer, 2006       --
--  Implementation                                                    --
--                                Last revision :  07:55 21 Jul 2016  --
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
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Intuitionistic;    use Fuzzy.Intuitionistic;
with Fuzzy.Lecture;           use Fuzzy.Lecture;
with Fuzzy.Logic;             use Fuzzy.Logic;
with Glib.Messages;           use Glib.Messages;
with Gtk.Enums;               use Gtk.Enums;

with Ada.Unchecked_Conversion;
with Fuzzy.Feature.Handle.Container;

with GLib.Values.Fuzzy.Logic;
use  GLib.Values.Fuzzy.Logic;

with GLib.Values.Fuzzy.Intuitionistic;
use  GLib.Values.Fuzzy.Intuitionistic;

package body Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store is
   use Fuzzy.Classifier;
   use Fuzzy.Lecture.Handle;
   use type Fuzzy.Set;

   Fuzzy_Lectures_Diff_Store_Type : GType := GType_Invalid;

   type Example_No is new GUInt;
   function To_Address is
      new Ada.Unchecked_Conversion (Example_No, System.Address);

   function To_Address is
      new Ada.Unchecked_Conversion (Lecture_Object_Ptr, System.Address);

   function To_Example_No is
      new Ada.Unchecked_Conversion (System.Address, Example_No);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store." & Name;
   end Where;

   function Compose
            (  Reference : Lecture_Object_Ptr;
               Result    : Lecture_Object_Ptr;
               Example   : Example_No
            )  return Gtk_Tree_Iter is
      pragma Inline (Compose);
   begin
      return
      (  Stamp      => 1,
         User_Data  => To_Address (Example),
         User_Data2 => To_Address (Reference),
         User_Data3 => To_Address (Result)
      );
   end Compose;

   function Get_Example_No (Iter : Gtk_Tree_Iter) return Example_No is
      pragma Inline (Get_Example_No);
   begin
      return To_Example_No (Iter.User_Data);
   end Get_Example_No;

   procedure Set_Example_No
             (  Iter    : in out Gtk_Tree_Iter;
                Example : Example_No
             )  is
      pragma Inline (Set_Example_No);
   begin
      Iter.User_Data := To_Address (Example);
   end Set_Example_No;

   function Is_Positive (Example : Example_No) return Boolean is
   begin
      return 0 = (Example mod 2);
   end Is_Positive;

   function Is_In
            (  Store   : access Gtk_Fuzzy_Lectures_Diff_Record'Class;
               Example : GInt
            )  return Boolean is
      pragma Inline (Is_In);
   begin
      return
      (  Is_Valid (Store.Reference)
      and then
         (  Example
         in 0..GInt (Get_Examples_Number (Store.Reference)) * 2 - 1
      )  );
   end Is_In;

   function Check
            (  Store : access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
      pragma Inline (Check);
      function To_Ptr is
         new Ada.Unchecked_Conversion
             (  System.Address,
                Lecture_Object_Ptr
             );
   begin
      return
      (  Iter /= Null_Iter
      and then
         To_Ptr (Iter.User_Data2) = Ptr (Store.Reference)
      and then
         To_Ptr (Iter.User_Data3) = Ptr (Store.Result)
      );
   end Check;

   function Children
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if (  Parent = Null_Iter
         and then
            Is_Valid (Store.Reference)
         and then
            Get_Examples_Number (Store.Reference) > 0
         )
      then
         return Compose (Ptr (Store.Reference), Ptr (Store.Result), 0);
      else
         return Null_Iter;
      end if;
   end Children;

   function Diff
            (  Pos_1      : Fuzzy.Set;
               Pos_2      : Fuzzy.Set;
               Not_Pos_1  : Fuzzy.Set;
               Not_Pos_2  : Fuzzy.Set;
               Difference : Divergence_Function
            )  return Divergence_Range is
      use type Divergence_Range;
      Result : Divergence_Range := (0.0, 0.0);
   begin
      for Index in Pos_1'Range loop
         Result :=
            (  Result
            +  Difference
               (  Fuzzy_Boolean'
                  (  Possibility => Pos_1 (Index),
                     Necessity   => not Not_Pos_1 (Index)
                  ),
                  Fuzzy_Boolean'
                  (  Possibility => Pos_2 (Index),
                     Necessity   => not Not_Pos_2 (Index)
            )  )  );
      end loop;
      return Result;
   end Diff;

   function Mean (Upper, Lower : Fuzzy.Set) return Divergence is
      Result : Divergence := 0.0;
   begin
      for Index in Upper'Range loop
         Result :=
            (  Result
            +  (  Divergence (Index)
               *  (  Divergence (Upper (Index))
                  +  Divergence (not Lower (Index))
            )  )  );
      end loop;
      return Result;
   end Mean;

   function Compare (Left, Right : Divergence)
      return Gtk.Missed.Row_Order is
      use Gtk.Missed;
      pragma Inline (Compare);
   begin
      if Left < Right then
         return Before;
      elsif Left > Right then
         return After;
      else
         return Equal;
      end if;
   end Compare;

   function Compare (Left, Right : Divergence_Range)
      return Gtk.Missed.Row_Order is
      use Gtk.Missed;
      pragma Inline (Compare);
      Left_Mean  : constant Divergence := Left.From  + Left.To;
      Right_Mean : constant Divergence := Right.From + Right.To;
   begin
      if Left_Mean < Right_Mean then
         return Before;
      elsif Left_Mean > Right_Mean then
         return After;
      elsif Left.From < Right.From then
         return Before;
      elsif Left.From > Right.From then
         return After;
      else
         return Equal;
      end if;
   end Compare;

   function Compare
            (  Store  : not null access
                        Gtk_Fuzzy_Lectures_Diff_Store_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order is
      use Gtk.Missed;
      Model  : constant Gtk_Fuzzy_Lectures_Diff := Get_Model (Store);
      Count  : constant GInt := N_Children (To_Interface (Store));
      Column : GInt;
      Order  : Gtk_Sort_Type;
      No_1   : Example_No := Get_Example_No (Left);
      No_2   : Example_No := Get_Example_No (Right);
      Pos_1  : constant Boolean := Is_Positive (No_1);
      Pos_2  : constant Boolean := Is_Positive (No_2);

      function Compare
               (  Lesson  : Lecture_Object_Ptr;
                  Feature : Feature_Object'Class
               )  return Row_Order is
         function Get_Mean
                  (  Example : Example_No;
                     Pos     : Image_Type;
                     Nec     : Image_Type
                  )  return Divergence is
            Index : constant Positive := Positive (Example);
         begin
            if (  Is_Defined (Lesson.all, Index, Feature, Pos)
               or else
                  Is_Defined (Lesson.all, Index, Feature, Nec)
               )
            then
               return
                  Mean
                  (  Get (Lesson, Index, Feature, Pos),
                     Get (Lesson, Index, Feature, Nec)
                  );
            else
               return Divergence'Last;
            end if;
         end Get_Mean;
      begin
         if Pos_1 then
            if Pos_2 then
               return
                  Compare
                  (  Get_Mean (No_1, Has_In, Has_Not),
                     Get_Mean (No_2, Has_In, Has_Not)
                  );
            else
               return
                  Compare
                  (  Get_Mean (No_1, Has_In,  Has_Not),
                     Get_Mean (No_2, Has_Out, Has_Not_Out)
                  );
            end if;
         else
            if Pos_2 then
               return
                  Compare
                  (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                     Get_Mean (No_2, Has_In,  Has_Not)
                  );
            else
               return
                  Compare
                  (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                     Get_Mean (No_2, Has_Out, Has_Not_Out)
                  );
            end if;
         end if;
      end Compare;

      function Compare (Feature : Feature_Object'Class)
         return Row_Order is
         function Get_Mean
                  (  Example : Example_No;
                     Pos     : Image_Type;
                     Nec     : Image_Type
                  )  return Divergence_Range is
            Index : constant Positive := Positive (Example);
         begin
            return
               Diff
               (  Get (Ptr (Model.Reference), Index, Feature, Pos),
                  Get (Ptr (Model.Result),    Index, Feature, Pos),
                  Get (Ptr (Model.Reference), Index, Feature, Nec),
                  Get (Ptr (Model.Result),    Index, Feature, Nec),
                  Model.Difference
               );
         end Get_Mean;
      begin
         if Pos_1 then
            if Pos_2 then
               return
                  Compare
                  (  Get_Mean (No_1, Has_In, Has_Not),
                     Get_Mean (No_2, Has_In, Has_Not)
                  );
            else
               return
                  Compare
                  (  Get_Mean (No_1, Has_In,  Has_Not),
                     Get_Mean (No_2, Has_Out, Has_Not_Out)
                  );
            end if;
         else
            if Pos_2 then
               return
                  Compare
                  (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                     Get_Mean (No_2, Has_In,  Has_Not)
                  );
            else
               return
                  Compare
                  (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                     Get_Mean (No_2, Has_Out, Has_Not_Out)
                  );
            end if;
         end if;
      end Compare;
   begin
      Get_Sort_Column_ID (Store, Column, Order);
      Set_First
      (  Store.Order,
         Column,
         Get_N_Columns (To_Interface (Store))
      );
      No_1 := No_1 / 2 + 1;
      No_2 := No_2 / 2 + 1;
      for Position in 1
                   .. Positive (Get_N_Columns (To_Interface (Store)))
      loop
         Column := Get (Store.Order, Position);
         exit when Column < 0;
         if Column = 0 then
            if Pos_1 then
               if not Pos_2 then
                  return Before;
               end if;
            else
               if Pos_2 then
                  return After;
               end if;
            end if;
         elsif Column = 1 then
            if No_1 < No_2 then
               return Before;
            elsif No_1 > No_2 then
               return After;
            end if;
         elsif (  No_1 /= No_2
               and then
                  No_1 <= Example_No (Count)
               and then
                  No_2 <= Example_No (Count)
               )
         then
            declare
               Result : Row_Order;
            begin
               if Column - 1 <= Model.Shared_Count then
                  case Column - 2 mod 3 is
                     when 2 =>
                        Result :=
                           Compare
                           (  Ptr (Model.Reference),
                              Get
                              (  Model.Shared,
                                 Positive ((Column - 2) / 3 + 1)
                              ) .all
                           );
                     when 0 =>
                        Result :=
                           Compare
                           (  Ptr (Model.Result),
                              Get
                              (  Model.Shared,
                                 Positive ((Column - 2) / 3 + 1)
                              ) .all
                           );
                     when others =>
                        Result :=
                           Compare
                           (  Get
                              (  Model.Shared,
                                 Positive ((Column - 2) / 3 + 1)
                              ) .all
                           );
                  end case;
               else
                  Result :=
                     Compare
                     (  Ptr (Model.Reference),
                        Get
                        (  Model.Unique,
                           Positive (Column - Model.Shared_Count - 1)
                        ) .all
                     );
               end if;
               if Result /= Equal then
                  return Result;
               end if;
            end;
         end if;
      end loop;
      return Equal;
   end Compare;

   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Index : Gint
            )  return GType is
   begin
      if Index < 0 then
         return GType_Invalid;
      elsif Index = 0 then
         return GType_String;
      elsif Index = 1 then
         return GType_Int;
      elsif Index - 1 <= Store.Shared_Count then
         case (Index - 2) mod 3 is
            when 0 | 2 =>
               return GType_Classification;
            when others =>
               return GType_Fuzzy_Boolean;
         end case;
      elsif Index < Get_N_Columns (Store) then
         return GType_Classification;
      else
         return GType_Invalid;
      end if;
   end Get_Column_Type;

   procedure Get_Feature_Column
             (  Store   : not null access
                          Gtk_Fuzzy_Lectures_Diff_Store_Record;
                Feature : Feature_Object'Class;
                Column  : out Positive;
                Shared  : out Boolean
             )  is
      This : Gtk_Fuzzy_Lectures_Diff_Record'Class renames
                Get_Model (Store).all;
   begin
      for Index in This.Shared.First..This.Shared.Last loop
         if Get (This.Shared, Index).ID = Feature.ID then
            Column := (Index - This.Shared.First + 1) * 3;
            Shared := True;
            return;
         end if;
      end loop;
      for Index in This.Unique.First..This.Unique.Last loop
         if Get (This.Unique, Index).ID = Feature.ID then
            Column :=
               (  (This.Shared.Last - This.Shared.First + 2) * 3
               +  Index
               -  This.Unique.First
               );
               Shared := False;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Get_Feature_Column;

   procedure Get_Feature_Column
             (  Store   : not null access
                          Gtk_Fuzzy_Lectures_Diff_Store_Record;
                Feature : Feature_Handle;
                Column  : out Positive;
                Shared  : out Boolean
             )  is
   begin
      Get_Feature_Column (Store, Ptr (Feature).all, Column, Shared);
   end Get_Feature_Column;

   function Get_Flags
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record
            )  return Tree_Model_Flags is
   begin
      return Tree_Model_Iters_Persist + Tree_Model_List_Only;
   end Get_Flags;

   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Is_Valid (Store.Reference) and then Get_Depth (Path) = 1 then
         declare
            Indices : GInt_Array renames Get_Indices (Path);
            No      : constant GInt := Indices (Indices'First);
         begin
            if Is_In (Store, No) then
               return
                  Compose
                  (  Ptr (Store.Reference),
                     Ptr (Store.Result),
                     Example_No (No)
                  );
            end if;
         end;
      end if;
      return Null_Iter;
   end Get_Iter;

   function Get_Iter
            (  Store      : not null access
                            Gtk_Fuzzy_Lectures_Diff_Store_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter is
      Model : constant Gtk_Fuzzy_Lectures_Diff := Get_Model (Store);
      Iter  : Gtk_Tree_Iter;
   begin
      if not Is_In (Model, GInt (Example)) then
         raise Constraint_Error;
      end if;
      if Complement then
         Iter :=
            Compose
            (  Ptr (Model.Reference),
               Ptr (Model.Result),
               Example_No (Example) * 2 - 1
            );
      else
         Iter :=
            Compose
            (  Ptr (Model.Reference),
               Ptr (Model.Result),
               Example_No (Example) * 2 - 2
            );
      end if;
      return Convert_Child_Iter_To_Iter (Store, Iter);
   end Get_Iter;

   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record
            )  return GInt is
   begin
      return
      (  Store.Shared_Count
      +  GInt (Store.Unique_Last - Store.Unique_First + 1)
      +  2
      );
   end Get_N_Columns;

   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
   begin
      if Check (Store, Iter) then
         declare
            Path : Gtk_Tree_Path;
         begin
            Gtk_New (Path);
            Append_Index (Path, GInt (Get_Example_No (Iter)));
            return Path;
         end;
      else
         return Null_Gtk_Tree_Path;
      end if;
   end Get_Path;

   function Get_Reference_Lesson
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Lecture_Handle is
   begin
      return Get_Model (Store).Reference;
   end Get_Reference_Lesson;

   function Get_Result_Lesson
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Lecture_Handle is
   begin
      return Get_Model (Store).Result;
   end Get_Result_Lesson;

   function Get_Shared_Features
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Bounded_Array is
   begin
      return Get_Model (Store).Shared;
   end Get_Shared_Features;

   function Get_Type return Gtk_Type is
   begin
      if Fuzzy_Lectures_Diff_Store_Type = GType_Invalid then
         Fuzzy_Lectures_Diff_Store_Type :=
            Register ("FuzzyLecturesDiffStore");
      end if;
      return Fuzzy_Lectures_Diff_Store_Type;
   end Get_Type;

   function Get_Unique_Features
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Bounded_Array is
   begin
      return Get_Model (Store).Unique;
   end Get_Unique_Features;

   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
                Iter   : Gtk_Tree_Iter;
                Column : GInt;
                Value  : out GValue
             )  is
      procedure Get_Value
                (  Lesson  : Lecture_Object_Ptr;
                   Feature : Feature_Object'Class
                )  is
         No      : constant Example_No := Get_Example_No (Iter);
         Example : constant Positive   := Positive (No / 2 + 1);
      begin
         Init (Value, GType_Classification);
         if Is_Positive (No) then
            if (  Is_Defined (Lesson.all, Example, Feature, Has_In)
               or else
                  Is_Defined (Lesson.all, Example, Feature, Has_Not)
               )
            then
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Value,
                  Classification'
                  (  Cardinality =>
                        Feature.Cardinality,
                     Possibility =>
                        Get (Lesson, Example, Feature, Has_In),
                     Necessity =>
                         not Get (Lesson, Example, Feature, Has_Not)
               )  );
            else
               GLib.Values.Fuzzy.Intuitionistic.Set_Undefined (Value);
            end if;
         else
            if (  Is_Defined (Lesson.all, Example, Feature, Has_Out)
               or else
                  Is_Defined (Lesson.all, Example, Feature, Has_Not_Out)
               )
            then
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Value,
                  Classification'
                  (  Cardinality =>
                        Feature.Cardinality,
                     Possibility =>
                        Get (Lesson, Example, Feature, Has_Out),
                     Necessity =>
                        not Get (Lesson, Example, Feature, Has_Not_Out)
               )  );
            else
               GLib.Values.Fuzzy.Intuitionistic.Set_Undefined (Value);
            end if;
         end if;
      end Get_Value;

      procedure Get_Value (Feature : Feature_Object'Class) is
         No      : constant Example_No := Get_Example_No (Iter);
         Example : constant Positive   := Positive (No / 2 + 1);
         Result  : Divergence_Range;
         procedure Get_Value (Pos, Nec : Image_Type) is
            use type Divergence_Range;
         begin
            Result :=
               Diff
               (  Get (Ptr (Store.Reference), Example, Feature, Pos),
                  Get (Ptr (Store.Result),    Example, Feature, Pos),
                  Get (Ptr (Store.Reference), Example, Feature, Nec),
                  Get (Ptr (Store.Result),    Example, Feature, Nec),
                  Store.Difference
               );
            Result := Result / Divergence (Feature.Cardinality);
         end Get_Value;
      begin
         if Is_Positive (No) then
            Get_Value (Has_In, Has_Not);
         else
            Get_Value (Has_Out, Has_Not_Out);
         end if;
         Init (Value, GType_Fuzzy_Boolean);
         GLib.Values.Fuzzy.Logic.Set (Value, To_Fuzzy_Boolean (Result));
      end Get_Value;

   begin
      if Column < 0 or else not Check (Store, Iter) then
         Init (Value, GType_String);
         Set_String (Value, "");
         return;
      end if;
      if Column = 0 then
         Init (Value, GType_String);
         if Is_Positive (Get_Example_No (Iter)) then
            Set_String (Value, Fuzzy.Gtk_Icon_Factory.Positive_Icon);
         else
            Set_String (Value, Fuzzy.Gtk_Icon_Factory.Negative_Icon);
         end if;
         return;
      end if;
      if Column = 1 then
         Init (Value, GType_Int);
         Set_Int (Value, GInt (Get_Example_No (Iter)) / 2 + 1);
         return;
      end if;
      if Column >= Get_N_Columns (Store) then
         Init (Value, GType_String);
         Set_String (Value, "");
         return;
      end if;
      if Column - 1 <= Store.Shared_Count then
         case (Column - 2) mod 3 is
            when 0 =>
               Get_Value
               (  Ptr (Store.Result),
                  Get
                  (  Store.Shared,
                     Positive ((Column - 2) / 3 + 1)
                  ) .all
               );
            when 2 =>
               Get_Value
               (  Ptr (Store.Reference),
                  Get
                  (  Store.Shared,
                     Positive ((Column - 2) / 3 + 1)
                  ) .all
               );
            when others =>
               Get_Value
               (  Get
                  (  Store.Shared,
                     Positive ((Column - 2) / 3 + 1)
                  ) .all
               );
         end case;
      else
         Get_Value
         (  Ptr (Store.Reference),
            Get
            (  Store.Unique,
               Positive (Column - Store.Shared_Count - 1)
            ) .all
         );
      end if;
   end Get_Value;

   procedure Gtk_New
             (  Store      : out Gtk_Fuzzy_Lectures_Diff;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function := Diff'Access
             )  is
      use Fuzzy.Feature.Handle.Container;
      function Get_Set (Lesson : Lecture_Handle) return Container.Set is
         Result : Container.Set;
      begin
         for Index in 1..Get_Features_Number (Lesson) loop
            Add (Result, Get_Feature (Lesson, Index));
         end loop;
         return Result;
      end Get_Set;

      Shared : Container.Set;
      Unique : Container.Set;
   begin
      if Is_Valid (Reference) then
         Unique := Get_Set (Reference);
         if Is_Valid (Result) then
            Shared := Get_Set (Result) and Unique;
            Unique := Unique xor Shared;
         end if;
      end if;
      Store :=
         new Gtk_Fuzzy_Lectures_Diff_Record
             (  Shared_First => 1,
                Shared_Last  => Get_Size (Shared),
                Unique_First => 1,
                Unique_Last  => Get_Size (Unique)
             );
      Store.Reference := Reference;
      Store.Result    := Result;
      for Index in 1..Get_Size (Shared) loop
         Put (Store.Shared, Index, Get (Shared, Index));
      end loop;
      for Index in 1..Get_Size (Unique) loop
         Put (Store.Unique, Index, Get (Unique, Index));
      end loop;
      begin
         Initialize (Store, Difference);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Lectures_Diff)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Store      : out Gtk_Fuzzy_Lectures_Diff_Store;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function := Diff'Access
             )  is
   begin
      Store := new Gtk_Fuzzy_Lectures_Diff_Store_Record;
      begin
         Initialize
         (  Store      => Store,
            Reference  => Reference,
            Result     => Result,
            Difference => Difference
         );
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Lectures_Diff_Store)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return False;
   end Has_Child;

   procedure Initialize
             (  Store      : not null access
                             Gtk_Fuzzy_Lectures_Diff_Record'Class;
                Difference : Divergence_Function
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize (Store, Get_Type);
      Store.Shared_Count :=
         GInt (Store.Shared_Last - Store.Shared_First + 1) * 3;
   end Initialize;

   procedure Initialize
             (  Store : not null access
                        Gtk_Fuzzy_Lectures_Diff_Store_Record'Class;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function
             )  is
      Unsorted : Gtk_Fuzzy_Lectures_Diff;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Gtk_New
      (  Store      => Unsorted,
         Reference  => Reference,
         Result     => Result,
         Difference => Difference
      );
      Initialize (Store, Unsorted);
      Unref (Unsorted);
      for Column in 0..Get_N_Columns (To_Interface (Store)) - 1 loop
         Set_Sort_Func (Store, Column);
      end loop;
   end Initialize;

   procedure Next
             (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if Check (Store, Iter) then
         declare
            No : constant Example_No := Get_Example_No (Iter) + 1;
         begin
            if Is_In (Store, GInt (No)) then
               Set_Example_No (Iter, No);
               return;
            end if;
         end;
      end if;
      Iter := Null_Iter;
   end Next;

   function Nth_Child
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter then
         if Is_Valid (Store.Reference) and then Is_In (Store, N) then
            return
               Compose
               (  Ptr (Store.Reference),
                  Ptr (Store.Result),
                  Example_No (N)
               );
         end if;
      end if;
      return Null_Iter;
   end Nth_Child;

   function N_Children
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
   begin
      if Iter = Null_Iter and then Is_Valid (Store.Reference) then
         return GInt (Get_Examples_Number (Store.Reference)) * 2;
      else
         return 0;
      end if;
   end N_Children;

   function Parent
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Parent;

   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if Check (Store, Iter) then
         declare
            No : constant Example_No := Get_Example_No (Iter);
         begin
            if No > 1 and then Is_In (Store, GInt (No - 1)) then
               Set_Example_No (Iter, No - 1);
               return;
            end if;
         end;
      end if;
      Iter := Null_Iter;
   end Previous;

end Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store;
