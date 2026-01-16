--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Tree_Model.                            Luebeck            --
--         Fuzzy_Lecture_Store                     Summer, 2006       --
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
with Fuzzy;                   use Fuzzy;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Intuitionistic;    use Fuzzy.Intuitionistic;
with Fuzzy.Lecture;           use Fuzzy.Lecture;
with Glib.Messages;           use Glib.Messages;
with Gtk.Enums;               use Gtk.Enums;

with Ada.Unchecked_Conversion;

with GLib.Values.Fuzzy.Intuitionistic;
use  GLib.Values.Fuzzy.Intuitionistic;

package body Gtk.Tree_Model.Fuzzy_Lecture_Store is
   use Fuzzy.Lecture.Handle;

   Fuzzy_Lecture_Store_Type : GType := GType_Invalid;

   type Example_No is new GUInt;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Tree_Model.Fuzzy_Lecture_Store." & Name;
   end Where;

   function Compose
            (  Lesson  : Lecture_Object'Class;
               Example : Example_No
            )  return Gtk_Tree_Iter;
   pragma Inline (Compose);

   function Get_Example_No (Iter : Gtk_Tree_Iter) return Example_No;
   pragma Inline (Get_Example_No);

   function Is_In
            (  Store   : access Gtk_Fuzzy_Lecture_Record;
               Example : GInt
            )  return Boolean;
   pragma Inline (Is_In);

   function Is_Positive (Example : Example_No) return Boolean;
   pragma Inline (Is_Positive);

   function Mean (Upper, Lower : Fuzzy.Set) return Float;
   pragma Inline (Mean);

   procedure Set_Example_No
             (  Iter    : in out Gtk_Tree_Iter;
                Example : Example_No
             );
   pragma Inline (Set_Example_No);

   function To_Address is
      new Ada.Unchecked_Conversion (Example_No, System.Address);

   function To_Example_No is
      new Ada.Unchecked_Conversion (System.Address, Example_No);

   procedure Added
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
   begin
      null;
   end Added;

   procedure Changed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             )  is
      Iter : Gtk_Tree_Iter;
   begin
      case Image is
         when Has_In | Has_Not =>
            Iter := Compose (Lesson, Example_No (Example) * 2 - 2);
         when Has_Out | Has_Not_Out =>
            Iter := Compose (Lesson, Example_No (Example) * 2 - 1);
      end case;
      declare
         Path : constant Gtk_Tree_Path :=
                         Get_Path (Observer.Store, Iter);
      begin
         Row_Changed (To_Interface (Observer.Store), Path, Iter);
         Path_Free (Path);
      exception
         when others =>
            Path_Free (Path);
      end;
   end Changed;

   function Check
            (  Store : access Gtk_Fuzzy_Lecture_Record;
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
         To_Ptr (Iter.User_Data2) = Ptr (Store.Lesson)
      );
   end Check;

   function Children
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if (  Parent = Null_Iter
         and then
            Is_Valid (Store.Lesson)
         and then
            Get_Examples_Number (Store.Lesson) > 0
         )
      then
         return Compose (Ptr (Store.Lesson).all, 0);
      else
         return Null_Iter;
      end if;
   end Children;

   function Compare (Left, Right : Float) return Gtk.Missed.Row_Order is
      pragma Inline (Compare);
      use Gtk.Missed;
   begin
      if Left < Right then
         return Before;
      elsif Left > Right then
         return After;
      else
         return Equal;
      end if;
   end Compare;

   function Compare
            (  Store  : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order is
      use Gtk.Missed;
      Lesson : constant Lecture_Handle := Get_Model (Store).Lesson;
      Count  : constant GInt  := N_Children (To_Interface (Store));
      Column : GInt;
      Order  : Gtk_Sort_Type;
      No_1   : Example_No := Get_Example_No (Left);
      No_2   : Example_No := Get_Example_No (Right);
      Pos_1  : constant Boolean := Is_Positive (No_1);
      Pos_2  : constant Boolean := Is_Positive (No_2);
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
               Feature : constant Feature_Handle :=
                         Get_Feature (Lesson, Positive (Column - 1));
               Result  : Row_Order;
               function Get_Mean
                        (  Example : Example_No;
                           Pos     : Image_Type;
                           Nec     : Image_Type
                        )  return Float is
               begin
                  if (  Is_Defined
                        (  Lesson,
                           Positive (Example),
                           Feature,
                           Pos
                        )
                     or else
                        Is_Defined
                        (  Lesson,
                           Positive (Example),
                           Feature,
                           Nec
                     )  )
                  then
                     return
                        Mean
                        (  Get
                           (  Lesson,
                              Positive (Example),
                              Feature,
                              Pos
                           ),
                           Get
                           (  Lesson,
                              Positive (Example),
                              Feature,
                              Nec
                        )  );
                  else
                     return Float'Last;
                  end if;
               end Get_Mean;
            begin
               if Pos_1 then
                  if Pos_2 then
                     Result :=
                        Compare
                        (  Get_Mean (No_1, Has_In, Has_Not),
                           Get_Mean (No_2, Has_In, Has_Not)
                        );
                  else
                     Result :=
                        Compare
                        (  Get_Mean (No_1, Has_In,  Has_Not),
                           Get_Mean (No_2, Has_Out, Has_Not_Out)
                        );
                  end if;
               else
                  if Pos_2 then
                     Result :=
                        Compare
                        (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                           Get_Mean (No_2, Has_In,  Has_Not)
                        );
                  else
                     Result :=
                        Compare
                        (  Get_Mean (No_1, Has_Out, Has_Not_Out),
                           Get_Mean (No_2, Has_Out, Has_Not_Out)
                        );
                  end if;
               end if;
               if Result /= Equal then
                  return Result;
               end if;
            end;
         end if;
      end loop;
      return Equal;
   end Compare;

   function Compose
            (  Lesson  : Lecture_Object'Class;
               Example : Example_No
            )  return Gtk_Tree_Iter is
   begin
      return
      (  Stamp      => 1,
         User_Data  => To_Address (Example),
         User_Data2 => Lesson'Address,
         User_Data3 => System.Null_Address
      );
   end Compose;

   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             )  is
   begin
      Changed (Observer, Lesson, Example, Feature, Has_In);
      Changed (Observer, Lesson, Example, Feature, Has_Out);
   end Deleted;

   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
   begin
      null;
   end Deleted;

   function Get_Feature_Column
            (  Store   : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Feature : Feature_Object'Class
            )  return Positive is
      Lesson : Lecture_Object'Class renames
                  Ptr (Get_Model (Store).Lesson).all;
   begin
      for Index in 1..Get_Features_Number (Lesson) loop
         if Ptr (Get_Feature (Lesson, Index)).ID = Feature.ID then
            return Index + 2;
         end if;
      end loop;
      raise Constraint_Error;
   end Get_Feature_Column;

   function Get_Feature_Column
            (  Store   : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Feature : Feature_Handle
            )  return Positive is
   begin
      return Get_Feature_Column (Store, Ptr (Feature).all);
   end Get_Feature_Column;

   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Index : Gint
            )  return GType is
   begin
      if Index < 0 then
         return GType_Invalid;
      elsif Index = 0 then
         return GType_String;
      elsif Index = 1 then
         return GType_Int;
      elsif Index - 1 <= GInt (Get_Features_Number (Store.Lesson)) then
         return GType_Classification;
      else
         return GType_Invalid;
      end if;
   end Get_Column_Type;

   function Get_Example_No (Iter : Gtk_Tree_Iter) return Example_No is
   begin
      return To_Example_No (Iter.User_Data);
   end Get_Example_No;

   function Get_Flags
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return Tree_Model_Flags is
   begin
      return Tree_Model_Iters_Persist + Tree_Model_List_Only;
   end Get_Flags;

   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Is_Valid (Store.Lesson) and then Get_Depth (Path) = 1 then
         declare
            Indices : GInt_Array renames Get_Indices (Path);
            No      : constant GInt := Indices (Indices'First);
         begin
            if Is_In (Store, No) then
               return Compose (Ptr (Store.Lesson).all, Example_No (No));
            end if;
         end;
      end if;
      return Null_Iter;
   end Get_Iter;

   function Get_Iter
            (  Store      : not null access
                            Gtk_Fuzzy_Lecture_Store_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter is
      Lesson : constant Lecture_Handle := Get_Lesson (Store);
      Iter   : Gtk_Tree_Iter;
   begin
      if Example > Get_Examples_Number (Lesson) then
         raise Constraint_Error;
      end if;
      if Complement then
         Iter :=
            Compose (Ptr (Lesson).all, Example_No (Example) * 2 - 1);
      else
         Iter :=
            Compose (Ptr (Lesson).all, Example_No (Example) * 2 - 2);
      end if;
      return Convert_Child_Iter_To_Iter (Store, Iter);
   end Get_Iter;

   function Get_Lesson
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return Lecture_Handle is
   begin
      return Store.Lesson;
   end Get_Lesson;

   function Get_Lesson
            (  Store : not null access Gtk_Fuzzy_Lecture_Store_Record
            )  return Lecture_Handle is
   begin
      return Get_Model (Store).Lesson;
   end Get_Lesson;

   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return GInt is
   begin
      if Is_Valid (Store.Lesson) then
         return GInt (Get_Features_Number (Store.Lesson)) + 2;
      else
         return 2;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_N_Columns")
         )  );
         return 2;
   end Get_N_Columns;

   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
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

   function Get_Type return Gtk_Type is
   begin
      if Fuzzy_Lecture_Store_Type = GType_Invalid then
         Fuzzy_Lecture_Store_Type := Register ("GtkFuzzyLectureStore");
      end if;
      return Fuzzy_Lecture_Store_Type;
   end Get_Type;

   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
                Iter   : Gtk_Tree_Iter;
                Column : GInt;
                Value  : out GValue
             )  is
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
      if Column - 1 > GInt (Get_Features_Number (Store.Lesson)) then
         Init (Value, GType_String);
         Set_String (Value, "");
         return;
      end if;
      declare
         Feature : constant Feature_Handle :=
                   Get_Feature (Store.Lesson, Positive (Column - 1));
         No      : constant Example_No := Get_Example_No (Iter);
         Example : constant Positive   := Positive (No / 2 + 1);
      begin
         Init (Value, GType_Classification);
         if Is_Positive (No) then
            if (  Is_Defined (Store.Lesson, Example, Feature, Has_In)
               or else
                  Is_Defined (Store.Lesson, Example, Feature, Has_Not)
               )
            then
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Value,
                  Classification'
                  (  Cardinality =>
                        Get_Cardinality (Feature),
                     Possibility =>
                        Get (Store.Lesson, Example, Feature, Has_In),
                     Necessity =>
                        not Get
                            (  Store.Lesson,
                               Example,
                               Feature,
                               Has_Not
               )  )         );
            else
               Set_Undefined (Value);
            end if;
         else
            if (  Is_Defined (Store.Lesson, Example, Feature, Has_Out)
               or else
                  Is_Defined
                  (  Store.Lesson,
                     Example,
                     Feature,
                     Has_Not_Out
               )  )
            then
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Value,
                  Classification'
                  (  Cardinality =>
                        Get_Cardinality (Feature),
                     Possibility =>
                        Get (Store.Lesson, Example, Feature, Has_Out),
                     Necessity =>
                        not Get
                            (  Store.Lesson,
                               Example,
                               Feature,
                               Has_Not_Out
               )  )         );
            else
               Set_Undefined (Value);
            end if;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Value")
         )  );
   end Get_Value;

   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Lecture;
                Lesson : Lecture_Handle
             )  is
   begin
      Store := new Gtk_Fuzzy_Lecture_Record;
      begin
         Initialize (Store, Lesson);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Lecture)")
            )  );
            Unref (Store);
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Lecture_Store;
                Lesson : Lecture_Handle
             )  is
   begin
      Store := new Gtk_Fuzzy_Lecture_Store_Record;
      begin
         Initialize (Store, Lesson);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Lecture_Store)")
            )  );
            Unref (Store);
            raise;
      end;
   end Gtk_New;

   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return False;
   end Has_Child;

   procedure Initialize
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record'Class;
                Lesson : Lecture_Handle
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize (Store, Get_Type);
      Set_Lesson (Store, Lesson);
   end Initialize;

   procedure Initialize
             (  Store  : not null access
                         Gtk_Fuzzy_Lecture_Store_Record'Class;
                Lesson : Lecture_Handle
             )  is
      Unsorted : Gtk_Fuzzy_Lecture;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Gtk_New (Unsorted, Lesson);
      Initialize (Store, Unsorted);
      Unref (Unsorted);
      for Column in 0..Get_N_Columns (To_Interface (Store)) - 1 loop
         Set_Sort_Func (Store, Column);
      end loop;
   end Initialize;

   function Is_In
            (  Store   : access Gtk_Fuzzy_Lecture_Record;
               Example : GInt
            )  return Boolean is
      pragma Inline (Is_In);
   begin
      return
      (  Is_Valid (Store.Lesson)
      and then
         (  Example
         in 0..GInt (Get_Examples_Number (Store.Lesson)) * 2 - 1
      )  );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Is_In")
         )  );
         return False;
   end Is_In;

   function Is_Positive (Example : Example_No) return Boolean is
   begin
      return 0 = (Example mod 2);
   end Is_Positive;

   function Mean (Upper, Lower : Fuzzy.Set) return Float is
      Result : Float := 0.0;
   begin
      for Index in Upper'Range loop
         Result :=
            (  Result
            +  (  Float (Index)
               *  (  Float (Upper (Index))
                  +  Float (not Lower (Index))
            )  )  );
      end loop;
      return Result;
   end Mean;

   procedure Next
             (  Store : not null access Gtk_Fuzzy_Lecture_Record;
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
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter then
         if Is_Valid (Store.Lesson) and then Is_In (Store, N) then
            return Compose (Ptr (Store.Lesson).all, Example_No (N));
         end if;
      end if;
      return Null_Iter;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Nth_Child")
         )  );
         return Null_Iter;
   end Nth_Child;

   function N_Children
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
   begin
      if Iter = Null_Iter and then Is_Valid (Store.Lesson) then
         return GInt (Get_Examples_Number (Store.Lesson)) * 2;
      else
         return 0;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("N_Children")
         )  );
         return 0;
   end N_Children;

   function Parent
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Parent;

   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Lecture_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if Check (Store, Iter) then
         declare
            No : constant Example_No := Get_Example_No (Iter);
         begin
            if No > 0 and then Is_In (Store, GInt (No - 1)) then
               Set_Example_No (Iter, No - 1);
               return;
            end if;
         end;
      end if;
      Iter := Null_Iter;
   end Previous;

   procedure Renamed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             )  is
   begin
      null;
   end Renamed;

   procedure Set_Example_No
             (  Iter    : in out Gtk_Tree_Iter;
                Example : Example_No
             )  is
   begin
      Iter.User_Data := To_Address (Example);
   end Set_Example_No;

   procedure Set_Lesson
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle
             )  is
   begin
      Store.Lesson := Lesson;
      if Is_Valid (Lesson) then
         Fuzzy.Lecture.Observer_Handle.Set
         (  Store.Observer,
            new Fuzzy_Lecture_Observer
                (  Store.all'Unchecked_Access,
                   Ptr (Lesson)
            )   );
      else
         Invalidate (Store.Observer);
      end if;
   end Set_Lesson;

end Gtk.Tree_Model.Fuzzy_Lecture_Store;
