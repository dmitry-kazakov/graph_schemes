--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Sets                      Luebeck            --
--  Implementation                                 Summer, 2003       --
--                                                                    --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

package body Fuzzy.Linguistics.Sets is

   function To_Confidence (Value : Boolean) return Confidence
      renames Confidence_Factors.To_Confidence;

   procedure Free is
      new Ada.Unchecked_Deallocation (Set_Body'Class, Set_Body_Ptr);

   function Accumulate
            (  Set          : Linguistic_Set;
               Distribution : Fuzzy.Set
            )  return Variable is
      Result : Variable;
   begin
      if Ptr (Set) = null then
         if Distribution'Length /= 0 then
            raise Constraint_Error;
         end if;
      else
         declare
            Object : Set_Body'Class renames Ptr (Set).all;
            Size   : constant Natural :=
                        Get_Cardinality (Object.Domain);
         begin
            if Distribution'Length /= Size then
               raise Constraint_Error;
            end if;
            for Index in Distribution'Range loop
               Result :=
                  (  Result
                  or (  Get
                        (  Object.List,
                           Index - Distribution'First + 1
                        )
                     and
                        Distribution (Index)
                  )  );
            end loop;
         end;
      end if;
      return Result;
   end Accumulate;

   procedure Add
             (  Set   : in out Linguistic_Set;
                Name  : String;
                Value : Variable
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
      Index    : Positive;
   begin
      if Body_Ptr = null then
         Body_Ptr := new Set_Body;
         Cloned := True;
      elsif Body_Ptr.Use_Count > 1 then
         Body_Ptr := Clone (Body_Ptr.all);
         Cloned := True;
      end if;
      Index := Get_Cardinality (Body_Ptr.Domain) + 1;
      Add (Body_Ptr.Domain, Name, Index);
      Put (Body_Ptr.List, Index, Value);
      if Cloned then
         Handles.Set (Handles.Handle (Set), Body_Ptr);
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Add;

   type User_Data is record
      Undefined   : Boolean := True;
      Left        : Boolean;
      Possibility : Confidence;
      Necessity   : Confidence;
      Result      : Variable;
   end record;

   procedure Do_Empty (Data : in out User_Data) is
   begin
      null;
   end Do_Empty;

   procedure Add_Point
             (  Data : in out User_Data;
                X    : Number'Base;
                Y    : Boolean
             )  is
   begin
      if Data.Undefined then
         Data.Undefined := False;
         Data.Left := Y;
         Append (Data.Result, X, To_Confidence (Y));
      elsif Y /= Data.Left then
         Append (Data.Result, X, To_Confidence (Data.Left));
         Append (Data.Result, X, To_Confidence (Y));
         Data.Left := Y;
      end if;
   end Add_Point;

   procedure Do_Point
             (  Data : in out User_Data;
                X    : Number'Base;
                Y    : Confidence
             )  is
   begin
      Add_Point
      (  Data,
         X,
         Y >= Data.Necessity and then Y <= Data.Possibility
      );
   end Do_Point;

   procedure Do_Interval
             (  Data : in out User_Data;
                X1   : Number'Base;
                Y1   : Confidence;
                X2   : Number'Base;
                Y2   : Confidence
             )  is
      X_P : constant Number'Base :=
          Root (X1, Y1, Data.Possibility, X2, Y2, Data.Possibility);
      X_N : constant Number'Base :=
          Root (X1, Y1, Data.Necessity, X2, Y2, Data.Necessity);
   begin
      if Y1 >= Data.Necessity and then Y1 <= Data.Possibility then
         if X_P > X1 and then X_P < X2 then
            if X_N > X1 and then X_N < X2 then
               declare
                  X : constant Number'Base := Number'Min (X_P, X_N);
               begin
                  Add_Point (Data, X, True);
                  Add_Point (Data, X, False);
               end;
            else
               Add_Point (Data, X_P, True);
               Add_Point (Data, X_P, False);
            end if;
         else
            if X_N > X1 and then X_N < X2 then
               Add_Point (Data, X_N, True);
               Add_Point (Data, X_N, False);
            elsif Y2 < Data.Necessity or else Y2 > Data.Possibility then
               Add_Point (Data, X1, True);
               Add_Point (Data, X1, False);
            end if;
         end if;
      elsif Y2 >= Data.Necessity and then Y2 <= Data.Possibility then
         if X_P > X1 and then X_P < X2 then
            if X_N > X1 and then X_N < X2 then
               Add_Point (Data, Number'Max (X_P, X_N), True);
            else
               Add_Point (Data, X_P, True);
            end if;
         else
            if X_N > X1 and then X_N < X2 then
               Add_Point (Data, X_N, True);
            end if;
         end if;
      else
         if X_P > X1 and then X_P < X2 then
            if X_N > X1 and then X_N < X2 then
               if X_P <= X_N then
                  Add_Point (Data, X_P, True);
                  Add_Point (Data, X_N, False);
               else
                  Add_Point (Data, X_N, True);
                  Add_Point (Data, X_P, False);
               end if;
            end if;
         end if;
      end if;
   end Do_Interval;

   procedure Do_Slice is
      new Unary_Operation
          (  User_Data   => User_Data,
             Do_Empty    => Do_Empty,
             Do_Point    => Do_Point,
             Do_Interval => Do_Interval
          );

   function Defuzzify
            (  Set   : Linguistic_Set;
               Value : Classification
            )  return Defuzzification_Result is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Variable;
      Accum  : User_Data;
   begin
      if Value.Cardinality /= Get_Cardinality (Object.Domain) then
         raise Constraint_Error;
      end if;
      for Index in 1..Value.Cardinality loop
         Accum.Possibility := Value.Possibility (Index);
         Accum.Necessity   := Value.Necessity   (Index);
         Do_Slice (Accum, Get (List, Index));
         if Index = 1 then
            Result := Accum.Result;
         else
            Result := Result and Accum.Result;
         end if;
         Erase (Accum.Result);
         Accum.Undefined := True;
      end loop;
      declare
         Cardinality : constant Natural :=
            Get_Points_Number (Result);
      begin
         case Cardinality is
            when 0 =>
               return (Class => Empty);
            when 1 =>
               if Is_Singleton (Result) then
                  return
                  (  Class     => Singleton,
                     Singleton => Get (Result.Data, 1).Value
                  );
               else
                  return (Class => Subset, Subset => Result);
               end if;
            when 2 =>
               if Is_Interval (Result) then
                  return
                  (  Class   => Segment,
                     Segment => (  Get (Result.Data, 1).Value,
                                   Get (Result.Data, 2).Value
                  )             );
               else
                  return (Class => Subset, Subset => Result);
               end if;
            when others =>
               return (Class => Subset, Subset => Result);
         end case;
      end;
   end Defuzzify;

   function Empty return Linguistic_Set is
      Result : Linguistic_Set;
   begin
      return Result;
   end Empty;

   function Equal
            (  Left, Right : Linguistic_Set;
               Eps         : Number'Base := 0.0
            )  return Boolean is
      Size : constant Natural := Get_Cardinality (Left);
   begin
      if Get_Cardinality (Right) /= Size then
         return False;
      elsif Size /= 0 and then Ptr (Left) /= Ptr (Right) then
         declare
            Left_Names  : Domain_Description renames Ptr (Left).Domain;
            Right_Names : Domain_Description renames Ptr (Right).Domain;
            Left_List   : Array_Of_Variables renames
                             Ptr (Left).List.Vector.all;
            Right_List  : Array_Of_Variables renames
                             Ptr (Left).List.Vector.all;
            function Same (Index : Positive) return Boolean is
               Left  : String renames Get_Name (Left_Names,  Index);
               Right : String renames Get_Name (Right_Names, Index);
            begin
               if Left'Length /= Right'Length then
                  return False;
               else
                  for Index in Left'Range loop
                     if (  To_Lower (Left (Index))
                        /= To_Lower (Right (Index))
                        )
                     then
                        return False;
                     end if;
                  end loop;
               end if;
               return True;
            end Same;
         begin
            for Index in 1..Size loop
               if not (  Same (Index)
                      and then
                         Equal
                         (  Left_List (Index),
                            Right_List (Index),
                            Eps
                      )  )
               then
                  return False;
               end if;
            end loop;
         end;
      end if;
      return True;
   end Equal;

   procedure Erase (Set : in out Linguistic_Set) is
   begin
      if Ptr (Set) /= null then
         declare
            Object : Set_Body'Class renames Ptr (Set).all;
         begin
            if Object.Use_Count = 1 then
               for Index in 1..Get_Cardinality (Object.Domain) loop
                  Erase (Object.List.Vector (Index));
               end loop;
               Erase (Ptr (Set).Domain);
            else
               Invalidate (Set);
            end if;
         end;
      end if;
   end Erase;

   function Get
            (  Set   : Linguistic_Set;
               Index : Positive
            )  return Variable is
   begin
      return Get (Ptr (Set).List, Index);
   end Get;

   function Get
            (  Set  : Linguistic_Set;
               Name : String
            )  return Positive is
   begin
      if Ptr (Set) = null then
         raise End_Error;
      else
         declare
            Object : Set_Body'Class renames Ptr (Set).all;
         begin
            return Get_Index (Object.Domain, Name);
         end;
      end if;
   end Get;

   function Get (Set : Linguistic_Set; Name : String)
      return Variable is
   begin
      if Ptr (Set) = null then
         raise End_Error;
      else
         declare
            Object : Set_Body'Class renames Ptr (Set).all;
         begin
            return Get (Object.List, Get_Index (Object.Domain, Name));
         end;
      end if;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Index   : out Positive
             )  is
   begin
      if Ptr (Set) = null then
         raise End_Error;
      else
         Get (Source, Pointer, Ptr (Set).Domain, Index);
      end if;
   end Get;

   function Get_Cardinality (Set : Linguistic_Set) return Natural is
   begin
      if Ptr (Set) = null then
         return 0;
      else
         return Get_Cardinality (Ptr (Set).Domain);
      end if;
   end Get_Cardinality;

   function Get_Domain (Set : Linguistic_Set)
      return Domain_Description_Ptr is
   begin
      if Get_Cardinality (Set) = 0 then
         raise Constraint_Error;
      else
         return Ptr (Set).Domain'Unchecked_Access;
      end if;
   end Get_Domain;

   function Get_Name (Set : Linguistic_Set; Index : Positive)
      return String is
   begin
      return Get_Name (Ptr (Set).Domain, Index);
   end Get_Name;

   function Get_Use_Count (Set : Linguistic_Set) return Natural is
   begin
      if Ptr (Set) = null then
         return 0;
      else
         return Ptr (Set).Use_Count;
      end if;
   end Get_Use_Count;

   function Classify
            (  Set   : Linguistic_Set;
               Value : Number
            )  return Classification is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Classification (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Value, Get (List, Index));
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end Classify;

   function Classify
            (  Set   : Linguistic_Set;
               Value : Interval
            )  return Classification is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Classification (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Value, Get (List, Index));
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end Classify;

   function Classify
            (  Set   : Linguistic_Set;
               Value : Fuzzy_Float
            )  return Classification is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Classification (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Value, Get (List, Index));
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end Classify;

   function Classify
            (  Set   : Linguistic_Set;
               Value : Variable
            )  return Classification is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Classification (Get_Cardinality (Object.Domain));
   begin
      for Index in Result.Possibility'Range loop
         declare
            Var : Variable renames Get (List, Index);
         begin
            Result.Possibility (Index) := Possibility (Var, Value);
            Result.Necessity   (Index) := Necessity   (Var, Value);
         end;
      end loop;
      return Result;
   end Classify;

   procedure Insert
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String;
                Value : Variable
             )  is
      Cardinality : constant Natural := Get_Cardinality (Set);
   begin
      if Index = Cardinality + 1 then
         Add (Set, Name, Value);
      elsif Index <= Cardinality then
         declare
            Cloned   : Boolean      := False;
            Body_Ptr : Set_Body_Ptr := Ptr (Set);
         begin
            if Body_Ptr.Use_Count > 1 then
               Body_Ptr := Clone (Body_Ptr.all);
               Cloned := True;
            end if;
            Add (Body_Ptr.Domain, Name, Index);
            Put
            (  Body_Ptr.List,
               Cardinality + 1,
               Body_Ptr.List.Vector (Cardinality)
            );
            Body_Ptr.List.Vector (Index + 1..Cardinality - 1) :=
               Body_Ptr.List.Vector (Index..Cardinality - 2);
            Body_Ptr.List.Vector (Index) := Value;
            if Cloned then
               Handles.Set (Handles.Handle (Set), Body_Ptr);
            end if;
         exception
            when others =>
               if Cloned then
                  Free (Body_Ptr);
               end if;
               raise;
         end;
      else
         raise Constraint_Error;
      end if;
   end Insert;

   function Is_Empty (Set : Linguistic_Set) return Boolean is
   begin
      return Ptr (Set) = null or else Is_Empty (Ptr (Set).Domain);
   end Is_Empty;

   procedure Move
             (  Set  : in out Linguistic_Set;
                From : Positive;
                To   : Positive
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
      Moved    : constant Variable := Get (Body_Ptr.List, From);
   begin
      if From /= To then
         if Body_Ptr.Use_Count > 1 then
            Body_Ptr := Clone (Body_Ptr.all);
            Cloned := True;
         end if;
         Move (Body_Ptr.Domain, From, To);
         if From < To then
            Body_Ptr.List.Vector (From..To - 1) :=
               Body_Ptr.List.Vector (From + 1..To);
            Body_Ptr.List.Vector (To) := Moved;
         else
            Body_Ptr.List.Vector (To + 1..From) :=
               Body_Ptr.List.Vector (To..From - 1);
         end if;
         Body_Ptr.List.Vector (To) := Moved;
         if Cloned then
            Handles.Set (Handles.Handle (Set), Body_Ptr);
         end if;
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Move;

   procedure Remove (Set : in out Linguistic_Set; Index : Positive) is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
   begin
      if (  Body_Ptr = null
         or else
            Index > Get_Cardinality (Body_Ptr.Domain)
         )
      then
         raise Constraint_Error;
      end if;
      if Body_Ptr = null then
         Body_Ptr := new Set_Body;
         Cloned := True;
      elsif Body_Ptr.Use_Count > 1 then
         Body_Ptr := Clone (Body_Ptr.all);
         Cloned := True;
      end if;
      declare
         Cardinality : constant Natural :=
            Get_Cardinality (Body_Ptr.Domain);
      begin
         Body_Ptr.List.Vector (Index..Cardinality - 1) :=
            Body_Ptr.List.Vector (Index + 1..Cardinality);
         Body_Ptr.List.Vector (Cardinality) := Empty;
      end;
      Remove (Body_Ptr.Domain, Index);
      if Cloned then
         Handles.Set (Handles.Handle (Set), Body_Ptr);
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Remove;

   procedure Remove (Set : in out Linguistic_Set; Name : String) is
      Body_Ptr : constant Set_Body_Ptr := Ptr (Set);
   begin
      if Body_Ptr /= null then
         Remove (Set, Get_Index (Body_Ptr.Domain, Name));
      end if;
   end Remove;

   procedure Rename
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
   begin
      if Body_Ptr = null then
         Body_Ptr := new Set_Body;
         Cloned := True;
      elsif Body_Ptr.Use_Count > 1 then
         Body_Ptr := Clone (Body_Ptr.all);
         Cloned := True;
      end if;
      Rename (Body_Ptr.Domain, Index, Name);
      if Cloned then
         Handles.Set (Handles.Handle (Set), Body_Ptr);
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Rename;

   procedure Replace
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String;
                Value : Variable
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
   begin
      if Body_Ptr = null then
         raise Constraint_Error;
      elsif Body_Ptr.Use_Count > 1 then
         Body_Ptr := Clone (Body_Ptr.all);
         Cloned := True;
      end if;
      Rename (Body_Ptr.Domain, Index, Name);
      Put (Body_Ptr.List, Index, Value);
      if Cloned then
         Handles.Set (Handles.Handle (Set), Body_Ptr);
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Replace;

   procedure Replace
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Value : Variable
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
   begin
      if Body_Ptr = null then
         raise Constraint_Error;
      elsif Body_Ptr.Use_Count > 1 then
         Body_Ptr := Clone (Body_Ptr.all);
         Cloned := True;
      end if;
      Put (Body_Ptr.List, Index, Value);
      if Cloned then
         Handles.Set (Handles.Handle (Set), Body_Ptr);
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Replace;

   procedure Swap
             (  Set     : in out Linguistic_Set;
                Index_1 : Positive;
                Index_2 : Positive
             )  is
      Cloned   : Boolean      := False;
      Body_Ptr : Set_Body_Ptr := Ptr (Set);
      Value_1  : constant Variable := Get (Body_Ptr.List, Index_1);
      Value_2  : constant Variable := Get (Body_Ptr.List, Index_2);
   begin
      if Index_1 /= Index_2 then
         if Body_Ptr.Use_Count /= 1 then
            Body_Ptr := Clone (Body_Ptr.all);
            Cloned := True;
         end if;
         Swap (Body_Ptr.Domain, Index_1, Index_2);
         Put (Body_Ptr.List, Index_1, Value_2);
         Put (Body_Ptr.List, Index_2, Value_1);
         if Cloned then
            Set := Ref (Body_Ptr);
         end if;
      end if;
   exception
      when others =>
         if Cloned then
            Free (Body_Ptr);
         end if;
         raise;
   end Swap;

   function To_Set
            (  Set   : Linguistic_Set;
               Value : Number
            )  return Fuzzy.Intuitionistic.Set is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Fuzzy.Intuitionistic.Set
                  (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Get (List, Index), Value);
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end To_Set;

   function To_Set
            (  Set   : Linguistic_Set;
               Value : Interval
            )  return Fuzzy.Intuitionistic.Set is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Fuzzy.Intuitionistic.Set
                  (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Get (List, Index), Value);
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end To_Set;

   function To_Set
            (  Set   : Linguistic_Set;
               Value : Fuzzy_Float
            )  return Fuzzy.Intuitionistic.Set is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Fuzzy.Intuitionistic.Set
                  (Get_Cardinality (Object.Domain));
      Point  : Fuzzy_Boolean;
   begin
      for Index in Result.Possibility'Range loop
         Point := Is_In (Get (List, Index), Value);
         Result.Possibility (Index) := Point.Possibility;
         Result.Necessity   (Index) := Point.Necessity;
      end loop;
      return Result;
   end To_Set;

   function To_Set
            (  Set   : Linguistic_Set;
               Value : Variable
            )  return Fuzzy.Intuitionistic.Set is
      Object : Set_Body'Class renames Ptr (Set).all;
      List   : Unbounded_Array renames Object.List;
      Result : Fuzzy.Intuitionistic.Set
                  (Get_Cardinality (Object.Domain));
   begin
      for Index in Result.Possibility'Range loop
         declare
            Var : Variable renames Get (List, Index);
         begin
            Result.Possibility (Index) := Possibility (Value, Var);
            Result.Necessity   (Index) := Necessity   (Value, Var);
         end;
      end loop;
      return Result;
   end To_Set;

   function Ref (Thing : Set_Body_Ptr) return Linguistic_Set is
   begin
      return (Handles.Ref (Thing) with null record);
   end Ref;

   procedure Copy
             (  Source : Unbounded_Array;
                Target : in out Unbounded_Array;
                Count  : Natural
             )  is
   begin
      for Index in 1..Count loop
         Put (Target, Index, Get (Source, Index));
      end loop;
   end Copy;

   function Clone (Set : Set_Body) return Set_Body_Ptr is
      Body_Ptr : Set_Body_Ptr;
   begin
      Body_Ptr := new Set_Body;
      Copy (Set.Domain, Body_Ptr.Domain);
      Copy (Set.List, Body_Ptr.List, Get_Cardinality (Set.Domain));
      return Body_Ptr;
   end Clone;

end Fuzzy.Linguistics.Sets;
