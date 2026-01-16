--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Separator                  Luebeck            --
--  Implementation                                 Winter, 2005       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit;             use Strings_Edit;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;

package body Fuzzy.Classifier.Separator is

   function Exist
            (  X         : Set;
               Exclude   : Integer;
               Threshold : Confidence
            )  return Boolean is
   begin
      for Index in X'Range loop
         if Index /= Exclude and then X (Index) > Threshold then
            return True;
         end if;
      end loop;
      return False;
   end Exist;

   function Get_Classifier (Classifier : Classifier_Handle)
      return Classifier_Handle is
   begin
      return Separator_Object'Class (Ptr (Classifier).all).Classifier;
   end Get_Classifier;

   function Get_Training_Set_From (Classifier : Separator_Object)
      return Positive is
   begin
      return Get_Training_Set_From (Classifier.Classifier);
   end Get_Training_Set_From;

   function Get_Training_Set_Length (Classifier : Separator_Object)
      return Natural is
   begin
      return Get_Training_Set_Length (Classifier.Classifier);
   end Get_Training_Set_Length;

   function Get_Training_Set_Name (Classifier : Separator_Object)
      return String is
   begin
      return Get_Training_Set_Name (Classifier.Classifier);
   end Get_Training_Set_Name;

   function Is_Separator (Classifier : Classifier_Handle)
      return Boolean is
   begin
      return Ptr (Classifier).all in Separator_Object'Class;
   end Is_Separator;

   function Not_Exist
            (  X         : Set;
               Exclude   : Integer;
               Threshold : Confidence
            )  return Boolean is
   begin
      for Index in X'Range loop
         if Index /= Exclude and then X (Index) >= Threshold then
            return False;
         end if;
      end loop;
      return True;
   end Not_Exist;

   procedure Refine
             (  Result       : in out Classification;
                Separation   : Confidence;
                Completeness : Confidence
             )  is
      List : array (Positive range 1..Result.Cardinality) of Positive;
      Last : Natural := 1;
   begin
      List (1) := 1;
      declare
         Weight : Float;
         Max    : Float :=
                  (  Float (Result.Possibility (1))
                  +  Float (Result.Necessity   (1))
                  );
      begin
         for I in 2..Result.Cardinality loop
            Weight :=
               (  Float (Result.Possibility (I))
               +  Float (Result.Necessity   (I))
               );
            if Weight >= Max then
               if Weight > Max then
                  Last := 1;
                  Max  := Weight;
               else
                  Last := Last + 1;
               end if;
               List (Last) := I;
            end if;
         end loop;
      end;
      declare
         Top : Confidence;
      begin
         if Last > 1 then
            Top := Confidence'First;
         else
            Top := Confidence'Last;
         end if;
         for I in reverse 1..Result.Cardinality loop
            if Last > 0 and then I = List (Last) then
               Last := Last - 1;
               Result.Possibility (I) := Confidence'Last;
               Result.Necessity   (I) := Top;
            else
               Result.Possibility (I) := Confidence'First;
               Result.Necessity   (I) := Confidence'First;
            end if;
         end loop;
      end;
--      if Possibility (Result.Necessity) > Separation then
--         for Index in 1..Result.Cardinality loop
--            if (  Result.Possibility (Index) > Separation
--               and then
--                  Exist (Result.Necessity, Index, Separation)
--               )
--            then
--               Result.Possibility (Index) := Separation;
--            end if;
--         end loop;
--      end if;
--      if Necessity (Result.Possibility) < Completeness then
--         for Index in 1..Result.Cardinality loop
--            if (  Result.Necessity (Index) < Completeness
--               and then
--                  Not_Exist (Result.Possibility, Index, Completeness)
--               )
--            then
--               Result.Necessity (Index) := Completeness;
--            end if;
--         end loop;
--      end if;
   end Refine;

   function Classify
            (  Classifier : Separator_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is
      Result : Classification :=
                  Classify
                  (  Classifier => Classifier.Classifier,
                     Context    => Context,
                     Complement => Complement
                  );
   begin
      Refine (Result, Classifier.Separation, Classifier.Completeness);
      return Result;
   end Classify;

   function Create
            (  Classifier   : Classifier_Handle;
               Separation   : Confidence := Confidence'First;
               Completeness : Confidence := Confidence'Last
            )  return Classifier_Handle is
      This   : constant Classifier_Object_Ptr := new Separator_Object;
      Result : constant Classifier_Handle := Ref (This);
      Object : Separator_Object renames Separator_Object (This.all);
   begin
      if not Is_Valid (Classifier) then
         raise Constraint_Error;
      end if;
      Object.Classifier   := Classifier;
      Object.Separation   := Separation;
      Object.Completeness := Completeness;
      return Result;
   end Create;

   function Estimate
            (  Classifier : Separator_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Intuitionistic.Set is
   begin
      return Estimate (Classifier.Classifier, Context, Complement);
   end Estimate;

   function Get_Class (Classifier : Separator_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Classes (Classifier : Separator_Object)
      return Feature_Handle is
   begin
      return Get_Classes (Classifier.Classifier);
   end Get_Classes;

   procedure Get_Examples
             (  Classifier : Separator_Object;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      Get_Examples (Classifier.Classifier, Lesson, Viewer);
   end Get_Examples;

   function Get_Features (Classifier : Separator_Object)
      return Fuzzy.Feature.Handle.Container.Set is
   begin
      return Get_Features (Classifier.Classifier);
   end Get_Features;

   procedure Get_Referents
             (  Classifier : Separator_Object;
                List       : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Classifier.Classifier)), False);
   end Get_Referents;

   function Is_Modified (Classifier : Separator_Object)
      return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Reset_Modified (Classifier : in out Separator_Object) is
   begin
      null;
   end Reset_Modified;

   procedure Learn
             (  Classifier : in out Separator_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is
   begin
      Learn (Classifier.Classifier, Context, Features, From, To);
   end Learn;

   procedure Restore
             (  Source     : String;
                Pointer    : in out Integer;
                Class      : String;
                List       : Deposit_Container'Class;
                Object     : out Deposit_Ptr
             )  is
      Separation   : Confidence;
      Completeness : Confidence;
      Classifier   : Classifier_Handle;
      Position     : Integer := Pointer;
   begin
      Get (Source, Position);
      Get (Source, Position, Separation);
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ',' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      Get (Source, Position, Completeness);
      begin
         Classifier := Ref (To_Classifier_Object_Ptr (Get (List, 1)));
      exception
         when Constraint_Error =>
            raise Use_Error;
      end;
      Object := new Separator_Object;
      declare
         Result : Separator_Object renames
                     Separator_Object (Object.all);
      begin
         Result.Classifier   := Classifier;
         Result.Separation   := Separation;
         Result.Completeness := Completeness;
      end;
      Pointer := Position;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Classifier  : Separator_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put (Destination, Position, Classifier.Separation);
      Put (Destination, Position, ", ");
      Put (Destination, Position, Classifier.Completeness);
      Pointer := Position;
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Classifier.Separator;
