--                                                                    --
--  package Fuzzy.Classifier        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2003       --
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

with Fuzzy.Lecture.General;  use Fuzzy.Lecture.General;

package body Fuzzy.Classifier is

   function Classify
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality (Get_Classes (Classifier))
                        );
   begin
      Select_Example (Context, Example);
      Context.Threshold  := Threshold;
      Context.Generalize := Generalize;
      return Classify (Classifier, Context'Access, Complement);
   end Classify;

   function Diff (Left, Right : Fuzzy_Boolean)
      return Divergence_Range is
      Possibility : constant Divergence :=
         Divergence (Left.Possibility xor Right.Possibility);
      Necessity   : constant Divergence :=
         Divergence (Left.Necessity xor Right.Necessity);
   begin
      if Possibility > Necessity then
         return
         (  To   => Possibility,
            From => Necessity
         );
      else
         return
         (  To   => Necessity,
            From => Possibility
         );
      end if;
   end Diff;

   function Diff (Left, Right : Classification)
      return Divergence_Vector is
   begin
      if Left.Cardinality /= Right.Cardinality then
         raise Constraint_Error;
      end if;
      declare
         Result : Divergence_Vector (1..Left.Cardinality);
      begin
         for Index in 1..Left.Cardinality loop
            Result (Index) :=
               Diff
               (  (  Possibility => Left.Possibility (Index),
                     Necessity   => Left.Necessity   (Index)
                  ),
                  (  Possibility => Right.Possibility (Index),
                     Necessity   => Right.Necessity   (Index)
               )  );
         end loop;
         return Result;
      end;
   end Diff;

   function Estimate
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Intuitionistic.Set is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality (Get_Classes (Classifier))
                        );
   begin
      Select_Example (Context, Example);
      Context.Threshold  := Threshold;
      Context.Generalize := Generalize;
      return Estimate (Classifier, Context'Access, Complement);
   end Estimate;

   procedure Learn
             (  Classifier  : in out Classifier_Object'Class;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             )  is
      Context : Training_Data (Ptr (Lesson), 0, Viewer);
   begin
      Context.Threshold   := Threshold;
      Context.Equivalence := Equivalence;
      Learn (Classifier, Context, Features, From, To);
   end Learn;

   procedure Learn
             (  Classifier  : in out Classifier_Object'Class;
                Lesson      : Lecture_Handle;
                Name        : String;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             )  is
      Context : Training_Data (Ptr (Lesson), Name'Length, Viewer);
   begin
      Context.Set_Name    := Name;
      Context.Threshold   := Threshold;
      Context.Equivalence := Equivalence;
      Learn (Classifier, Context, Features, From, To);
   end Learn;

   function To_Classifier_Ptr is
      new Ada.Unchecked_Conversion
          (  Deposit_Ptr,
             Classifier_Object_Ptr
          );

   function To_Classifier_Object_Ptr (Ptr : Deposit_Ptr)
      return Classifier_Object_Ptr is
   begin
      if Ptr.all in Classifier_Object'Class then
         return To_Classifier_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Classifier_Object_Ptr;

   function Verify
            (  Classifier : Classifier_Object'Class;
               Context    : access Classification_Parameters'Class;
               Result     : Lecture_Object_Ptr;
               From       : Positive := 1;
               To         : Positive := Positive'Last;
               Viewer     : access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
      Classes     : Feature_Object'Class renames
                       Ptr (Get_Classes (Classifier)).all;
      Cardinality : constant Positive := Classes.Cardinality;
      Examples_No : constant Natural :=
                       Get_Examples_Number (Context.Lesson.all);
      Last  : constant Natural := Natural'Min (Examples_No, To);
      Diffs : Divergence_Vector (1..Cardinality);

      procedure Check
                (  Result       : Classification;
                   In_Class     : Set;
                   Not_In_Class : Set
                )  is
         pragma Inline (Check);
      begin
         for Index in 1..Cardinality loop
            Diffs (Index) :=
               (  Diffs (Index)
               +  Difference
                  (  Fuzzy_Boolean'
                     (  Possibility => Result.Possibility (Index),
                        Necessity   => Result.Necessity   (Index)
                     ),
                     Fuzzy_Boolean'
                     (  Possibility => In_Class (Index),
                        Necessity   => not Not_In_Class (Index)
               )  )  );
         end loop;
      end Check;
   begin
      Reset (Viewer.all, Last - From + 1);
      for Example in From..Last loop
         Select_Example (Context.all, Example);
         declare
            Got : Classification renames
                     Classify (Classifier, Context, False);
         begin
            Check
            (  Got,
               Get (Context, Example, Classes, Has_In),
               Get (Context, Example, Classes, Has_Not)
            );
            if Result /= null then
               Put
               (  Result.all,
                  Example,
                  Classes,
                  Has_In,
                  Got.Possibility
               );
               Put
               (  Result.all,
                  Example,
                  Classes,
                  Has_Not,
                  not Got.Necessity
               );
            end if;
         end;
         Check (Viewer.all);
      end loop;
      Done (Viewer.all);
      return To_Set (Diffs, (Last - From + 1));
   end Verify;

   function To_Confidence (Left : Divergence) return Confidence is
   begin
      if Left <= Divergence (Confidence'First) then
         return Confidence'First;
      elsif Left >= Divergence (Confidence'Last) then
         return Confidence'Last;
      else
         return Confidence (Left);
      end if;
   exception
      when others =>
         return Confidence'Last;
   end To_Confidence;

   function To_Divergence_Range
            (  Value : Classification;
               Index : Positive
            )  return Divergence_Range is
   begin
      return
      (  From =>
            Divergence
            (  Value.Possibility (Index)
            and
               Value.Necessity (Index)
            ),
         To =>
            Divergence (Value.Possibility (Index))
      );
   end To_Divergence_Range;

   function To_Set (Left : Divergence_Vector; Norm : Natural)
      return Intuitionistic.Set is
   begin
      if Norm > 0 then
         declare
            Result   : Intuitionistic.Set (Left'Length);
            Count    : constant Divergence := Divergence (Norm);
            Position : Positive := 1;
         begin
            for Index in Left'Range loop
               Result.Possibility (Position) :=
                  To_Confidence (Left (Index).To / Count);
               Result.Necessity (Position) :=
                  To_Confidence (Left (Index).From / Count);
               Position := Position + 1;
            end loop;
            return Result;
         end;
      else
         return
         (  Cardinality => Left'Length,
            Possibility => (others => Confidence'First),
            Necessity   => (others => Confidence'First)
         );
      end if;
   end To_Set;

   function To_Fuzzy_Boolean (Left : Divergence_Range)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => To_Confidence (Left.To),
         Necessity   => To_Confidence (Left.From)
      );
   end To_Fuzzy_Boolean;

   function Verify
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality (Get_Classes (Classifier))
                        );
   begin
      Context.Threshold  := Threshold;
      Context.Generalize := Generalize;
      return
         Verify
         (  Classifier => Classifier,
            Context    => Context'Access,
            Result     => null,
            From       => From,
            To         => To,
            Viewer     => Viewer,
            Difference => Difference
         );
   end Verify;

   function Verify
            (  Classifier : Classifier_Object'Class;
               Context    : not null access
                            Classification_Parameters'Class;
               From       : Positive   := 1;
               To         : Positive   := Positive'Last;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
   begin
      return
         Verify
         (  Classifier => Classifier,
            Context    => Context,
            Result     => null,
            From       => From,
            To         => To,
            Viewer     => Viewer,
            Difference => Difference
         );
   end Verify;

   procedure Verify
             (  Classifier : Classifier_Object'Class;
                Lesson     : Lecture_Handle;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Generalize : Generalization_Mode := Linear;
                Threshold  : Confidence          := Confidence'First;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access;
                Difference : Divergence_Function := Diff'Access
             )  is
   begin
      if (  Get_Cardinality (Get_Classes (Classifier))
         /= Report.Cardinality
         )
      then
         raise Constraint_Error;
      end if;
      declare
         Context : aliased Classification_Parameters
                           (  Ptr (Lesson),
                              Get_Cardinality (Get_Classes (Classifier))
                           );
      begin
         Context.Threshold  := Threshold;
         Context.Generalize := Generalize;
         if not Is_Valid (Result) then
            Result := Create;
         end if;
         Report :=
            Verify
            (  Classifier => Classifier,
               Context    => Context'Access,
               Result     => Ptr (Result),
               From       => From,
               To         => To,
               Viewer     => Viewer,
               Difference => Difference
            );
      end;
   end Verify;

   procedure Verify
             (  Classifier : Classifier_Object'Class;
                Context    : in out Classification_Parameters'Class;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive   := 1;
                To         : Positive   := Positive'Last;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access;
                Difference : Divergence_Function := Diff'Access
            )   is
   begin
      if (  Get_Cardinality (Get_Classes (Classifier))
         /= Report.Cardinality
         )
      then
         raise Constraint_Error;
      end if;
      if not Is_Valid (Result) then
         Result := Create;
      end if;
      Report :=
         Verify
         (  Classifier => Classifier,
            Context    => Context'Access,
            Result     => Ptr (Result),
            From       => From,
            To         => To,
            Viewer     => Viewer,
            Difference => Difference
         );
   end Verify;

end Fuzzy.Classifier;
