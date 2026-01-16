--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Handle                     Luebeck            --
--  Implementation                                 Spring, 2003       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

package body Fuzzy.Classifier.Handle is

   function Is_Valid (Classifier : Classifier_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Classifier));
   end Is_Valid;

   function Classify
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is
   begin
      return Classify (Ptr (Classifier).all, Context, Complement);
   end Classify;

   function Classify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification is
   begin
      return
         Classify
         (  Classifier => Ptr (Classifier).all,
            Lesson     => Lesson,
            Example    => Example,
            Generalize => Generalize,
            Threshold  => Threshold,
            Complement => Complement
         );
   end Classify;

   function Estimate
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return Estimate (Ptr (Classifier).all, Context, Complement);
   end Estimate;

   function Estimate
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         Estimate
         (  Classifier => Ptr (Classifier).all,
            Lesson     => Lesson,
            Example    => Example,
            Generalize => Generalize,
            Threshold  => Threshold,
            Complement => Complement
         );
   end Estimate;

   function Get_Class (Classifier : Classifier_Handle) return String is
   begin
      return Get_Class (Ptr (Classifier).all);
   end Get_Class;

   function Get_Classes (Classifier : Classifier_Handle)
      return Feature_Handle is
   begin
      return Get_Classes (Ptr (Classifier).all);
   end Get_Classes;

   procedure Get_Examples
             (  Classifier : Classifier_Handle;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      Get_Examples (Ptr (Classifier).all, Lesson, Viewer);
   end Get_Examples;

   procedure Get_Examples
             (  Classifier : Classifier_Handle;
                Lesson     : in out Lecture_Handle;
                Viewer     : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Get_Examples (Ptr (Classifier).all, Lesson, Ptr (Viewer));
      else
         Get_Examples (Ptr (Classifier).all, Lesson);
      end if;
   end Get_Examples;

   function Get_Features (Classifier : Classifier_Handle)
      return Fuzzy.Feature.Handle.Container.Set is
   begin
      return Get_Features (Ptr (Classifier).all);
   end Get_Features;

   function Get_Training_Set_From (Classifier : Classifier_Handle)
      return Positive is
   begin
      return Get_Training_Set_From (Ptr (Classifier).all);
   end Get_Training_Set_From;

   function Get_Training_Set_Length (Classifier : Classifier_Handle)
      return Natural is
   begin
      return Get_Training_Set_Length (Ptr (Classifier).all);
   end Get_Training_Set_Length;

   function Get_Training_Set_Name (Classifier : Classifier_Handle)
      return String is
   begin
      return Get_Training_Set_Name (Ptr (Classifier).all);
   end Get_Training_Set_Name;

   procedure Invalidate (Classifier : in out Classifier_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Classifier));
   end Invalidate;

   procedure Learn
             (  Classifier : in out Classifier_Handle;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is
   begin
      Learn (Ptr (Classifier).all, Context, Features, From, To);
   end Learn;

   procedure Learn
             (  Classifier  : in out Classifier_Handle;
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
   begin
      Learn
      (  Classifier  => Ptr (Classifier).all,
         Lesson      => Lesson,
         Name        => Name,
         Features    => Features,
         From        => From,
         To          => To,
         Threshold   => Threshold,
         Equivalence => Equivalence,
         Viewer      => Viewer
      );
   end Learn;

   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             )  is
   begin
      Learn
      (  Classifier  => Ptr (Classifier).all,
         Lesson      => Lesson,
         Features    => Features,
         From        => From,
         To          => To,
         Threshold   => Threshold,
         Equivalence => Equivalence,
         Viewer      => Viewer
      );
   end Learn;

   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Name        : String;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Learn
         (  Classifier  => Ptr (Classifier).all,
            Lesson      => Lesson,
            Name        => Name,
            Features    => Features,
            From        => From,
            To          => To,
            Threshold   => Threshold,
            Equivalence => Equivalence,
            Viewer      => Ptr (Viewer)
         );
      else
         Learn
         (  Classifier  => Ptr (Classifier).all,
            Lesson      => Lesson,
            Name        => Name,
            Features    => Features,
            From        => From,
            To          => To,
            Threshold   => Threshold,
            Equivalence => Equivalence
         );
      end if;
   end Learn;

   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Learn
         (  Classifier  => Ptr (Classifier).all,
            Lesson      => Lesson,
            Features    => Features,
            From        => From,
            To          => To,
            Threshold   => Threshold,
            Equivalence => Equivalence,
            Viewer      => Ptr (Viewer)
         );
      else
         Learn
         (  Classifier  => Ptr (Classifier).all,
            Lesson      => Lesson,
            Features    => Features,
            From        => From,
            To          => To,
            Threshold   => Threshold,
            Equivalence => Equivalence
         );
      end if;
   end Learn;

   function Ptr (Classifier : Classifier_Handle)
      return Classifier_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Classifier));
   end Ptr;

   function Ref (Classifier : Classifier_Object_Ptr)
      return Classifier_Handle is
   begin
      return (Handles.Ref (Classifier) with null record);
   end Ref;

   procedure Ref
             (  Handle     : in out Classifier_Handle;
                Classifier : Classifier_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Classifier);
   end Ref;

   function To_Classifier_Handle
            (  Classifier : Deposit_Handles.Handle
            )  return Classifier_Handle is
   begin
      return
         Ref
         (  To_Classifier_Object_Ptr
            (  Deposit_Handles.Ptr (Classifier)
         )  );
   end To_Classifier_Handle;

   function To_Deposit_Handle (Classifier : Classifier_Handle)
      return Deposit_Handles.Handle is
   begin
      return
         Deposit_Handles.Ref
         (  To_Deposit_Ptr (Ptr (Classifier))
         );
   end To_Deposit_Handle;

   procedure Verify
             (  Classifier : Classifier_Handle;
                Context    : in out Classification_Parameters'Class;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Difference : Divergence_Function := Diff'Access
             )  is
   begin
      Verify
      (  Classifier => Ptr (Classifier).all,
         Context    => Context,
         Result     => Result,
         Report     => Report,
         From       => From,
         To         => To,
         Difference => Difference
      );
   end Verify;

   procedure Verify
             (  Classifier : Classifier_Handle;
                Lesson     : Lecture_Handle;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Generalize : Generalization_Mode := Linear;
                Threshold  : Confidence          := Confidence'First;
                Viewer     : Indicator_Handle;
                Difference : Divergence_Function := Diff'Access
             )  is
   begin
      if Is_Valid (Viewer) then
         Verify
         (  Classifier => Ptr (Classifier).all,
            Lesson     => Lesson,
            Result     => Result,
            Report     => Report,
            From       => From,
            To         => To,
            Generalize => Generalize,
            Threshold  => Threshold,
            Viewer     => Ptr (Viewer),
            Difference => Difference
         );
      else
         Verify
         (  Classifier => Ptr (Classifier).all,
            Lesson     => Lesson,
            Result     => Result,
            Report     => Report,
            From       => From,
            To         => To,
            Generalize => Generalize,
            Threshold  => Threshold,
            Difference => Difference
         );
      end if;
   end Verify;

   procedure Verify
             (  Classifier : Classifier_Handle;
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
      Verify
      (  Classifier => Ptr (Classifier).all,
         Lesson     => Lesson,
         Result     => Result,
         Report     => Report,
         From       => From,
         To         => To,
         Generalize => Generalize,
         Threshold  => Threshold,
         Viewer     => Viewer,
         Difference => Difference
      );
   end Verify;

   function Verify
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               From       : Positive := 1;
               To         : Positive := Positive'Last;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
   begin
      return
         Verify
         (  Classifier => Ptr (Classifier).all,
            Context    => Context,
            From       => From,
            To         => To,
            Difference => Difference
         );
   end Verify;

   function Verify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
   begin
      return
         Verify
         (  Classifier => Ptr (Classifier).all,
            Lesson     => Lesson,
            From       => From,
            To         => To,
            Generalize => Generalize,
            Threshold  => Threshold,
            Viewer     => Viewer,
            Difference => Difference
         );
   end Verify;

   function Verify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : Indicator_Handle;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set is
   begin
      if Is_Valid (Viewer) then
         return
            Verify
            (  Classifier => Ptr (Classifier).all,
               Lesson     => Lesson,
               From       => From,
               To         => To,
               Generalize => Generalize,
               Threshold  => Threshold,
               Viewer     => Ptr (Viewer),
               Difference => Difference
            );
      else
         return
            Verify
            (  Classifier => Ptr (Classifier).all,
               Lesson     => Lesson,
               From       => From,
               To         => To,
               Generalize => Generalize,
               Threshold  => Threshold,
               Difference => Difference
            );
      end if;
   end Verify;

end Fuzzy.Classifier.Handle;
