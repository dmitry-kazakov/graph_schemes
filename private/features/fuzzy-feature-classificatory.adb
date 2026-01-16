--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Classificatory                Luebeck            --
--  Implementation                                 Autumn, 2002       --
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

with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Fuzzy.Basic_Edit;               use Fuzzy.Basic_Edit;
with Fuzzy.Feature.Context;          use Fuzzy.Feature.Context;
with Fuzzy.Feature.Handle.Container; use Fuzzy.Feature.Handle.Container;
with Fuzzy.Lecture;                  use Fuzzy.Lecture;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;

with Fuzzy.Intuitionistic;

package body Fuzzy.Feature.Classificatory is

   procedure Classify
             (  Feature : Classificatory_Feature_Object;
                Context : in out Classification_Parameters'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data'Class
             )  is
      pragma Inline (Classify);
   begin
      case Image is
         when Has_In | Has_Not =>
            declare
               Value : Fuzzy.Intuitionistic.Classification renames
                  Classify (Feature.Classifier, Context'Access, False);
            begin
               Data.Has_In  := Value.Possibility;
               Data.Has_Not := not Value.Necessity;
            end;
         when Has_Out | Has_Not_Out =>
            declare
               Value : Fuzzy.Intuitionistic.Classification renames
                  Classify (Feature.Classifier, Context'Access, True);
            begin
               Data.Has_Out     := Value.Possibility;
               Data.Has_Not_Out := not Value.Necessity;
            end;
      end case;
      Data.Defined (Image) := True;
   end Classify;

   function Create
            (  Name       : String;
               Classifier : Classifier_Handle;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Feature_Handle is
      Result  : constant Feature_Object_Ptr :=
                   new Classificatory_Feature_Object
                       (  Get_Cardinality (Get_Classes (Classifier))
                       );
      Feature : Classificatory_Feature_Object renames
                   Classificatory_Feature_Object (Result.all);
   begin
      Feature.Self       := Result;
      Feature.Name       := new String'(Name);
      Feature.Classifier := Classifier;
      Feature.Generalize := Generalize;
      Feature.Threshold  := Threshold;
      return Ref (Result);
   end Create;

   function Get_Class (Feature : Classificatory_Feature_Object)
      return String is
   begin
      return Classificatory_Class;
   end Get_Class;

   function Get_Classifier (Feature : Feature_Object'Class)
      return Classifier_Handle is
   begin
      return
         Classificatory_Feature_Object'Class (Feature).Classifier;
   end Get_Classifier;

   function Get_Classifier (Feature : Feature_Handle)
      return Classifier_Handle is
   begin
      return
         Classificatory_Feature_Object'Class
         (  Ptr (Feature).all
         ) .Classifier;
   end Get_Classifier;

   function Get_Generalization (Feature : Feature_Object'Class)
      return Generalization_Mode is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Feature);
   begin
      return Object.Generalize;
   end Get_Generalization;

   function Get_Generalization (Feature : Feature_Handle)
      return Generalization_Mode is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Ptr (Feature).all);
   begin
      return Object.Generalize;
   end Get_Generalization;

   function Get_Threshold (Feature : Feature_Object'Class)
      return Confidence is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Feature);
   begin
      return Object.Threshold;
   end Get_Threshold;

   function Get_Threshold (Feature : Feature_Handle)
      return Confidence is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Ptr (Feature).all);
   begin
      return Object.Threshold;
   end Get_Threshold;

   procedure Query
             (  Feature : Classificatory_Feature_Object;
                Context : in out Context_Object'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data'Class
             )  is
      This : Lecture_Context'Class renames
                Lecture_Context'Class (Context);
   begin
      if Is_Defined (This.Lesson.all, This.Example, Feature, Image) then
         case Image is
            when Has_In =>
               Data.Has_In :=
                  Get (This.Lesson, This.Example, Feature, Image);
            when Has_Out =>
               Data.Has_Out :=
                  Get (This.Lesson, This.Example, Feature, Image);
            when Has_Not =>
               Data.Has_Not :=
                  Get (This.Lesson, This.Example, Feature, Image);
            when Has_Not_Out =>
               Data.Has_Not_Out :=
                  Get (This.Lesson, This.Example, Feature, Image);
         end case;
      else
         if This in Classification_Parameters'Class then
            Classify
            (  Feature,
               Classification_Parameters'Class (This),
               Image,
               Data
            );
         else
            declare
               Context : Classification_Parameters
                         (  This.Lesson,
                            Feature.Cardinality
                         );
            begin
               Select_Example (Context, This.Example);
               Context.Generalize := Feature.Generalize;
               Context.Threshold  := Feature.Threshold;
               Classify (Feature, Context, Image, Data);
            end;
         end if;
      end if;
      Data.Known (Image) := True;
   end Query;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Classificatory_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
   begin
      Get_Range
      (  Source,
         Pointer,
         Ptr (Get_Classes (Feature.Classifier)).all,
         From,
         To,
         Exclusive,
         Parameters
      );
   end Get_Range;

   procedure Get_Referents
             (  Feature : Classificatory_Feature_Object;
                List    : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Feature.Classifier)), False);
   end Get_Referents;

   function Is_Classificatory (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Classificatory_Feature_Object'Class;
   end Is_Classificatory;

   function Is_Classificatory (Feature : Feature_Handle)
      return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
      (  This /= null
      and then
         This.all in Classificatory_Feature_Object'Class
      );
   end Is_Classificatory;

   function Is_Computed
            (  Feature : Classificatory_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is
      List : Fuzzy.Feature.Handle.Container.Set renames
                Get_Features (Feature.Classifier);
   begin
      for Index in 1..Get_Size (List) loop
         if Is_Computed (Get (List, Index).all, Source) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Computed;

   function Is_Defined
            (  Feature : Classificatory_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return True;
   end Is_Defined;

   function Is_Modified
            (  Feature : Classificatory_Feature_Object
            )  return Boolean is
   begin
      return Feature.Updated;
   end Is_Modified;

   procedure Learn
             (  Feature  : in out Feature_Object'Class;
                Context  : in out Training_Data'Class;
                Features : Feature_Array;
                From     : Positive := 1;
                To       : Positive := Positive'Last
             )  is
   begin
      Learn
      (  Classificatory_Feature_Object'Class
            (Feature).Classifier,
         Context,
         Features,
         From,
         To
      );
   end Learn;

   procedure Learn
             (  Feature  : in out Feature_Handle;
                Context  : in out Training_Data'Class;
                Features : Feature_Array;
                From     : Positive := 1;
                To       : Positive := Positive'Last
             )  is
   begin
      Learn
      (  Classificatory_Feature_Object'Class
            (Ptr (Feature).all).Classifier,
         Context,
         Features,
         From,
         To
      );
   end Learn;

   procedure Learn
             (  Feature   : in out Feature_Object'Class;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Learn
         (  Classifier => Classificatory_Feature_Object'Class
                             (Feature).Classifier,
            Lesson     => Lesson,
            Features   => Features,
            From       => From,
            To         => To,
            Threshold  => Threshold,
            Viewer     => Ptr (Viewer)
         );
      else
         Learn
         (  Classifier => Classificatory_Feature_Object'Class
                             (Feature).Classifier,
            Lesson     => Lesson,
            Features   => Features,
            From       => From,
            To         => To,
            Threshold  => Threshold
         );
      end if;
   end Learn;

   procedure Learn
             (  Feature   : in out Feature_Handle;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Learn
         (  Classifier => Classificatory_Feature_Object'Class
                             (Ptr (Feature).all).Classifier,
            Lesson     => Lesson,
            Features   => Features,
            From       => From,
            To         => To,
            Threshold  => Threshold,
            Viewer     => Ptr (Viewer)
         );
      else
         Learn
         (  Classifier => Classificatory_Feature_Object'Class
                             (Ptr (Feature).all).Classifier,
            Lesson     => Lesson,
            Features   => Features,
            From       => From,
            To         => To,
            Threshold  => Threshold
         );
      end if;
   end Learn;

   procedure Learn
             (  Feature   : in out Feature_Object'Class;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : not null access Indicator_Object'Class :=
                               Negleter'Access
             )  is
   begin
      Learn
      (  Classifier => Classificatory_Feature_Object'Class
                          (Feature).Classifier,
         Lesson     => Lesson,
         Features   => Features,
         From       => From,
         To         => To,
         Threshold  => Threshold,
         Viewer     => Viewer
      );
   end Learn;

   procedure Learn
             (  Feature   : in out Feature_Handle;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : not null access Indicator_Object'Class :=
                               Negleter'Access
             )  is
   begin
      Learn
      (  Classifier => Classificatory_Feature_Object'Class
                          (Ptr (Feature).all).Classifier,
         Lesson     => Lesson,
         Features   => Features,
         From       => From,
         To         => To,
         Threshold  => Threshold,
         Viewer     => Viewer
      );
   end Learn;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Classificatory_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put_Range
      (  Destination,
         Pointer,
         Ptr (Get_Classes (Feature.Classifier)).all,
         From,
         To,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put_Range;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      Generalize : Generalization_Mode;
      Threshold  : Confidence;
      Classifier : Classifier_Handle;
      Position   : aliased Integer := Pointer;
      Got_It     : Boolean         := False;
      Name       : constant String :=
                      Get_Quoted (Source, Position'Access);
   begin
      Get_Delimiter (Source, Position, Got_It, Colon);
      if not Got_It then
         raise Data_Error;
      end if;
      for Index in Position..Source'Last loop
         if Source (Index) = ':' then
            declare
               Text : constant String :=
                         Trim (Source (Position..Index - 1));
            begin
               Generalize := Generalization_Mode'Value (Text);
            exception
               when others =>
                  raise Data_Error;
            end;
            Position := Index + 1;
            Got_It   := True;
            exit;
         end if;
      end loop;
      if not Got_It then
         raise Data_Error;
      end if;
      Get (Source, Position);
      begin
         Get (Source, Position, Threshold);
         Classifier := Ref (To_Classifier_Object_Ptr (Get (List, 1)));
      exception
         when Constraint_Error | End_Error =>
            raise Use_Error;
      end;
      Feature :=
         new Classificatory_Feature_Object
             (  Get_Cardinality (Get_Classes (Classifier))
             );
      declare
         Result : Classificatory_Feature_Object renames
                     Classificatory_Feature_Object (Feature.all);
      begin
         Result.Self       := To_Feature_Object_Ptr (Feature);
         Result.Name       := new String'(Name);
         Result.Classifier := Classifier;
         Result.Generalize := Generalize;
         Result.Threshold  := Threshold;
      end;
      Pointer := Position;
   end Restore;

   procedure Set_Classifier
             (  Feature    : in out Feature_Object'Class;
                Classifier : Classifier_Handle
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Feature);
   begin
      if Get_Classes (Object.Classifier) /= Get_Classes (Classifier) then
         raise Constraint_Error;
      else
         Object.Classifier := Classifier;
      end if;
   end Set_Classifier;

   procedure Set_Classifier
             (  Feature    : in out Feature_Handle;
                Classifier : Classifier_Handle
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Ptr (Feature).all);
   begin
      if Get_Classes (Object.Classifier) /= Get_Classes (Classifier) then
         raise Constraint_Error;
      else
         if Object.Classifier /= Classifier then
            Object.Updated    := True;
            Object.Classifier := Classifier;
         end if;
      end if;
   end Set_Classifier;

   procedure Set_Generalization
             (  Feature    : in out Feature_Handle;
                Generalize : Generalization_Mode
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Ptr (Feature).all);
   begin
      if Object.Generalize /= Generalize then
         Object.Updated    := True;
         Object.Generalize := Generalize;
      end if;
   end Set_Generalization;

   procedure Set_Generalization
             (  Feature    : in out Feature_Object'Class;
                Generalize : Generalization_Mode
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Feature);
   begin
      if Object.Generalize /= Generalize then
         Object.Updated    := True;
         Object.Generalize := Generalize;
      end if;
   end Set_Generalization;

   procedure Set_Threshold
             (  Feature   : in out Feature_Object'Class;
                Threshold : Confidence
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Feature);
   begin
      if Object.Threshold /= Threshold then
         Object.Updated   := True;
         Object.Threshold := Threshold;
      end if;
   end Set_Threshold;

   procedure Set_Threshold
             (  Feature   : in out Feature_Handle;
                Threshold : Confidence
             )  is
      Object : Classificatory_Feature_Object'Class renames
          Classificatory_Feature_Object'Class (Ptr (Feature).all);
   begin
      if Object.Threshold /= Threshold then
         Object.Updated   := True;
         Object.Threshold := Threshold;
      end if;
   end Set_Threshold;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Classificatory_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Put
      (  Destination,
         Position,
         Generalization_Mode'Image (Feature.Generalize)
      );
      Put (Destination, Position, ": ");
      Put (Destination, Position, Feature.Threshold);
      Pointer := Position;
   end Store;

begin
   Register_Class (Classificatory_Class, Restore'Access);
end Fuzzy.Feature.Classificatory;
