--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Classificatory.               Luebeck            --
--        Clustering                               Autumn, 2006       --
--  Implementation                                                    --
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
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Fuzzy.Basic_Edit;         use Fuzzy.Basic_Edit;
with Fuzzy.Classifier;         use Fuzzy.Classifier;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

package body Fuzzy.Feature.Classificatory.Clustering is

   function "<" (Left, Right : Frequency_To_Value) return Boolean is
   begin
      return
      (  Left.Frequency > Right.Frequency
      or else
         (  Left.Frequency = Right.Frequency
         and then
            Left.Value < Right.Value
      )  );
   end "<";

   function Create
            (  Name        : String;
               Classifier  : Classifier_Handle;
               Generalize  : Generalization_Mode := Linear;
               Threshold   : Confidence          := Confidence'First;
               Equivalence : Cluster_Frequency   := Cluster_Equivalence
            )  return Feature_Handle is
      Result  : constant Feature_Object_Ptr :=
                   new Clustering_Feature_Object
                       (  Get_Cardinality (Get_Classes (Classifier))
                       );
      Feature : Clustering_Feature_Object renames
                   Clustering_Feature_Object (Result.all);
   begin
      Feature.Self        := Result;
      Feature.Name        := new String'(Name);
      Feature.Classifier  := Classifier;
      Feature.Generalize  := Generalize;
      Feature.Threshold   := Threshold;
      Feature.Equivalence := Equivalence;
      return Ref (Result);
   end Create;

   function Get_Class (Feature : Clustering_Feature_Object)
      return String is
   begin
      return Clustering_Class;
   end Get_Class;

   function Get_Equivalence (Feature : Feature_Handle)
      return Cluster_Frequency is
   begin
      return
         Clustering_Feature_Object'Class
            (Ptr (Feature).all).Equivalence;
   end Get_Equivalence;

   function Get_Equivalence (Feature : Feature_Object'Class)
      return Cluster_Frequency is
   begin
      return Clustering_Feature_Object'Class (Feature).Equivalence;
   end Get_Equivalence;

   function Is_Clustering (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Clustering_Feature_Object'Class;
   end Is_Clustering;

   function Is_Clustering (Feature : Feature_Handle)
      return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
      (  This /= null
      and then
         This.all in Clustering_Feature_Object'Class
      );
   end Is_Clustering;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      Generalize  : Generalization_Mode;
      Threshold   : Confidence;
      Equivalence : Cluster_Frequency;
      Classifier  : Classifier_Handle;
      Position    : aliased Integer := Pointer;
      Got_It      : Boolean         := False;
      Name        : constant String :=
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
                         Trim (Source (Pointer..Index - 1));
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
         Get_Delimiter (Source, Position, Got_It, Colon);
         if not Got_It then
            raise Data_Error;
         end if;
         Get (Source, Position, Float (Equivalence));
         Classifier := Ref (To_Classifier_Object_Ptr (Get (List, 1)));
      exception
         when Constraint_Error | End_Error =>
            raise Use_Error;
      end;
      Feature :=
         new Clustering_Feature_Object
             (  Get_Cardinality (Get_Classes (Classifier))
             );
      declare
         Result : Clustering_Feature_Object renames
                     Clustering_Feature_Object (Feature.all);
      begin
         Result.Self        := To_Feature_Object_Ptr (Feature);
         Result.Name        := new String'(Name);
         Result.Classifier  := Classifier;
         Result.Generalize  := Generalize;
         Result.Threshold   := Threshold;
         Result.Equivalence := Equivalence;
      end;
      Pointer := Position;
   end Restore;

begin
   Register_Class (Clustering_Class, Restore'Access);
end Fuzzy.Feature.Classificatory.Clustering;
