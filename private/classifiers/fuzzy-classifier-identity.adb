--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Identity                   Luebeck            --
--  Implementation                                 Aututmn, 2006       --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Fuzzy.Feature.Handle.Container; use Fuzzy.Feature.Handle.Container;

package body Fuzzy.Classifier.Identity is

   Class : constant String := Classifier_Class & "Identity";

   function Classify
            (  Classifier : Identity_Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is
      Classes : Feature_Object'Class renames
                   Ptr (Classifier.Classes).all;
   begin
      if Complement then
         return
         (  Cardinality =>
               Classes.Cardinality,
            Possibility =>
               Get (Context, Context.Example, Classes, Has_Out),
            Necessity =>
               not Get (Context, Context.Example, Classes, Has_Not_Out)
         );
      else
         return
         (  Cardinality =>
               Classes.Cardinality,
            Possibility =>
               Get (Context, Context.Example, Classes, Has_In),
            Necessity =>
               not Get (Context, Context.Example, Classes, Has_Not)
         );
      end if;
   end Classify;

   function Create (Classes : Feature_Handle)
      return Classifier_Handle is
   begin
      if not Is_Valid (Classes) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid class-feature"
         );
      end if;
      declare
         Object : constant Classifier_Object_Ptr :=
                     new Identity_Classifier_Object;
         Result : constant Classifier_Handle := Ref (Object);
      begin
         Identity_Classifier_Object'Class (Object.all).Classes :=
            Classes;
         return Result;
      end;
   end Create;

   function Estimate
            (  Classifier : Identity_Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set is
      Classes : Feature_Object'Class renames
                   Ptr (Classifier.Classes).all;
      Result  : Fuzzy.Intuitionistic.Set (Context.Cardinality);
   begin
      if Complement then
         Result.Possibility :=
            Get (Context, Context.Example, Classes, Has_Out);
         Result.Necessity :=
            not Get (Context, Context.Example, Classes, Has_In);
      else
         Result.Possibility :=
            Get (Context, Context.Example, Classes, Has_In);
         Result.Necessity :=
            not Get (Context, Context.Example, Classes, Has_Out);
      end if;
      return Result;
   end Estimate;

   function Get_Class (Classifier : Identity_Classifier_Object)
      return String is
   begin
      return Class;
   end Get_Class;

   function Get_Classes (Classifier : Identity_Classifier_Object)
      return Feature_Handle is
   begin
      return Classifier.Classes;
   end Get_Classes;

   procedure Get_Examples
             (  Classifier : Identity_Classifier_Object;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      null;
   end Get_Examples;

   function Get_Features (Classifier : Identity_Classifier_Object)
      return Fuzzy.Feature.Handle.Container.Set is
      Result : Fuzzy.Feature.Handle.Container.Set;
   begin
      Add (Result, Classifier.Classes);
      return Result;
   end Get_Features;

   procedure Get_Referents
             (  Classifier : Identity_Classifier_Object;
                List       : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Classifier.Classes)), False);
   end Get_Referents;

   function Get_Training_Set_From
            (  Classifier : Identity_Classifier_Object
            )  return Positive is
   begin
      return 1;
   end Get_Training_Set_From;

   function Get_Training_Set_Length
            (  Classifier : Identity_Classifier_Object
            )  return Natural is
   begin
      return 0;
   end Get_Training_Set_Length;

   function Get_Training_Set_Name
            (  Classifier : Identity_Classifier_Object
            )  return String is
   begin
      return "";
   end Get_Training_Set_Name;

   function Is_Identity (Classifier : Classifier_Handle)
      return Boolean is
   begin
      return Ptr (Classifier).all in Identity_Classifier_Object'Class;
   end Is_Identity;

   function Is_Modified (Classifier : Identity_Classifier_Object)
      return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Learn
             (  Classifier : in out Identity_Classifier_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is
   begin
      null;
   end Learn;

   procedure Reset_Modified
             (  Classifier : in out Identity_Classifier_Object
             )  is
   begin
      null;
   end Reset_Modified;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
      Classes : constant Feature_Handle :=
                   Ref (To_Feature_Object_Ptr (Get (List, 1)));
   begin
      Object := new Identity_Classifier_Object;
      Identity_Classifier_Object'Class (Object.all).Classes := Classes;
   exception
      when Constraint_Error =>
         Free (Object);
         raise Data_Error;
      when others =>
         Free (Object);
         raise;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Classifier  : Identity_Classifier_Object
             )  is
   begin
      null;
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Classifier.Identity;
