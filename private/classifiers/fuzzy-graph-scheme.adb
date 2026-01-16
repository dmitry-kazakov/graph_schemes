--                                                                    --
--  package Fuzzy.Graph.Scheme      Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Fuzzy.Lecture;                  use Fuzzy.Lecture;
with Fuzzy.Feature.Handle.Container; use Fuzzy.Feature.Handle.Container;
with Fuzzy.Graph.Handle.Learning;    use Fuzzy.Graph.Handle.Learning;
with Fuzzy.Graph.Handle.Layer;       use Fuzzy.Graph.Handle.Layer;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;

package body Fuzzy.Graph.Scheme is
--
-- Classify -- One example using two graphs
--
--    Scheme       - The graph-scheme
--    Pro_Image    - The image it should classify
--    Contra_Image - The complement image it should classify
--    Data         - The classification data to use
--
-- Returns :
--
--     The classification
--
   function Classify
            (  Scheme       : Graph_Scheme_Object;
               Pro_Image    : Image_Type;
               Contra_Image : Image_Type;
               Data         : access Classification_Parameters'Class
            )  return Classification;

   function Classify
            (  Scheme       : Graph_Scheme_Object;
               Pro_Image    : Image_Type;
               Contra_Image : Image_Type;
               Data         : access Classification_Parameters'Class
            )  return Classification is
      Result : Classification (Data.Cardinality);
      Mode   : constant Generalization_Mode := Data.Generalize;
      Node   : Graph_Node_Ptr;
   begin
      Data.Ready := False;
      Data.Result.Possibility := (others => Confidence'First);
      Data.Result.Necessity   := (others => Confidence'Last);
      for Image in Scheme.Roots'Range loop
         Node := Ptr (Scheme.Roots (Image));
         if Node /= null then
            case Image is
               when Has_In =>
                  Data.Image      := Pro_Image;
                  Data.Generalize := Mode;
               when Has_Out =>
                  Data.Image      := Pro_Image;
                  Data.Generalize := None;
               when Has_Not =>
                  Data.Image      := Contra_Image;
                  Data.Generalize := Mode;
               when Has_Not_Out =>
                  Data.Image      := Contra_Image;
                  Data.Generalize := None;
            end case;
            Classify (Node.all, Data.all);
            if Image = Scheme.Roots'First then
               Result := Data.Result;
            else
               And_At (Result, Data.Result);
            end if;
         end if;
      end loop;
      Data.Generalize := Mode;
      return Result;
   exception
      when others =>
         Data.Generalize := Mode;
         raise;
   end Classify;

   function Classify
            (  Scheme     : Graph_Scheme_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is
   begin
      if Complement then
         return Classify (Scheme, Has_Out, Has_Not_Out, Context);
      else
         return Classify (Scheme, Has_In, Has_Not, Context);
      end if;
   end Classify;

   function Create
            (  Context  : not null access Graph_Training_Data'Class;
               Features : Feature_Array;
               From     : Positive := 1;
               To       : Positive := Positive'Last
            )  return Classifier_Handle is
      Object : constant Classifier_Object_Ptr :=
                  new Graph_Scheme_Object;
      Result : constant Classifier_Handle := Ref (Object);
   begin
      Learn
      (  Classifier => Graph_Scheme_Object'Class (Object.all),
         Context    => Context.all,
         Features   => Features,
         From       => From,
         To         => To
      );
      return Result;
   end Create;

   function Create
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle is
   begin
      if not Is_Valid (Classes) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid class-feature"
         );
      end if;
      declare
         Context : Graph_Training_Data
                   (  Ptr (Lesson),
                      0,
                      Get_Cardinality (Classes),
                      Viewer
                   );
         Object : constant Classifier_Object_Ptr :=
                     new Graph_Scheme_Object;
         Result : constant Classifier_Handle := Ref (Object);
      begin
         Context.Factory     := Factory.all'Unchecked_Access;
         Context.Classes     := Classes;
         Context.Threshold   := Threshold;
         Context.Equivalence := Equivalence;
         Learn
         (  Classifier => Graph_Scheme_Object'Class (Object.all),
            Context    => Context,
            Features   => Features,
            From       => From,
            To         => To
         );
         return Result;
      end;
   end Create;

   function Create
            (  Lesson      : Lecture_Handle;
               Name        : String;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle is
   begin
      if not Is_Valid (Classes) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid class-feature"
         );
      end if;
      declare
         Context : Graph_Training_Data
                   (  Ptr (Lesson),
                      Name'Length,
                      Get_Cardinality (Classes),
                      Viewer
                   );
         Object : constant Classifier_Object_Ptr :=
                     new Graph_Scheme_Object;
         Result : constant Classifier_Handle := Ref (Object);
      begin
         Context.Set_Name    := Name;
         Context.Factory     := Factory.all'Unchecked_Access;
         Context.Classes     := Classes;
         Context.Threshold   := Threshold;
         Context.Equivalence := Equivalence;
         Learn
         (  Classifier => Graph_Scheme_Object'Class (Object.all),
            Context    => Context,
            Features   => Features,
            From       => From,
            To         => To
         );
         return Result;
      end;
   end Create;

   function Estimate
            (  Scheme     : Graph_Scheme_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set is
      Result : Fuzzy.Intuitionistic.Set (Context.Cardinality);
   begin
      Result.Possibility :=
         Classify (Scheme, Has_In, Has_Not, Context).Possibility;
      Result.Necessity :=
         not Classify
             (  Scheme,
                Has_Out,
                Has_Not_Out,
                Context
             ) .Possibility;
      if Complement then
         return not Result;
      else
         return Result;
      end if;
   end Estimate;

   procedure Finalize (Scheme : in out Graph_Scheme_Object) is
   begin
      Free (Scheme.Set_Name);
      Finalize (Classifier_Object (Scheme));
   end Finalize;

   function Get_Class (Scheme : Graph_Scheme_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Classes (Scheme : Graph_Scheme_Object)
      return Feature_Handle is
      Classes : Feature_Object_Ptr;
   begin
      for Image in Scheme.Roots'Range loop
         if Is_Valid (Scheme.Roots (Image)) then
            Classes := Get_Classes (Ptr (Scheme.Roots (Image)).all);
            return Ref (Classes);
         end if;
      end loop;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Requesting class-feature for an empty graph-scheme"
      );
   end Get_Classes;

   procedure Get_Examples
             (  Scheme : Graph_Scheme_Object;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Count : Natural := 0;
      Node  : Graph_Node_Ptr;
   begin
      for Image in Scheme.Roots'Range loop
         Node := Ptr (Scheme.Roots (Image));
         if Node /= null then
            Count := Count + 1;
         end if;
      end loop;
      if Count /= 0 then
         Reset (Viewer.all, Count);
         for Image in Scheme.Roots'Range loop
            Node := Ptr (Scheme.Roots (Image));
            if Node /= null then
               Get_Examples (Node.all, Lesson, Image, Viewer);
            end if;
         end loop;
         Done (Viewer.all);
      end if;
   end Get_Examples;

   function Get_Features (Scheme : Graph_Scheme_Object)
      return Fuzzy.Feature.Handle.Container.Set is
      Result : Fuzzy.Feature.Handle.Container.Set;
   begin
      for Image in Scheme.Roots'Range loop
         if Is_Valid (Scheme.Roots (Image)) then
            Add (Result, Get_Features (Scheme.Roots (Image)));
         end if;
      end loop;
      return Result;
   end Get_Features;

   function Get_Graph
            (  Classifier : Classifier_Handle;
               Image      : Image_Type
            )  return Node_Handle is
   begin
      return Get_Graph (Ptr (Classifier).all, Image);
   end Get_Graph;

   function Get_Graph
            (  Classifier : Classifier_Object'Class;
               Image      : Image_Type
            )  return Node_Handle is
   begin
      return Graph_Scheme_Object'Class (Classifier).Roots (Image);
   end Get_Graph;

   function Get_Number_Of_Nodes (Classifier : Classifier_Handle)
      return Natural is
   begin
      return Get_Number_Of_Nodes (Ptr (Classifier).all);
   end Get_Number_Of_Nodes;

   function Get_Number_Of_Nodes (Classifier : Classifier_Object'Class)
      return Natural is
      Scheme : Graph_Scheme_Object'Class renames
                  Graph_Scheme_Object'Class (Classifier);
      Result : Natural := 0;
      Node   : Graph_Node_Ptr;
   begin
      for Image in Scheme.Roots'Range loop
         Node := Ptr (Scheme.Roots (Image));
         if Node /= null then
            Result :=
               Result + Get_Children_Number (Node.all, False) + 1;
         end if;
      end loop;
      return Result;
   end Get_Number_Of_Nodes;

   procedure Get_Referents
             (  Scheme : Graph_Scheme_Object;
                List   : in out Deposit_Container'Class
             )  is
      Node : Graph_Node_Ptr;
   begin
      for Image in Scheme.Roots'Range loop
         Node := Ptr (Scheme.Roots (Image));
         if Node /= null then
            Add (List, To_Deposit_Ptr (Node), False);
         end if;
      end loop;
   end Get_Referents;

   function Get_Training_Set_From (Classifier : Graph_Scheme_Object)
      return Positive is
   begin
      return Classifier.From;
   end Get_Training_Set_From;

   function Get_Training_Set_Length (Classifier : Graph_Scheme_Object)
      return Natural is
   begin
      return Classifier.Length;
   end Get_Training_Set_Length;

   function Get_Training_Set_Name (Classifier : Graph_Scheme_Object)
      return String is
   begin
      if Classifier.Set_Name = null then
         return "";
      else
         return Classifier.Set_Name.all;
      end if;
   end Get_Training_Set_Name;

   function Is_Modified (Scheme : Graph_Scheme_Object) return Boolean is
      function Is_Modified (Node : Node_Handle) return Boolean is
      begin
         return Is_Valid (Node) and then Is_Modified (Ptr (Node).all);
      end Is_Modified;
   begin
      return
      (  Scheme.Updated
      or else
         Is_Modified (Scheme.Roots (Has_in))
      or else
         Is_Modified (Scheme.Roots (Has_Out))
      or else
         Is_Modified (Scheme.Roots (Has_Not))
      or else
         Is_Modified (Scheme.Roots (Has_Not_Out))
      );
   end Is_Modified;

   procedure Learn_Scheme
             (  Classifier : in out Graph_Scheme_Object;
                Context    : in out Graph_Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is
      Last : constant Natural :=
                Positive'Min
                (  Get_Examples_Number (Context.Lesson.all),
                   To
                );
      Status : Graph_Training_Indication_Data (Context'Access);
   begin
      if From <= Last then
         Set_Features (Context, Features);
         Set_Data (Context.Viewer.all, Status);
         Reset (Context. Viewer.all, 4 * (Last - From + 1));
         for Example in From..Last loop
            Select_Example (Context, Example);
            --
            -- Evaluation of Example_Cut [ N(r|r) ]
            --
            Context.Example_Cut := Confidence'Last;
            for Index in 1
                      .. Get_Features_Number (Context.Lesson.all) loop
               declare
                  Feature : Feature_Object'Class renames
                     Ptr (Get_Feature (Context.Lesson.all, Index)).all;
               begin
                  Context.Example_Cut :=
                     (  Context.Example_Cut
                     and not
                        Possibility
                        (  Get (Feature, Context'Access, Has_Out),
                           Get (Feature, Context'Access, Has_In)
                        )
                     and not
                        Possibility
                        (  Get (Feature, Context'Access, Has_Not_Out),
                           Get (Feature, Context'Access, Has_Not)
                     )  );
               end;
            end loop;
            for Image in Classifier.Roots'Range loop
               Learn (Classifier.Roots (Image), Context, Image);
               Check (Context.Viewer.all);
            end loop;
         end loop;
         Done (Context.Viewer.all);
         Set_Data (Context.Viewer.all);
         if Classifier.Set_Name = null then
            if Context.Length > 0 then
               Classifier.Set_Name := new String'(Context.Set_Name);
            end if;
         else
            if Context.Set_Name /= Classifier.Set_Name.all then
               Free (Classifier.Set_Name);
               if Context.Length > 0 then
                  Classifier.Set_Name := new String'(Context.Set_Name);
               end if;
            end if;
         end if;
         Classifier.From   := From;
         Classifier.Length := Last - From + 1;
      end if;
   exception
      when others =>
         Set_Data (Context.Viewer.all);
         raise;
   end Learn_Scheme;

   procedure Learn
             (  Classifier : in out Graph_Scheme_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is
   begin
      if Context in Graph_Training_Data'Class then
         declare
            Data : Graph_Training_Data'Class renames
                   Graph_Training_Data'Class (Context);
         begin
            Learn_Scheme (Classifier, Data, Features, From, To);
         end;
      else
         for Image in Classifier.Roots'Range loop
            if Is_Valid (Classifier.Roots (Image)) then
               declare
                  Root    : Graph_Node'Class renames
                               Ptr (Classifier.Roots (Image)).all;
                  Classes : constant Feature_Object_Ptr :=
                               Get_Classes (Root);
                  Data    : Graph_Training_Data
                            (  Context.Lesson,
                               Context.Length,
                               Classes.Cardinality,
                               Context.Viewer
                            );
               begin
                  Data.Set_Name    := Context.Set_Name;
                  Data.Factory     := Get_Factory (Root);
                  Data.Classes     := Ref (Classes);
                  Data.Threshold   := Context.Threshold;
                  Data.Equivalence := Context.Equivalence;
                  Learn_Scheme (Classifier, Data, Features, From, To);
                  return;
               end;
            end if;
         end loop;
         Raise_Exception
         (  Constraint_Error'Identity,
            "Learning an empty graph-scheme"
         );
      end if;
   end Learn;

   procedure Reset_Modified (Scheme : in out Graph_Scheme_Object) is
   begin
      Scheme.Updated := False;
   end Reset_Modified;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
   begin
      if Pointer not in Source'First..Source'Last + 1 then
         raise Layout_Error;
      end if;
      Object := new Graph_Scheme_Object;
      declare
         Node     : Graph_Node_Ptr;
         Index    : aliased Integer := Pointer;
         Referent : Positive := 1;
         Result   : Graph_Scheme_Object renames
                       Graph_Scheme_Object (Object.all);
      begin
         for Image in Image_Type loop
            if Source (Index) = '+' then
               begin
                  Node := To_Graph_Node_Ptr (Get (List, Referent));
                  Referent := Referent + 1;
               exception
                  when Constraint_Error =>
                     Raise_Exception
                     (  Use_Error'Identity,
                        Image_Type'Image (Image) & " is not a node"
                     );
               end;
               Set_Graph (Result, Ref (Node), Image);
            elsif Source (Index) /= '-' then
               Raise_Exception
               (  Data_Error'Identity,
                  "Invalid node use refernece"
               );
            end if;
            Index := Index + 1;
         end loop;
         Get (Source, Index);
         if Index <= Source'Last and then Source (Index) = '"' then
            Result.Set_Name :=
               new String'(Get_Quoted (Source, Index'Access));
            if Index <= Source'Last and then Source (Index) = ':' then
               Index := Index + 1;
               Get (Source, Index, Result.From, First => 1);
               if Index <= Source'Last and then Source (Index) = ':'
               then
                  Index := Index + 1;
                  Get (Source, Index, Result.Length, First => 0);
               end if;
            end if;
         end if;
         Pointer := Index;
         Result.Updated := False;
      end;
   exception
      when Constraint_Error =>
         Free (Object);
         raise Data_Error;
      when others =>
         Free (Object);
         raise;
   end Restore;

   procedure Set_Graph
             (  Classifier : in out Classifier_Handle;
                Node       : Node_Handle;
                Image      : Image_Type
             )  is
   begin
      Set_Graph
      (  Ptr (Classifier).all,
         Node,
         Image
      );
   end Set_Graph;

   procedure Set_Graph
             (  Classifier : in out Classifier_Object'Class;
                Node       : Node_Handle;
                Image      : Image_Type
             )  is
      Scheme : Graph_Scheme_Object'Class renames
                  Graph_Scheme_Object'Class (Classifier);
   begin
      if Scheme.Roots (Image) = Node then
         return;
      end if;
      if Is_Valid (Node) then
         declare
            Root : Graph_Node_Ptr;
         begin
            for Set_Image in Scheme.Roots'Range loop
               if Image /= Set_Image then
                  Root := Ptr (Scheme.Roots (Set_Image));
                  if Root /= null then
                     exit when
                        Equal
                        (  Get_Classes (Root.all).all,
                           Ptr (Get_Classes (Node)).all
                        );
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Setting incompatible root node"
                     );
                  end if;
               end if;
            end loop;
         end;
      end if;
      Scheme.Roots (Image) := Node;
      Scheme.Updated := True;
   end Set_Graph;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Scheme      : Graph_Scheme_Object
             )  is
   begin
      if (  Pointer not in Destination'Range
         or else
            Destination'Last - Pointer < 3
         )
      then
         raise Layout_Error;
      end if;
      for Image in Scheme.Roots'Range loop
         if Is_Valid (Scheme.Roots (Image)) then
            Destination (Pointer) := '+';
         else
            Destination (Pointer) := '-';
         end if;
         Pointer := Pointer + 1;
      end loop;
      if Scheme.Set_Name = null then
         Put (Destination, Pointer, """""");
      else
         Put (Destination, Pointer, Quote (Scheme.Set_Name.all));
      end if;
      Put (Destination, Pointer, ':');
      Put (Destination, Pointer, Scheme.From);
      Put (Destination, Pointer, ':');
      Put (Destination, Pointer, Scheme.Length);
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Graph.Scheme;
