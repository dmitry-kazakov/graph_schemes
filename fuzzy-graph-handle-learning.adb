--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Learning                 Luebeck            --
--  Implementation                                 Spring, 2002       --
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

with Ada.Calendar;   use Ada.Calendar;
with Fuzzy.Lecture;  use Fuzzy.Lecture;

with Fuzzy.Graph.Learning.Implementation.Necessity;

package body Fuzzy.Graph.Handle.Learning is

   procedure Check
             (  Context : in out Graph_Training_Data'Class;
                Feature : Feature_Object'Class;
                Set     : Fuzzy.Set
             )  is
      pragma Inline (Check);
   begin
      if Possibility (Set) < Confidence'Last then
         Add_Example (Context, Feature);
      end if;
   end Check;

   package Do_Has_In is
      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                );
      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                );
      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence;
      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean;
      pragma Inline (Get);
      pragma Inline (Is_Known);
   end Do_Has_In;

   package Do_Has_Not is
      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                );
      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                )  renames Do_Has_In.Get;
      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence;
      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean;
      pragma Inline (Get);
      pragma Inline (Is_Known);
   end Do_Has_Not;

   package Do_Has_Out is
      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                );
      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                );
      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence;
      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean;
      pragma Inline (Get);
      pragma Inline (Is_Known);
   end Do_Has_Out;

   package Do_Has_Not_Out is
      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                );
      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                )  renames Do_Has_Out.Get;
      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence;
      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean;
      pragma Inline (Get);
      pragma Inline (Is_Known);
   end Do_Has_Not_Out;

   procedure Replace
             (  Target     : in out Fuzzy.Set;
                Constraint : Domain_Subset;
                Value      : Confidence
             )  is
   begin
      for Index in Target'Range loop
         if Constraint (Index) then
            Target (Index) := Value;
         end if;
      end loop;
   end Replace;

   package body Do_Has_In is

      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                )  is
      begin
         Distribution.Possibility :=
            Get (Ptr (Context.Classes).all, Context'Access, Has_In);
         And_At (Distribution.Possibility, Weight);
         Distribution.Necessity :=
            Get (Ptr (Context.Classes).all, Context'Access, Has_Not);
         And_At (Distribution.Necessity, Weight);
         Not_At (Distribution.Necessity);
      end Get;

      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                )  is
      begin
         Result := Get (Feature, Context'Access, Has_In);
         Check (Context, Feature, Result);
         And_At (Result, Weight);
      end Get;

      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence is
      begin
         return Get (Feature, Context, Has_In, Value);
      end Get;

      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean is
      begin
         return Is_Known (Feature, Context, Has_In);
      end Is_Known;
   end Do_Has_In;

   package body Do_Has_Not is

      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                )  is
      begin
         if Equal (Feature, Ptr (Context.Selected).all) then
            Result := Get (Feature, Context'Access, Has_Not);
            Check (Context, Feature, Result);
            And_At (Result, Weight);
         else
            for Index in Result'Range loop
               Result (Index) := Weight;
            end loop;
         end if;
      end Get;

      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence is
      begin
         if Equal (Feature, Ptr (Context.Selected).all) then
            return Get (Feature, Context, Has_Not, Value);
         else
            return Confidence'Last;
         end if;
      end Get;

      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean is
      begin
         if Equal (Feature, Ptr (Context.Selected).all) then
            return Is_Known (Feature, Context, Has_Not);
         else
            return True;
         end if;
      end Is_Known;
   end Do_Has_Not;

   package body Do_Has_Out is
      procedure Get
                (  Context      : in out Graph_Training_Data'Class;
                   Weight       : Confidence;
                   Distribution : in out Classification
                )  is
         Constraint : Domain_Subset renames
            Get_Constraint (Ptr (Context.Classes).all, Context'Access);
      begin
         Distribution.Possibility := (others => Confidence'Last);
         Distribution.Necessity   := (others => Confidence'First);
         if Context.Pro then
            Replace (Distribution.Possibility, Constraint, Weight);
         else
            Replace (Distribution.Necessity, Constraint, not Weight);
         end if;
      end Get;

      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                )  is
      begin
         if Context.Example_Cut > Weight then
            Result := Get (Feature, Context'Access, Has_Out);
            Or_At  (Result, Context.Class_Cut);
            Rem_At (Result, Context.Example_Cut);
            And_At (Result, Weight);
         else
            for Index in Result'Range loop
               Result (Index) := Weight;
            end loop;
         end if;
      end Get;

      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence is
      begin
         return
         (  (  Get (Feature, Context, Has_Out, Value)
            or Context.Class_Cut
            )
         rem
            Context.Example_Cut
         );
      end Get;

      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean is
      begin
         return Is_Known (Feature, Context, Has_Out);
      end Is_Known;
   end Do_Has_Out;

   package body Do_Has_Not_Out is
      procedure Get
                (  Context : in out Graph_Training_Data'Class;
                   Feature : Feature_Object'Class;
                   Weight  : Confidence;
                   Result  : in out Fuzzy.Set
                )  is
      begin
         if (  Equal (Feature, Ptr (Context.Selected).all)
            and then
               Context.Example_Cut > Weight
            )
         then
            Result := Get (Feature, Context'Access, Has_Not_Out);
            Or_At  (Result, Context.Class_Cut);
            Rem_At (Result, Context.Example_Cut);
            And_At (Result, Weight);
         else
            for Index in Result'Range loop
               Result (Index) := Weight;
            end loop;
         end if;
      end Get;

      function Get
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class;
                  Value   : Positive
               )  return Confidence is
      begin
         if Equal (Feature, Ptr (Context.Selected).all) then
            return
            (  (  Get (Feature, Context, Has_Not_Out, Value)
               or Context.Class_Cut
               )
            rem
               Context.Example_Cut
            );
         else
            return Confidence'Last;
         end if;
      end Get;

      function Is_Known
               (  Context : not null access Graph_Training_Data'Class;
                  Feature : Feature_Object'Class
               )  return Boolean is
      begin
         if Equal (Feature, Ptr (Context.Selected).all) then
            return Is_Known (Feature, Context, Has_Not_Out);
         else
            return True;
         end if;
      end Is_Known;
   end Do_Has_Not_Out;

   package From_Has_In is
      new Fuzzy.Graph.Learning.Implementation
          (  Combine_With_Or => True,
             Current_Image   => Has_In,
             Get             => Do_Has_In.Get,
             Get_Leaf        => Do_Has_In.Get,
             Get_Point       => Do_Has_In.Get,
             Is_Known        => Do_Has_In.Is_Known
          );

   package From_Has_Out is
      new Fuzzy.Graph.Learning.Implementation
          (  Combine_With_Or => False,
             Current_Image   => Has_Out,
             Get             => Do_Has_Out.Get,
             Get_Leaf        => Do_Has_Out.Get,
             Get_Point       => Do_Has_Out.Get,
             Is_Known        => Do_Has_Out.Is_Known
          );
   procedure Learn_From_Has_Out is new From_Has_Out.Necessity;

   package From_Has_Not is
      new Fuzzy.Graph.Learning.Implementation
          (  Combine_With_Or => True,
             Current_Image   => Has_Not,
             Get             => Do_Has_Not.Get,
             Get_Leaf        => Do_Has_Not.Get,
             Get_Point       => Do_Has_Not.Get,
             Is_Known        => Do_Has_Not.Is_Known
          );

   package From_Has_Not_Out is
      new Fuzzy.Graph.Learning.Implementation
          (  Combine_With_Or => False,
             Current_Image   => Has_Not_Out,
             Get             => Do_Has_Not_Out.Get,
             Get_Leaf        => Do_Has_Not_Out.Get,
             Get_Point       => Do_Has_Not_Out.Get,
             Is_Known        => Do_Has_Not_Out.Is_Known
          );
   procedure Learn_From_Has_Not_Out is new From_Has_Not_Out.Necessity;

   procedure Learn
             (  Node    : in out Node_Handle;
                Context : in out Graph_Training_Data'Class;
                Image   : Image_Type
             )  is
      use Features_Lists;
      subtype Distribution_Set is
         Fuzzy.Set (1..Get_Cardinality (Context.Classes));
      Start       : constant Time := Clock;
      Example_Cut : Confidence := Confidence'Last;
   begin
      case Image is
         when Has_In =>
            From_Has_In.Update (Node, Context);
         when Has_Out =>
            declare
               Distribution : Distribution_Set;
               Classes      : Feature_Object'Class renames
                                 Ptr (Context.Classes).all;
            begin
               Context.Pro  := False;
               Distribution := Get (Classes, Context'Access, Has_Not);
               Learn_From_Has_Out (Node, Context, Distribution);
               Context.Pro  := True;
               Distribution := Get (Classes, Context'Access, Has_In);
               Learn_From_Has_Out (Node, Context, Distribution);
            end;
         when Has_Not =>
            for Index in 1..Get_Size (Context.Features) loop
               Context.Selected :=
                  Get (Context.Features, Index).Feature;
               if Is_Valid (Context.Selected) then
                  From_Has_Not.Update (Node, Context);
               end if;
            end loop;
         when Has_Not_Out =>
            declare
               Distribution : Distribution_Set;
               Classes      : Feature_Object'Class renames
                                 Ptr (Context.Classes).all;
            begin
               for Index in 1..Get_Size (Context.Features) loop
                  Context.Selected :=
                     Get (Context.Features, Index).Feature;
                  if Is_Valid (Context.Selected) then
                     Context.Pro := False;
                     Distribution :=
                        Get (Classes, Context'Access, Has_Not);
                     Learn_From_Has_Not_Out
                     (  Node,
                        Context,
                        Distribution
                     );
                     Context.Pro := True;
                     Distribution :=
                        Get (Classes, Context'Access, Has_In);
                     Learn_From_Has_Not_Out
                     (  Node,
                        Context,
                        Distribution
                     );
                  end if;
               end loop;
            end;
      end case;
      declare
         Data : Graph_Image_Training_Statistics renames
                   Context.Profiling.Data (Image);
      begin
         Data.Current_Time := Clock - Start;
         Data.Total_Time   := Data.Total_Time + Data.Current_Time;
      end;
   end Learn;

   procedure Learn_Implementation
             (  Node     : in out Node_Handle;
                Context  : in out Graph_Training_Data'Class;
                Features : Feature_Array;
                Image    : Image_Type;
                From     : Positive;
                To       : Positive
             )  is
      Last : constant Natural :=
                Positive'Min
                (  Get_Examples_Number (Context.Lesson.all),
                   To
                );
   begin
      if From <= Last then
         declare
            Status : Graph_Training_Indication_Data (Context'Access);
         begin
            Set_Features (Context, Features);
            Set_Data (Context.Viewer.all, Status);
            Reset (Context. Viewer.all, Last - From + 1);
            for Example in From..Last loop
               Select_Example (Context, Example);
               Learn (Node, Context, Image);
               Check (Context.Viewer.all);
            end loop;
            Done (Context.Viewer.all);
            Set_Data (Context.Viewer.all);
         exception
            when others =>
               Set_Data (Context.Viewer.all);
         end;
      end if;
   end Learn_Implementation;

   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               Image       : Image_Type;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle is
      Context : Graph_Training_Data
                (  Ptr (Lesson),
                   0,
                   Get_Cardinality (Classes),
                   Viewer
                );
      Result  : Node_Handle;
   begin
      Context.Factory     := Factory.all'Unchecked_Access;
      Context.Classes     := Classes;
      Context.Threshold   := Threshold;
      Context.Equivalence := Equivalence;
      Learn_Implementation
      (  Node     => Result,
         Context  => Context,
         Features => Features,
         Image    => Image,
         From     => From,
         To       => To
      );
      return Result;
   end Learn;

   procedure Learn
             (  Node        : in out Node_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                Image       : Image_Type;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             )  is
      Context : Graph_Training_Data
                (  Ptr (Lesson),
                   0,
                   Get_Cardinality (Get_Classes (Node)),
                   Viewer
                );
      Result  : Node_Handle;
   begin
      Context.Factory     := Get_Factory (Ptr (Node).all);
      Context.Classes     := Get_Classes (Node);
      Context.Threshold   := Threshold;
      Context.Equivalence := Equivalence;
      Learn_Implementation
      (  Node     => Result,
         Context  => Context,
         Features => Features,
         Image    => Image,
         From     => From,
         To       => To
      );
   end Learn;

   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               Image       : Image_Type;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle is
      Result : Node_Handle;
   begin
      if Is_Valid (Viewer) then
         declare
            Context : Graph_Training_Data
                      (  Ptr (Lesson),
                         0,
                         Get_Cardinality (Classes),
                         Ptr (Viewer)
                      );
         begin
            Context.Factory     := Factory.all'Unchecked_Access;
            Context.Classes     := Classes;
            Context.Threshold   := Threshold;
            Context.Equivalence := Equivalence;
            Learn_Implementation
            (  Node     => Result,
               Context  => Context,
               Features => Features,
               Image    => Image,
               From     => From,
               To       => To
            );
         end;
      else
         declare
            Context : Graph_Training_Data
                      (  Ptr (Lesson),
                         0,
                         Get_Cardinality (Classes),
                         Negleter'Access
                      );
         begin
            Context.Factory     := Factory.all'Unchecked_Access;
            Context.Classes     := Classes;
            Context.Threshold   := Threshold;
            Context.Equivalence := Equivalence;
            Learn_Implementation
            (  Node     => Result,
               Context  => Context,
               Features => Features,
               Image    => Image,
               From     => From,
               To       => To
            );
         end;
      end if;
      return Result;
   end Learn;

   procedure Learn
             (  Node        : in out Node_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                Image       : Image_Type;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         declare
            Context : Graph_Training_Data
                      (  Ptr (Lesson),
                         0,
                         Get_Cardinality (Get_Classes (Node)),
                         Ptr (Viewer)
                      );
         begin
            Context.Factory     := Get_Factory (Ptr (Node).all);
            Context.Classes     := Get_Classes (Node);
            Context.Threshold   := Threshold;
            Context.Equivalence := Equivalence;
            Learn_Implementation
            (  Node     => Node,
               Context  => Context,
               Features => Features,
               Image    => Image,
               From     => From,
               To       => To
            );
         end;
      else
         declare
            Context : Graph_Training_Data
                      (  Ptr (Lesson),
                         0,
                         Get_Cardinality (Get_Classes (Node)),
                         Negleter'Access
                      );
         begin
            Context.Factory     := Get_Factory (Ptr (Node).all);
            Context.Classes     := Get_Classes (Node);
            Context.Threshold   := Threshold;
            Context.Equivalence := Equivalence;
            Learn_Implementation
            (  Node     => Node,
               Context  => Context,
               Features => Features,
               Image    => Image,
               From     => From,
               To       => To
            );
         end;
      end if;
   end Learn;

end Fuzzy.Graph.Handle.Learning;
