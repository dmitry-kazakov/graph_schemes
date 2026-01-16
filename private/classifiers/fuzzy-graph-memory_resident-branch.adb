--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident.                Luebeck            --
--        Branch                                   Autumn, 2005       --
--  Interface                                                         --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Ada.Tags;                     use Ada.Tags;
with Fuzzy.Feature;                use Fuzzy.Feature;
with Fuzzy.Graph.Node_Class_Sets;  use Fuzzy.Graph.Node_Class_Sets;
with Fuzzy.Edit;                   use Fuzzy.Edit;
with System;                       use System;

package body Fuzzy.Graph.Memory_Resident.Branch is
   type Node_Ptr is access constant Graph_Node'Class;
   Class : constant String := Node_Class & "Branch";
--
-- Create_Unchecked -- A new branch node
--
--    Header    - The graph header
--    Feature   - The node feature
--    Successor - The outgoing arcs
--    Sequence  - The sum of sequence numbers of the arcs
--
-- Returns :
--
--    Pointer to the node
--
   function Create_Unchecked
            (  Header    : Header_Handle;
               Feature   : Feature_Object'Class;
               Successor : Node_Ptr_Array;
               Sequence  : Sequence_No
            )  return Graph_Node_Ptr;
--
-- Get_Children -- Get distinct children of a node
--
--    Node     - Parent node
--    Children - The result (buffer to accept children nodes)
--    Pointer  - Index in the buffer
--
-- Children  are  placed into the array Children starting from the index
-- specified by Pointer which is then advanced.
--
   procedure Get_Children
             (  Node     : Branch_Node;
                Children : in out Node_Ptr_Array;
                Pointer  : in out Integer
             );
--
-- Modify_Unchecked -- A branch node
--
--    Parent    - The node to modify
--    Successor - The outgoing arcs of the result
--    Exclusive - Controls new node creation
--    New_Node  - The result if a new node is created
--
   procedure Modify_Unchecked
             (  Parent    : in out Branch_Node;
                Successor : in out Node_Ptr_Array;
                Exclusive : Boolean;
                New_Node  : in out Graph_Node_Ptr
             );

   procedure Next_Child
             (  Successor : Node_Ptr_Array;
                Index     : in out Positive;
                Child     : in out Graph_Node_Ptr
             )  is
      pragma Inline (Next_Child);
   begin
      while Index <= Successor'Last loop
         Child := Successor (Index);
         if Child /= null then
            return;
         end if;
         Index := Index + 1;
      end loop;
      Child := null;
   end Next_Child;

   procedure Or_Mul
             (  X      : in out Fuzzy.Set;
                Y      : Fuzzy.Set;
                Level  : Divergence;
                Weight : Confidence
             )  is
      pragma Inline (Or_Mul);
   begin
      for Index in X'Range loop
         X (Index) :=
            (  X (Index)
            or (  To_Confidence (Divergence (Y (Index)) * Level)
               and
                  Weight
            )  );
      end loop;
   end Or_Mul;

   procedure And_Mul
             (  X      : in out Fuzzy.Set;
                Y      : Fuzzy.Set;
                Level  : Divergence;
                Weight : Confidence
             )  is
      pragma Inline (And_Mul);
   begin
      for Index in X'Range loop
         X (Index) :=
            (  X (Index)
            and
               (  To_Confidence (Divergence (Y (Index)) * Level)
               or Weight
            )  );
      end loop;
   end And_Mul;

   procedure Add
             (  X      : in out Classification;
                Y      : Classification;
                Level  : Divergence;
                Weight : Confidence
             )  is
      pragma Inline (Add);
   begin
      Or_Mul  (X.Possibility, Y.Possibility, Level, Weight);
      And_Mul (X.Necessity, Y.Necessity, 1.0 - Level, not Weight);
   end Add;

   procedure Add
             (  X      : in out Classification;
                Y      : Classification;
                Z      : Classification;
                Level  : Divergence;
                Weight : Confidence
             )  is
      pragma Inline (Add);
      Not_Level  : constant Divergence := 1.0 - Level;
      Not_Weight : constant Confidence := not Weight;
   begin
      for Index in 1..X.Cardinality loop
         X.Possibility (Index) :=
            (  X.Possibility (Index)
            or (  To_Confidence
                  (  Divergence (Y.Possibility (Index)) * Level
                  +  Divergence (Z.Possibility (Index)) * Not_Level
                  )
               and
                  Weight
            )  );
         X.Necessity (Index) :=
            (  X.Necessity (Index)
            and
               (  To_Confidence
                  (  Divergence (Y.Necessity (Index)) * Not_Level
                  +  Divergence (Z.Necessity (Index)) * Level
                  )
               or
                  Not_Weight
            )  );
      end loop;
   end Add;

   procedure Classify
             (  Node   : Branch_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             )  is
      Follow  : Confidence;
      Feature : Feature_Object'Class renames Get_Feature (Node).all;

      Snap        : Context_Snap;
      Left_Child  : Graph_Node_Ptr;
      Right_Child : Graph_Node_Ptr;
      Left_Index  : Positive := 1;
      Right_Index : Positive := 1;

      procedure Check_Arc (Index : Positive) is
         pragma Inline (Check_Arc);
         Current : Confidence;
      begin
         Current := Get (Feature, Data'Access, Data.Image, Index);
         if Current > Data.Threshold then
            Follow := Follow or Current;
         else
            Set_Constraint (Feature, Data, Index);
         end if;
      end Check_Arc;

      procedure Check_Arc
                (  Value : in out Fuzzy.Set;
                   Index : Positive
                )  is
         pragma Inline (Check_Arc);
         Current : Confidence;
      begin
         Current := Get (Feature, Data'Access, Data.Image, Index);
         if Current > Data.Threshold then
            Follow := Follow or Current;
            Value (Index) := Current;
         else
            Set_Constraint (Feature, Data, Index);
            Value (Index) := Confidence'First;
         end if;
      end Check_Arc;

      procedure Check_Arc
                (  Value  : in out Fuzzy.Set;
                   Index  : Positive;
                   Weight : Divergence
                )  is
         Current : Confidence;
         pragma Inline (Check_Arc);
      begin
         Current := Get (Feature, Data'Access, Data.Image, Index);
         Value (Index) := Current;
         Current := Current and To_Confidence (Weight);
         if Current > Data.Threshold then
            Follow := Follow or Current;
         else
            Set_Constraint (Feature, Data, Index);
            Value (Index) := Confidence'First;
         end if;
      end Check_Arc;

   begin
      if Weight <= Data.Threshold then
         return;   -- Nothing can increase Weight
      end if;
      Create_Constraint (Feature, Data, False);
      Next_Child (Node.Successor, Left_Index, Left_Child);
      case Data.Generalize is
         when None =>
            --
            -- No  generalization.  Only  paths  that  have children are
            -- navigated.
            --
            while Left_Child /= null loop
               Follow := Confidence'First;
               Set_Constraint (Feature, Data, Left_Index, True);
               Check_Arc (Left_Index);
               --
               -- The consequent paths leading to the same child  are
               -- collected here to improve performance.
               --
               Right_Index := Left_Index + 1;
               loop
                  Next_Child
                  (  Node.Successor,
                     Right_Index,
                     Right_Child
                  );
                  exit when Right_Child /= Left_Child;
                  Set_Constraint (Feature, Data, Right_Index, True);
                  Check_Arc (Right_Index);
                  Right_Index := Right_Index + 1;
               end loop;
               if Follow > Data.Threshold then
                  Classify (Left_Child.all, Data, Follow and Weight);
                  if Data.Ready then
                     return;
                  end if;
               end if;
               Set_Constraint_Range   -- Disable visited values
               (  Feature,
                  Data,
                  Left_Index,
                  Right_Index - 1,
                  False
               );
               Left_Child := Right_Child;
               Left_Index := Right_Index;
            end loop;
         when Nearest =>
            --
            -- Generalization  to  the  nearest neighbour. All the paths
            -- leading to no children are navigated when selected by the
            -- classified example. The child to navigate is the  nearest
            -- neighbour.
            --
            declare
               From : Positive := 1;
            begin
               while Left_Child /= null loop
                  --
                  -- The consequent paths leading to the same child  are
                  -- collected here to improve performance.
                  --
                  Right_Index := Left_Index + 1;
                  loop
                     Next_Child
                     (  Node.Successor,
                        Right_Index,
                        Right_Child
                     );
                     exit when Right_Child /= Left_Child;
                     Right_Index := Right_Index + 1;
                  end loop;
                  if Right_Child = null then
                     Left_Index := Feature.Cardinality;
                  else
                     Left_Index := (Left_Index + Right_Index) / 2;
                  end if;
                  --
                  -- Here From..Left_Index is the range of values served
                  -- by Left_Child.
                  --
                  Set_Constraint_Range   -- Allow this range
                  (  Feature,
                     Data,
                     From,
                     Left_Index,
                     True
                  );
                  Follow := Confidence'First;
                  for Index in From..Left_Index loop
                     Check_Arc (Index);
                  end loop;
                  if Follow > Data.Threshold then
                     Classify
                     (  Left_Child.all,
                        Data,
                        Follow and Weight
                     );
                     if Data.Ready then
                        return;
                     end if;
                  end if;
                  Set_Constraint_Range   -- Disallow processed values
                  (  Feature,
                     Data,
                     From,
                     Left_Index,
                     False
                  );
                  From       := Left_Index + 1;
                  Left_Index := Right_Index;
                  Left_Child := Right_Child;
               end loop;
            end;
         when Linear =>
            --
            -- Generalization   by   linear   interpolation    of    the
            -- classification results between two  children.  These  are
            -- made under the constraint as if either would be navigated
            -- through the path.
            --
            declare
               Previous         : Classification (Data.Cardinality);
               Result           : Classification := Data.Result;
               Value            : Fuzzy.Set (1..Node.Cardinality);
               Has_Left         : Boolean  := False;
               Left_Index_First : Positive := 1;
               Left_Index_Left  : Positive;
            begin
               Left_Index_Left := Left_Index;
               while Left_Child /= null loop
                  --
                  -- The consequent paths leading to the same child  are
                  -- collected here to improve performance.
                  --
                  Right_Index := Left_Index + 1;
                  loop
                     Next_Child
                     (  Node.Successor,
                        Right_Index,
                        Right_Child
                     );
                     exit when Right_Child /= Left_Child;
                     Left_Index  := Right_Index;
                     Right_Index := Right_Index + 1;
                  end loop;
                  if Right_Child = null then
                     Left_Index := Feature.Cardinality;
                  end if;
                  --
                  -- Here  the  set of values served by Left_Child looks
                  -- like:
                  --
                  --   Left_Index_Left __________________ Left_Index
                  --                  /|/ / / / / / / / |\
                  --                 / | / / / / / / / /| \
                  --                /  |/ / / / / / / / |  \
                  -- Left_Index_First  |<- Left_Child ->|   \Right_Index
                  --
                  Set_Constraint_Range
                  (  Feature,
                     Data,
                     Left_Index_First,
                     Right_Index - 1,
                     True
                  );
                  Follow := Confidence'First;
                  declare
                     Factor : Divergence;
                     Step   : Divergence;
                  begin
                     if Left_Index_First < Left_Index_Left then
                        Step :=
                           (  1.0
                           /  Divergence
                              (  Left_Index_Left
                              -  Left_Index_First
                           )  );
                        Factor := Step;
                        for Index in Left_Index_First
                                  .. Left_Index_Left - 1
                        loop
                           Check_Arc (Value, Index, Factor);
                           Factor := Factor + Step;
                        end loop;
                     end if;
                     for Index in Left_Index_Left..Left_Index loop
                        Check_Arc (Value, Index);
                     end loop;
                     if Left_Index < Right_Index - 1 then
                        Step :=
                           (  1.0
                           /  Divergence (Right_Index - Left_Index - 1)
                           );
                        Factor := 1.0 - Step;
                        for Index in Left_Index  + 1
                                  .. Right_Index - 1
                        loop
                           Check_Arc (Value, Index, Factor);
                           Factor := Factor - Step;
                        end loop;
                     end if;
                  end;
                  if Follow > Data.Threshold then
                     --
                     -- Here  we  classify the example using Left_Child.
                     -- The result will be stored in Data.Result.
                     --
                     Data.Result.Possibility :=
                        (others => Confidence'First);
                     Data.Result.Necessity :=
                        (others => Confidence'Last);
                     Classify
                     (  Left_Child.all,
                        Data,
                        Follow and Weight
                     );
                     --
                     -- Adding  the result of the classification for the
                     -- values  between  Left_Index_Left and Left_Index.
                     -- These values  are  all  classified  as  ones  of
                     -- Left_Child.
                     --
                     declare
                        Level : constant Confidence :=
                                   (  Possibility
                                      (  Value
                                         (  Left_Index_Left
                                         .. Left_Index
                                      )  )
                                   and
                                      Weight
                                   );
                     begin
                        if Level > Data.Threshold then
                           Or_At
                           (  Result.Possibility,
                              Data.Result.Possibility,
                              Level
                           );
                           And_At
                           (  Result.Necessity,
                              Data.Result.Necessity,
                              not Level
                           );
                        end if;
                     end;
                     --
                     -- The  left  shoulder of  Left_Child  is   treated
                     -- depending on whether there  is  a  node  on  the
                     -- left.
                     --
                     if Has_Left then
                        --
                        -- Interpolating   between   the  classification
                        -- result  of  Left_Child   and   the   previous
                        -- classification result.
                        --
                        if Left_Index_Left > Left_Index_First then
                           declare
                              Factor : Divergence;
                              Step   : Divergence;
                              Level  : Confidence;
                           begin
                              Step :=
                                 (  1.0
                                 /  Divergence
                                    (  Left_Index_Left
                                    -  Left_Index_First
                                 )  );
                              Factor := Step;
                              for Index in Left_Index_First
                                        .. Left_Index_Left - 1
                              loop
                                 Level := Value (Index) and Weight;
                                 if Level > Data.Threshold then
                                    Add
                                    (  Result,
                                       Data.Result,
                                       Previous,
                                       Factor,
                                       Level
                                    );
                                 end if;
                                 Factor := Factor + Step;
                              end loop;
                           end;
                        end if;
                     else
                        --
                        -- There  is  no  node  on  the  left.  So   the
                        -- classification    of    the   Left_Child   is
                        -- extrapolated.    The    shoulder  is   linear
                        -- ascending.  Its  conditional possibility with
                        -- the feature value is used as a multiplicative
                        -- for the truth values of the classification.
                        --
                        if Left_Index_Left > Left_Index_First then
                           declare
                              Factor : Divergence;
                              Step   : Divergence;
                              Level  : Divergence := 0.0;
                           begin
                              Step :=
                                 (  1.0
                                 /  Divergence
                                    (  Left_Index_Left
                                    -  Left_Index_First
                                 )  );
                              Factor := Step;
                              for Index in Left_Index_First
                                        .. Left_Index_Left - 1
                              loop
                                 Level :=
                                    Divergence'Max
                                    (  Divergence (Value (Index)),
                                       Factor
                                    );
                                 Factor := Factor + Step;
                              end loop;
                              Add (Result, Data.Result, Level, Weight);
                           end;
                        end if;
                     end if;
                     --
                     -- The  right  shoulder is used only if there is no
                     -- node on the right.
                     --
                     if Right_Child = null then
                        --
                        -- Extrapolate  right.  It is similar to dealing
                        -- with the left shoulder.
                        --
                        if Right_Index - Left_Index > 1 then
                           declare
                              Factor : Divergence;
                              Step   : Divergence;
                              Level  : Divergence := 0.0;
                           begin
                              Step :=
                                 (  1.0
                                 /  Divergence
                                    (  Right_Index
                                    -  Left_Index
                                    -  1
                                 )  );
                              Factor := 1.0 - Step;
                              for Index in Left_Index + 1
                                        .. Right_Index - 1
                              loop
                                 Level :=
                                    Divergence'Max
                                    (  Divergence (Value (Index)),
                                       Factor
                                    );
                                 Factor := Factor - Step;
                              end loop;
                              Add (Result, Data.Result, Level, Weight);
                           end;
                        end if;
                     else
                        --
                        -- Store the classification for interpolation of
                        -- the left shoulder of Right_Child.
                        --
                        Previous := Data.Result;
                        Has_Left := True;
                     end if;
                     Data.Ready := False;
                  end if;
                  Set_Constraint_Range
                  (  Feature,
                     Data,
                     Left_Index_First,
                     Left_Index,
                     False
                  );
                  Left_Index_First := Left_Index + 1;
                  Left_Index_Left  := Right_Index;
                  Left_Child       := Right_Child;
               end loop;
               Data.Result := Result;
               Data.Ready :=
                  (  (  Possibility (Data.Result.Possibility)
                     =  Confidence'Last
                     )
                  and then
                     (  Necessity (Data.Result.Necessity)
                     =  Confidence'First
                  )  );
            end;
      end case;
   end Classify;

   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr is
      Successor : constant Node_Ptr_Array (1..Feature.Cardinality) :=
                     Children;
      Child     : Graph_Node_Ptr;
      Sequence  : Sequence_No := 0;
      Empty     : Boolean     := True;
   begin
      if Children'Length /= Feature.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the children array"
         );
      end if;
      for Index in Successor'Range loop
         Child := Successor (Index);
         if Child /= null then
            Check_Child (Header, Child.all);
            Sequence := Sequence + Child.Sequence;
            Empty    := False;
         end if;
      end loop;
      if Empty then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Creating a branch with no children"
         );
      end if;
      return Create_Unchecked (Header, Feature, Successor, Sequence);
   end Create;

   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr is
      Successor : constant Node_Ptr_Array (1..Feature.Cardinality) :=
                     Children;
      Child     : Graph_Node_Ptr;
      Sequence  : Sequence_No := 0;
      Empty     : Boolean     := True;
   begin
      if Children'Length /= Feature.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the children array"
         );
      elsif Feature.Cardinality /= Weights'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the distribution"
         );
      end if;
      for Index in Successor'Range loop
         Child := Successor (Index);
         if (  Child /= null
            and then
               Weights (Index - 1 + Weights'First) /= Confidence'First
            )
         then
            Check_Child (Header, Child.all);
            Sequence := Sequence + Child.Sequence;
            Empty    := False;
         end if;
      end loop;
      if Empty then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Creating a branch with no children"
         );
      end if;
      return Create_Unchecked (Header, Feature, Successor, Sequence);
   end Create;

   function Create_Unchecked
            (  Header    : Header_Handle;
               Feature   : Feature_Object'Class;
               Successor : Node_Ptr_Array;
               Sequence  : Sequence_No
            )  return Graph_Node_Ptr is
      New_Ptr : Graph_Node_Ptr := new Branch_Node (Feature.Cardinality);
      Branch  : Branch_Node renames Branch_Node (New_Ptr.all);
   begin
      Branch.Self      := New_Ptr;
      Branch.Successor := Successor;
      Branch.Sequence  := Branch.Sequence + Sequence;
      for Index in Successor'Range loop
         if Successor (Index) /= null then
            Increment_Count (Successor (Index).all);
         end if;
      end loop;
      Attach (New_Ptr, Header, Feature);
      return New_Ptr;
   exception
      when others =>
         if New_Ptr /= null and then New_Ptr.Use_Count = 0 then
            Free (New_Ptr);
         end if;
         raise;
   end Create_Unchecked;

   procedure Finalize (Node : in out Branch_Node) is
   begin
      Close (Node);
      for Index in Node.Successor'Range loop
         Release (Node.Successor (Index));
      end loop;
      Finalize (Memory_Node (Node));
   end Finalize;

   function Find
            (  Parent : Branch_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural is
      Node : constant Node_Ptr := Child'Unchecked_Access;
   begin
      for Child in Index..Parent.Cardinality loop
         if Node = Node_Ptr (Parent.Successor (Index)) then
            return Child;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Get_Children
             (  Node     : Branch_Node;
                Children : in out Node_Ptr_Array;
                Pointer  : in out Integer
             )  is
      Place : Integer;
   begin
      for Index in Node.Successor'Range loop
         Add
         (  Children,
            Pointer,
            Node.Successor (Index),
            Place
         );
      end loop;
   end Get_Children;

   function Get_Child (Node : Branch_Node; Index : Positive)
      return Graph_Node_Ptr is
   begin
      if Index <= Node.Cardinality then
         return Node.Successor (Index);
      else
         return null;
      end if;
   end Get_Child;

   function Get_Children_Number
            (  Node      : Branch_Node;
               Immediate : Boolean := True
            )  return Natural is
      Children : Node_Ptr_Array (0..Node.Cardinality - 1);
      Result   : Natural := Children'First;
   begin
      Get_Children (Node, Children, Result);
      if Immediate then
         return Result;
      else
         for Index in 0..Result - 1 loop
            Result :=
               (  Result
               +  Get_Children_Number (Children (Index).all, False)
               );
         end loop;
         return Result;
      end if;
   end Get_Children_Number;

   function Get_Class (Node : Branch_Node) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Distribution (Node : Branch_Node)
      return Classification is
      Result : Classification (Node.Cardinality);
   begin
      for Index in Node.Successor'Range loop
         if Node.Successor (Index) = null then
            Result.Possibility (Index) := Confidence'First;
            Result.Necessity   (Index) := Confidence'First;
         else
            Result.Possibility (Index) := Confidence'Last;
            Result.Necessity   (Index) := Confidence'Last;
         end if;
      end loop;
      return Result;
   end Get_Distribution;

   procedure Get_Examples
             (  Node   : Branch_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Feature      : constant Feature_Handle :=
                        Ref (Get_Feature (Node));
      Distribution : Fuzzy.Set (Node.Successor'Range);
      Child        : Graph_Node_Ptr;
      First        : Natural;
      Skip         : Domain_Subset (Node.Successor'Range) :=
                        (others => False);
   begin
      Reset (Viewer.all, Node.Cardinality);
      for Index in Skip'Range loop
         if not Skip (Index) then
            Child := Node.Successor (Index);
            if Child /= null then
               Distribution := (others => Confidence'First);
               Distribution (Index) := Confidence'Last;
               for Next in Index + 1..Skip'Last loop
                  if (  not Skip (Next)
                     and then
                        Node.Successor (Next) = Child
                     )
                  then
                     Skip (Next) := True;
                     Distribution (Next) := Confidence'Last;
                  end if;
               end loop;
               First := Get_Examples_Number (Lesson) + 1;
               Get_Examples (Child.all, Lesson, Image, Viewer);
               for Example in First..Get_Examples_Number (Lesson) loop
                  Put
                  (  Lesson,
                     Example,
                     Feature,
                     Image,
                     Distribution
                  );
               end loop;
             end if;
          else
             Check (Viewer.all);
          end if;
       end loop;
       Done (Viewer.all);
   end Get_Examples;

   procedure Get_Referents
             (  Node : Branch_Node;
                List : in out Deposit_Container'Class
             )  is
   begin
      Add
      (  List,
         To_Deposit_Ptr (Ptr (Node.Layer.Header.Handle)),
         False
      );
      Add (List, To_Deposit_Ptr (Ptr (Node.Layer.Feature)), False);
      for Child in Node.Successor'Range loop
         Add (List, To_Deposit_Ptr (Node.Successor (Child)), False);
      end loop;
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Use_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Referents;

   function Get_Type (Node : Branch_Node)
      return Node_Type is
   begin
      return Tree_Branch;
   end Get_Type;

   function Like
            (  Left  : Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if (  Branch_Node'Tag /= Right'Tag
         or else
            Left.Cardinality /= Right.Cardinality
         )
      then
         return False;
      else
         return Left.Successor = Branch_Node'Class (Right).Successor;
      end if;
   end Like;

   procedure Modify_Unchecked
             (  Parent    : in out Branch_Node;
                Successor : in out Node_Ptr_Array;
                Exclusive : Boolean;
                New_Node  : in out Graph_Node_Ptr
             )  is
      Sequence : Sequence_No := 0;
      Same     : Boolean     := True;
      Child    : Graph_Node_Ptr;
   begin
      for Index in Successor'Range loop
         Child := Successor (Index);
         if Parent.Successor (Index) /= Child then
            Same := False;
            exit;
         end if;
         if Child /= null then
            Sequence := Sequence + Child.Sequence;
         end if;
      end loop;
      if Same and then Parent.Sequence = Sequence then
         return;
      elsif Exclusive then
         declare
            Old_Node  : constant Graph_Node_Ptr := Parent.Self;
            Used_Node : Graph_Node_Ptr := Old_Node;
         begin
            Remove (Parent.Layer.Nodes, Used_Node);
            for Index in Successor'Range loop
               Child := Successor (Index);
               if Child /= null then
                  Increment_Count (Child.all);
               end if;
            end loop;
            for Index in Parent.Successor'Range loop
               if Parent.Successor (Index) /= null then
                  Release (Parent.Successor (Index));
               end if;
            end loop;
            Parent.Successor := Successor;
            Insert (Parent.Layer.Nodes, Used_Node);
            if Old_Node = Used_Node then
               Parent.Sequence := Get_Sequence_No + Sequence;
            else
               New_Node     := Used_Node;
               Parent.Layer := null;
            end if;
         end;
      else
         New_Node :=
            Create_Unchecked
            (  Parent.Layer.Header,
               Ptr (Parent.Layer.Feature).all,
               Successor,
               Sequence
            );
      end if;
   end Modify_Unchecked;

   procedure Modify
             (  Node        : in out Branch_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean;
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             )  is
      Successor : Node_Ptr_Array (1..Node.Cardinality) := Children;
   begin
      if Node.Layer = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Modifying no graph"
         );
      end if;
      case Combination is
         when Union =>
            for Index in Successor'Range loop
               if Successor (Index) = null then
                  if Node.Successor (Index) /= null then
                     Successor (Index) := Node.Successor (Index);
                  end if;
               else
                  Check_Child (Node, Successor (Index).all);
               end if;
            end loop;
         when Intersection =>
            for Index in Successor'Range loop
               if Successor (Index) /= null then
                  if Node.Successor (Index) = null then
                     Successor (Index) := null;
                  else
                     Check_Child (Node, Successor (Index).all);
                  end if;
               end if;
            end loop;
         when Replacement =>
            for Index in Successor'Range loop
               if Successor (Index) /= null then
                  Check_Child (Node, Successor (Index).all);
               end if;
            end loop;
      end case;
      Modify_Unchecked (Node, Successor, Exclusive, New_Node);
   end Modify;

   procedure Modify
             (  Node         : in out Branch_Node;
                Data         : in out Node_Modification_Data'Class;
                Distribution : Classification;
                Exclusive    : Boolean;
                Combination  : Node_Modification;
                New_Node     : in out Graph_Node_Ptr
             )  is
   begin
      Raise_Exception
      (  Constraint_Error'Identity,
         "Modify was called on a branch node"
      );
   end Modify;

   function Precedent
            (  Left  : Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if Left.Cardinality = Right.Cardinality then
         if Branch_Node'Tag = Right'Tag then
            declare
               L : Node_Ptr_Array renames Left.Successor;
               R : Node_Ptr_Array renames Branch_Node (Right).Successor;
            begin
               for Index in L'Range loop
                  if L (Index) /= R (Index) then
                     return
                     (  L (Index) = null
                     or else
                        (  R (Index) /= null
                        and then
                           L (Index).all'Address < R (Index).all'Address
                     )  );
                  end if;
               end loop;
               return False;
            end;
         else
            return Branch_Node'External_Tag < External_Tag (Right'Tag);
         end if;
      else
         return Left.Cardinality < Right.Cardinality;
      end if;
   end Precedent;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Node    : out Deposit_Ptr
             )  is
      Header   : Header_Handle;
      Feature  : Feature_Handle;
      Node_Ptr : Graph_Node_Ptr;
   begin
      begin
         Header.Handle := Ref (To_Graph_Header_Ptr (Get (List, 1)));
         Feature := Ref (To_Feature_Object_Ptr (Get (List, 2)));
         if not Is_Valid (Header.Handle) then
            Raise_Exception
            (  Use_Error'Identity,
               "No graph for restored branch node"
            );
         elsif not Feature.Is_Valid then
            Raise_Exception
            (  Use_Error'Identity,
               "No feature for restored branch node"
            );
         end if;
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Use_Error'Identity,
               Exception_Message (Error)
            );
      end;
      Node_Ptr := new Branch_Node (Get_Cardinality (Feature));
      declare
         Result : Branch_Node renames Branch_Node (Node_Ptr.all);
         Used   : Fuzzy.Set (Result.Successor'Range);
         Child  : Graph_Node_Ptr;
         No     : Positive := 3;
      begin
         Result.Self := Node_Ptr;
         Get (Source, Pointer, Used);
         for Index in Used'Range loop
            if Used (Index) = Confidence'Last then
               Child := To_Graph_Node_Ptr (Get (List, No));
               Result.Successor (Index) := Child;
               Increment_Count (Child.all);
               Result.Sequence := Result.Sequence + Child.Sequence;
               No := No + 1;
            end if;
         end loop;
         Attach (Node_Ptr, Header, Ptr (Feature).all);
         Node := To_Deposit_Ptr (Node_Ptr);
         Reset_Modified (Node_Ptr.all);
      exception
         when Use_Error =>
            Raise_Exception
            (  Use_Error'Identity,
               "Circular dependency encountered"
            );
         when Error : Constraint_Error =>
            Raise_Exception
            (  Use_Error'Identity,
               Exception_Message (Error)
            );
      end;
   exception
      when others =>
         if Node_Ptr /= null and then Node_Ptr.Use_Count = 0 then
            Free (Node_Ptr);
         end if;
         raise;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Branch_Node
             )  is
      Used : Fuzzy.Set (Node.Successor'Range) :=
                (others => Confidence'First);
   begin
      for Index in Node.Successor'Range loop
         if Node.Successor (Index) /= null then
            Used (Index) := Confidence'Last;
         end if;
      end loop;
      Put (Destination, Pointer, Used);
   end Store;

   function Tests
            (  Node    : Branch_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean is
   begin
      if Node.Layer /= null then
         if Equal (Ptr (Node.Layer.Feature).all, Feature.all) then
            return True;
         end if;
         for Index in Node.Successor'Range loop
            if (  Node.Successor (Index) /= null
               and then
                  Tests (Node.Successor (Index).all, Feature)
               )
            then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Tests;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Graph.Memory_Resident.Branch;
