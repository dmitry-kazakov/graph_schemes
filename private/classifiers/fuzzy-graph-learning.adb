--                                                                    --
--  package Fuzzy.Graph.Learning    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
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

with Ada.Exceptions;      use Ada.Exceptions;
with Fuzzy.Graph.Handle;  use Fuzzy.Graph.Handle;

with Fuzzy.Feature.Classificatory.Clustering;
with Fuzzy.Graph.Handle.Transformations;

package body Fuzzy.Graph.Learning is
   use Bounded_Arrays;
   use Fuzzy.Feature.Sets;
   use Features_Lists;
   use Fuzzy.Feature.Classificatory.Clustering;

   generic
      type Array_Type (<>) is private;
      with function Get (Container : Array_Type; Index : Integer)
         return Feature_Object_Ptr is <>;
      with function First (Container : Array_Type)
         return Integer is <>;
      with function Last (Container : Array_Type)
         return Integer is <>;
   procedure Generic_Set_Data
             (  Context  : in out Graph_Training_Data'Class;
                Features : Array_Type
             );

   function "<" (Left, Right : Feature_Statistics_Ptr) return Boolean is
   begin
      return
      (  Left.Order > Right.Order
      or else
         (  Left.Order = Right.Order
         and then
            Left.Feature < Right.Feature
      )  );
   end "<";

   procedure Add_Example
             (  Data    : in out Graph_Training_Data;
                Feature : Feature_Object'Class
             )  is
   begin
      Add
      (  Get_Statistics (Data'Access, Feature).Erroneous,
         Data.Example
      );
   end Add_Example;

   procedure Add_Example
             (  Data    : in out Graph_Training_Data;
                Feature : Feature_Handle
             )  is
   begin
      Add
      (  Get_Statistics (Data'Access, Ptr (Feature).all).Erroneous,
         Data.Example
      );
   end Add_Example;

   procedure Adjust (Node : in out Branch_Proxy) is
   begin
      Adjust (Node_Proxy (Node));
      for Index in Node.Children'Range loop
         if Node.Children (Index) /= null then
            Increment_Count (Node.Children (Index).all);
         end if;
      end loop;
   end Adjust;

   function Allocate
            (  Cache : not null access Separation_Hypotheses_Cache
            )  return Separation_Hypotheses_Ptr is
   begin
      if Cache.Free = null then
         return new Separation_Hypotheses;
      else
         declare
            Result : constant Separation_Hypotheses_Ptr := Cache.Free;
         begin
            Cache.Free := Result.Next;
            return Result;
         end;
      end if;
   end Allocate;

   procedure Connect
             (  Parent : in out Branch_Proxy;
                Child  : Graph_Node_Ptr;
                Index  : Positive
             )  is
      Child_Ptr : constant Graph_Node_Ptr := Parent.Children (Index);
   begin
      if Child_Ptr /= Child then
         if Child_Ptr /= null then
            Parent.Children (Index) := null;
            Release (Child_Ptr);
         end if;
         if Child /= null then
            Parent.Children (Index) := Child;
            Increment_Count (Child.all);
         end if;
      end if;
   end Connect;

   procedure Commit
             (  Data : in out Graph_Training_Data'Class;
                Node : in out Branch_Proxy
             )  is
      New_Node : Graph_Node_Ptr;
   begin
      Modify
      (  Ptr (Node).all,
         Data,
         Node.Children,
         Node.Exclusive,
         Union,
         New_Node
      );
      if New_Node /= null then
         Set (Node, New_Node);
      end if;
   exception
      when others =>
         if New_Node /= null and then New_Node.Use_Count = 0 then
            Free (New_Node);
            raise;
         end if;
   end Commit;

   procedure Commit
             (  Data   : in out Graph_Training_Data'Class;
                Parent : in out Separation_Data;
                Child  : in out Separation_Data
             )  is
      use Fuzzy.Graph.Handle.Transformations;
      use Feature_Statistics_Maps;
      This_Feature : Feature_Object_Ptr;
      Best_Feature : Feature_Object_Ptr;
   begin
      if Is_Empty (Child.Separators.List) and then not Child.Splitter
      then
         return;
      end if;
      declare
         Node         : Graph_Node'Class renames Ptr (Child.Node).all;
         Best_Quality : Divergence_Range := Zero;
         This_Quality : Divergence_Range;
         Separator    : Feature_Object_Ptr;
      begin
         This_Feature := Get_Feature (Node);
         Best_Feature := This_Feature;
         Add (Child.Separators.List, This_Feature);
         if Get_Size (Child.Separators.List) > 1 then
            --
            -- There are many candidates. We choose the best among them,
            -- with  the  qualities  equal   to   each   other   up   to
            -- Data.Equivalence.
            --
            Erase (Data.Cache.Scratch);
            for Index in 1..Get_Size (Child.Separators.List) loop
               Separator := Get (Child.Separators.List, Index);
               This_Quality :=
                  Get_Quality
                  (  Ptr (Child.Node),
                     Separator.all,
                     Data.Cache'Unchecked_Access
                  );
               case Compare
                    (  This_Quality,
                       Best_Quality,
                       Data.Equivalence
                    )  is
                  when Lower =>
                     null;
                  when Same =>
                     if Separator = This_Feature then
                        Best_Feature := This_Feature;
                     end if;
                     Add (Data.Cache.Scratch, Separator);
                  when Higher =>
                     Best_Quality := This_Quality;
                     Best_Feature := Separator;
                     Erase (Data.Cache.Scratch);
                     Add (Data.Cache.Scratch, Separator);
               end case;
            end loop;
            --
            -- Reporting  the  leaders  to  the  parent only if a parent
            -- exists  and  the  leaders which are not the node feature,
            -- that is not a splitter. The  node  feature  is  always  a
            -- candidate.  But  if it does not split, we will not bother
            -- the parent.
            --
            if (  Is_Valid (Parent.Node)
               and then
                  (  Best_Feature /= This_Feature
                  or else
                     Child.Splitter
                  or else
                     Get_Size (Data.Cache.Scratch) > 1
               )  )
            then
               Add (Parent.Separators.List, Data.Cache.Scratch);
            end if;
         else
            --
            -- There is only one candidate. Let's take it for the  best,
            -- without estimating its quality.
            --
            Best_Feature := Get (Child.Separators.List, 1);
            if (  Is_Valid (Parent.Node)
               and then
                  (  Best_Feature /= This_Feature
                  or else
                     Child.Splitter
               )  )
            then
               Add (Parent.Separators.List, Best_Feature);
            end if;
         end if;
      end;
      if Best_Feature /= This_Feature and then Best_Feature /= null then
         --
         -- Rotate to the best feature found
         --
         declare
            This : constant Feature_Statistics_Ptr :=
                      Get_Statistics
                      (  Data'Unchecked_Access,
                         This_Feature.all
                      );
            Statistics : Feature_Statistics'Class renames This.all;
         begin
            Statistics.Down  := Statistics.Down + 1;
            Statistics.Depth :=
               Statistics.Depth + Float (Data.Depth + 1);
            Statistics.Order :=
               (  Divergence (Get_Size (Data.Statistics) + 1)
               -  (  Divergence (Statistics.Depth)
                  /  (  Divergence (Statistics.Down)
                     +  Divergence (Statistics.Up)
               )  )  );
         end;
         declare
            Node : Node_Handle := Ref (Ptr (Child.Node));
         begin
            Rotate (Node, Ref (Best_Feature));
            Set (Child.Node, Ptr (Node));
            Data.Rotations := Data.Rotations + 1;
         end;
         declare
            This : constant Feature_Statistics_Ptr :=
                      Get_Statistics
                      (  Data'Unchecked_Access,
                         Best_Feature.all
                      );
            Statistics : Feature_Statistics'Class renames This.all;
            Position   : constant Integer := Find (Data.Features, This);
         begin
            Statistics.Up    := Statistics.Up    + 1;
            Statistics.Depth := Statistics.Depth + Float (Data.Depth);
            Statistics.Order :=
               (  Divergence (Get_Size (Data.Statistics) + 1)
               -  (  Divergence (Statistics.Depth)
                  /  (  Divergence (Statistics.Down)
                     +  Divergence (Statistics.Up)
               )  )  );
            if Position > 0 then
               Remove (Data.Features, Position);
               Add (Data.Features, This);
            end if;
         end;
      end if;
   end Commit;

   procedure Free
             (  Cache : in out Separation_Hypotheses_Cache;
                List  : Separation_Hypotheses_Ptr
             )  is
   begin
      List.Next  := Cache.Free;
      Cache.Free := List;
   end Free;

   procedure Free (Data : in out Graph_Training_Data_Ptr) is
      procedure Delete is
         new Ada.Unchecked_Deallocation
             (  Graph_Training_Data'Class,
                Graph_Training_Data_Ptr
             );
   begin
      Delete (Data);
   end Free;

   function Equal (Value : Fuzzy.Set; X, Y : Positive) return Boolean is
   begin
      return Value (X) = Value (Y);
   end Equal;

   procedure Finalize (Cache : in out Separation_Hypotheses_Cache) is
      Ptr : Separation_Hypotheses_Ptr := Cache.Free;
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Separation_Hypotheses,
                Separation_Hypotheses_Ptr
             );
   begin
      Finalize (Quality_Cache (Cache));
      while Ptr /= null loop
         Cache.Free := Ptr.Next;
         Free (Ptr);
         Ptr := Cache.Free;
      end loop;
   end Finalize;

   function Get_Statistics
            (  Data    : Graph_Training_Data;
               Feature : Feature_Handle
            )  return Feature_Statistics_Handle'Class is
      use Feature_Statistics_Maps;
      Index : constant Integer := Find (Data.Statistics, Ptr (Feature));
   begin
      if Index > 0 then
         return Get (Data.Statistics, Index);
      else
         return
            Feature_Statistics_Handle'
            (  Feature_Statistics_Handles.Null_Handle with null record
            );
      end if;
   end Get_Statistics;

   function Get_Statistics
            (  Data    : not null access Graph_Training_Data'Class;
               Feature : Feature_Object'Class
            )  return Feature_Statistics_Ptr is
      use Feature_Statistics_Maps;
      Index : constant Integer := Find (Data.Statistics, Feature.Self);
   begin
      if Index > 0 then
         return Ptr (Get (Data.Statistics, Index));
      else
         declare
            Result : constant Feature_Statistics_Ptr :=
                        new Feature_Statistics;
            Handle : constant Feature_Statistics_Handle := Ref (Result);
         begin
            Add (Data.Statistics, Feature.Self, Handle);
            return Result;
         end;
      end if;
   end Get_Statistics;

   procedure Finalize (Node : in out Branch_Proxy) is
   begin
      for Index in Node.Children'Range loop
         Release (Node.Children (Index));
      end loop;
      Finalize (Node_Proxy (Node));
   end Finalize;

   procedure Modify
             (  Data : in out Separation_Data;
                Node : Node_Proxy'Class
             )  is
      Branch : constant Graph_Node_Ptr := Ptr (Node);
   begin
      if Branch = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Modifying an invald node handle"
         );
      end if;
      if Data.Cardinality /= Branch.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Modifying an incompatible node"
         );
      end if;
      if Get_Type (Branch.all) = Tree_Leaf then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Modifying an leaf node as if it were a branch"
         );
      end if;
      if Ptr (Data.Node) /= null then
         Data.Node.Children := (others => null);
         Data.Current       := 1;
         Data.Splitter      := False;
      end if;
      Set (Data.Node, Branch);
      Data.Node.Exclusive := Node.Exclusive;
   end Modify;

   function Ref (Statistics : Feature_Statistics_Ptr)
      return Feature_Statistics_Handle is
   begin
      return
         (Feature_Statistics_Handles.Ref (Statistics) with null record);
   end Ref;

   function Ref (Node : Graph_Node_Ptr) return Node_Proxy is
   begin
      if Node /= null and then Node.Use_Count = 0 then
         return (Handles.Ref (Node) with True);
      else
         return (Handles.Ref (Node) with False);
      end if;
   end Ref;

   function Ref (Node : Graph_Node_Ptr) return Branch_Proxy is
   begin
      if Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Referencing an invald node handle"
         );
      end if;
      declare
         Branch : Graph_Node'Class renames Node.all;
      begin
         if Get_Type (Branch) = Tree_Leaf then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Referencing an leaf node as if it were a branch"
            );
         end if;
         if Node.Use_Count = 0 then
            return
            (  Handles.Ref (Node)
            with
               Exclusive   => True,
               Cardinality => Branch.Cardinality,
               Children    => (others => null)
            );
         else
            return
            (  Handles.Ref (Node)
            with
               Exclusive   => False,
               Cardinality => Branch.Cardinality,
               Children    => (others => null)
            );
         end if;
      end;
   end Ref;

   function Get (Container : Feature_Array; Index : Integer)
      return Feature_Object_Ptr is
      pragma Inline (Get);
   begin
      return Ptr (Container (Index));
   end Get;

   function First (Container : Bounded_Array) return Integer is
      pragma Inline (First);
   begin
      return Container.First;
   end First;

   function First (Container : Feature_Array) return Integer is
      pragma Inline (First);
   begin
      return Container'First;
   end First;

   function Last (Container : Bounded_Array) return Integer is
      pragma Inline (Last);
   begin
      return Container.Last;
   end Last;

   function Last (Container : Feature_Array) return Integer is
      pragma Inline (Last);
   begin
      return Container'Last;
   end Last;

   procedure Generic_Set_Data
             (  Context  : in out Graph_Training_Data'Class;
                Features : Array_Type
             )  is
      Feature : Feature_Object_Ptr;
      Order   : Divergence := 0.0;
   begin
      for Index in First (Features)..Last (Features) loop
         Order := Divergence'Succ (Order);
      end loop;
      for Index in First (Features)..Last (Features) loop
         Feature := Get (Features, Index);
         if (  Feature /= null
            and then
               Feature /= Ptr (Context.Classes)
            )
         then
            declare
               Statistics : constant Feature_Statistics_Ptr :=
                  Get_Statistics (Context'Access, Feature.all);
            begin
               if not Is_Valid (Statistics.Feature) then
                  if Is_Clustering (Feature.all) then
                     Statistics.Order := 0.0;
                  else
                     Statistics.Order := Order;
                     Order := Divergence'Pred (Order);
                  end if;
                  Statistics.Feature := Ref (Feature);
                  Add (Context.Features, Statistics);
               end if;
            end;
         end if;
      end loop;
   end Generic_Set_Data;

   procedure Set_Bounded_Array is new Generic_Set_Data (Bounded_Array);
   procedure Set_Features
             (  Context  : in out Graph_Training_Data;
                Features : Bounded_Arrays.Bounded_Array
             )  is
   begin
      Set_Bounded_Array (Context, Features);
   end Set_Features;

   procedure Set_Feature_Array is new Generic_Set_Data (Feature_Array);
   procedure Set_Features
             (  Context  : in out Graph_Training_Data;
                Features : Feature_Array
             )  is
   begin
      Set_Feature_Array (Context, Features);
   end Set_Features;

end Fuzzy.Graph.Learning;
