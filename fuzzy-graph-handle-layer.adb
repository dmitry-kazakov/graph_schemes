--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Layer                    Luebeck            --
--  Implementation                                 Winter, 2002       --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Io_Exceptions;    use Ada.Io_Exceptions;
with Deposit_Handles;      use Deposit_Handles;
with Fuzzy.Lecture.Empty;  use Fuzzy.Lecture.Empty;

with Fuzzy.Feature.Handle.Container;
use  Fuzzy.Feature.Handle.Container;

package body Fuzzy.Graph.Handle.Layer is
   use Container;

   procedure Connect
             (  Parent : in out Node_Handle;
                Child  : Node_Handle;
                Index  : Positive;
                Data   : in out Node_Modification_Data'Class
             )  is
   begin
      if Get_Type (Parent) = Tree_Leaf then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Connecting to no branch"
         );
      end if;
      if Is_Valid (Child) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Connecting no node"
         );
      end if;
      if Is_Dependent
         (  To_Deposit_Handle (Parent),
            To_Deposit_Handle (Child)
         )
      then
         Raise_Exception
         (  Use_Error'Identity,
            "Circular dependency may appear upon node replacement"
         );
      end if;
      declare
         Children : Node_Ptr_Array (1..Get_Cardinality (Parent)) :=
                       (others => null);
         New_Node : Graph_Node_Ptr;
      begin
         Children (Index) := Ptr (Child);
         Modify
         (  Ptr (Parent).all,
            Data,
            Children,
            Ptr (Parent).Use_Count = 1,
            Union,
            New_Node
         );
         if New_Node /= null then
            Parent := Ref (New_Node);
         end if;
      end;
   end Connect;

   procedure Connect
             (  Parent : in out Node_Handle;
                Child  : Node_Handle;
                Index  : Positive
             )  is
      Data : Node_Modification_Data (Empty_Set, 0, Negleter'Access);
   begin
      Data.Factory := Get_Factory (Ptr (Parent).all);
      Connect (Parent, Child, Index, Data);
   end Connect;

   function Get_Children (Node : Node_Handle)
      return Fuzzy.Graph.Handle.Container.Set is
      This   : Graph_Node'Class renames Ptr (Node).all;
      Child  : Graph_Node_Ptr;
      Result : Fuzzy.Graph.Handle.Container.Set;
   begin
      for No in 1..This.Cardinality loop
         Child := Get_Child (This, No);
         if Child /= null then
            Add (Result, Ref (Child));
         end if;
      end loop;
      return Result;
   end Get_Children;

   function Get_Elder_Feature (Node : Node_Handle)
      return Feature_Handle is
      Parent : Graph_Node'Class renames Ptr (Node).all;
      Child  : Graph_Node_Ptr;
      Found  : Feature_Object_Ptr;
   begin
      for Index in 1..Parent.Cardinality loop
         Child := Get_Child (Parent, Index);
         if (  Tree_Leaf /= Get_Type (Child.all)
            and then
               (  Found = null
               or else
                  (  Found /= Get_Feature (Child.all)
                  and then
                     Tests (Child.all, Found)
            )  )  )
         then
            Found := Get_Feature (Child.all);
         end if;
      end loop;
      return Ref (Found);
   end Get_Elder_Feature;

   function Get_Feature (Node : Node_Handle; Index : Positive)
      return Feature_Handle is
   begin
      return Ref (Get_Feature (Ptr (Node).all, Index));
   end Get_Feature;

   function Get_Features (Node : Node_Handle)
      return Fuzzy.Feature.Handle.Container.Set is
      This   : Graph_Node'Class renames Ptr (Node).all;
      Result : Fuzzy.Feature.Handle.Container.Set;
   begin
      for Index in 1..Get_Features_Number (This) loop
         Add (Result, Ref (Get_Feature (This, Index)));
      end loop;
      return Result;
   end Get_Features;

   function Get_Features_Number (Node : Node_Handle) return Natural is
   begin
      if Is_Valid (Node) then
         return Get_Features_Number (Ptr (Node).all);
      else
         return 0;
      end if;
   end Get_Features_Number;

   function Get_Layer
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Fuzzy.Graph.Handle.Container.Set is
      Result : Fuzzy.Graph.Handle.Container.Set;
      First  : constant Graph_Node_Ptr :=
                  Find (Ptr (Node).all, Ptr (Feature).all);
   begin
      if First /= null then
         declare
            This : Graph_Node'Class renames First.all;
         begin
            for Index in 1..Get_Siblings_Number (This) loop
               Add (Result, Ref (Get_Sibling (This, Index)));
            end loop;
         end;
      end if;
      return Result;
   end Get_Layer;

   function Get_Layer_Size
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Natural is
      First : constant Graph_Node_Ptr :=
                 Find (Ptr (Node).all, Ptr (Feature).all);
   begin
      if First = null then
         return 0;
      else
         return Get_Siblings_Number (First.all);
      end if;
   end Get_Layer_Size;

   function Get_Parents (Node : Node_Handle)
      return Fuzzy.Graph.Handle.Container.Set is
      Branch_Ptr : Graph_Node_Ptr;
      Child_Ptr  : constant Graph_Node_Ptr := Ptr (Node);
      Child      : Graph_Node'Class renames Child_Ptr.all;
      Classes    : constant Feature_Object_Ptr := Get_Classes (Child);
      Same       : constant Feature_Object_Ptr := Get_Feature (Child);
      Feature    : Feature_Object_Ptr;
      Result     : Fuzzy.Graph.Handle.Container.Set;
   begin
      for Feature_No in 1..Get_Features_Number (Child) loop
         Feature := Get_Feature (Child, Feature_No);
         if Feature /= Classes and then Feature /= Same then
            Branch_Ptr := Find (Child, Feature.all);
            for Node_No in 1..Get_Siblings_Number (Branch_Ptr.all) loop
               Branch_Ptr := Get_Sibling (Branch_Ptr.all, Node_No);
               declare
                  Branch : Graph_Node'Class renames Branch_Ptr.all;
               begin
                  for Child_No in 1..Branch.Cardinality loop
                     if Get_Child (Branch, Child_No) = Child_Ptr then
                        Add (Result, Ref (Branch_Ptr));
                        exit;
                     end if;
                  end loop;
               end;
            end loop;
         end if;
      end loop;
      return Result;
   end Get_Parents;

   function Get_Sibling (Node : Node_Handle; Index : Positive)
      return Node_Handle is
      Result : constant Graph_Node_Ptr :=
                        Get_Sibling (Ptr (Node).all, Index);
   begin
      return Ref (Result);
   end Get_Sibling;

   function Get_Siblings_Number (Node : Node_Handle)
      return Natural is
   begin
      if Is_Valid (Node) then
         return Get_Siblings_Number (Ptr (Node).all);
      else
         return 0;
      end if;
   end Get_Siblings_Number;

   function Get_Size (Node : Node_Handle) return Positive is
      This   : Graph_Node'Class renames Ptr (Node).all;
      Result : Natural := 0;
   begin
      for Index in 1..Get_Features_Number (This) loop
         Result :=
            (  Result
            +  Get_Siblings_Number
               (  Find
                  (  This,
                     Get_Feature (This, Index).all
                  ) .all
            )  );
      end loop;
      return Result;
   end Get_Size;

   procedure Replace
             (  Parent    : in out Node_Handle;
                Old_Child : Node_Handle;
                New_Child : Node_Handle;
                Data      : in out Node_Modification_Data'Class
             )  is
   begin
      if Get_Type (Parent) = Tree_Leaf then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Replacing children of no branch"
         );
      end if;
      if (  Is_Valid (New_Child)
         and then
            Is_Dependent
            (  To_Deposit_Handle (Parent),
               To_Deposit_Handle (New_Child)
         )  )
      then
         Raise_Exception
         (  Use_Error'Identity,
            "Circular dependency may appear upon node replacement"
         );
      end if;
      declare
         Empty        : Boolean := True;
         Branch       : Graph_Node'Class renames Ptr (Parent).all;
         Parent_Ptr   : Graph_Node_Ptr;
         New_Node_Ptr : constant Graph_Node_Ptr := Ptr (New_Child);
         Old_Node_Ptr : constant Graph_Node_Ptr := Ptr (Old_Child);
         Children     : Node_Ptr_Array (1..Get_Cardinality (Parent));
      begin
         for Index in Children'Range loop
            Children (Index) := Get_Child (Branch, Index);
            if Children (Index) = Old_Node_Ptr then
               Children (Index) := New_Node_Ptr;
            end if;
            if Empty or else Children (Index) /= null then
               Empty := False;
            end if;
         end loop;
         if Empty then
            Invalidate (Parent);
         else
            Modify
            (  Branch,
               Data,
               Children,
               Branch.Use_Count = 1,
               Replacement,
               Parent_Ptr
            );
            if Parent_Ptr /= null then
               Parent := Ref (Parent_Ptr);
            end if;
         end if;
      end;
   end Replace;

   procedure Replace
             (  Parent    : in out Node_Handle;
                Old_Child : Node_Handle;
                New_Child : Node_Handle
             )  is
      Data : Node_Modification_Data (Empty_Set, 0, Negleter'Access);
   begin
      Data.Factory := Get_Factory (Ptr (Parent).all);
      Replace (Parent, Old_Child, New_Child, Data);
   end Replace;

   procedure Sew (Node : in out Node_Handle) is
   begin
      if Is_Valid (Node) then
         declare
            Parent  : Node_Handle;
            Parents : constant Fuzzy.Graph.Handle.Container.Set :=
                               Get_Parents (Node);
         begin
            --
            -- Disconnect  the node from all its parents by replacing it
            -- with no node. The parents that after that  will  have  no
            -- children are deleted by calling Delete recursively.
            --
            for Index in 1..Get_Size (Parents) loop
               Parent := Ref (Parents, Index);
               Replace (Parent, Node, No_Node);
               if 0 = Get_Children_Number (Parent) then
                  Sew (Parent);
               end if;
            end loop;
         end;
         Invalidate (Node);
      end if;
   end Sew;

end Fuzzy.Graph.Handle.Layer;
