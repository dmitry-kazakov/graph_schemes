--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident                 Luebeck            --
--  Implementation                                 Winter, 2002       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Deposit_Handles;    use Deposit_Handles;
with Fuzzy.Feature;      use Fuzzy.Feature;

with Ada.Unchecked_Deallocation;
with Fuzzy.Graph.Memory_Resident.Branch;
with Fuzzy.Graph.Memory_Resident.Clustering_Branch;
with Fuzzy.Graph.Memory_Resident.Leaf;

pragma Elaborate (Fuzzy.Graph.Memory_Resident.Branch);
pragma Elaborate (Fuzzy.Graph.Memory_Resident.Leaf);

package body Fuzzy.Graph.Memory_Resident is
   use Layer_Maps;
   use Fuzzy.Feature.Classificatory.Clustering;
   use Fuzzy.Graph.Node_Class_Sets;

   Class        : constant String := Graph_Class & "Header";
   Node_Factory : aliased First_Node_Factory;

   procedure Free is new
      Ada.Unchecked_Deallocation (Graph_Layer'Class, Graph_Layer_Ptr);

   procedure Add
             (  List    : in out Node_Ptr_Array;
                Free    : in out Integer;
                Node    : Graph_Node_Ptr;
                Pointer : out Integer
             )  is
   begin
      if Node /= null then
         for Index in List'Range loop
            exit when Index >= Free;
            if List (Index) = Node then
               Pointer := Index;
               return;
            end if;
         end loop;
         Pointer := Free;
         List (Free) := Node;
         Free := Free + 1;
      end if;
   end Add;

   procedure Check_Child
             (  Parent : Memory_Node;
                Child  : Graph_Node'Class
             )  is
   begin
      if Child not in Memory_Node'Class then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Connecting wrong type of node"
         );
      else
         declare
            This : Memory_Node'Class renames Memory_Node'Class (Child);
         begin
            if This.Layer = null then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Connecting node of no graph"
               );
            elsif This.Layer.Header /= Parent.Layer.Header then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Connecting node of another graph"
               );
            end if;
         end;
      end if;
   end Check_Child;

   procedure Detach (Node : in out Memory_Node'Class) is
   begin
      if Node.Layer /= null then
         Remove (Node.Layer.Nodes, Node.Self);
         declare
            Header : constant Graph_Header_Ptr :=
                        Ptr (Node.Layer.Header.Handle);
         begin
            if Is_Empty (Node.Layer.Nodes) then
               if Header /= null then
                  Remove (Header.Layers, Get_ID (Node.Layer.Feature));
                  Header.Sequence := Header.Sequence + 1;
               end if;
               Free (Node.Layer);
            else
               if Header /= null then
                  Header.Sequence := Header.Sequence + 1;
               end if;
            end if;
         end;
         Node.Layer := null;
      end if;
   end Detach;

   procedure Attach
             (  Node    : in out Graph_Node_Ptr;
                Header  : Header_Handle;
                Feature : Feature_Object'Class
             )  is
      This : Memory_Node'Class renames Memory_Node'Class (Node.all);
   begin
      if This.Layer /= null then
         if Ptr (This.Layer.Header.Handle) = Header.Handle then
            return;
         end if;
         Detach (This);
      end if;
      declare
         Graph : constant Graph_Header_Ptr :=
                    Ptr (Header.Handle);
         Layer : Graph_Layer_Ptr := Get_Layer (Graph.all, Feature);
      begin
         if Layer = null then
            if Is_Dependent
               (  Ref (To_Deposit_Ptr (Feature.Self)),
                  Ref (To_Deposit_Ptr (Graph))
               )
            then
               raise Use_Error;
            end if;
            Layer := Create (Ptr (Header.Handle).all);
            Layer.Header  := Header;
            Layer.Feature := Ref (Feature.Self);
            Add (Graph.Layers, Feature.ID, Layer);
         end if;
         declare
            Ptr : Graph_Node_Ptr := Node;
         begin
            Insert (Layer.Nodes, Node);
            if Ptr = Node then
               This.Layer := Layer;
            elsif Ptr.Use_Count = 0 then
               This.Layer := null;
               Free (Ptr);
            else
               Raise_Exception
               (  Program_Error'Identity,
                  "Duplicated graph node is still in use"
               );
            end if;
         end;
         Graph.Sequence := Graph.Sequence + 1;
      exception
         when others =>
            if Layer /= null and then Is_Empty (Layer.Nodes) then
               Free (Layer);
         end if;
         raise;
      end;
   end Attach;

   procedure Check_Child
             (  Header : Header_Handle;
                Child  : Graph_Node'Class
             )  is
      pragma Inline (Check_Child);
      type Reason is (Type_Error, Not_In_Graph, Another_Graph);
      Error : Reason;
   begin
      declare
         This : Memory_Node'Class renames Memory_Node'Class (Child);
      begin
         if This.Layer = null then
            Error := Not_In_Graph;
         elsif This.Layer.Header /= Header then
            Error := Another_Graph;
         else
            return;
         end if;
      exception
         when Constraint_Error =>
            Error := Type_Error;
      end;
      case Error is
         when Type_Error =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Connecting wrong type of node"
            );
         when Not_In_Graph =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Connecting node of no graph"
            );
         when Another_Graph =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Connecting node of another graph"
            );
      end case;
   end Check_Child;

   function Create
            (  Factory  : not null access Graph_Layer;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr is
   begin
      if Is_Clustering (Feature) then
         return
            Clustering_Branch.Create
            (  Factory.Header,
               Feature,
               Children
            );
      else
         return Branch.Create (Factory.Header, Feature, Children);
      end if;
   end Create;

   function Create
            (  Factory  : not null access Graph_Layer;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr is
   begin
      if Is_Clustering (Feature) then
         return
            Clustering_Branch.Create
            (  Factory.Header,
               Feature,
               Children,
               Weights
            );
      else
         return
            Branch.Create
            (  Factory.Header,
               Feature,
               Children,
               Weights
            );
      end if;
   end Create;

   function Create
            (  Factory  : not null access First_Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr is
   begin
      for Index in Children'Range loop
         if Children (Index) /= null then
            return
               Create
               (  Get_Factory (Children (Index).all),
                  Feature,
                  Children
               );
         end if;
      end loop;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Creating a graph's branch node any before children"
      );
   end Create;

   function Create
            (  Factory  : not null access First_Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr is
   begin
      for Index in Children'Range loop
         if Children (Index) /= null then
            return
               Create
               (  Get_Factory (Children (Index).all),
                  Feature,
                  Children,
                  Weights
               );
         end if;
      end loop;
      Raise_Exception
      (  Constraint_Error'Identity,
         "Creating a graph's branch node any before children"
      );
   end Create;

   function Create
            (  Factory      : not null access Graph_Layer;
               Classes      : Feature_Object'Class;
               Distribution : Classification
            )  return Graph_Node_Ptr is
   begin
      if not Equal
             (  Classes,
                Ptr (Ptr (Factory.Header.Handle).Classes).all
             )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Creating leaf node for a non-class feature"
         );
      end if;
      return Leaf.Create (Factory.Header, Distribution);
   end Create;

   function Create
            (  Factory      : not null access First_Node_Factory;
               Classes      : Feature_Object'Class;
               Distribution : Classification
            )  return Graph_Node_Ptr is
      Header : constant Graph_Header_Ptr := new Graph_Header;
      Handle : Header_Handle;
   begin
      Header_Handles.Set (Handle.Handle, Header);
      Header.Classes := Ref (Classes.Self);
      return Leaf.Create (Handle, Distribution);
   end Create;

   function Create (Header : Graph_Header) return Graph_Layer_Ptr is
   begin
      return new Graph_Layer;
   end Create;

   function Factory return Node_Factory_Ptr is
   begin
      return Node_Factory'Unchecked_Access;
   end Factory;

   procedure Finalize (Node : in out Memory_Node) is
   begin
      Close  (Node);
      Detach (Node);
      Finalize (Graph_Node (Node));
   end Finalize;

   procedure Finalize (Header : in out Graph_Header) is
   begin
      Close (Header);
      if not Is_Empty (Header.Layers) then
         Raise_Exception
         (  Program_Error'Identity,
            "Destroying a non-empty graph header"
         );
      end if;
      Finalize (Deposit (Header));
   end Finalize;

   function Find
            (  Node    : Memory_Node;
               Feature : Feature_Object'Class
            )  return Graph_Node_Ptr is
   begin
      if Node.Layer /= null then
         declare
            Layer : constant Graph_Layer_Ptr :=
                    Get_Layer
                    (  Ptr (Node.Layer.Header.Handle).all,
                       Feature
                    );
         begin
            if Layer /= null then
               return Get (Layer.Nodes, 1);
            end if;
         end;
      end if;
      return null;
   end Find;

   function Get_Class (Header : Graph_Header) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Classes (Node : Memory_Node)
      return Feature_Object_Ptr is
   begin
      return Ptr (Ptr (Node.Layer.Header.Handle).Classes);
   end Get_Classes;

   function Get_Factory (Node : Memory_Node)
      return Node_Factory_Ptr is
   begin
      return Node_Factory_Ptr (Node.Layer);
   end Get_Factory;

   function Get_Feature (Layer : Graph_Layer)
      return Feature_Object_Ptr is
   begin
      return Ptr (Layer.Feature);
   end Get_Feature;

   function Get_Feature (Node : Memory_Node)
      return Feature_Object_Ptr is
   begin
      if Node.Layer = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Node tests no feature for it belongs to no graph"
         );
      else
         return Ptr (Node.Layer.Feature);
      end if;
   end Get_Feature;

   function Get_Feature (Node : Memory_Node; Index : Positive)
      return Feature_Object_Ptr is
   begin
      if Node.Layer /= null then
         declare
            Layers : Layer_Maps.Map renames
                     Ptr (Node.Layer.Header.Handle).Layers;
         begin
            if Index <= Get_Size (Layers) then
               return Ptr (Get (Layers, Index).Feature);
            end if;
         end;
      end if;
      Raise_Exception
      (  Constraint_Error'Identity,
         "No such feature"
      );
   end Get_Feature;

   function Get_Features_Number (Node : Memory_Node) return Natural is
   begin
      if Node.Layer = null then
         return 0;
      else
         return Get_Size (Ptr (Node.Layer.Header.Handle).Layers);
      end if;
   end Get_Features_Number;

   function Get_Layer
            (  Header  : Graph_Header;
               Feature : Feature_Object'Class
            )  return Graph_Layer_Ptr is
      Index : constant Integer := Find (Header.Layers, Feature.ID);
   begin
      if Index > 0 then
         return Get (Header.Layers, Index);
      else
         return null;
      end if;
   end Get_Layer;

   procedure Get_Referents
             (  Header : Graph_Header;
                List   : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Header.Classes)), False);
   end Get_Referents;

   function Get_Sibling (Node : Memory_Node; Index : Positive)
      return Graph_Node_Ptr is
   begin
      if Node.Layer = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Node of no graph"
         );
      end if;
      declare
         Nodes : Node_Class_Sets.Set renames Node.Layer.Nodes;
      begin
         if Index > Get_Size (Nodes) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "No such sibling"
            );
         end if;
         return Get (Nodes, Index);
      end;
   end Get_Sibling;

   function Get_Siblings_Number (Node : Memory_Node)
      return Natural is
   begin
      if Node.Layer = null then
         return 0;
      else
         return Get_Size (Node.Layer.Nodes);
      end if;
   end Get_Siblings_Number;

   function Is_Modified (Header : Graph_Header) return Boolean is
   begin
      return Header.Sequence /= 0;
   end Is_Modified;

   procedure Reset_Modified (Header : in out Graph_Header) is
   begin
      Header.Sequence := 0;
   end Reset_Modified;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Header  : out Deposit_Ptr
             )  is
      Result : Graph_Header_Ptr;
   begin
      Result := new Graph_Header;
      Result.Classes :=
         Ref (To_Feature_Object_Ptr (Get (List, 1)));
      Header := To_Deposit_Ptr (Result);
   exception
      when Error : Constraint_Error =>
         Free (Result);
         Raise_Exception
         (  Use_Error'Identity,
            Exception_Message (Error)
         );
      when others =>
         Free (Result);
         raise;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Header      : Graph_Header
             )  is
   begin
      null;
   end Store;

   function To_Header_Ptr is
      new Ada.Unchecked_Conversion (Deposit_Ptr, Graph_Header_Ptr);

   function To_Graph_Header_Ptr (Ptr : Deposit_Ptr)
      return Graph_Header_Ptr is
   begin
      if Ptr.all in Graph_Header'Class then
         return To_Header_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Graph_Header_Ptr;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Graph.Memory_Resident;
