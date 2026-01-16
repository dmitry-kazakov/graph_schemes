--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Tree_Model.                            Luebeck            --
--         Fuzzy_Graph_Store                       Summer, 2006       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Confidence_Factors;       use Confidence_Factors;
with Fuzzy;                    use Fuzzy;
with Fuzzy.Feature;            use Fuzzy.Feature;
with Fuzzy.Graph.Node_Sets;    use Fuzzy.Graph.Node_Sets;
with Fuzzy.Gtk_Icon_Factory;   use Fuzzy.Gtk_Icon_Factory;
with Glib.Messages;            use Glib.Messages;
with Gtk.Enums;                use Gtk.Enums;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Fuzzy.Intuitionistic;
with GLib.Values.Feature_Value;
with GLib.Values.Fuzzy.Logic;

package body Gtk.Tree_Model.Fuzzy_Graph_Store is
   use Fuzzy.Feature.Handle;
   use System;

   Fuzzy_Graph_Store_Type : GType := GType_Invalid;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Tree_Model.Fuzzy_Graph_Store." & Name;
   end Where;

   function Append
            (  Tree   : not null access Stack;
               Top    : Natural;
               After  : Natural;
               Parent : Graph_Node_Ptr;
               Child  : Graph_Node_Ptr;
               Branch : Boolean
            )  return Natural;

   procedure Add_Branches
             (  Tree        : not null access Stack;
                Tree_Parent : Natural;
                Parent      : Graph_Node_Ptr
             );

   function Add_Node
            (  Tree        : not null access Stack;
               Tree_Parent : Natural;
               Parent      : Graph_Node_Ptr;
               Child       : Graph_Node_Ptr
            )  return Natural;

   procedure Add_Branches
             (  Tree        : not null access Stack;
                Tree_Parent : Natural;
                Parent      : Graph_Node_Ptr
             )  is
      Parent_Node : Graph_Node'Class renames Parent.all;
      Child       : Graph_Node_Ptr;
      Visited     : Fuzzy.Graph.Node_Sets.Set;
      This        : Natural := 0;
      Path        : Natural;
   begin
      for Index in 1..Parent_Node.Cardinality loop
         Child := Get_Child (Parent_Node, Index);
         if Child /= null and then not Is_In (Visited, Child) then
            Add (Visited, Child);
            This :=
               Append
               (  Tree   => Tree,
                  Top    => Tree_Parent,
                  After  => This,
                  Parent => Parent,
                  Child  => Child,
                  Branch => True
               );
            Path := Add_Node (Tree, This, Parent, Child);
         end if;
      end loop;
   end Add_Branches;

   function Add_Node
            (  Tree        : not null access Stack;
               Tree_Parent : Natural;
               Parent      : Graph_Node_Ptr;
               Child       : Graph_Node_Ptr
            )  return Natural is
      Result : Natural;
   begin
      Result :=
         Append
         (  Tree   => Tree,
            Top    => Tree_Parent,
            After  => 0,
            Parent => Parent,
            Child  => Child,
            Branch => False
         );
      case Get_Type (Child.all) is
         when Tree_Leaf =>
            null;
         when Tree_Cluster | Tree_Branch =>
            Add_Branches (Tree, Result, Child);
      end case;
      return Result;
   end Add_Node;

   function Append
            (  Tree   : not null access Stack;
               Top    : Natural;
               After  : Natural;
               Parent : Graph_Node_Ptr;
               Child  : Graph_Node_Ptr;
               Branch : Boolean
            )  return Natural is
      This   : Stack renames Tree.all;
      Result : Natural;
   begin
      Push
      (  This,
         (  Parent => Ref (Parent),
            Child  => Ref (Child),
            Branch => Branch,
            Up     => Top,
            Down   => 0,
            Next   => 0,
            Prev   => After
      )  );
      Result := Mark (This);
      if Top /= 0 then
         if After = 0 then
            declare
               Ancestor : Parent_Child_Pair := Get (This, Top);
            begin
               Ancestor.Down := Result;
               Put (This, Top, Ancestor);
            end;
         else
            declare
               Ancestor : Parent_Child_Pair := Get (This, After);
            begin
               Ancestor.Next := Result;
               Put (This, After, Ancestor);
            end;
         end if;
      end if;
      return Result;
   end Append;

   function Compose
            (  Store : not null access Gtk_Fuzzy_Graph_Record'Class;
               Index : Natural
            )  return Gtk_Tree_Iter is
      pragma Inline (Compose);
      pragma Assert (Address'Size >= Natural'Size);
      function To_Address is
         new Ada.Unchecked_Conversion (Natural, Address);
   begin
      if Index = 0 then
         return Null_Iter;
      else
         return
         (  Stamp      => 1,
            User_Data  => To_Address (Index),
            User_Data2 => Store.all'Address,
            User_Data3 => Null_Address
         );
      end if;
   end Compose;

   function Decompose
            (  Store : not null access Gtk_Fuzzy_Graph_Record'Class;
               Iter  : Gtk_Tree_Iter
            )  return Natural is
      pragma Inline (Decompose);
      function To_Natural is
         new Ada.Unchecked_Conversion (Address, Natural);
   begin
      if (  Iter = Null_Iter
         or else
            Iter.User_Data2 /= Store.all'Address
         )
      then
         return 0;
      else
         return To_Natural (Iter.User_Data);
      end if;
   end Decompose;

   function Children
            (  Store  : not null access Gtk_Fuzzy_Graph_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter then
         return Compose (Store, Store.Top);
      end if;
      declare
         Index : constant Natural := Decompose (Store, Parent);
      begin
         if Index = 0 then
            return Null_Iter;
         else
            return Compose (Store, Store.Tree.Get (Index).Down);
         end if;
      end;
   end Children;

   function Mean (Set : Fuzzy.Set) return Float is
      Result : Float := 0.0;
   begin
      for Index in Set'Range loop
         Result :=
            (  Result
            +  (  Float (Index)
               *  Float (Set (Index))
            )  );
      end loop;
      return Result;
   end Mean;

   function Compare (Left, Right : Float) return Row_Order is
      pragma Inline (Compare);
   begin
      if Left < Right then
         return Before;
      elsif Left > Right then
         return After;
      else
         return Equal;
      end if;
   end Compare;

   function Compare (Left, Right : Fuzzy_Boolean) return Row_Order is
      pragma Inline (Compare);
   begin
      return
         Compare
         (  Float (Left.Possibility)  + Float (Left.Necessity),
            Float (Right.Possibility) + Float (Right.Necessity)
         );
   end Compare;

   function Compare (Left, Right : String) return Row_Order is
      pragma Inline (Compare);
   begin
      if Left < Right then
         return Before;
      elsif Left > Right then
         return After;
      else
         return Equal;
      end if;
   end Compare;

   function Compare
            (  Store  : not null access Gtk_Fuzzy_Graph_Store_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Row_Order is
      Model       : constant Gtk_Fuzzy_Graph := Get_Model (Store);
      Column      : GInt;
      Columns     : constant GInt :=
                       Get_N_Columns (To_Interface (Store));
      Left_Index  : constant Natural := Decompose (Model, Left);
      Right_Index : constant Natural := Decompose (Model, Right);
      Order       : Gtk_Sort_Type;
   begin
      Get_Sort_Column_ID (Store, Column, Order);
      Set_First (Store.Order, Column, Columns);
      if (  Left_Index = 0
         or else
            Right_Index = 0
         or else
            Left_Index = Right_Index
         )
      then
         return Equal;
      end if;
      declare
         Left_Pair  : Parent_Child_Pair renames
                         Model.Tree.Get (Left_Index);
         Right_Pair : Parent_Child_Pair renames
                         Model.Tree.Get (Right_Index);
      begin
         for Position in 1..Positive (Columns) loop
            Column := Get (Store.Order, Position);
            exit when Column < 0;
            if Column = 0 then
               declare
                  Left_Type  : constant Row_Type :=
                                        Get_Type (Left_Pair);
                  Right_Type : constant Row_Type :=
                                        Get_Type (Right_Pair);
               begin
                  if Left_Type /= Right_Type then
                     if (  Left_Type = Node_Row
                        or else
                           (  Left_Type = Branch_Row
                           and then
                              Right_Type = Leaf_Row
                        )  )
                     then
                        return Before;
                     else
                        return After;
                     end if;
                  end if;
               end;
            elsif Column = 1 then
               declare
                  Left_Type  : constant Row_Type :=
                                        Get_Type (Left_Pair);
                  Right_Type : constant Row_Type :=
                                        Get_Type (Right_Pair);
                  Result     : Row_Order;
               begin
                  if Left_Type /= Right_Type then
                     if (  Left_Type = Node_Row
                        or else
                           (  Left_Type = Branch_Row
                           and then
                              Right_Type = Leaf_Row
                        )  )
                     then
                        return Before;
                     else
                        return After;
                     end if;
                  elsif Left_Type = Branch_Row then
                     Result :=
                        Compare
                        (  Mean (Get_Children (Left_Pair)),
                           Mean (Get_Children (Right_Pair))
                        );
                     if Result /= Equal then
                        return Result;
                     end if;
                  else
                     Result :=
                        Compare
                        (  Get_Name (Model, Left_Pair),
                           Get_Name (Model, Right_Pair)
                        );
                     if Result /= Equal then
                        return Result;
                     end if;
                  end if;
               end;
            else
               declare
                  Left_Value  : Fuzzy_Boolean;
                  Right_Value : Fuzzy_Boolean;
                  Got_It      : Boolean;
                  Result      : Row_Order;
               begin
                  Get_Classification
                  (  Left_Pair,
                     Column,
                     Left_Value,
                     Got_It
                  );
                  Get_Classification
                  (  Right_Pair,
                     Column,
                     Right_Value,
                     Got_It
                  );
                  Result :=
                     Compare (Left_Value, Right_Value);
                  if Result /= Equal then
                     return Result;
                  end if;
               end;
            end if;
         end loop;
      end;
      return Equal;
   end Compare;

   procedure Free is
      new Ada.Unchecked_Deallocation (Stack, Stack_Ptr);

   procedure Finalize
             (  Store : not null access Gtk_Fuzzy_Graph_Record
             )  is
   begin
      Free (Store.Tree);
      Gtk_Abstract_Model_Record (Store.all).Finalize;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   function Get_Branch
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Fuzzy.Set is
      Model : Gtk_Fuzzy_Graph_Record'Class
                 renames Get_Model (Store).all;
      Index : constant Natural :=
                 Decompose
                 (  Model'Access,
                    Convert_Iter_To_Child_Iter (Store, Iter)
                 );
   begin
      if Index = 0 then
         raise Constraint_Error;
      end if;
      declare
         Pair : Parent_Child_Pair renames Get (Model.Tree.all, Index);
      begin
         if Get_Type (Pair) /= Branch_Row then
            raise Constraint_Error;
         else
            return Get_Children (Pair);
         end if;
      end;
   end Get_Branch;

   function Get_Children (Pair : Parent_Child_Pair) return Fuzzy.Set is
      use Fuzzy.Intuitionistic;
      Parent : Graph_Node'Class renames Ptr (Pair.Parent).all;
      Child  : constant Graph_Node_Ptr := Ptr (Pair.Child);
      Result : Classification := Get_Distribution (Parent);
   begin
      for Index in 1..Parent.Cardinality loop
         if Get_Child (Parent, Index) /= Child then
            Result.Possibility (Index) := Confidence'First;
         end if;
      end loop;
      return Result.Possibility;
   end Get_Children;

   procedure Get_Classification
             (  Pair   : Parent_Child_Pair;
                Column : GInt;
                Value  : out Fuzzy_Boolean;
                Got_It : out Boolean
             )  is
      Child : constant Graph_Node_Ptr := Ptr (Pair.Child);
   begin
      Got_It :=
         (  not Pair.Branch
         and then
            Get_Type (Child.all) = Tree_Leaf
         and then
            Column - 2 in 1..GInt (Child.Cardinality)
         );
      if Got_It then
         declare
            Distribution : Fuzzy.Intuitionistic.Classification renames
                              Get_Distribution (Child.all);
            Index : constant Positive := Positive (Column - 2);
         begin
            Value.Possibility := Distribution.Possibility (Index);
            Value.Necessity   := Distribution.Necessity   (Index);
         end;
      else
         Value := Certain_False;
      end if;
   end Get_Classification;

   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Index : Gint
            )  return GType is
      Columns : constant GInt := Get_N_Columns (Store);
   begin
      case Index is
         when 0 | 1 =>
            return GType_String;
         when 2 =>
            return Glib.Values.Feature_Value.GType_Feature_Value;
         when others =>
            if Index < 0 or else Index >= Get_N_Columns (Store) then
               return GType_Invalid;
            else
               return Glib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean;
            end if;
      end case;
   end Get_Column_Type;

   function Get_Feature
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return Feature_Handle is
      Node   : constant Node_Handle := Get_Graph (Store, Iter);
      Result : Feature_Handle := Get_Feature (Node);
   begin
      return Get_Feature (Node);
   end Get_Feature;

   function Get_Flags (Store : not null access Gtk_Fuzzy_Graph_Record)
      return Tree_Model_Flags is
   begin
      return Tree_Model_Iters_Persist;
   end Get_Flags;

   function Get_Graph
            (  Store  : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter;
               Parent : Boolean       := True
            )  return Node_Handle is
      Model : Gtk_Fuzzy_Graph_Record'Class
                 renames Get_Model (Store).all;
      Index : Natural;
   begin
      if Iter = Null_Iter then
         return Model.Graph;
      end if;
      Index :=
         Decompose
         (  Model'Access,
            Convert_Iter_To_Child_Iter (Store, Iter)
         );
      if Index = 0 then
         raise Constraint_Error;
      end if;
      declare
         Pair : Parent_Child_Pair renames Get (Model.Tree.all, Index);
      begin
         case Get_Type (Pair) is
            when Branch_Row =>
               if Parent then
                  return Pair.Parent;
               else
                  return Pair.Child;
               end if;
            when Node_Row | Leaf_Row  =>
               return Pair.Child;
         end case;
      end;
   end Get_Graph;

   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if (  Store.Tree = null
         or else
            Store.Top = 0
         or else
            Get_Depth (Path) = 0
         )
      then
         return Null_Iter;
      end if;
      declare
         Tree    : Stack renames Store.Tree.all;
         Indices : GInt_Array renames Get_Indices (Path);
         This    : Natural := Store.Top;
      begin
         if Indices (Indices'First) /= 0 then
            return Null_Iter;
         end if;
         for No in Indices'First + 1..Indices'Last loop
            This := Tree.Get (This).Down;
            if This = 0 then
               return Null_Iter;
            end if;
            for Count in 1..Indices (No) loop
               This := Tree.Get (This).Next;
               if This = 0 then
                  return Null_Iter;
               end if;
            end loop;
         end loop;
         return Compose (Store, This);
      end;
   end Get_Iter;

   function Get_Name
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Pair  : Parent_Child_Pair
            )  return String is
   begin
      return Get_Name (Get_Feature (Pair.Child));
   end Get_Name;

   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Graph_Record
            )  return GInt is
   begin
      if Is_Valid (Store.Graph) then
         return
            GInt (Get_Cardinality (Get_Classes (Store.Graph))) + 3;
      else
         return 3;
      end if;
   end Get_N_Columns;

   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
      This : Natural := Decompose (Store, Iter);
   begin
      if This = 0 then
         return Null_Gtk_Tree_Path;
      else
         declare
            Tree : Stack renames Store.Tree.all;
            Path : Gtk_Tree_Path;
            No   : GInt;
            Next : Natural;
            Item : Natural;
         begin
            Gtk_New (Path);
            loop
               Next := Tree.Get (This).Up;
               if Next = 0 then
                  Prepend_Index (Path, 0);
                  return Path;
               end if;
               No   := 0;
               Item := Tree.Get (Next).Down;
               while Item /= This loop
                  Item := Tree.Get (Item).Next;
                  No   := No + 1;
               end loop;
               Prepend_Index (Path, No);
               This := Next;
            end loop;
         end;
      end if;
   end Get_Path;

   function Get_Type (Pair : Parent_Child_Pair) return Row_Type is
   begin
      if Pair.Branch then
         return Branch_Row;
      elsif Get_Type (Ptr (Pair.Child).all) = Tree_Leaf then
         return Leaf_Row;
      else
         return Node_Row;
      end if;
   end Get_Type;

   function Get_Type
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return Row_Type is
      Model : Gtk_Fuzzy_Graph_Record'Class
                 renames Get_Model (Store).all;
      Index : Natural := 1;
   begin
      if Iter /= Null_Iter then
         Index :=
            Decompose
            (  Model'Access,
               Convert_Iter_To_Child_Iter (Store, Iter)
            );
      end if;
      if Index = 0 then
         raise Constraint_Error;
      else
         return Get_Type (Get (Model.Tree.all, Index));
      end if;
   end Get_Type;

   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Graph_Record;
                Iter   : Gtk_Tree_Iter;
                Column : GInt;
                Value  : out GValue
             )  is
      Index : constant Natural := Decompose (Store, Iter);
   begin
      if Index = 0 or else Column < 0 then
         Init (Value, GType_String);
         Set_String (Value, "");
         return;
      end if;
      declare
         Pair : Parent_Child_Pair renames Store.Tree.Get (Index);
      begin
         case Column is
            when 0 =>
               Init (Value, GType_String);
               case Get_Type (Pair) is
                  when Branch_Row =>
                     Set_String (Value, Graph_Branch_Icon);
                  when Leaf_Row =>
                     Set_String (Value, Graph_Leaf_Icon);
                  when Node_Row =>
                     Set_String (Value, Graph_Node_Icon);
               end case;
            when 1 =>
               Init (Value, GType_String);
               case Get_Type (Pair) is
                  when Branch_Row =>
                     Set_String (Value, "");
                  when Node_Row | Leaf_Row  =>
                     Set_String
                     (  Value,
                        Get_Name (Get_Feature (Pair.Child))
                     );
               end case;
            when 2 =>
               Init
               (  Value,
                  GLib.Values.Feature_Value.GType_Feature_Value
               );
               case Get_Type (Pair) is
                  when Branch_Row =>
                     GLib.Values.Feature_Value.Set
                     (  Value,
                        Get_Feature (Pair.Parent),
                        Get_Children (Pair)
                     );
                  when Leaf_Row | Node_Row =>
                     GLib.Values.Feature_Value.Set_Undefined (Value);
               end case;
            when others =>
               declare
                  Level  : Fuzzy_Boolean;
                  Got_It : Boolean;
               begin
                  Init
                  (  Value,
                     GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean
                  );
                  Get_Classification (Pair, Column, Level, Got_It);
                  if Got_It then
                     GLib.Values.Fuzzy.Logic.Set (Value, Level);
                  else
                     GLib.Values.Fuzzy.Logic.Set_Undefined (Value);
                  end if;
               end;
         end case;
      end;
   end Get_Value;

   function Get_Type return Gtk_Type is
   begin
      if Fuzzy_Graph_Store_Type = GType_Invalid then
         Fuzzy_Graph_Store_Type := Register ("FuzzyGraphStore");
      end if;
      return Fuzzy_Graph_Store_Type;
   end Get_Type;

   procedure Gtk_New
             (  Store : out Gtk_Fuzzy_Graph;
                Graph : Node_Handle
             )  is
   begin
      Store := new Gtk_Fuzzy_Graph_Record;
      begin
         Initialize (Store, Graph);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Graph)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Store : out Gtk_Fuzzy_Graph_Store;
                Graph : Node_Handle
             )  is
   begin
      Store := new Gtk_Fuzzy_Graph_Store_Record;
      begin
         Initialize (Store, Graph);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Graph_Store)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
      Index : constant Natural := Decompose (Store, Iter);
   begin
      return Index /= 0 and then Store.Tree.Get (Index).Down /= 0;
   end Has_Child;

   procedure Initialize
             (  Store : not null access Gtk_Fuzzy_Graph_Record'Class;
                Graph : Node_Handle
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize (Store, Get_Type);
      Store.Tree := new Stack;
      Set_Graph (Store, Graph);
   end Initialize;

   procedure Initialize
             (  Store : not null access
                        Gtk_Fuzzy_Graph_Store_Record'Class;
                Graph : Node_Handle
             )  is
      Unsorted : Gtk_Fuzzy_Graph;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Gtk_New (Unsorted, Graph);
      Initialize (Store, Unsorted);
      Unsorted.Unref;
      for Column in 0..Get_N_Columns (To_Interface (Store)) - 1 loop
         Set_Sort_Func (Store, Column);
      end loop;
   end Initialize;

   procedure Next
             (  Store : not null access Gtk_Fuzzy_Graph_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
      Index : constant Natural := Decompose (Store, Iter);
   begin
      if Index = 0 then
         Iter := Null_Iter;
      else
         Iter := Compose (Store, Store.Tree.Get (Index).Next);
      end if;
   end Next;

   function Nth_Child
            (  Store  : not null access Gtk_Fuzzy_Graph_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter then
         if N /= 0 then
            return Null_Iter;
         else
            return Compose (Store, Store.Top);
         end if;
      end if;
      declare
         This : Natural := Decompose (Store, Parent);
      begin
         if This = 0 or else N < 0 then
            return Null_Iter;
         end if;
         declare
            Tree : Stack renames Store.Tree.all;
         begin
            This := Tree.Get (This).Down;
            for Count in 1..N loop
               exit when This = 0;
               This := Tree.Get (This).Next;
            end loop;
            return Compose (Store, This);
         end;
      end;
   end Nth_Child;

   function N_Children
            (  Store  : not null access Gtk_Fuzzy_Graph_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
   begin
      if Iter = Null_Iter then
         if Store.Tree = null or else Store.Top = 0 then
            return 0;
         else
            return 1;
         end if;
      end if;
      declare
         This : Natural := Decompose (Store, Iter);
      begin
         if This = 0 then
            return 0;
         end if;
         declare
            Tree : Stack renames Store.Tree.all;
            No   : GInt := 0;
         begin
            This := Tree.Get (This).Down;
            while This /= 0 loop
               This := Tree.Get (This).Next;
               No := No + 1;
            end loop;
            return No;
         end;
      end;
   end N_Children;

   function Parent
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      This : constant Natural := Decompose (Store, Child);
   begin
      if This = 0 then
         return Null_Iter;
      else
         return Compose (Store, Store.Tree.Get (This).Up);
      end if;
   end Parent;

   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Graph_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
      Index : constant Natural := Decompose (Store, Iter);
   begin
      if Index = 0 then
         Iter := Null_Iter;
      else
         Iter := Compose (Store, Store.Tree.Get (Index).Prev);
      end if;
   end Previous;

   procedure Set_Graph
             (  Store : not null access Gtk_Fuzzy_Graph_Record;
                Graph : Node_Handle
             )  is
   begin
      Store.Tree.Erase;
      if Is_Valid (Graph) then
         Store.Top := Add_Node (Store.Tree, 0, null, Ptr (Graph));
      end if;
      Store.Graph := Graph;
   end Set_Graph;

end Gtk.Tree_Model.Fuzzy_Graph_Store;
