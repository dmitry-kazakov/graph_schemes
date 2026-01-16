--                                                                    --
--  package Fuzzy.Graph.Handle      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
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

package body Fuzzy.Graph.Handle is

   procedure Invalidate (Node : in out Node_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Node));
   end Invalidate;

   function Is_Valid (Node : Node_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Node));
   end Is_Valid;

   function Classify
            (  Node    : Node_Handle;
               Context : not null access
                         Classification_Parameters'Class;
               Image   : Image_Type
            )  return Classification is
      Cardinality : constant Positive :=
                       Get_Cardinality (Get_Classes (Node));
   begin
      if Is_Valid (Node) then
         Context.Image              := Image;
         Context.Ready              := False;
         Context.Result.Possibility := (others => Confidence'First);
         Context.Result.Necessity   := (others => Confidence'Last);
         Classify (Ptr (Node).all, Context.all);
         return Context.Result;
      else
         return
         (  Cardinality => Cardinality,
            Possibility => (others => Confidence'Last),
            Necessity   => (others => Confidence'First)
         );
      end if;
   end Classify;

   function Classify
            (  Node       : Node_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Image      : Image_Type;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Classification is
      Context : aliased Classification_Parameters
                        (  Ptr (Lesson),
                           Get_Cardinality (Get_Classes (Node))
                        );
   begin
      Context.Generalize := Generalize;
      Context.Threshold  := Threshold;
      Select_Example (Context, Example);
      return Classify (Node, Context'Access, Image);
   end Classify;

   function Find
            (  Parent : Node_Handle;
               Child  : Node_Handle;
               Index  : Positive
            )  return Natural is
   begin
      if Is_Valid (Parent) and then Is_Valid (Child) then
         declare
            Branch : Graph_Node'Class renames Ptr (Parent).all;
            This   : constant Graph_Node_Ptr := Ptr (Child);
         begin
            for Next in Index..Branch.Cardinality loop
               if Get_Child (Branch, Next) = This then
                  return Next;
               end if;
            end loop;
         end;
      end if;
      return 0;
   end Find;

   function Find
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Node_Handle is
      Node_Ptr : constant Graph_Node_Ptr :=
                    Find (Ptr (Node).all, Ptr (Feature).all);
   begin
      return Ref (Node_Ptr);
   end Find;

   function Get_Cardinality (Node : Node_Handle)
      return Positive is
   begin
      return Ptr (Node).Cardinality;
   end Get_Cardinality;

   function Get_Child (Node : Node_Handle; Index : Positive)
      return Node_Handle is
      Child : constant Graph_Node_Ptr :=
                 Get_Child (Ptr (Node).all, Index);
   begin
      if Child /= null then
         return Ref (Child);
      else
         return No_Node;
      end if;
   end Get_Child;

   function Get_Children_Number
            (  Node      : Node_Handle;
               Immediate : Boolean := True
            )
      return Natural is
   begin
      if Is_Valid (Node) then
         return Get_Children_Number (Ptr (Node).all, Immediate);
      else
         return 0;
      end if;
   end Get_Children_Number;

   function Get_Class (Node : Node_Handle) return String is
   begin
      return Get_Class (Ptr (Node).all);
   end Get_Class;

   function Get_Classes (Node : Node_Handle)
      return Feature_Handle is
   begin
      return Ref (Get_Classes (Ptr (Node).all));
   end Get_Classes;

   function Get_Distribution (Node : Node_Handle)
      return Classification is
   begin
      return Get_Distribution (Ptr (Node).all);
   end Get_Distribution;

   procedure Get_Examples
             (  Node   : Node_Handle;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
   begin
      Get_Examples (Ptr (Node).all, Lesson, Image, Viewer);
   end Get_Examples;

   procedure Get_Examples
             (  Node   : Node_Handle;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Get_Examples (Ptr (Node).all, Lesson, Image, Ptr (Viewer));
      else
         Get_Examples (Ptr (Node).all, Lesson, Image);
      end if;
   end Get_Examples;

   function Get_Feature (Node : Node_Handle)
      return Feature_Handle is
   begin
      return Ref (Get_Feature (Ptr (Node).all));
   end Get_Feature;

   function Get_Type (Node : Node_Handle) return Node_Type is
   begin
      return Get_Type (Ptr (Node).all);
   end Get_Type;

   function Ptr (Node : Node_Handle)
      return Graph_Node_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Node));
   end Ptr;

   function Ref (Node : Graph_Node_Ptr) return Node_Handle is
   begin
      return (Handles.Ref (Node) with null record);
   end Ref;

   procedure Ref
             (  Handle : in out Node_Handle;
                Node   : Graph_Node_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Node);
   end Ref;

   function Tests
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Boolean is
   begin
      return Tests (Ptr (Node).all, Ptr (Feature));
   end Tests;

   function To_Node_Handle
            (  Node : Deposit_Handles.Handle
            )  return Node_Handle is
   begin
      return
         Ref
         (  To_Graph_Node_Ptr
            (  Deposit_Handles.Ptr (Node)
         )  );
   end To_Node_Handle;

   function To_Deposit_Handle (Node : Node_Handle)
      return Deposit_Handles.Handle is
   begin
      return Deposit_Handles.Ref (To_Deposit_Ptr (Ptr (Node)));
   end To_Deposit_Handle;

   function "<" (Left, Right : Node_Handle) return Boolean is
   begin
      return
         Handles."<" (Handles.Handle (Left), Handles.Handle (Right));
   end "<";

   function "<=" (Left, Right : Node_Handle) return Boolean is
   begin
      return
         Handles."<=" (Handles.Handle (Left), Handles.Handle (Right));
   end "<=";

   function "="  (Left, Right : Node_Handle) return Boolean is
   begin
      return
         Handles."=" (Handles.Handle (Left), Handles.Handle (Right));
   end "=";

   function ">=" (Left, Right : Node_Handle) return Boolean is
   begin
      return
         Handles.">=" (Handles.Handle (Left), Handles.Handle (Right));
   end ">=";

   function ">"  (Left, Right : Node_Handle) return Boolean is
   begin
      return
         Handles.">" (Handles.Handle (Left), Handles.Handle (Right));
   end ">";

end Fuzzy.Graph.Handle;
