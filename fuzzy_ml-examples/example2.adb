--                                                                    --
--  procedure Example2              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2002       --
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Feature.Handle.HTML;     use Fuzzy.Feature.Handle.HTML;
with Fuzzy.Graph.Handle;            use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Handle.HTML;       use Fuzzy.Graph.Handle.HTML;
with Fuzzy.Graph.Handle.Manual;     use Fuzzy.Graph.Handle.Manual;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;

procedure Example2 is
   File                      : File_Type;
   Covering, Limbs, Creature : Feature_Handle;
   Graph                     : Node_Handle;
begin
   Covering :=
      Create_Discrete ("Covering", "Feather, Fur, Scale, None");
   Limbs :=
      Create_Discrete ("Limbs", "Legs, Wings, Fins");
   Creature :=
      Create_Discrete
      (  "Creature",
         "Fish, Reptile, Bird, Human, Seal, Beast"
      );
   --
   -- Create the output file
   --
   Create (File, Out_File, "Example2.htm");
   declare
      Has_Feather, Has_Fur, Has_Scale, Has_None : Node_Handle;
      Bird, Beast, Seal, Reptile, Fish, Human   : Node_Handle;
   begin
      --
      -- Create leaf nodes
      --
      Bird :=
         Create_Leaf (Creature, Value ("Bird:1:1",    Creature));
      Beast :=
         Create_Leaf (Creature, Value ("Beast:1:1",   Creature), Bird);
      Seal :=
         Create_Leaf (Creature, Value ("Seal:1:1",    Creature), Bird);
      Reptile :=
         Create_Leaf (Creature, Value ("Reptile:1:1", Creature), Bird);
      Fish :=
         Create_Leaf (Creature, Value ("Fish:1:1",    Creature), Bird);
      Human :=
         Create_Leaf (Creature, Value ("Human:1:1",   Creature), Bird);
      --
      -- Create nodes testing limbs
      --
      Has_Feather :=
         Create_Branch (Limbs, (Bird, Bird, No_Node));
      Has_Fur :=
         Create_Branch (Limbs, (Beast, Beast, Seal));
      Has_Scale :=
         Create_Branch (Limbs, (Reptile, No_Node, Fish));
      Has_None :=
         Create_Branch (Limbs, (Human, No_Node, No_Node));
      --
      -- Create the root node testing the feature Covering
      --
      Graph :=
         Create_Branch
         (  Covering,
            (Has_Feather, Has_Fur, Has_Scale, Has_None)
         );
   end;
   --
   -- Write the built graph into the file
   --
   Put_Line (File, "The built graph:");
   Put (File, Graph);
   --
   -- Let's see how the graph classifies
   --
   declare
      Something : Lecture_Handle;
   begin
      --
      -- Define the feature Limbs in the example 1
      --
      Put
      (  Something,
         1,
         Limbs,
         Value ("Legs:1:1", Limbs)
      );
      --
      -- Define the feature Covering in the example 1
      --
      Put
      (  Something,
         1,
         Covering,
         Value ("Fur, Feather:0.8, Scale:0.5", Covering)
      );
      Put_Line (File, "The result of classification:");
      --
      -- Classify the example 1 and write the result into the file
      --
      Put
      (  File,
         Creature,
         Classify (Graph, Something, 1, Fuzzy.Feature.Has_In)
      );
   end;
   --
   -- Close the file
   --
   Close (File);
end Example2;
