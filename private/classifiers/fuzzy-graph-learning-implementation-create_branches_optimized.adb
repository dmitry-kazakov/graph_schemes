--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.Implementation.        Luebeck            --
--        Create_Branches_Optimized                Spring, 2003       --
--  Separate body implementation                                      --
--                                Last revision :  21:30 10 Nov 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

separate (Fuzzy.Graph.Learning.Implementation)
   procedure Create_Branches_Optimized
             (  Data       : in out Graph_Training_Data'Class;
                Children   : in out Node_Ptr_Array;
                Weight     : Confidence;
                Feature    : Feature_Object'Class;
                Separators : in out Separation_Data;
                Created    : out Boolean
             )  is
   Value : Fuzzy.Set (1..Feature.Cardinality);
begin
   Created := False;
   Get (Data, Feature, Weight, Value);
   for Index in Value'Range loop
      if Value (Index) > Data.Threshold then
         --
         -- Create a new child node for this domain point
         --
         Learn_New (Children (Index), Data, Value (Index), Separators);
         if Children (Index) /= null then
            Created := True;
            Increment_Count (Children (Index).all);
            for Next in Index + 1..Value'Last loop
               --
               -- Check if the child node can be reused for this point
               --
               if Equal (Value, Index, Next) then
                  Children (Next) := Children (Index);
                  Increment_Count (Children (Index).all);
                  Value (Next) := Confidence'First;
               end if;
            end loop;
         end if;
      end if;
   end loop;
end Create_Branches_Optimized;
