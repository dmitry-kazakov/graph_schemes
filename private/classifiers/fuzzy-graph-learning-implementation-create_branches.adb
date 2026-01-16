--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.                       Luebeck            --
--        Implementation.Create_Branches           Spring, 2003       --
--  Separate body implementation                                      --
--                                Last revision :  21:30 10 Nov 2009  --
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

separate (Fuzzy.Graph.Learning.Implementation)
   procedure Create_Branches
             (  Data       : in out Graph_Training_Data'Class;
                Children   : in out Node_Ptr_Array;
                Weight     : Confidence;
                Feature    : Feature_Object'Class;
                Separators : in out Separation_Data;
                Created    : out Boolean
             )  is
   Value : Confidence;
   Sup   : Confidence := Confidence'First;
begin
   Created := False;
   for Index in 1..Feature.Cardinality loop
      Value := Get_Point (Data'Access, Feature, Index);
      Sup   := Sup or Value;
      Value := Value and Weight;
      if Value > Data.Threshold then
         declare
            Snap : Context_Snap;
         begin
            Create_Constraint (Feature, Data, False);
            Set_Constraint (Feature, Data, Index, True);
            Learn_New (Children (Index), Data, Value, Separators);
            if Children (Index) /= null then
               Created := True;
               Increment_Count (Children (Index).all);
            end if;
         end;
      end if;
   end loop;
   if Sup < Confidence'Last then
      Add_Example (Data, Feature);
   end if;
end Create_Branches;
