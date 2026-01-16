--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.                       Luebeck            --
--        Implementation.Necessity                 Spring, 2002       --
--  Interface                                                         --
--                                Last revision :  12:48 16 Oct 2010  --
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

procedure Fuzzy.Graph.Learning.Implementation.Necessity
          (  Node         : in out Node_Handle;
             Context      : in out Graph_Training_Data'Class;
             Distribution : in out Fuzzy.Set
          )  is
   use Feature;
   use Fuzzy.Lecture;
   Classes : Feature_Object'Class renames Ptr (Context.Classes).all;
begin
   for Index in Distribution'Range loop
      if Distribution (Index) < Confidence'Last then
         declare
            Snap : Context_Snap;
         begin
            Create_Constraint (Classes, Context, False);
            Set_Constraint (Classes, Context, Index, True);
            Context.Class_Cut := Distribution (Index);
            for Next in Index + 1..Distribution'Last loop
               if Equal (Distribution, Index, Next) then
                  Set_Constraint (Classes, Context, Next, True);
                  Distribution (Next) := Confidence'Last;
               end if;
            end loop;
            Update (Node, Context);
         end;
      end if;
   end loop;
end Fuzzy.Graph.Learning.Implementation.Necessity;
