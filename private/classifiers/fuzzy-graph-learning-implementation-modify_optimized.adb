--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.                       Luebeck            --
--        Implementation.Modify_Optimized          Spring, 2003       --
--  Separate body implementation                                      --
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

separate (Fuzzy.Graph.Learning.Implementation)
   procedure Modify_Optimized
             (  Data   : in out Graph_Training_Data'Class;
                Parent : in out Separation_Data;
                Value  : in out Fuzzy.Set
             )  is
   Proxy : Node_Proxy :=
              Ref
              (  Get_Child
                 (  Ptr (Parent.Node).all,
                    Parent.Current
              )  );
begin
   if Is_Valid (Proxy) then
      --
      -- Here we are because an existing child node is being modified
      --
      declare
         Affected : Domain_Subset (Parent.Current..Value'Last) :=
                       (others => False);
      begin
         Affected (Parent.Current) := True;
         declare
            Branch : Graph_Node'Class renames Ptr (Parent.Node).all;
            Child  : constant Graph_Node_Ptr := Ptr (Proxy);
            Count  : Positive       := 2;
         begin            
            --
            -- The child can be re-used at  the  points  with  the  arcs
            -- leading  to the same node. These nodes increase the share
            -- count  of  the proxy, because all of them will be updated
            -- in the course of learning.  
            --
            for Next in Parent.Current + 1..Value'Last loop
               if (  Equal (Value, Parent.Current, Next)
                 and then
                     Get_Child (Branch, Next) = Child 
                  )
               then
                  Count           := Count + 1;
                  Affected (Next) := True; 
                  Value    (Next) := Confidence'First;
               end if;
            end loop;
            Proxy.Exclusive :=
               Parent.Node.Exclusive and then Child.Use_Count = Count;
         end;
         Learn_Old (Proxy, Data, Value (Parent.Current), Parent); 
         declare
            Child : constant Graph_Node_Ptr := Ptr (Proxy);
         begin
            for Index in Affected'Range loop
               if Affected (Index) then
                  Connect (Parent.Node, Child, Index);
               end if;
            end loop;
         end;
      end;
   else   
      --
      -- There is no node associated with this domain  point.  So  we
      -- have to create a new node. 
      --
      declare
         Child : Graph_Node_Ptr;
      begin
         Learn_New (Child, Data, Value (Parent.Current), Parent);
         Connect (Parent.Node, Child, Parent.Current);
         Parent.Splitter := True;
         --
         -- Try to reuse the newly created node
         --
         declare
            Branch : Graph_Node'Class renames Ptr (Parent.Node).all;
         begin
            for Next in Parent.Current + 1..Value'Last loop
               if (  Equal (Value, Parent.Current, Next)
                  and then
                     Get_Child (Branch, Next) = null
                  )
               then
                  Connect (Parent.Node, Child, Next);
                  Value (Next) := Confidence'First;
               end if;
            end loop;
         end;
         return;
      exception
         when others =>
            if Child /= null and then Child.Use_Count = 0 then
               Free (Child);
            end if;
            raise;
      end;
   end if;
end Modify_Optimized;
