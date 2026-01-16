--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.                       Luebeck            --
--        Implementation.Modify                    Spring, 2003       --
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
   procedure Modify
             (  Data       : in out Graph_Training_Data'Class;
                Parent     : in out Separation_Data;
                Feature    : Feature_Object'Class;
                Value      : Confidence
             )  is
   Snap  : Context_Snap;
   Proxy : Node_Proxy :=
              Ref
              (  Get_Child
                 (  Ptr (Parent.Node).all,
                    Parent.Current
              )  );
begin
   Create_Constraint (Feature, Data, False);
   Set_Constraint (Feature, Data, Parent.Current, True);
   if Is_Valid (Proxy) then
      --
      -- The node associated with this domain point gets modified. It is
      -- referenced  twice  once  by  the  parent  and once by the proxy
      -- handle. 
      --
      Proxy.Exclusive :=
         Parent.Node.Exclusive and then Ptr (Proxy).Use_Count = 2;
      Learn_Old (Proxy, Data, Value, Parent);
      Connect (Parent.Node, Ptr (Proxy), Parent.Current);
   else
      --
      -- There  is no node associated with this domain point. So we have
      -- to create a new node. 
      --
      declare
         Child : Graph_Node_Ptr;
      begin
         Learn_New (Child, Data, Value, Parent);
         Connect (Parent.Node, Child, Parent.Current);
         Parent.Splitter := True;
      exception
         when others =>
            if Child /= null and then Child.Use_Count = 0 then
               Free (Child);
            end if;
            raise;
      end;
   end if;
end Modify;
