--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.                         Luebeck            --
--        Transformations                          Winter, 2005       --
--  Interface                                                         --
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

package Fuzzy.Graph.Handle.Transformations is
   pragma Elaborate_Body (Fuzzy.Graph.Handle.Transformations);
--
-- Rotate -- Graph rotation
--
--    Node    - A handle to the root node
--    Feature - To rotate the graph around
--  [ Data ]  - The node modification context to use
--
-- This procedure rotates the  graph  rooted  in  Node.  The  result  is
-- returned through Node. The rotated graph is defined as an  equivalent
-- graph  with  the  root  node  testing  the  feature  specified by the
-- parameter Feature. When the  graph  rooted  in  Node  does  not  test
-- Feature, it is not changed. The newly nodes  are  created  using  the
-- factory specified by the  parameter  Data.  When  this  parameter  is
-- omitted, the root node factory is used. 
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--
   procedure Rotate
             (  Node    : in out Node_Handle;
                Feature : Feature_Handle
             );
   procedure Rotate
             (  Node    : in out Node_Handle;
                Feature : Feature_Handle;
                Data    : in out Node_Modification_Data'Class 
             );

end Fuzzy.Graph.Handle.Transformations;
