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
--
-- Necessity -- Update a graph with one additional example
--
--    Node         - To be modified (a handle to, maybe invalid)
--    Context      - The training data
--    Distribution - The image of the class-feature
--
-- This procedure uses either Learn_New or Learn_Old to create or update
-- a graph node specified by a handle. For an invalid handle it  creates
-- a new node.
--
generic
procedure Fuzzy.Graph.Learning.Implementation.Necessity
          (  Node         : in out Node_Handle;
             Context      : in out Graph_Training_Data'Class;
             Distribution : in out Fuzzy.Set
          );
