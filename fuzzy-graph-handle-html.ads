--                                                                    --
--  package Fuzzy.Graph.Handle.HTML Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
--
--  This package provides HTML output for fuzzy graphs.
--
with Ada.Text_IO;                use Ada.Text_IO;
with Fuzzy.Feature.HTML;         use Fuzzy.Feature.HTML;
with Fuzzy.Feature.Handle.HTML;  use Fuzzy.Feature.Handle.HTML;

package Fuzzy.Graph.Handle.HTML is
--
-- Put -- Output a graph rooted in the specified node
--
--  [ File ]     - The output file
--    Node       - The root node handle
--    Parameters - The output parameters
--
-- These procedures  write  the  graph  rooted  in  Node  into  File  or
-- strandard  output.  The  fields  of  Parameters control the output of
-- feature values as described in Fuzzy.Feature.Handle.HTML. 
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or node
--    I/O exceptions
--
   procedure Put
             (  File       : File_Type;
                Node       : Node_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  Node       : Node_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
private
   pragma Inline (Put);
   
end Fuzzy.Graph.Handle.HTML;
