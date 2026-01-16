--                                                                    --
--  package Fuzzy.Graph.Handle.HTML Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Fuzzy.Graph.HTML;  use Fuzzy.Graph.HTML; 

package body Fuzzy.Graph.Handle.HTML is

   procedure Put
             (  File       : File_Type;
                Node       : Node_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             )  is
   begin
      Put (File, Ptr (Node).all, Parameters);
   end Put;

   procedure Put
             (  Node       : Node_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             )  is
   begin
      Put (Standard_Output, Ptr (Node).all, Parameters);
   end Put;

end Fuzzy.Graph.Handle.HTML;
