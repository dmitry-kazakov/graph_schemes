--                                                                    --
--  package Fuzzy.Graph.Edit        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2004       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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

with Fuzzy.Feature.Handle.Tables;  use Fuzzy.Feature.Handle.Tables;
with Fuzzy.Graph.Memory_Resident;  use Fuzzy.Graph.Memory_Resident;
with Strings_Edit;                 use Strings_Edit;

package Fuzzy.Graph.Edit is
--
-- Get -- Input a graph from a string
--
--    Source     - The source string
--    Pointer    - The position to start
--    Node       - The result, a pointer to the root node
--    Features   - The table of features used
--    Parameters - The input parameters
--    Factory    - The node factory (a pointer to)
--
-- This procedure gets a graph from the text string Source. It starts in
-- Source  (Pointer)  and  advances Pointer to the first string position
-- following the graph image. The syntax of a graph is as follows: 
--
--    <graph>     := <branch> | <leaf>
--    <leaf>      := <feature> = <classification>
--    <branch>    := <feature> (<children>)
--    <children>  := <child> [; <children> ]
--    <child>     := <condition> => <graph>
--    <condition> := <interval> [, <condition> ]
--
-- Here <feature> is a feature name. The names of all features to expect
-- are provided by the table Features. This parameter is a  table  which
-- maps unique feature names  to  handles.  <classification>  is  the  a
-- feature classification as described in Fuzzy.Feature.Handle.Edit. The
-- syntax depends on the actual feature specified before  (see  <leaf>).
-- <interval>  is  an  interval  domain  feature   domain   set   values
-- (Fuzzy.Feature.Handle.Edit). The syntax depends on the  feature  (see
-- <branch>). Singular values are valid. Additional  constraint  imposed
-- is  that  all  leaf  nodes  have  same  feature.  Also the conditions
-- specified for one branch may not overlap. The graph nodes are created
-- using the node factory provided by the parameter Factory. The  result
-- is returned as a pointer to the root node of created graph.  
-- 
-- Exceptions :
--
--    Data_Error   - Syntax error
--    End_Error    - No graph description recognized
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--    Use_Error    - An invalid handle in Features
--
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Node       : out Graph_Node_Ptr;
                Features   : Table;
                Parameters : Input_Parameters'Class;
                Factory    : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
             );
--
-- Image -- Graph to string conversion
--
--    Node       - The root node
--    Parameters - The output parameters
--
-- Returns :
--
--    A string containing image of the graph rooted in Node
--
-- Exceptions :
--
--    Constraint_Error - An invalid node
--
   function Image
            (  Node       : Graph_Node'Class;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Put -- Output a graph rooted in the specified node
--
--    Destination - The output string
--    Pointer     - The output position there
--    Node        - The root node
--    Parameters  - The output parameters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure outputs Node into the string Destination.  The  string
-- is written  starting  from  the  Destination  (Pointer).  Pointer  is
-- advanced to point to the first character following the output. Field,
-- Justify and Fill are described in Strings_Edit. 
--
-- Exceptions :
--
--    Constraint_Error - An invalid node
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output. 
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Graph_Node'Class;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value - String to graph conversion
--
--    Source     - The source string
--    Features   - The table of features used
--    Parameters - The input parameters
--    Factory    - The node factory (a pointer to)
--
-- This function is a simplified version of the procedure Get. It parses
-- the  string  Source  and  returns  the corresponding graph. The whole
-- string have to be matched. Otherwise, Data_Error is propagated. 
--
-- Returns :
--
--    A pointer to the root node
--
-- Exceptions :
--
--    Data_Error - Syntax error
--    End_Error  - No graph description recognized
--    Use_Error  - An invalid handle in Features
--
   function Value
            (  Source     : String;
               Features   : Table;
               Parameters : Input_Parameters'Class;
               Factory    : not null access Node_Factory'Class :=
                               Fuzzy.Graph.Memory_Resident.Factory
            )  return Graph_Node_Ptr;

end Fuzzy.Graph.Edit;
