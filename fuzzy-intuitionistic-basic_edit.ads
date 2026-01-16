--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Intuitionistic.Basic_Edit             Luebeck            --
--  Interface                                      Summer, 2003       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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

package Fuzzy.Intuitionistic.Basic_Edit is
   pragma Elaborate_Body (Fuzzy.Intuitionistic.Basic_Edit);
--
-- Get_Weight -- Get weight of a domain point value
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Default - The default
--
-- This procedure is used to get the weight of a domain point  value  in
-- the form [:<possibility>[:<necessity>]]. If no colon present, Default
-- is set into Value. If necessity is missing  then  Value.Necessity  is
-- Default.Necessity   and   the  specified  possiblity.  Colon  may  be
-- surrounded by spaces and tabs. 
--
-- Exceptions :
--
--    Data_Error   - Syntax error or a level is out of range
--    Layout_Error - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get_Weight
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Boolean;
                Default : Fuzzy_Boolean := Certain_True
             );

end Fuzzy.Intuitionistic.Basic_Edit;
