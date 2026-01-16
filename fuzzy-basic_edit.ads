--                                                                    --
--  package Fuzzy.Basic_Edit        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2003       --
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

package Fuzzy.Basic_Edit is
   type Delimiter_Type is (Arrow, Ellipsis, Comma, Colon, Semicolon);
--
-- Get_Delimiter -- Get a delimiter
--
--    Source    - The string to be processed
--    Pointer   - The current position in the string
--    Success   - True if a delimiter was matched
--    Delimiter - The expected type of delimiter
--
-- This  procedure  is  used to skip one delimiter {->|:|,|;|..|...} and
-- surrounding it characters representing UTF-8 encoded  code  positions
-- from the  set  Name_Tables.Blanks.  Pointer  is  changed  only  if  a
-- delimiter was found. The parameter Delimiter specifies what should be
-- recognized.
--
-- Exceptions :
--
--    Layout_Error - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get_Delimiter
             (  Source    : String;
                Pointer   : in out Integer;
                Success   : out Boolean;
                Delimiter : Delimiter_Type
             );
--
-- Get_Weight -- Get weight of a domain point value
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Default - The default
--
-- This procedure is used to get the weight of a domain point  value  in
-- the  form  [:<possibility>]. If no colon present, Default is set into
-- Value.  Colon  may  be  surrounded  by  characters representing UTF-8
-- encoded code points from the set Name_Tables.Blanks.
--
-- Exceptions :
--
--    Data_Error   - Syntax error or level is out of range
--    Layout_Error - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get_Weight
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Confidence;
                Default : Confidence := Confidence'Last
             );
end Fuzzy.Basic_Edit;
