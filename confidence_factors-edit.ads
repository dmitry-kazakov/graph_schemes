--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Confidence_Factors.Edit                     Luebeck            --
--  Interface                                      Winter, 2000       --
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

with Strings_Edit;  use Strings_Edit;

package Confidence_Factors.Edit is
   pragma Elaborate_Body (Confidence_Factors.Edit);
--
-- Get -- Get a confidence factor from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--
-- This  procedure  gets a confidence factor from the string Source. The
-- factor  can  be  given  either  in a numeric form or as true or false
-- (case insensitive). The process starts from Source (Pointer).
--
-- Exceptions:
--
--    Constraint_Error - The factor is not in 0.0..1.0
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any factor
--    Layout_Error     - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Confidence
             );
--
-- Value -- String to confidence factor conversion
--
--    Source  - The string to be processed
--
-- This function gets a confidence factor from the  string  Source.  The
-- factor  can be surrounded by characters representing code points from
-- the  set  Name_Tables.Blanks.  The  whole  string  Source  should  be
-- matched. Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Constraint_Error - The factor is not in First..Last
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value (Source : in String) return Confidence;
--
-- Put -- Put a confidence factor into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The number to be put
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places  the  confidence  factor  specified  by   the
-- parameter Value into the output string  Destination.  The  string  is
-- written starting from Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error -- Pointer is not in Destination'Range or there is no
--                    room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Confidence;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             );
--
-- Image -- Confidence to string conversion
--
--    Value      - The value to be converted
--    RelSmall   - Relative precision of the output
--    AbsSmall   - Absolute one
--
-- This function converts Value to string.
--
-- Returns :
--
--    The string
--
   function Image (Value : in Confidence) return String;

end Confidence_Factors.Edit;
