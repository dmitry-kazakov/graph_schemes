--                                                                    --
--  package Fuzzy.Integers.Edit     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
with Strings_Edit.Integer_Edit;

generic
   with package Integer_Edit is new Strings_Edit.Integer_Edit (Number);
package Fuzzy.Integers.Edit is
--
-- Get -- Get a fuzzy integer number from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base of interval bounds
--
-- This procedure gets a fuzzy number from the string Source. The number
-- has  the  syntax  of  a  fuzzy  set (see Fuzzy.Generic_Edit) with the
-- difference  that  the  domain  set  values has the form of an integer
-- number. The fuzzy set representing a number must be normalized,  i.e.
-- at least one number of the  domain  shall  have  confidence  1.  Then
-- before that number the distribution of confidence shall monotonically
-- ascend  and  after  it  the distribution shall monotonically descend.
-- Otherwise Constraint_Error is propagated.
--
-- Exceptions:
--
--    Constraint_Error - Not a valid fuzzy integer
--    Data_Error       - Syntax error
--    End_Error        - There is no any fuzzy number
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Integer;
                Base    : NumberBase := 10
             );
--
-- Value -- String to fuzzy number conversion
--
--    Source - The string to be processed
--    Base   - The base of interval bounds
--
-- This function gets a fuzzy number from the string Source. The set  in
-- the string can be surrounded by characters representing UTF-8 encoded
-- code  points from the set Name_Tables.Blanks. The whole string Source
-- should be matched. Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Constraint_Error - Not a valid fuzzy integer
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Fuzzy_Integer;
--
-- Put -- Put a fuzzy number into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The set to be put
--    Base        - The base of interval bounds
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the fuzzy number  specified  by  the  parameter
-- Value into the output  string  Destination.  The  string  is  written
-- starting from Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or there is  no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Fuzzy_Integer;
                Base        : NumberBase := 10;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Image -- Fuzzy number to string conversion
--
--    Value - The value to be converted
--    Base  - The base of interval bounds
--
-- This function converts Value to string.
--
-- Returns :
--
--    The string
--
   function Image
            (  Value : Fuzzy_Integer;
               Base  : NumberBase := 10
            )  return String;

end Fuzzy.Integers.Edit;
