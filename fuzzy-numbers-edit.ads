--                                                                    --
--  package Fuzzy.Numbers.Edit      Copyright (c)  Dmitry A. Kazakov  --
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

generic
   with procedure Get
                  (  Source  : in String;
                     Pointer : in out Integer;
                     Value   : out Number;
                     Base    : in NumberBase := 10
                  )  is <>;
   with procedure Put
                  (  Destination : in out String;
                     Pointer     : in out Integer;
                     Value       : in Number;
                     Base        : in NumberBase := 10;
                     RelSmall    : in Positive   := MaxSmall;
                     AbsSmall    : in Integer    := -MaxSmall;
                     Field       : in Natural    := 0;
                     Justify     : in Alignment  := Left;
                     Fill        : in Character  := ' '
                  )  is <>;
package Fuzzy.Numbers.Edit is
--
-- Get -- Get a fuzzy number from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base of interval bounds
--
-- This procedure gets a fuzzy number from the string Source. The number
-- has  the  syntax  of a fuzzy set (see Fuzzy.Edit) with the difference
-- that  <index>  has  the  form  of  a  number (according to the actual
-- parameter provided for Get). The fuzzy set representing a number must
-- be  normalized,  i.e.  at  least  one number of the domain shall have
-- confidence  1. Then before that number the distribution of confidence
-- shall monotonically  ascend  and  after  it  the  distribution  shall
-- monotonically descend. Otherwise Constraint_Error is propagated.
--
-- Exceptions:
--
--    Constraint_Error - Not a valid number
--    Data_Error       - Syntax error
--    End_Error        - There is no any fuzzy number
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Number;
                Base    : in NumberBase := 10
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
--    Constraint_Error - Not a valid number
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value
            (  Source : in String;
               Base   : in NumberBase := 10
            )  return Fuzzy_Number;
--
-- Put -- Put a fuzzy number into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The set to be put
--    Base        - The base used to output interval bounds
--    RelSmall    - Relative precision of interval bounds
--    AbsSmall    - Absolute one
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the fuzzy number  specified  by  the  parameter
-- Value into the output  string  Destination.  The  string  is  written
-- starting from Destination (Pointer). See Strings_Edit.Float_Edit  for
-- further information about other parameters.
--
-- Exceptions:
--
--    Layout_Error -- Pointer is not in Destination'Range or there is no
--                    room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Fuzzy_Number;
                Base        : in NumberBase := 10;
                RelSmall    : in Positive   := MaxSmall;
                AbsSmall    : in Integer    := -MaxSmall;
                Field       : in Natural    := 0;
                Justify     : in Alignment  := Left;
                Fill        : in Character  := ' '
             );
--
-- Image -- Fuzzy number to string conversion
--
--    Value    - The value to be converted
--    Base     - The base used to output interval bounds
--    RelSmall - Relative precision of interval bounds
--    AbsSmall - Absolute one
--
-- This function converts Value to string.
--
-- Returns :
--
--    The string
--
   function Image
            (  Value    : in Fuzzy_Number;
               Base     : in NumberBase := 10;
               RelSmall : in Positive   := MaxSmall;
               AbsSmall : in Integer    := -MaxSmall
            )  return String;

end Fuzzy.Numbers.Edit;
