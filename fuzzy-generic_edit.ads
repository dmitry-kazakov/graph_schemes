--                                                                    --
--  package Fuzzy.Generic_Edit      Copyright (c)  Dmitry A. Kazakov  --
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
--
--  This generic package provides string I/O operations on  fuzzy  sets.
--  It has the following parameters:
--
--     User_Data - A type to provide additional parameter
--     Get       - A procedure to get a range of domain set values
--     Put       - A procedure to put a range of values
--
--  Get. An implementation of Get has the following parameters:
--
--     Source    - The string to be processed
--     Pointer   - The current position in the string
--     Data      - The user data
--     From      - The index of the lower boundary of the range
--     To        - The index of the upper boundary
--     Exclusive - Range exclusion flag
--
--  This procedure gets a range of the domain set values from the string
--  Source.  The  process  starts  from  Source  (Pointer).  Pointer  is
--  advanced  to  the string position following the range. The result is
--  From..To, which is an interval of indices in Set (a fuzzy set).  The
--  range syntax is usually:
--
--     <value> [{..|...}<value>]
--
--  Characters  representing  UTF-8  encoded  code  points  from the set
--  Name_Tables.Blanks  may  surround ellipsis. Ellipsis may be built of
--  two  two  or  three  dots.  The  syntax  of  <value>  depends on the
--  implementation. When the output parameter Exclusive is set  to  True
--  the range From..To is cchecked for being exclusive,  non-overlapping
--  with  other  ranges  unless  the  truth  values  are  exactly  same.
--  Otherwise, it  is  allowed  for  overlapping  and  truth  values  in
--  overlapping domain points accumulate.
--
--  Exceptions:
--
--     Constraint_Error - A value is out of range or illegal range
--     Data_Error       - Syntax error
--     End_Error        - There is no any value range
--     Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
-- Put. An implementation of Put has the following parameters:
--
--     Destination - The string that accepts the output
--     Pointer     - The current position in the string
--     Data        - The user data
--     From        - The index of the lower boundary of the range
--     To          - The index of the upper boundary
--     Field       - The output field
--     Justify     - Alignment within the field
--     Fill        - The fill character
--
-- This procedure  places  a range of the domain set values specified by
-- the  parameters  From..To  into  the  output  string Destination. The
-- string  is  written  starting  from Destination (Pointer). Pointer is
-- then advanced to point  after  the  end  of  the  output  field.  The
-- parameter Field determines the width of the output field. Zero  width
-- means the minimal possible  width.  If  Field  is  not  zero  Justify
-- determines the way the value should  be  aligned  within  the  output
-- field.  The  parameter  Fill is then the fill character. If the range
-- From..To  contains  only  one  point  the  syntax  <value>  is  used.
-- Otherwise, the syntax of output is <value>..<value>.
--
-- Exceptions:
--
--    Constraint_Error - Illegal range
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
with Strings_Edit;  use Strings_Edit;

generic
   type User_Data (<>) is limited private;
   with procedure Get
                  (  Source    : String;
                     Pointer   : in out Integer;
                     Data      : User_Data;
                     From      : out Integer;
                     To        : out Integer;
                     Exclusive : out Boolean
                  );
   with procedure Put
                  (  Destination : in out String;
                     Pointer     : in out Integer;
                     Data        : User_Data;
                     From        : Integer;
                     To          : Integer;
                     Field       : Natural   := 0;
                     Justify     : Alignment := Left;
                     Fill        : Character := ' '
                  );
package Fuzzy.Generic_Edit is
--
-- Get -- Get a fuzzy set from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Data    - The user data
--    Value   - The result
--
-- This  procedure  gets a fuzzy set from the string Source. The set has
-- the following syntax:
--
--    <fuzzy_set> ::= <item> [, <fuzzy_set>]
--    <item>      ::= <range> [: <level>]
--    <level>     ::= <confidence_factor_specification>
--
-- Here the syntax of <range> is defined by the procedure Get (a  formal
-- generic parameter). Commas and colons may be surrounded by characters
-- representing   UTF-8   encoded   code    points    from    the    set
-- Name_Tables.Blanks. Trailing  comma  or  colon  is  ignored  and  not
-- matched.
--
-- Exceptions:
--
--    Data_Error   - Syntax error
--    End_Error    - Nothing matched
--    Layout_Error - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data;
                Value   : out Set
             );
--
-- Value -- String to fuzzy set conversion
--
--    Source - The string to be processed
--    Data   - The user data
--    First  - The lower boundary of the set index
--    Last   - The upper boundary of the index
--
-- This  function  gets  a  fuzzy set from the string Source. The set is
-- defined  over First..Last. The set in the string can be surrounded by
-- representing   UTF-8   encoded   code    points    from    the    set
-- Name_Tables.Blanks.  The  whole  string  Source  should  be  matched.
-- Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Data_Error - Syntax error
--    End_Error  - Nothing matched
--
   function Value
            (  Source : String;
               Data   : User_Data;
               First  : Integer;
               Last   : Integer
            )  return Set;
--
-- Put -- Put a fuzzy set into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Data        - The user data
--    Value       - The set to be put
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the fuzzy set specified by the parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer).
--
-- Exceptions:
--
--    Constraint_Error - Value is an empty array
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Set;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Image -- Fuzzy set to string conversion
--
--    Data  - The user data
--    Value - The value to be converted
--
-- This function converts Value to string.
--
-- Returns :
--
--    The string
--
-- Exceptions :
--
--    Constraint_Error - Value is an empty array
--
   function Image (Data : User_Data; Value : Set) return String;

end Fuzzy.Generic_Edit;
