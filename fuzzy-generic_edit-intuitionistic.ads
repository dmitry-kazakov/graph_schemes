--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Fuzzy.Generic_Edit.Intuitionistic          Luebeck            --
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

with Fuzzy.Logic;           use Fuzzy.Logic;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;

generic
package Fuzzy.Generic_Edit.Intuitionistic is
--
-- Get -- Get an intuitionistc set / classification on from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Data    - The user data
--    Value   - The result
--    Default - The default value for the confidence factors
--
-- These  procedures  get  a  fuzzy intuitionistic set or classification
-- from the string Source. The set has the following syntax:
--
--    <set>   ::= <item> [, <set>]
--    <item>  ::= <range> [:<possibility> [:<necessity>]]
--    <level> ::= <confidence_factor_specification>
--
-- Here the syntax of <range> is defined by the procedure Get (a  formal
-- generic parameter). The parameter Default determines  the  confidence
-- factors when omitted. If both are, then Default  is  the  result.  If
-- only the necessity is  omitted  then  the  result  necessity  is  the
-- specified possibility and Default.Necessity. Commas and colons may be
-- surrounded by characters representing UTF-8 encoded code points  from
-- the  set  Name_Tables.Blanks.  Trailing comma or colon is ignored and
-- not matched.
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
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Certain_True
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Certain_True
             );
--
-- Value -- String to intuitionistic set / classification conversion
--
--    Source      - The string to be processed
--    Data        - The user data
--    Cardinality - Of the result
--    Default     - The default value for the confidence factors
--
-- These functions get a fuzzy intuitionistic set or classification from
-- the  string  Source.  The  cardinality  is  defined  by the parameter
-- Cardinality. The input in the string can be surrounded by  characters
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
            (  Source      : String;
               Data        : User_Data;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Set;
   function Value
            (  Source      : String;
               Data        : User_Data;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Put -- Put an intuitionistic set / classification into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Data        - The user data
--    Value       - To be put
--    Default     - The way to omit confidence levels
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--    Default     - The default value for necessitiy
--
-- These  procedures  place a fuzzy intuitionistic set or classification
-- specified  by the parameter Value into the output string Destination.
-- The string  is  written  starting  from  Destination  (Pointer).  The
-- parameter  Default  determines  the  way  confidence  levels  can  be
-- omitted.  If the possibility / necessity pair equals to Default it is
-- omitted.   If   the   necessity   equals   to   the  possibility  and
-- Default.Necessity then only the possibility is output.
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or there is  no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             );
--
-- Image -- Yntuitionistic set / classification to string conversion
--
--    Data    - The user data
--    Value   - The value to be converted
--    Default - The default value for necessitiy
--
-- This function converts Value to string.
--
-- Returns :
--
--    The string
--
-- Exceptions :
--
--    Constraint_Error - Value is empty
--
   function Image
            (  Data    : User_Data;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Certain_True
            )  return String;
   function Image
            (  Data    : User_Data;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Certain_True
            )  return String;

end Fuzzy.Generic_Edit.Intuitionistic;
