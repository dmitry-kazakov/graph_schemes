--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Edit                      Luebeck            --
--  Interface                                      Winter, 2003       --
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

with Strings_Edit.Float_Edit;

generic
   with package Float_Edit is
      new Strings_Edit.Float_Edit
          (  Fuzzy_Floats_Of.Float_Intervals_Of.Number_Of
          );
package Fuzzy.Linguistics.Edit is
   subtype Number_Of is Fuzzy_Floats_Of.Float_Intervals_Of.Number_Of;
--
-- Get -- Get linguistic variable from a string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base used for numbers
--
-- This procedure gets a variable from the string  Source.  The  process
-- starts from Source (Pointer).  Pointer  is  advanced  to  the  string
-- position following the variable. The syntax is:
--
--    <variable> ::= [->] <item> [{,|->}<variable>] [->]
--    <item>     ::= <number>[{..|...}<number>] [:<level>]
--
-- Here <number> has the syntax of a floating-point number as the formal
-- parameter  Float_Edit  requires.  The  parameter  Base determines its
-- base. <level> is a confidence factor. When missing it is  assumed  to
-- be Confidence'Last. A variable is specified as a list of items. Items
-- can be joined by either commas or arrows (->). Each item determines a
-- value of the membership function in one domain point or in a range of
-- points.  The  value  is specified by <level>. Numbers of items in the
-- list  are  specified  in  ascending  order.  When items are joined by
-- arrows, the membership function is linear in between.  Otherwise,  it
-- is 0 in between. Arrow can be given in front of the first item and/or
-- after  the  last  item  to  specify  extrapolation  of the membership
-- function  to  the  left  and/or  right. Numbers specified in singular
-- point  may  repeat to specify multiple values of the function. Commas
-- and  colons may be surrounded by characters representing UTF-8 encode
-- code points from the set Name_Tables.Blanks. When trailing  they  are
-- not matched.
--
-- Exceptions :
--
--    Constraint_Error - A number is out of range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
-- Examples :
--
--    ->0->10:0         -- Shoulder
--    0                 -- Singleton
--    0..10             -- Rectangle
--    -5:0->0..10->15:0 -- Trapezoid
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Variable;
                Base    : NumberBase := 10
             );
--
-- Image -- A string representation of a variable
--
--    Value    - The variable
--    Base     - The base used for numbers
--    RelSmall - Relative precision of the values
--    AbsSmall - Absolute one
--
-- This function is a shortcut for the procedure Put.
--
-- Returns :
--
--    Image string of the value
--
   function Image
            (  Value    : Variable;
               Base     : NumberBase := 10;
               RelSmall : Positive   := MaxSmall;
               AbsSmall : Integer    := -MaxSmall
            )  return String;
--
-- Value -- Conversion of a string to variable
--
--    Source - The string to be processed
--    Base   - The base used for numbers
--
-- This function gets a variable from the string Source. The  syntax  is
-- described in Get. It can be surrounded by  surrounded  by  characters
-- representing    UTF-8    encode    code    points    from   the   set
-- Name_Tables.Blanks.  The  whole  string  Source  should  be  matched.
-- Otherwise the exception Data_Error is propagated.
--
-- Returns :
--
--    The linguistic variable
--
-- Exceptions :
--
--    Constraint_Error - A number is out of range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--
   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Variable;
--
-- Put -- Put a variable into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The variable to put
--    Base        - The base of the domain point values
--    RelSmall    - Relative precision of the values
--    AbsSmall    - Absolute one
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the variable specified by the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer). Pointer is then advanced to  point  after
-- the end of the output field. The parameters Base, RelSmall,  AbsSmall
-- have meaning described in Strings_Edit.Float_Edit. The output has the
-- syntax described in the procedure Get. The parameter Field determines
-- the  width of the output field. Zero width means the minimal possible
-- width.  If  Field  is  not  zero Justify determines the way the value
-- should be aligned within the output field. The parameter Fill is then
-- the fill character.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Destination'Range or there is  no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Variable;
                Base        : NumberBase := 10;
                RelSmall    : Positive   := MaxSmall;
                AbsSmall    : Integer    := -MaxSmall;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             );

end Fuzzy.Linguistics.Edit;
