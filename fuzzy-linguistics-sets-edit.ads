--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Sets.Edit                 Luebeck            --
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

with Strings_Edit;  use Strings_Edit;

with Fuzzy.Linguistics.Variables_Edit;

generic
   with package Linguistic_Edit is new Variables_Edit (<>);
package Fuzzy.Linguistics.Sets.Edit is
--
-- Get -- Get a set of linguistic variables
--
--    Source  - String to parse
--    Pointer - Starting string position
--    Value   - The set of linguistic variables
--    Base    - The base used for numbers
--
-- This procedure parses the string Source. If a description of a set of
-- lingustic  variables  is  successfully  matched  starting from Source
-- (Pointer),  Pointer  is  advanced  to  the  position  following   the
-- description. The syntax is:
--
--    <set>  ::= <item>[,<set>]
--    <item> ::= <name>(<variable>)
--
-- Here  <name>  is a valid name as described in Name_Tables. <variable>
-- has  the  same  syntax as described in Fuzzy.Linguistics.Edit for the
-- procedure  Get.  Commas  and brackets can be surrounded by characters
-- representing UTF-8 encoded code points of the set Name_Tables.Blanks.
-- Trailing comma is ignored and so not matched.
--
-- Exceptions :
--
--    Data_Error   - Syntax error
--    End_Error    - Nothing matched
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--    Name_Error   - Duplicate variable name
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Linguistic_Set;
                Base    : NumberBase := 10
             );
--
-- Get -- Get a subset of / classification on linguistic variables
--
--    Source  - String to parse
--    Pointer - Starting string position
--    Set     - The set of linguistic variables
--    Value   - The result
--    Default - The defaults used for the confidence factors
--    Base    - The base used for numbers
--
-- This procedure parses the string Source. If subset  /  classification
-- is successfully matched starting from Source  (Pointer),  Pointer  is
-- advanced to the position following the matched text. A value  can  be
-- given as either:
--
-- (o)  In  terms  of  the names of the variables from Set. This variant
--      uses  the syntax described in Fuzzy.Generic_Edit.Intuitionistic.
--      For example, if Set contains variables Red, Green, Blue. Then  a
--      subset  /  classification  can  be  specified as Red..Green:0.5,
--      Blue:1:0.8. The parameter Default specifies the  possiblity  and
--      necessity   when   missing.  See  Fuzzy.Edit.Intuitionistic  for
--      details.
--
-- (o)  A variable over the original numeric domain. This  variant  uses
--      the  syntax  described  in  Fuzzy.Linguistics.Edit. For example:
--      -10.0:0, 12, 14:0. In this case  the  variable  is  converted  /
--      fuzzified and the result is stored  into  Value.  The  parameter
--      Base determines the number base.
--
-- Exceptions :
--
--    Constraint_Error - Different cardinalities, number is out of range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Uncertain;
                Base    : NumberBase    := 10
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Uncertain;
                Base    : NumberBase    := 10
             );
--
-- Image -- A string representation of a linguistic set
--
--    Value - The set
--    Base  - The base used for numbers
--
-- This function is a shortcut for the procedure Put.
--
-- Returns :
--
--    Image string of the set
--
   function Image
            (  Value    : Linguistic_Set;
               Base     : NumberBase := 10;
               RelSmall : Positive   := MaxSmall;
               AbsSmall : Integer    := -MaxSmall
            )  return String;
--
-- Image -- A string representation of a subset / classification
--
--    Set     - The set of linguistic variables
--    Value   - The subset / classification
--    Default - The defaults used for the confidence factors
--
-- This function is a shortcut for the procedure Put.
--
-- Returns :
--
--    Image string of Value
--
-- Exceptions :
--
--    Constraint_Error - Different cardinalities
--
   function Image
            (  Set     : Linguistic_Set;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Uncertain
            )  return String;
   function Image
            (  Set     : Linguistic_Set;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Uncertain
            )  return String;
--
-- Put -- Put a set of linguistic variables
--
--    Destination - The string that accepts the output
--    Pointer     - Starting string position
--    Value       - The set of linguistic variables
--    Base        - The base of the domain point values
--    RelSmall    - Relative precision of the values
--    AbsSmall    - Absolute one
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places a description of a set of linguistic  variables
-- specified  by the parameter Value into the output string Destination.
-- The string is written starting from Destination (Pointer). Pointer is
-- then advanced to point  after  the  end  of  the  output  field.  The
-- parameters  Base,  RelSmall,  AbsSmall  have  meaning  described   in
-- Strings_Edit.Float_Edit.  The  output has the syntax described in the
-- procedure Get. The parameter Field determines the width of the output
-- field. Zero width means the minimal possible width. If Field  is  not
-- zero Justify determines the way the value should  be  aligned  within
-- the output field. The parameter Fill is then the fill character.
--
-- Exceptions :
--
--    Constraint_Error - Empty set
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Linguistic_Set;
                Base        : NumberBase := 10;
                RelSmall    : Positive   := MaxSmall;
                AbsSmall    : Integer    := -MaxSmall;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             );
--
-- Put -- Put a subset / classification into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Set         - The lingustic set
--    Value       - To put
--    Default     - The defaults used for the confidence factors
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places a subset of / classification on Set specified
-- by the parameter Value into the output string Destination. The string
-- is  written  starting  from  Destination  (Pointer).  Pointer is then
-- advanced to point after the end of the output field.  The  output  is
-- always in terms of  linguistic  variables  from  Set.  The  parameter
-- Default   specifies   how   to   omit   confidence    factors.    See
-- Fuzzy.Edit.Intuitionistic for details. The parameter Field determines
-- the  width of the output field. Zero width means the minimal possible
-- width.  If  Field  is  not  zero Justify determines the way the value
-- should be aligned within the output field. The parameter Fill is then
-- the fill character.
--
-- Exceptions :
--
--    Constraint_Error - Different cardinalities
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Set         : Linguistic_Set;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Uncertain;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Set         : Linguistic_Set;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Uncertain;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             );
--
-- Value -- Get a set of linguistic variables
--
--    Source - String to parse
--    Base   - The base used for numbers
--
-- This function is a  simplified  version  of  the  procedure  Get.  It
-- converts the string Source. The description of a set in Source may be
-- surrounded by characters representing UTF-8 encoded  code  points  of
-- the set  Name_Tables.Blanks.  The  whole  string  shall  be  matched.
-- Otherwise Data_Error is propagated.
--
-- Returns :
--
--    The set
--
-- Exceptions :
--
--    End_Error  - Nothing matched
--    Data_Error - Syntax error
--    Name_Error - Duplicate vriable name
--
   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Linguistic_Set;
--
-- Value -- Get a subset / classification
--
--    Source  - String to parse
--    Set     - The set of linguistic variables
--    Base    - The base used for numbers
--    Default - The defaults used for the confidence factors
--
-- This function is a  simplified  version  of  the  procedure  Get.  It
-- converts the string Source. The description of a set in Source may be
-- surrounded by characters representing UTF-8 encoded  code  points  of
-- the set  Name_Tables.Blanks.  The  whole  string  shall  be  matched.
-- Otherwise Data_Error is propagated.
--
-- Returns :
--
--    The a subset of / classification on Set
--
-- Exceptions :
--
--    Constraint_Error - Number is out of range
--    End_Error        - Nothing matched
--    Data_Error       - Syntax error
--
   function Value
            (  Source  : String;
               Set     : Linguistic_Set;
               Default : Fuzzy_Boolean := Uncertain;
               Base    : NumberBase := 10
            )  return Fuzzy.Intuitionistic.Set;
   function Value
            (  Source  : String;
               Set     : Linguistic_Set;
               Default : Fuzzy_Boolean := Uncertain;
               Base    : NumberBase := 10
            )  return Fuzzy.Intuitionistic.Classification;

end Fuzzy.Linguistics.Sets.Edit;
