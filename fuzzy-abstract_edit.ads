--                                                                    --
--  package Fuzzy.Abstract_Edit     Copyright (c)  Dmitry A. Kazakov  --
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

with Strings_Edit;  use Strings_Edit;

with Fuzzy.Generic_Edit;
with Object;

package Fuzzy.Abstract_Edit is
   pragma Elaborate_Body (Fuzzy.Abstract_Edit);
--
-- User_Data -- The abstract base type for set domain description
--
-- A derived type has to override the procedures Get and Put to  provide
-- I/O for the domain set values.
--
   type User_Data is abstract new Object.Entity with private;
   type User_Data_Ptr is access User_Data'Class;
--
-- Value_Index -- The index type used for the domain set values
--
-- The  domain set values are indexed starting from 1. These indices are
-- unrelated to ones of the set being input or output.
--
   type Value_Index is new Positive;
--
-- Get -- Instantiated from Fuzzy.Generic_Edit...
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Set
             );
--
-- Get -- Get a range of domain set values from a string
--
--    Source    - The string to be processed
--    Pointer   - The current position in the string
--    Data      - The user data
--    From      - The index of the lower boundary of the range
--    To        - The index of the upper boundary
--    Exclusive - Range exclusion flag
--
-- This procedure gets a range of the domain set values from the  string
-- Source. The process starts from Source (Pointer). Pointer is advanced
-- to  the  string position following the range. The result is From..To,
-- which   is   an   interval   of   indices.   From..To  should  be  in
-- 1..Last-First+1,  where  First  and  Last  are   ones   returned   by
-- Get_Max_Range. The range syntax is usually:
--
--    <value> [{..|...}<value>]
--
-- Spaces and tabs may surround ellipsis. Ellipsis may be built  of  two
-- two   or   three   dots.   The  syntax  of  <value>  depends  on  the
-- implementation.  When  the  output parameter Exclusive is set to True
-- the  range  From..To is cchecked for being exclusive, non-overlapping
-- with  other  ranges  unless  the  truth  values  are  exactly   same.
-- Otherwise,  it  is  allowed  for  overlapping  and  truth  values  in
-- overlapping domain points accumulate.
--
-- Exceptions:
--
--    Constraint_Error - A value is out of range or illegal range
--    Data_Error       - Syntax error
--    End_Error        - There is no any value range
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : User_Data;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             )  is abstract;
--
-- Get_Max_Range -- Get the maximal range of the set indices
--
--    Data - The user data
--    From - The lower boundary of the range
--    To   - The upper boundary
--
-- When a set is returned as  a  result  this  procedure  is  called  to
-- determine the index range of the result. Which is From..To then.
--
   procedure Get_Max_Range
             (  Data    : User_Data;
                From    : out Integer;
                To      : out Integer
             )  is abstract;
--
-- Image -- Instantiated from Fuzzy.Generic_Edit...
--
   function Image (Data : User_Data'Class; Value : Set) return String;
--
-- Put -- Instantiated from Fuzzy.Generic_Edit...
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data'Class;
                Value       : Set;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Put -- Put a range of domain set values into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Data        - The user data
--    From        - The index of the lower boundary of the range
--    To          - The index of the upper boundary
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places a range of the domain set values  specified  by
-- the  parameters From..To into the output string Destination. From and
-- To are positive indices of the domain set values. Assuming  that  the
-- first  value  has  the  index  1. The string is written starting from
-- Destination  (Pointer).  Pointer  is then advanced to point after the
-- end  of the output field. The parameter Field determines the width of
-- the  output  field.  Zero  width means the minimal possible width. If
-- Field  is  not  zero  Justify  determines the way the value should be
-- aligned  within the output field. The parameter Fill is then the fill
-- character.  If  the range From..To contains only one point the syntax
-- <value> is used. Otherwise, the syntax of output is <value>..<value>.
--
-- Exceptions:
--
--    Constraint_Error - Illegal range
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is abstract;
--
-- Value -- Instantiated from Fuzzy.Generic_Edit...
--
   function Value
            (  Source : String;
               Data   : User_Data'Class
            )  return Set;

private
   pragma Inline (Get);
   pragma Inline (Image);
   pragma Inline (Put);
   pragma Inline (Value);

   type User_Data is abstract new Object.Entity with null record;

   type User_Data_Ref is access constant User_Data'Class;
   type IO_Data
        (  Data : User_Data_Ref;
           From : Integer;
           To   : Integer
        )  is limited null record;
--
-- Get_Range -- Of domain values (dispatches to abstract Get)
--
   procedure Get_Range
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : IO_Data;
                From      : out Integer;
                To        : out Integer;
                Exclusive : out Boolean
             );
--
-- Put_Range -- Of domain values (dispatches to abstract Put)
--
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : IO_Data;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Set_Edit -- Implementation through an instantiation
--
   package Set_Edit is
      new Fuzzy.Generic_Edit
          (  User_Data => IO_Data,
             Get       => Get_Range,
             Put       => Put_Range
          );

   pragma Inline (Get_Range);
   pragma Inline (Put_Range);

end Fuzzy.Abstract_Edit;
