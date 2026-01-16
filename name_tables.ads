--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Name_Tables                                 Luebeck            --
--  Interface                                      Spring, 2003       --
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
--  This package  instantiates  table  packages  required  for  sets  of
--  linguistic variables.
--
with Strings_Edit.UTF8;       use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Strings_Edit.UTF8.Maps.Constants;
use  Strings_Edit.UTF8.Maps.Constants;

with Tables.UTF8_Names;

package Name_Tables is
   pragma Elaborate_Body (Name_Tables);
--
-- The following variables determine valid names:
--
-- (o)  Capitals.  A  valid name starts with one of characters from this
--      set specified;
-- (o)  Name_Body. A valid name can contain  any  number  of  characters
--      from this set;
-- (o)  Ignored.  Any characters from this set are ignored when matched.
--      These  characters  are remove from names, when they are put into
--      tables;
-- (o)  Blanks. Any non-empty chain of characters from this set and from
--      the set  Ignored,  is  matched  by  any  non-empty  sequence  of
--      characters from this set;
-- (o)  Singletons.   A   valid  name  cannot  contain  two  consecutive
--      characters from this set  after  removing  all  characters  from
--      Ignored. For example, if '_' is a singleton then "a__b" is not a
--      valid  name. A singleton may not appear at the end of a name. So
--      "a_" would be also an invalid name;
-- (o)  Non_Breaks. When matched a valid name may not be followed by any
--      of the characters from  this  set.
--
-- Is_Body -- The  indicator  function  for  default  indentifier   body
--            characters. These are letters,  numeric  letters,  digits,
--            punctuation  connectors,  space  separators,   non-spacing
--            marks,  spacing  combining marks, other format, horizontal
--            tab.
--
   function Is_Body (Value : UTF8_Code_Point) return Boolean;
--
-- Is_Blank -- The  indicator  function  for  default  blanks, which are
--             space separators and horizontal tab.
--
   function Is_Blank (Value : UTF8_Code_Point) return Boolean;
--
-- Is_Non_Break -- The default non-break characters are letters, numeric
--                 letters, digits, punctuation connectors,  non-spacing
--                 marks, spacing combining marks, other format
--
   function Is_Non_Break (Value : UTF8_Code_Point) return Boolean;
--
-- Is_Singletons -- The default singletons are  punctuation  connectors,
--                  space separators and horizontal tab
--
   function Is_Singleton (Value : UTF8_Code_Point) return Boolean;

   Capitals   : Unicode_Set := Identifier_Start_Set;
   Blanks     : Unicode_Set := To_Set (Is_Blank'Access);
   Ignored    : Unicode_Set := Other_Format_Set;
   Name_Body  : Unicode_Set := To_Set (Is_Body'Access);
   Non_Breaks : Unicode_Set := To_Set (Is_Non_Break'Access);
   Singletons : Unicode_Set := To_Set (Is_Singleton'Access);
--
-- Check_Matched -- Check name margin
--
--    Source  - The source string
--    Pointer - Pointer to the character following the name
--
-- Returns :
--
--    True if Source (Pointer) is not in Non_Breaks
--
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
--
-- Check_Name -- Check spelling of a name
--
--    Name - To be checked
--
-- See valid name description above.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name
--
   procedure Check_Name (Name : String);
--
-- String_Maps -- Tables of strings
--
   package String_Maps is new Tables (Integer);
--
-- Name_Maps -- Tables of identifiers
--
   package Name_Maps is
      new String_Maps.UTF8_Names
          (  Check_Spelling => Check_Name,
             Check_Matched  => Check_Matched,
             Blanks         => Blanks,
             Ignored        => Ignored
          );
--
-- Get_Name -- Get a valid name from string
--
--    Source  - The string
--    Pointer - Source (Pointer.all) is the character to start from
--
-- This  function  reconizes  a  valid  (see Check_Name) name in Source.
-- Blanks following the name do not  influence  success  or  failure  of
-- matching. On success Pointer.all is advanced to the  first  character
-- following the name.
--
-- Returns :
--
--    The name
--
-- Exceptions :
--
--    Constraint_Error - Illegal name
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer.all is not in Source'First..Source'Last
--
   function Get_Name (Source : String; Pointer : access Integer)
      return String;

end Name_Tables;
