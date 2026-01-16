--                                                                    --
--  package HTML                    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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
--  This  package  defines  some  constants  useful for HTML output. The
--  postfix Beg indicates a command beginnig. The postfix  End  is  used
--  for command termination. 
--
with Ada.Text_IO;        use Ada.Text_IO;
with Strings_Edit.UTF8;  use Strings_Edit.UTF8;

package HTML is
   pragma Elaborate_Body (HTML);

   Align_Center_Beg : constant String := "<div align=""center"">";
   Align_End        : constant String := "</div>";
   Align_Left_Beg   : constant String := "<div align=""left"">";
   Align_Right_Beg  : constant String := "<div align=""right"">";
   Bar              : constant String := "|";
   Bold_Beg         : constant String := "<b>";
   Bold_End         : constant String := "</b>";
   Break            : constant String := "<br>";
   Cell_Beg         : constant String := "<td>";
   Cell_End         : constant String := "</td>";
   Color_End        : constant String := "</font>";
   Horizontal_Line  : constant String := "<hr>";
   Row_Beg          : constant String := "<tr>";
   Row_End          : constant String := "</tr>";
   Space            : constant String := "&nbsp;";
   Table_Beg        : constant String :=
                         "<table cellpadding=""0"" cellspacing=""0"">";
   Table_End        : constant String := "</table>";

   type Color_Component is range 0..255;
   for Color_Component'Size use 8;

   type Color is record
      Red   : Color_Component := 0;
      Green : Color_Component := 0;
      Blue  : Color_Component := 0;
   end record;
   pragma Pack (Color);

   White : constant Color := (255, 255, 255);
   Black : constant Color := (  0,   0,   0);
--
-- Color_Beg -- Turns the indicated color on
--
--    Value - The color
--
-- Returns :
--
--    The command beginning. Use Color_End to terminate it.
--
   function Color_Beg (Value : Color) return String;
--
-- To_String -- Converts color to a HTML parameter
--
--    Value - The color
--
-- Returns :
--
--    The string which may be used as a color parameter
--
   function To_String (Value : Color) return String;
--
-- Put_Latin1 -- A latin-1 encoded string into a text file
--
--    File  - To write
--    Value - The string
--
-- This procedure writes Value onto File in HTML  format.  That  is  all
-- special  characters  such  as  <,  >  etc  are   converted   to   the
-- corresponding HTML equivalents. 
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Put_Latin1 (File : File_Type; Value : String);
--
-- Put_UTF8 -- An UTF-8 encoded string into a text file
--
--    File  - To write
--    Value - The string
--    Error - The code point to substitute illegal characters
--
-- This procedure writes Value onto File in HTML  format.  Value  is  an
-- UTF-encoded string. If Value contains improperly encoded code points,
-- each wrong character of those is replaced by Error. 
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Put_UTF8
             (  File  : File_Type;
                Value : String;
                Error : UTF8_Code_Point
             );
end HTML;
