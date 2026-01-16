--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Delimiter_Frame                          Winter, 2008       --
--  Interface                                                         --
--                                Last revision :  11:45 29 May 2020  --
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

with Gtk.Frame;         use Gtk.Frame;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Radio_Button;  use Gtk.Radio_Button;

package Gtk.Fuzzy_Catalogue.Delimiter_Frame is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Delimiter_Frame);
--
-- Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record -- Box of delimiters
--
   type Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_Frame_Record with private;
   type Gtk_Fuzzy_Catalogue_Deilimiter_Frame is
      access all Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record'Class;
--
-- Get -- The delimiter
--
--    Widget - The widget
--
-- Returns :
--
--    The delimiter specified by the widget
--
-- Exceptions :
--
--    Data_Error - The delimiter is invalid
--
   function Get
            (  Widget : not null access
                  Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record'Class
            )  return String;
--
-- Gtk_New -- Construction
--
--    Widget    - The result
--    Can_Space - Use space vs none choice
--    Title     - Frame label style name
--    Browser   - The parent widget
--
   procedure Gtk_New
             (  Widget    : out Gtk_Fuzzy_Catalogue_Deilimiter_Frame;
                Can_Space : Boolean;
                Title     : String;
                Browser   : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Widget    - To construct
--    Can_Space - Use space vs none choice
--    Title     - Frame label style name
--
   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record'Class;
                Can_Space : Boolean;
                Title     : String
             );
private
--
-- Lecture_Save_As_Text_Box_Record -- Box of feature creation
--
   type Gtk_Fuzzy_Catalogue_Deilimiter_Frame_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_Frame_Record with
   record
      Delimiter_Edit   : Gtk_Entry;
      Delimiter_Hint   : Gtk_Box;
      Other_Button     : Gtk_Radio_Button;
      Semicolon_Button : Gtk_Radio_Button;
      Space_Button     : Gtk_Radio_Button;
      Tabulator_Button : Gtk_Radio_Button;
      Can_Space        : Boolean;
   end record;

   package Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Gtk_Fuzzy_Catalogue_Deilimiter_Frame
           );

   procedure Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Parent : Gtk_Fuzzy_Catalogue_Deilimiter_Frame
             );

end Gtk.Fuzzy_Catalogue.Delimiter_Frame;
