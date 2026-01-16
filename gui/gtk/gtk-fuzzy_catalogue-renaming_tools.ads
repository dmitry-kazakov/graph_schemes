--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Renaming_Tools          Luebeck            --
--  Interface                                      Summer, 2009       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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

with Gtk.Handlers.References;  use Gtk.Handlers.References;

private package Gtk.Fuzzy_Catalogue.Renaming_Tools is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Renaming_Tools);
--
-- Renaming_Tool_Record -- Box of renaming tool
--
   type Renaming_Tool_Record (<>) is
      new Gtk_Item_Box_Record with private;
   type Renaming_Tool is access all Renaming_Tool_Record'Class;
--
-- Move -- Dialog box
--
--    Tool      - The tool widget
--    From      - Directory
--    To        - Directory
--    Items     - To move
--    Select_In - Where to select the items moved
--    Failed    - True if operation failed completely
--
   procedure Move
             (  Tool   : not null access Renaming_Tool_Record;
                From   : Item_Path;
                To     : Item_Path;
                Items  : in out Name_List;
                Failed : out Boolean
             );
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--    Mode    - Filter mode
--
   procedure Gtk_New
             (  Item    : out Renaming_Tool;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Mode    : Filter_Mode
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--    Mode - Filter mode
--
   procedure Initialize
             (  Item : not null access Renaming_Tool_Record'Class;
                Mode : Filter_Mode
             );

private
   package Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveItemsDown",
             Icon       => Stock_Go_Down,
             Tip        => "Move downwards",
             Relief     => Relief_None
          );
   use Down_Buttons;

   package Swap_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveItemsSwapFolders",
             Icon       => Swap_Folders_Icon,
             Tip        => "Swap folders on the panes",
             Relief     => Relief_None
          );
   use Swap_Buttons;

   package Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveItemsUp",
             Icon       => Stock_Go_Up,
             Tip        => "Move upwards",
             Relief     => Relief_None
          );
   use Up_Buttons;

   type Callbacks is array (1..6) of Handler_Reference;
   type Renaming_Tool_Record is new Gtk_Item_Box_Record with record
      Actions_Box : Gtk_HBox;
      Catalogue   : Gtk_Fuzzy_Catalogue;
      Handlers    : Callbacks;
      Down        : Down_Buttons.Gtk_Style_Button;
      Redo        : Redo_Buttons.Gtk_Style_Button;
      Swap        : Swap_Buttons.Gtk_Style_Button;
      Undo        : Undo_Buttons.Gtk_Style_Button;
      Up          : Up_Buttons.Gtk_Style_Button;
   end record;

   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   procedure Redo
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   procedure Swap
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   procedure Undo
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             );

   package Renaming_Tool_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Renaming_Tool
          );
end Gtk.Fuzzy_Catalogue.Renaming_Tools;
