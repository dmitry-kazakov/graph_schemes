--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Learn_Boxes                            Autumn, 2008       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Confidence_Factors;       use Confidence_Factors;
with Gtk.Activity_Indicator;   use Gtk.Activity_Indicator;
with Gtk.Button;               use Gtk.Button;
with Gtk.Fuzzy_Features_List;  use Gtk.Fuzzy_Features_List;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Table;                use Gtk.Table;

with Fuzzy.Feature.Handle.Container;
with Fuzzy.Feature.Handle.Unbounded_Arrays;

private package Gtk.Fuzzy_Catalogue.Learn_Boxes is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Learn_Boxes);
   use Fuzzy.Feature.Handle.Container;
   use Fuzzy.Feature.Handle.Unbounded_Arrays;
--
-- Learn_Box_Record -- Box of training
--
   type Learn_Box_Record is new Gtk_Item_Box_Record with record
      Trained           : Boolean := False;
         -- Configuration data
      Config_Grid       : Gtk_Table;
      Lecture_Name      : Gtk_Picker_Box;
      Lecture_Name_Hint : Gtk_Box;
      From_Edit         : Gtk_Entry;
      From_Hint         : Gtk_Box;
      To_Edit           : Gtk_Entry;
      To_Hint           : Gtk_Box;
      Threshold_Entry   : Gtk_Entry;
      Threshold_Hint    : Gtk_Box;
      Equivalence_Entry : Gtk_Entry;
      Equivalence_Hint  : Gtk_Box;
      Total             : Gtk_Label;
      List              : Gtk_Fuzzy_Features_List;
      Feature_Buttons   : Gtk_HBox;
      Up_Button         : Gtk_Button;
      Down_Button       : Gtk_Button;
      Directory_Changed : Handler_Reference;
      Panel_Switched    : Handler_Reference;
      Selection_Changed : Handler_Reference;
         -- Training data
      Learn_Grid        : Gtk_Table;
      Folder_Name       : Folder_Selection;
      Name_Edit         : Gtk_Entry;
      Name_Hint         : Gtk_Box;
      Animation         : Gtk_Activity_Indicator;
      Classifier        : Classifier_Handle;
      Lesson            : Lecture_Handle;
      Name              : Unbounded_String;
      Features          : Unbounded_Array;
      From              : Integer;
      To                : Integer;
      Threshold         : Confidence;
      Equivalence       : Confidence;
   end record;
   type Learn_Box is access all Learn_Box_Record'Class;
--
-- Aborted -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Aborted
             (  Item  : not null access Learn_Box_Record;
                Fault : Exception_Occurrence
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Learn_Box_Record);
--
-- Completed-- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed (Item : not null access Learn_Box_Record);
--
-- Create_Indicator-- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   function Create_Indicator (Item : not null access Learn_Box_Record)
      return Gtk_Indicator;
--
-- Down -- Clicked event
--
   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- Equivalence_Changed -- Changed event
--
   procedure Equivalence_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- Finalize -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Finalize (Item : not null access Learn_Box_Record);
--
-- From_Changed -- Changed event
--
   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- Gtk_New -- Construction
--
--    Item        - The result
--    Browser     - The parent widget
--    Set_Lecture - Initialize the training set name field
--
   procedure Gtk_New
             (  Item        : out Learn_Box;
                Browser     : not null access
                              Gtk_Fuzzy_Catalogue_Record'Class;
                Set_Lecture : Boolean
             );
--
-- Initialize -- To be called by derived types
--
--    Item        - To construct
--    Set_Lecture - Initialize the training set name field
--
   procedure Initialize
             (  Item        : not null access Learn_Box_Record'Class;
                Set_Lecture : Boolean
             );
--
-- Lecture_Name_Changed -- Name changed event
--
   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- Previous -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Previous (Item : not null access Learn_Box_Record);
--
-- Select_Features -- Overrides Gtk.Fuzzy_Catalogue...
--
--    Item - The widget
--
-- Returns :
--
--    Selected features set
--
   function Select_Features
            (  Widget : not null access Learn_Box_Record
            )  return Fuzzy.Feature.Handle.Container.Set;
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service (Item : not null access Learn_Box_Record);
--
-- State_Changed -- Changed event
--
   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- Threshold_Changed -- Changed event
--
   procedure Threshold_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );
--
-- To_Changed -- Changed event
--
   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             );

   package Trainers is
      new GLib.Object.Weak_References (Learn_Box_Record);

   package Trainer_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Learn_Box
           );

end Gtk.Fuzzy_Catalogue.Learn_Boxes;
