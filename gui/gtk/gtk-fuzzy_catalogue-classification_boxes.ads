--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Classification_Boxes                   Spring, 2008       --
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

with Fuzzy.Classifier;             use Fuzzy.Classifier;
with Gtk.Fuzzy_Classifier_Report;  use Gtk.Fuzzy_Classifier_Report;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Table;                    use Gtk.Table;
with Confidence_Factors;           use Confidence_Factors;

with Fuzzy.Intuitionistic;
with Gtk.Fuzzy_Generalization_Combo;

private package Gtk.Fuzzy_Catalogue.Classification_Boxes is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Classification_Boxes);
   use Gtk.Fuzzy_Generalization_Combo;

   type Classification_Box_Record;

   type Gtk_Fuzzy_Classifier_Report_Box_Record
        (  Item : access Classification_Box_Record'Class
        )  is new Gtk_Fuzzy_Classifier_Report_Record with null record;
   type Gtk_Fuzzy_Classifier_Report_Box is
      access all Gtk_Fuzzy_Classifier_Report_Box_Record'Class;
--
-- Gtk_New -- Factory
--
--    Widget   - The result
--    Item     - The classification box
--    Tooltips - The tooltips group to use or null
--
   procedure Gtk_New
             (  Widget   : out Gtk_Fuzzy_Classifier_Report_Box;
                Item     : not null access
                           Classification_Box_Record'Class
             );
--
-- Error -- Overrides Gtk.Fuzzy_Classifier_Report...
--
   overriding
   procedure Error
             (  Widget  : not null access
                          Gtk_Fuzzy_Classifier_Report_Box_Record;
                Message : UTF8_String
             );
--
-- Error_Reset -- Overrides Gtk.Fuzzy_Classifier_Report...
--
   overriding
   procedure Error_Reset
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Report_Box_Record
             );

   type Set_Ptr is access Fuzzy.Intuitionistic.Set;
--
-- Classification_Box_Record -- Box of training set classification
--
   type Classification_Box_Record is new Gtk_Item_Box_Record with record
      Classifier_Name      : Gtk_Picker_Box;
      Classifier_Name_Hint : Gtk_Box;
      Lecture_Name         : Gtk_Picker_Box;
      Lecture_Name_Hint    : Gtk_Box;
      Source_Name          : Gtk_Picker_Box;
      Source_Name_Hint     : Gtk_Box;
      From_Edit            : Gtk_Entry;
      From_Hint            : Gtk_Box;
      To_Edit              : Gtk_Entry;
      To_Hint              : Gtk_Box;
      Total                : Gtk_Label;
      Threshold_Entry      : Gtk_Entry;
      Threshold_Hint       : Gtk_Box;
      Generalize_Combo     : Gtk_Fuzzy_Generalization_Combo;
      Generalize_Hint      : Gtk_Box;
      Source_From_Edit     : Gtk_Entry;
      Source_From_Hint     : Gtk_Box;
      Source_To_Edit       : Gtk_Entry;
      Source_To_Hint       : Gtk_Box;
      Source_Total         : Gtk_Label;
      Verify               : Boolean;
         -- Classification data
      Classification_Grid  : Gtk_Table;
      Lesson		   : Lecture_Handle;
      From                 : Integer;
      To                   : Integer;
      Source_From          : Integer;
      Source_To            : Integer;
      Threshold            : Confidence;
      Classifier           : Classifier_Handle;
      Generalize           : Generalization_Mode;
         -- Report
      Result               : Lecture_Handle;
      Source               : Lecture_Handle;
      Lesson_Slice         : Lecture_Handle;
      Source_Slice         : Lecture_Handle;
      Data                 : Set_Ptr;
   end record;
   type Classification_Box is
      access all Classification_Box_Record'Class;
--
-- Classifier_Name_Changed -- Name changed event
--
   procedure Classifier_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Classification_Box_Record);
--
-- Completed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed
             (  Item : not null access Classification_Box_Record
             );
--
-- Finalize -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Finalize
             (  Item : not null access Classification_Box_Record
             );
--
-- From_Changed -- Changed event
--
   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Gtk_New -- Construction
--
--    Item           - The result
--    Browser        - The parent widget
--    Verify         - Against the source
--    Set_Classifier - Set classifier selected at the classifier pane
--    Set_Lecture    - Set classifier selected at the training set  pane
--
   procedure Gtk_New
             (  Item    : out Classification_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Verify         : Boolean;
                Set_Classifier : Boolean;
                Set_Lecture    : Boolean
             );
--
-- Initialize -- To be called by derived types
--
--    Item           - To construct
--    Verify         - Against the source
--    Set_Classifier - Set classifier selected at the classifier pane
--    Set_Lecture    - Set classifier selected at the training set  pane
--
   procedure Initialize
             (  Item           : not null access
                                 Classification_Box_Record'Class;
                Verify         : Boolean;
                Set_Classifier : Boolean;
                Set_Lecture    : Boolean
             );
--
-- Lecture_Name_Changed -- Name changed event
--
   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service (Item : not null access Classification_Box_Record);
--
-- Source_From_Changed -- Changed event
--
   procedure Source_From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Source_Name_Changed -- Name changed event
--
   procedure Source_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Source_To_Changed -- Changed event
--
   procedure Source_To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- Threshold_Changed -- Changed event
--
   procedure Threshold_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );
--
-- To_Changed -- Changed event
--
   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             );

   package Classifications is
      new GLib.Object.Weak_References (Classification_Box_Record);

   package Classification_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Classification_Box
           );

end Gtk.Fuzzy_Catalogue.Classification_Boxes;
