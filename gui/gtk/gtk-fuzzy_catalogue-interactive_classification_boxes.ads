--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Interactive_Classification_Boxes       Summer, 2010       --
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

with Confidence_Factors;    use Confidence_Factors;
with Fuzzy.Classifier;      use Fuzzy.Classifier;
with Fuzzy.Feature;         use Fuzzy.Feature;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Table;             use Gtk.Table;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View;         use Gtk.Tree_View;

with Fuzzy.Intuitionistic;
with Glib.Object;
with Gtk.Cell_Renderer_Fuzzy.Feature_Value;
with Gtk.Fuzzy_Generalization_Combo;

private package Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes is
   pragma Elaborate_Body
          (  Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes
          );
   use Gtk.Cell_Renderer_Fuzzy.Feature_Value;
   use Gtk.Fuzzy_Generalization_Combo;

   type Classification_Ptr is
      access Fuzzy.Intuitionistic.Classification;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Fuzzy.Intuitionistic.Classification,
             Classification_Ptr
          );
--
-- Ineractive_Classification_Box_Record -- Box of training set
--                                         classification
--
   type Interactive_Classification_Box_Record is
      new Gtk_Item_Box_Record with
   record
      Classification_Grid  : Gtk_Table;
      Classifier_Name      : Gtk_Picker_Box;
      Classifier_Name_Hint : Gtk_Box;
      Threshold_Entry      : Gtk_Entry;
      Threshold_Hint       : Gtk_Box;
      Generalize_Combo     : Gtk_Fuzzy_Generalization_Combo;
      Generalize_Hint      : Gtk_Box;
      Classification_Box   : Gtk_HPaned;
      Input                : Gtk_Tree_View;
      Left                 : Gtk_VBox;
      Right                : Gtk_VBox;
      Lesson               : Lecture_Handle;
      Value_Renderer       : Gtk_Cell_Renderer_Fuzzy_Feature_Value;
      Threshold            : Confidence;
      Classifier           : Classifier_Handle;
      Generalize           : Generalization_Mode;
      Data                 : Classification_Ptr;
   end record;
   type Interactive_Classification_Box is
      access all Interactive_Classification_Box_Record'Class;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit
             (  Item : not null access
                       Interactive_Classification_Box_Record
             );
--
-- Completed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed
             (  Item : not null access
                       Interactive_Classification_Box_Record
             );
--
-- Finalize -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Finalize
             (  Item : not null access
                       Interactive_Classification_Box_Record
             );
--
-- Gtk_New -- Construction
--
--    Item           - The result
--    Browser        - The parent widget
--    Set_Classifier - Set classifier selected at the classifier pane
--
   procedure Gtk_New
             (  Item    : out Interactive_Classification_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Set_Classifier : Boolean
             );
--
-- Initialize -- To be called by derived types
--
--    Item           - To construct
--    Set_Classifier - Set classifier selected at the classifier pane
--
   procedure Initialize
             (  Item : not null access
                       Interactive_Classification_Box_Record'Class;
                Set_Classifier : Boolean
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service
             (  Item : not null access
                       Interactive_Classification_Box_Record
             );

   package Interactive_Classifications is
      new GLib.Object.Weak_References
          (  Interactive_Classification_Box_Record
          );
private
--
-- Changed -- Changed event
--
   procedure Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             );
--
-- Classifier_Name_Changed -- Name changed event
--
   procedure Classifier_Name_Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             );
--
-- Set_Icon -- Cell data callback
--
   procedure Set_Icon
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             );
--
-- Set_Name -- Cell data callback
--
   procedure Set_Name
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             );
--
-- Set_Value -- Cell data callback
--
   procedure Set_Value
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             );
--
-- Threshold_Changed -- Changed event
--
   procedure Threshold_Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             );

   package Classification_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Interactive_Classification_Box
          );

   package Cell_Functions is
      new Set_Column_Cell_Data (Interactive_Classification_Box);
   use Cell_Functions;

end Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes;
