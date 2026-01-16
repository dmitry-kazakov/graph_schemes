--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.                             Luebeck            --
--        Fuzzy_Lecture_Store                      Summer, 2006       --
--  Interface                                                         --
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
--
--  This package defines a Tree_View_Model of a fuzzy training set.  The
--  set is represented as rows of examples with columns of features. The
--  first  column  (0)  is  Positive_Icon  or  Negative_Icon  indicating
--  positive and negative examples (see Gtk.Fuzzy_Icon_Factory  for  the
--  corresponding stock icons). The second column  (1)  is  the  example
--  number. The following columns are feature values of the set  of  the
--  type   GLib.Values.Fuzzy.Intuitionistic.GType_Classification.   Each
--  example of the set has two rows in the model.
--
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Observer_Handle;  use Fuzzy.Lecture.Observer_Handle;
with GLib.Values;                    use GLib.Values;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Gtk.Tree_Model.Column_Order;    use Gtk.Tree_Model.Column_Order;

with Gtk.Missed;
with Gtk.Tree_Model.Generic_Sort;

package Gtk.Tree_Model.Fuzzy_Lecture_Store is
--
-- Gtk_Fuzzy_Lecture_Store_Record -- A Tree_View_Model  based on a fuzzy
--                                   training set
--
   type Gtk_Fuzzy_Lecture_Store_Record is
      new Gtk_Root_Tree_Model_Record with private;
   type Gtk_Fuzzy_Lecture_Store is
      access all Gtk_Fuzzy_Lecture_Store_Record'Class;
--
-- Get_Feature_Column -- Get feature column
--
--    Store   - A pointer to
--    Feature - A feature to search for (object or a handle to)
--    Column  - The column number of the feature
--
-- Returns :
--
--    The column number of the feature in the store
--
-- Exceptions :
--
--    Constraint_Error - Not in the store or invalid handle
--
   function Get_Feature_Column
            (  Store   : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Feature : Feature_Object'Class
            )  return Positive;
   function Get_Feature_Column
            (  Store   : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Feature : Feature_Handle
            )  return Positive;
--
-- Get_Iter -- Composition of an iterator
--
--    Store      - A pointer to
--    Example    - An example number
--    Complement - The image type
--
-- Returns :
--
--    The tree iterator of
--
-- Exceptions :
--
--    Constraint_Error - Wrong example or store
--
   function Get_Iter
            (  Store      : not null access
                            Gtk_Fuzzy_Lecture_Store_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter;
--
-- Get_Lesson -- Get the training set associated with
--
--    Store - A pointer to
--
-- Returns :
--
--    A handle to the training set used in the store
--
   function Get_Lesson
            (  Store : not null access Gtk_Fuzzy_Lecture_Store_Record
            )  return Lecture_Handle;
--
-- Gtk_New -- Object creation
--
--    Store  - The result
--    Lesson - A handle to the training set
--
   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Lecture_Store;
                Lesson : Lecture_Handle
             );
--
-- Initialize -- Construction
--
--    Store  - A pointer to
--    Lesson - A handle to the training set
--
-- This procedure should be called by any derived  type  as  a  part  of
-- initialization process.
--
   procedure Initialize
             (  Store  : not null access
                         Gtk_Fuzzy_Lecture_Store_Record'Class;
                Lesson : Lecture_Handle
             );
private
   use Fuzzy.Lecture;

   type Gtk_Fuzzy_Lecture_Record;
   type Fuzzy_Lecture_Observer
        (  Store  : not null access Gtk_Fuzzy_Lecture_Record'Class;
           Lesson : not null access Lecture_Object'Class
        )  is new Lecture_Observer (Lesson) with null record;
--
-- Added -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Added
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Changed -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Changed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             );
--
-- Deleted -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             );
   overriding
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Renamed -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Renamed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             );
--
-- Gtk_Fuzzy_Lecture_Record -- An unsorted implementation of
--
   type Gtk_Fuzzy_Lecture_Record is
      new Gtk_Abstract_Model_Record with
   record
      Lesson   : Lecture_Handle;
      Observer : Fuzzy.Lecture.Observer_Handle.Handle;
   end record;
   type Gtk_Fuzzy_Lecture is
      access all Gtk_Fuzzy_Lecture_Record'Class;
--
-- Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result Null_Iter, except  for  a  special  case  when
-- Parent is Null_Iter. Then the result is the first positive example.
--
   overriding
   function Children
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Get_Column_Type -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns GType_String  for  Index  =  0,  GType_Int  for  Index  =  1,
-- GType_Classification  for  each  following  index  corresponding to a
-- feature. Otherwise it is GType_Invalid.
--
   overriding
   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Index : Gint
            )  return GType;
--
-- Get_Flags -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns Tree_Model_Iters_Persist and Tree_Model_List_Only set.
--
   overriding
   function Get_Flags
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return Tree_Model_Flags;
--
-- Get_Iter -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Converts  path  to  an  iterator. The path must have length 1 and its
-- index must be between 0 and doubled number of examples - 1.
--
   overriding
   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
--
-- Get_Lesson -- Get the training set associated with
--
--    Store - A pointer to
--
-- Returns :
--
--    A handle to the training set visualized by the widget
--
   function Get_Lesson
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return Lecture_Handle;
--
-- Get_N_Columns -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns the number of features + 2.
--
   overriding
   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Lecture_Record
            )  return GInt;
--
-- Get_Path -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns  a  path of one index length. The index in the path is set to
-- the (example number - 1) * 2, + 1 for a negative example.
--
   overriding
   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
--
-- Get_Type -- The GTK type of the lecture store
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Get_Value -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Sets  GValue  to  the  example value. The column 0 is +/- to indicate
-- positive  and  negative examples. The column 1 is the example number.
-- The following columns correspond to the features of the set.
--
   overriding
   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
--
-- Gtk_New -- Object creation
--
--    Store  - The result
--    Lesson - A handle to the training set
--
   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Lecture;
                Lesson : Lecture_Handle
             );
--
-- Has_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result False.
--
   overriding
   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
--
-- Initialize -- Construction
--
--    Store  - A pointer to
--    Lesson - A handle to the training set
--
-- This procedure should be called by any derived  type  as  a  part  of
-- initialization process.
--
   procedure Initialize
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record'Class;
                Lesson : Lecture_Handle
             );
--
-- Next -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Goes to the next example of the set.
--
   overriding
   procedure Next
             (  Store : not null access Gtk_Fuzzy_Lecture_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Nth_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- The result is Null_Iter, except a case when Parent is  Null_Iter,  in
-- which  case  the  result is the example N / 2 + 1, positive when N is
-- even.
--
   overriding
   function Nth_Child
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
--
-- N_Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- The  result  is  0, as the trainong set does not have any children to
-- any rows. A special case is when Iter is Null_Iter, in which case the
-- result is the number of examples * 2.
--
   overriding
   function N_Children
            (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
--
-- Parent -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result is Null_Iter.
--
   overriding
   function Parent
            (  Store : not null access Gtk_Fuzzy_Lecture_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;

--
-- Previous -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Goes to the previous example of the set.
--
   overriding
   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Lecture_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Set_Lesson -- Change the training set shown
--
--    Store  - A pointer to
--    Lesson - A handle to the training set
--
   procedure Set_Lesson
             (  Store  : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle
             );
--
-- Sorted_Lecture_Store -- Sorted training set models (instantiation)
--
   package Sorted_Lecture_Store is
      new Gtk.Tree_Model.Generic_Sort
          (  Gtk_Fuzzy_Lecture_Record,
             Gtk_Fuzzy_Lecture
          );
   use Sorted_Lecture_Store;

   type Gtk_Fuzzy_Lecture_Store_Record is
      new Gtk_Tree_Model_Sort_Record with
   record
      Order : Gtk_Column_Order;
   end record;
--
-- Compare -- Overrides Gtk.Tree_Model.Generic_Sort...
--
   overriding
   function Compare
            (  Store  : not null access Gtk_Fuzzy_Lecture_Store_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order;

end Gtk.Tree_Model.Fuzzy_Lecture_Store;
