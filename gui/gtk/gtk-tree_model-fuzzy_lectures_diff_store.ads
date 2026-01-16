--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.                             Luebeck            --
--        Fuzzy_Lectures_Diff_Store                Summer, 2006       --
--  Interface                                                         --
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
--
--  This  package  defines a Tree_View_Model of a pair of fuzzy training
--  sets. The sets are represented as rows of examples with  columns  of
--  features.  The  first  column  (0) is Positive_Icon or Negative_Icon
--  (see Fuzzy.Gtk_Icon_Factory for stock icons) indicating positive and
--  negative examples. The second column (1) is the example number.  The
--  following  columns  correspond  to  the  features from the sets. The
--  common features of both sets are represented as triples of  columns.
--  The  first column of a triplet is the values of the feature from the
--  result set. The third column is the feature value from the reference
--  set. The second column is the difference between two values. Columns
--  corresponding to the common features are followed by the columns  of
--  the features unique to the reference set. The type of the difference
--  column is  GType_Fuzzy_Boolean  (see  GLib.Values.Fuzzy.Logic).  The
--  type of the feature  values  columns  is  GType_Classification  (see
--  GLib.Values.Fuzzy.Intuitionisitc).
--
with Fuzzy.Classifier;               use Fuzzy.Classifier;
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with GLib.Values;                    use GLib.Values;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Gtk.Tree_Model.Column_Order;    use Gtk.Tree_Model.Column_Order;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

with Gtk.Missed;
with Gtk.Tree_Model.Generic_Sort;

package Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store is
--
-- Gtk_Fuzzy_Lectures_Diff_Store_Record -- A  Tree_View_Model  based  on
--                                         two fuzzy training sets.  The
-- first  training  set  is  the  reference  set.  The second set is the
-- comparison set.
--
   type Gtk_Fuzzy_Lectures_Diff_Store_Record is
      new Gtk_Root_Tree_Model_Record with private;
   type Gtk_Fuzzy_Lectures_Diff_Store is
      access all Gtk_Fuzzy_Lectures_Diff_Store_Record'Class;
--
-- Get_Feature_Column -- Get feature column
--
--    Store   - A pointer to
--    Feature - A feature to search for (object or a handle to)
--    Column  - The column number of the feature
--    Shared  - True if the feature is shared
--
-- This  function searches for Feature in the store. When the feature is
-- shared  Column  is  the  first  column where it appears and Shared is
-- True. The next column where it  appears  is  Column  +  2.  When  the
-- feature is not shared, Shared is  False  and  Column  is  the  single
-- column  number.  When  Feature  is  not in the store Constraint_Error
-- exception is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Not in the store
--
   procedure Get_Feature_Column
             (  Store   : not null access
                          Gtk_Fuzzy_Lectures_Diff_Store_Record;
                Feature : Feature_Object'Class;
                Column  : out Positive;
                Shared  : out Boolean
             );
   procedure Get_Feature_Column
             (  Store   : not null access
                          Gtk_Fuzzy_Lectures_Diff_Store_Record;
                Feature : Feature_Handle;
                Column  : out Positive;
                Shared  : out Boolean
             );
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
                            Gtk_Fuzzy_Lectures_Diff_Store_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter;
--
-- Get_Reference_Lesson -- Get the reference training set
--
--    Store - A pointer to
--
-- Returns :
--
--    A handle to the training set visualized by the widget
--
   function Get_Reference_Lesson
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Lecture_Handle;
--
-- Get_Result_Lesson -- Get the result training set associated with
--
--    Store - A pointer to
--
-- Returns :
--
--    A handle to the training set visualized by the widget
--
   function Get_Result_Lesson
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Lecture_Handle;
--
-- Get_Shared_Features -- Get the set of shared features
--
--    Store - A pointer to
--
-- This function returns the list of features shared  by  the  reference
-- and the result sets.
--
-- Returns :
--
--    The list of shared features
--
   function Get_Shared_Features
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Bounded_Array;
--
-- Get_Unique_Features -- Get the set of shared features
--
--    Store - A pointer to
--
-- This function returns the list of features unique  to  the  reference
-- set.
--
-- Returns :
--
--    The list of shared features
--
   function Get_Unique_Features
            (  Store : not null access
                       Gtk_Fuzzy_Lectures_Diff_Store_Record
            )  return Bounded_Array;
--
-- Gtk_New -- Object creation
--
--    Store      - The result
--    Reference  - A handle to the reference training set
--    Result     - A handle to the result training set
--    Difference - The function to calculate per-point differences
--
   procedure Gtk_New
             (  Store      : out Gtk_Fuzzy_Lectures_Diff_Store;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function := Diff'Access
             );
--
-- Initialize -- Construction
--
--    Store      - A pointer to
--    Reference  - A handle to the reference training set
--    Result     - A handle to the result training set
--    Difference - The function to calculate per-point differences
--
-- This procedure should be called by any derived  type  as  a  part  of
-- initialization process.
--
   procedure Initialize
             (  Store      : not null access
                             Gtk_Fuzzy_Lectures_Diff_Store_Record'Class;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function
             );
private
--
-- Gtk_Fuzzy_Lecture_Record -- An unsorted implementation of
--
   type Gtk_Fuzzy_Lectures_Diff_Record
        (  Shared_First : Integer;
           Shared_Last  : Integer;
           Unique_First : Integer;
           Unique_Last  : Integer
        )  is new Gtk_Abstract_Model_Record with
   record
      Reference    : Lecture_Handle;
      Result       : Lecture_Handle;
      Shared       : Bounded_Array (Shared_First, Shared_Last);
      Unique       : Bounded_Array (Unique_First, Unique_Last);
      Shared_Count : GInt;
      Difference   : Divergence_Function := Diff'Access;
   end record;
   type Gtk_Fuzzy_Lectures_Diff is
      access all Gtk_Fuzzy_Lectures_Diff_Record'Class;
--
-- Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result Null_Iter, except  for  a  special  case  when
-- Parent is Null_Iter. Then the result is the first positive example.
--
   overriding
   function Children
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Get_Column_Type -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Index : Gint
            )  return GType;
--
-- Get_Flags -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns Tree_Model_Iters_Persist and Tree_Model_List_Only set.
--
   overriding
   function Get_Flags
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record
            )  return Tree_Model_Flags;
--
-- Get_Iter -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Converts  path  to  an  iterator. The path must have length 1 and its
-- index must be between 0 and doubled number of examples - 1.
--
   overriding
   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
--
-- Get_N_Columns -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns the number of features + 2.
--
   overriding
   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record
            )  return GInt;
--
-- Get_Path -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns  a  path of one index length. The index in the path is set to
-- the (example number - 1) * 2, + 1 for a negative example.
--
   overriding
   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
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
   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
--
-- Gtk_New -- Object creation
--
--    Store          - The result
--    Reference      - A handle to the reference training set
--    Result         - A handle to the result training set
--    Difference     - The function to calculate per-point differences
--
   procedure Gtk_New
             (  Store      : out Gtk_Fuzzy_Lectures_Diff;
                Reference  : Lecture_Handle;
                Result     : Lecture_Handle;
                Difference : Divergence_Function := Diff'Access
             );
--
-- Has_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result False.
--
   overriding
   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
--
-- Initialize -- Construction
--
--    Store      - A pointer to
--    Difference - The function to calculate per-point differences
--
-- This procedure should be called by any derived  type  as  a  part  of
-- initialization  process.  The caller shall set the Reference, Result,
-- Shared and Unique fields of Store before the call.
--
   procedure Initialize
             (  Store : not null access
                        Gtk_Fuzzy_Lectures_Diff_Record'Class;
                Difference : Divergence_Function
             );
--
-- Next -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Goes to the next example of the set.
--
   overriding
   procedure Next
             (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
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
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
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
            (  Store  : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
--
-- Parent -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- No parents, the result is Null_Iter.
--
   overriding
   function Parent
            (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Previous -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Goes to the previous example of the set.
--
   overriding
   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Lectures_Diff_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Sorted_Lectures_Diff_Store -- Sorted training set models
--
   package Sorted_Lectures_Diff_Store is
      new Gtk.Tree_Model.Generic_Sort
          (  Gtk_Fuzzy_Lectures_Diff_Record,
             Gtk_Fuzzy_Lectures_Diff
          );
   use Sorted_Lectures_Diff_Store;

   type Gtk_Fuzzy_Lectures_Diff_Store_Record is
      new Gtk_Tree_Model_Sort_Record with
   record
      Order : Gtk_Column_Order;
   end record;
--
-- Compare -- Overrides Gtk.Tree_Model.Generic_Sort...
--
   overriding
   function Compare
            (  Store  : not null access
                        Gtk_Fuzzy_Lectures_Diff_Store_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order;

end Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store;
