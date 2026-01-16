--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Fuzzy_Data_Store             Luebeck            --
--  Interface                                      Autumn, 2009       --
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
--  This  package  provides  a tree model to represent editable training
--  sets. The model has n+1 columns.  The  first  column  has  the  type
--  GType_String and contains for each example  the  icon  name  of  the
--  positive  and negative examples. The following columns have the type
--  GType_Classification. Each column corresponds a feature.
--
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Intuitionistic;           use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Glib.Values;                    use Glib.Values;
with Gtk.Fuzzy_Object;               use Gtk.Fuzzy_Object;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Indicator;                      use Indicator;
with Indicator.Handle;               use Indicator.Handle;

with Ada.Numerics.Discrete_Random;
with Generic_Map;
with Generic_Unbounded_Array;
with Generic_Unbounded_Ptr_Array;
with Generic_Segmented_Stack;
with Generic_Set;
with Gtk.Handlers;
with Object.Handle;

package Gtk.Tree_Model.Fuzzy_Data_Store is
--
-- Class_Name -- The name of the tree view widget class
--
   Class_Name : constant String := Prefix & "LectureDataStore";
--
-- Gtk_Fuzzy_Data_Store_Record -- The store
--
-- Signals (additional):
--
--    column-deleted  - emitted when the column is deleted.  The  column
--                      number can be acquired using Get_Deleted_Column;
--    column-inserted - emitted   when   the  column  is  inserted.  The
--                      inserted  column  number  can  be acquired using
--                      Get_Inserted_Column;
--    redo-changed    - emitted  when  empty  state  of  the redo buffer
--                      changes;
--    undo-changed    - emitted  when  empty state of  the  undo  buffer
--                      changes.
--
   type Gtk_Fuzzy_Data_Store_Record is
      new Gtk_Abstract_Model_Record with private;
   type Gtk_Fuzzy_Data_Store is
      access all Gtk_Fuzzy_Data_Store_Record'Class;
--
-- Add_Column
--
--    Store   - The model
--    Feature - To insert
--    Column  - The column number
--
-- Exceptions :
--
--    Constraint_Error - Invalid  handle,  column  number,  an  existing
--                       feature
--
   procedure Add_Column
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Feature : Feature_Handle;
                Column  : Positive
             );
--
-- Add_Example
--
--    Store   - The model
--    Example - The example number to add
--
-- When example is greater than the number of examples + 1, the  example
-- is added at the end of the store.
--
   procedure Add_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             );
--
-- Add_Random_Singleton_Example
--
--    Store   - The model
--    Example - The example number to add
--
-- This  procedure  adds  an  example  classified as a randomly selected
-- singleton. When example is greater than the number of examples  +  1,
-- the example is added at the end of the store.
--
   procedure Add_Random_Singleton_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             );
--
-- Can_Redo -- Redo buffer check
--
--    Store - The model
--
-- Returns :
--
--    True if the redo buffer is not empty
--
   function Can_Redo
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Boolean;
--
-- Can_Undo -- Undo buffer check
--
--    Store - The model
--
-- Returns :
--
--    True if the undo buffer is not empty
--
   function Can_Undo
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Boolean;
--
-- Copy_Rows -- Selection deletion
--
--    Store - The model
--    Rows  - The list of rows numbers to copy
--
-- Each  example  corresponding  to  the  rows in the list is copied and
-- added to the end of the model.
--
   procedure Copy_Rows
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Rows  : GInt_Array
             );
--
-- Delete_Column -- Column deletion
--
--    Store  - The model
--    Column - To remove
--
   procedure Delete_Column
             (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
                Column : Positive
             );
--
-- Delete_Example -- Example deletion
--
--    Store   - The model
--    Example - The example number to remove
--
   procedure Delete_Example
             (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
                Example : Positive
             );
--
-- Delete_Rows -- Rows deletion
--
--    Store - The model
--    Rows  - The list of rows numbers to remove
--
-- Each example corresponding to the rows in the list is  removed.  Rows
-- are zero-based.
--
   procedure Delete_Rows
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Rows  : GInt_Array
             );
--
-- Get -- Training set data from the model
--
--    Store  - The model
--    Lesson - A handle to add training examples
--    Viewer - To use with
--
   procedure Get
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : in out Lecture_Handle;
                Viewer : Indicator_Handle
             );
   procedure Get
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Column -- Column number of the feature
--
--    Store   - The model
--    Feature - A handle to
--
-- Returns :
--
--    The column number associated with the feature
--
-- Exceptions :
--
--    Constraint_Error - Feature is not used
--
   function Get_Column
            (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
               Feature : Feature_Handle
            )  return Positive;
--
-- Get_Deleted_Column -- Deleted column number
--
--    Store - The model
--
-- Returns :
--
--    The deleted column number or else 0
--
   function Get_Deleted_Column
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural;
--
-- Get_Examples_Number -- The total number of examples
--
--    Store - The model
--
-- Returns :
--
--    The number of examples
--
   function Get_Examples_Number
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural;
--
-- Get_Feature -- The feature by column number
--
--    Store  - The model
--    Column - The column number
--
-- Returns :
--
--    The column feature
--
-- Exceptions :
--
--    Constraint_Error - No such column
--
   function Get_Feature
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Column : Positive
            )  return Feature_Handle;
--
-- Get_Features_Number -- The number of features
--
--    Store - The model
--
-- Returns :
--
--    The number of features
--
   function Get_Features_Number
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural;
--
-- Get_Inserted_Column -- Inserted column number
--
--    Store - The model
--
-- Returns :
--
--    The inserted column number or else 0
--
   function Get_Inserted_Column
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return Natural;
--
-- Get_Iter -- Iterator to an example
--
--    Store    - The model
--    Example  - The example number
--    Positive - Example type
--
-- Returns :
--
--    The iterator or Null_Iter if Example number is wrong
--
   function Get_Iter
            (  Store    : not null access Gtk_Fuzzy_Data_Store_Record;
               Example  : Positive;
               Positive : Boolean
            )  return Gtk_Tree_Iter;
--
-- Get_Type -- The GTK type of the lecture store
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Object creation
--
--    Store  - The result
--    Lesson - A handle to the training set (can be invalid)
--    Viewer - To use with
--
   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Data_Store;
                Lesson : Lecture_Handle;
                Viewer : Indicator_Handle
             );
   procedure Gtk_New
             (  Store  : out Gtk_Fuzzy_Data_Store;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Initialize -- Construction
--
--    Store  - A pointer to
--    Lesson - A handle to the training set (can be invalid)
--    Viewer - To use with
--
-- This procedure should be called by any derived  type  as  a  part  of
-- initialization process.
--
   procedure Initialize
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Is_In -- Feature check
--
--    Store   - The model
--    Feature - A handle to
--
-- Returns :
--
--    True if feature is used
--
   function Is_In
            (  Store   : not null access Gtk_Fuzzy_Data_Store_Record;
               Feature : Feature_Handle
            )  return Boolean;
--
-- Put -- Training set data into the model
--
--    Store  - The model
--    Lesson - A handle to the training set
--    Viewer - To use with
--
-- This procedure replaces current contents of the store with data  from
-- Lesson. Lesson can be an invalid handle, in which case the model will
-- contain no rows and only one column.
--
   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : Indicator_Handle
             );
   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Lesson : Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Put -- Cell modification
--
--    Store  - The model
--    Row    - The iterator to the row
--    Column - To set 1..
--    Value  - The value to set (must be of GType_Classification)
--
-- Exceptions :
--
--    Constrait_Error - Row or Value is invalid
--
   procedure Put
             (  Store  : not null access
                         Gtk_Fuzzy_Data_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Value  : GValue
             );
--
-- Redo -- Redo action
--
--    Store - The model
--
   procedure Redo (Store : not null access Gtk_Fuzzy_Data_Store_Record);
--
-- Set_Stored -- Empties the undo buffer
--
--    Store - The model
--
   procedure Set_Stored
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record
             );
--
-- Undo -- Undo action
--
--    Store - The model
--
   procedure Undo (Store : not null access Gtk_Fuzzy_Data_Store_Record);

private
   package Random_Numbers is
      new Ada.Numerics.Discrete_Random (Positive);

   Null_Value : GValue;

   type Column_Count is new Natural;
   subtype Column_Index is Column_Count range 1..Column_Count'Last;
   type Column_Index_Array is array (Positive range <>) of Column_Index;
   type Classification_Ptr is access Classification;
   type Classification_Ptr_Array is
      array (Column_Count range <>) of Classification_Ptr;
   package Lecture_Rows is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Column_Count,
             Object_Type           => Classification,
             Object_Ptr_Type       => Classification_Ptr,
             Object_Ptr_Array_Type => Classification_Ptr_Array
          );
   use Lecture_Rows;
   type Lecture_Row_Ptr is access Lecture_Rows.Unbounded_Ptr_Array;

   type Row_Count is new Natural;
   subtype Row_Index is Row_Count range 1..Row_Count'Last;
   type Row_Index_Array is array (Positive range <>) of Row_Index;
   type Lecture_Row_Ptr_Array is
      array (Row_Index range <>) of Lecture_Row_Ptr;
   package Unbounded_Row_Ptr_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Row_Index,
             Object_Type           => Lecture_Rows.Unbounded_Ptr_Array,
             Object_Ptr_Type       => Lecture_Row_Ptr,
             Object_Ptr_Array_Type => Lecture_Row_Ptr_Array
          );
   use Unbounded_Row_Ptr_Arrays;
   package Row_Sets is new Generic_Set (Row_Count, 0);
   use Row_Sets;
--
-- Abstract_Action -- Abstract undo/redo action
--
   type Action_Type is (Action_Do, Action_Redo, Action_Undo);
   type Abstract_Action is abstract new Object.Entity with null record;
   type Abstract_Action_Ptr is access Abstract_Action'Class;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Abstract_Action;
                What   : Action_Type
             )  is abstract;

   package Action_Handles is
      new Object.Handle (Abstract_Action, Abstract_Action_Ptr);
   use Action_Handles;

   Noop : Action_Handles.Handle;

   package Action_Stacks is
      new Generic_Segmented_Stack
          (  Index_Type   => Positive,
             Object_Type  => Action_Handles.Handle,
             Null_Element => Noop
          );
   use Action_Stacks.Segmented_Stack;
--
-- Column_Maps -- Arrays of column indices
--
   type Column_Count_Array is array (GInt range <>) of Column_Count;
   package Column_Maps is
      new Generic_Unbounded_Array
          (  Index_Type        => GInt,
             Object_Type       => Column_Count,
             Object_Array_Type => Column_Count_Array,
             Null_Element      => 0
          );
   use Column_Maps;
--
-- Feature_Maps -- Arrays of features
--
   package Feature_Maps is
      new Generic_Map
          (  Key_Type    => Column_Index,
             Object_Type => Feature_Handle
          );
   use Feature_Maps;

   type Lecture_Data is record
      Undo    : Action_Stacks.Segmented_Stack.Stack;
      Redo    : Action_Stacks.Segmented_Stack.Stack;
      Rows    : Unbounded_Row_Ptr_Arrays.Unbounded_Ptr_Array;
      Columns : Column_Maps.Unbounded_Array;
   end record;
   type Lecture_Data_Ptr is access Lecture_Data;

   package Column_Sets is new Generic_Set (Column_Count, 0);
   use Column_Sets;
--
-- Gtk_Fuzzy_Data_Store_Record -- Implementation
--
   type Gtk_Fuzzy_Data_Store_Record is
      new Gtk_Abstract_Model_Record with
   record
      Rows_Count : Row_Count := 0;
      Column     : Natural   := 0;
      Free       : Column_Sets.Set;
      Data       : Lecture_Data_Ptr;
      Features   : Feature_Maps.Map;
   end record;

   overriding
   function Children
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   procedure Complete
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record;
                Action : Abstract_Action_Ptr;
                What   : Action_Type
             );
   overriding
   procedure Finalize
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record
             );
   overriding
   function Get_Column_Type
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Index : GInt
            )  return GType;
   overriding
   function Get_Flags
            (  Store : not null access
                       Gtk_Fuzzy_Data_Store_Record
            )  return Tree_Model_Flags;
   overriding
   function Get_Iter
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
   overriding
   function Get_N_Columns
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record
            )  return GInt;
   overriding
   function Get_Path
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
   overriding
   procedure Get_Value
             (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
   overriding
   function Has_Child
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   overriding
   procedure Next
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
   overriding
   function Nth_Child
            (  Store  : not null access Gtk_Fuzzy_Data_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
   overriding
   function N_Children
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
   overriding
   function Parent
            (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   procedure Previous
             (  Store : not null access Gtk_Fuzzy_Data_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Fuzzy_Data_Store
          );

      -- Delete rows action
   type Delete_Action (Rows : Positive) is
      new Abstract_Action with
   record
      Rows_List : Row_Index_Array (1..Rows);
   end record;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Delete_Action;
                What   : Action_Type
             );

      -- Delete columns action
   type Delete_Column_Action is new Abstract_Action with record
      Column : GInt;
   end record;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Delete_Column_Action;
                What   : Action_Type
             );

      -- Cell edit action
   type Edit_Action (Row : Row_Index; Column : Column_Index) is
      new Abstract_Action with
   record
      Value : Classification_Ptr;
   end record;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Edit_Action;
                What   : Action_Type
             );
   procedure Finalize (Action : in out Edit_Action);

      -- Insert rows action
   type Row_Data is record
      Row  : Row_Index;
      Data : Lecture_Row_Ptr;
   end record;
   type Row_Data_Array is array (Positive range <>) of Row_Data;
   type Insert_Action (Rows : Positive) is
      new Abstract_Action with
   record
      Rows_List : Row_Data_Array (1..Rows);
   end record;
   type Insert_Action_Ptr is access all Insert_Action;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Insert_Action;
                What   : Action_Type
             );
   procedure Finalize (Action : in out Insert_Action);

      -- Insert column action
   type Column_Array is
      array (Row_Count range <>) of Classification_Ptr;
   type Insert_Column_Action (Rows : Row_Count) is
      new Abstract_Action with
   record
      Column   : GInt;
      Features : Feature_Handle;
      Data     : Column_Array (1..Rows);
   end record;
   procedure Do_It
             (  Store  : in out Gtk_Fuzzy_Data_Store_Record'Class;
                Action : in out Insert_Column_Action;
                What   : Action_Type
             );
   procedure Finalize (Action : in out Insert_Column_Action);

end Gtk.Tree_Model.Fuzzy_Data_Store;
