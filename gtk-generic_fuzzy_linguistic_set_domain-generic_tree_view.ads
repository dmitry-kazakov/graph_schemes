--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain.    Luebeck            --
--        Generic_Tree_View                        Winter, 2007       --
--  Interface                                                         --
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
--  Tree view panel for Gtk_Fuzzy_Linguistic_Set_Domain:
--
--     .___________._____________._____________._____________.__.
--     |           |             |             |             |/\|
--     | Domain    | Range       | Left        | Right       |  |
--     |___________|_____________|_____________|_____________|  |
--     | + Low     |             |             |             |  |
--     | - Middle  |             |             |             |  |
--     |      10.0 | |||||| 0.5  | |||||| 0.5  | |||||| 0.5  |  |
--     |      20.0 | ||||||||1|| | ||||||||1|| | ||||||||1|| |  |
--     |      30.0 | 0           |             |             |  |
--     | + High    |             |             |             |  |
--     |___________|_____________|_____________|_____________|\/|
--     |<___________________________________________________>|__|
--
-- This widget is used with Gtk_Fuzzy_Linguistic_Set to provide  viewing
-- and  editing  the  points  of the linguistic set. The widget supports
-- undo and redo buffers editing actions.
--
-- The package is generic it has  to  be  instantiated  at  the  library
-- level.  The widget is a scrolled window widget which decorates a tree
-- view. The class name applies to  the  tree  view  inside  the  scroll
-- window.
--
-- The package also provides various buttons and sliders  which  can  be
-- used for editing:
--
--    Gtk_Fuzzy_Linguistic_Set_Accumulate  -- Accumulation dialog button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Add    -- Add point button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Copy   -- Copy button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Down   -- Down button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Exec   -- Execute operation button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Find   -- Find button
--    Gtk_Fuzzy_Linguistic_Set_Edit_New    -- New variable button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Purge  -- Purge button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Redo   -- Redo button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Remove -- Remove button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Undo   -- Undo button
--    Gtk_Fuzzy_Linguistic_Set_Edit_Up     -- Up button
--    Gtk_Fuzzy_Linguistic_Set_Edit_X      -- X scale
--    Gtk_Fuzzy_Linguistic_Set_Edit_Y      -- Y scale
--
with Gdk.Event;                use Gdk.Event;
with Gdk.Window;               use Gdk.Window;
with GLib.Values;              use GLib.Values;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Fuzzy_Set;            use Gtk.Fuzzy_Set;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with Strings_Edit;             use Strings_Edit;

with Ada.Finalization;
with Gtk.Generic_Style_Button;

with Gtk.Cell_Renderer_Text;
use  Gtk.Cell_Renderer_Text;

with Gtk.Cell_Renderer_Fuzzy_Boolean;
use  Gtk.Cell_Renderer_Fuzzy_Boolean;

with Fuzzy.Abstract_Edit.Named;
with Generic_Segmented_Stack;
with Tables;

generic
package Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View is
--
-- Gtk_Fuzzy_Linguistic_Set_Tree_View_Record -- Editing
--
-- Style properties (of tree view) :
--
--    accumulation-dialog-title - The  title of the accumulation dialog.
--                                String. Default is "Accumulate".
--    accumulate-button-label   - The   label   of  the  button  in  the
--                                accumulation dialog.  String.  Default
--                                is "Accumulate".
--    cancel-button-label - The  label of the button in the accumulation
--                          dialog. String. Default is "Cancel".
--    checked-color       - The  color  used  for  valid  names  of  the
--                          variables and valid values of the points of.
--                          String. The default is black.
--    domain-column-title - The  title  of  the first column. The column
--                          contains the variables names and the  domain
--                          values. String. Default is "Domain".
--    hide-button-label   - The  label of the button in the accumulation
--                          dialog. String. Default is "Hide".
--    illegal-color       - The color used for  invalid  names.  String.
--                          The default is red.
--    left-column-title   - The  title  of  the third column. The column
--                          contains the left limit  of  the  membership
--                          function. String. Default is "Left".
--    range-column-title  - The title of the second column.  The  column
--                          contains  the  range   of   the   membership
--                          function. String. Default is "Range".
--    right-column-title  - The title of the fourth column.  The  column
--                          contains  the  right limit of the membership
--                          function String. Default is "Right".
--
--    Additional style properties are  same  as  ones  of  the  renderer
--    Gtk_Cell_Renderer_Fuzzy_Boolean_Record.
--
-- The  widget style properties are applied to the tree view returned by
-- Get_Tree_View.
--
   type Gtk_Fuzzy_Linguistic_Set_Tree_View_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Tree_View is
      access all Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
--
-- Gtk_Fuzzy_Linguistic_Set_Accumulate -- Accumulate button
--
-- The button pops the accumultated sets dialog. The dialog is  used  to
-- enter or edit a set to accumulate.
--
   package Accumulate_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Accumulate",
             Icon       => Stock_Index,
             Relief     => Relief_None,
             Tip        => "Show accumulated set"
          );
   type Gtk_Fuzzy_Linguistic_Set_Accumulate_Record is
      new Accumulate_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Accumulate  is
      access all Gtk_Fuzzy_Linguistic_Set_Accumulate_Record'Class;
--
-- Get_Accumulate_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Accumulate_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Accumulate;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Add -- Editing add point button
--
-- The button creates a new point of the linguistic variable selected.
--
   package Edit_Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditAdd",
             Icon       => Stock_Add,
             Relief     => Relief_None,
             Tip        => "Add membership function point"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record is
      new Edit_Add_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Add is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record'Class;
--
-- Get_Add_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Add_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Add;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Copy -- Editing copy button
--
-- The button creates copies of the linguistic variables selected.
--
   package Edit_Copy_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditCopy",
             Icon       => Stock_Copy,
             Relief     => Relief_None,
             Tip        => "Copy linguistic variables"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record is
      new Edit_Copy_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Copy is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record'Class;
--
-- Get_Copy_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Copy_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Copy;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Down -- Editing down button
--
-- The button moves selected linguistic variables down in the list.
--
   package Edit_Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditDown",
             Icon       => Stock_Go_Down,
             Relief     => Relief_None,
             Tip        => "Move linguistic variables down"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record is
      new Edit_Down_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Down is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record'Class;
--
-- Get_Down_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Down_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Down;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Exec -- Editing execute button
--
-- The button executes an  operation  selected  in  its  combo  box  and
-- inserts the result into  the  set  of  variables.  The  operation  is
-- executed on the current selection of the variables in the  list.  The
-- result is inserted after  the  last  selected  variable.  The  button
-- allows adding custom oparations to the combo box. For this  a  helper
-- type is defined:
--
-- Operation -- The abstract type of an operation
--
   type Operation is abstract
      new Ada.Finalization.Controlled with private;
--
-- Check -- The selection
--
--    Op       - The operation
--    Selected - A selection subtype
--
-- This function checks the selection for arguments. Note that only  the
-- selections containing  complete  variables  can  be  used.  If  Check
-- returns True for any other selection that will be have no effect.
--
-- Returns :
--
--    True if the operation can be applied to the selection
--
   function Check (Op : Operation; Selected : Selection_Subtype)
      return Boolean is abstract;
--
-- Execute -- The operation
--
--    Op        - The operation
--    Arguments - The argument list
--
-- Returns :
--
--    The operation result
--
   function Execute (Op : Operation; Arguments : Array_Of_Variables)
      return Variable is abstract;
--
-- Unary_Operation -- Abstract type for unary operations
--
   type Unary_Operation is abstract new Operation with private;
   function Check (Op : Unary_Operation; Selected : Selection_Subtype)
      return Boolean;
--
-- Binary_Operation -- Abstract type for binary operations
--
   type Binary_Operation is abstract new Operation with private;
   function Check (Op : Binary_Operation; Selected : Selection_Subtype)
      return Boolean;
--
-- Multiple_Operation -- Abstract type for multiple arguments operations
--
   type Multiple_Operation is abstract new Operation with private;
   function Check (Op : Multiple_Operation; Selected : Selection_Subtype)
      return Boolean;
--
-- Predefined operations: And, Not, Or, Xor
--
   type And_Operation is new Multiple_Operation with private;
   function Execute (Op : And_Operation; Arguments : Array_Of_Variables)
      return Variable;
   type Not_Operation is new Unary_Operation with private;
   function Execute (Op : Not_Operation; Arguments : Array_Of_Variables)
      return Variable;
   type Or_Operation is new Multiple_Operation with private;
   function Execute (Op : Or_Operation; Arguments : Array_Of_Variables)
      return Variable;
   type Xor_Operation is new Multiple_Operation with private;
   function Execute (Op : Xor_Operation; Arguments : Array_Of_Variables)
      return Variable;
--
-- The  button  type  definition.  Initially  the  button  will have the
-- predefined operations in its combo box.
--
   package Edit_Exec_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditExec",
             Icon       => Stock_Execute,
             Relief     => Relief_None,
             Tip        =>
                (  "Apply operation to the selected linguistic "
                &  "variables and insert the result as a new "
                &  "variable"
          )     );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record is
      new Edit_Exec_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Exec is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record'Class;
--
-- Add -- A new operation to the combo box
--
--    Button - The button
--    Name   - Of the operation
--    Op     - The operation
--
-- Exceptions :
--
--    Name_Error - Duplicate name
--
   procedure Add
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record;
                Name   : UTF8_String;
                Op     : Operation'Class
             );
--
-- Get_Combo -- A new operation to the combo box
--
--    Button - The button
--
-- Returns :
--
--    The combo box of
--
   function Get_Combo
            (  Button : not null access
                        Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record
            )  return not null access
                      Gtk_Combo_Box_Text_Record'Class;
--
-- Get_Exec_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Exec_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Exec;
--
-- Remove -- An operation
--
--    Button - The button
--    Name   - The operation to remove
--
-- Nothing happens if the operation is not in the combo box
--
   procedure Remove
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record;
                Name   : UTF8_String
             );
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Find -- Editing find button
--
-- The button selects the points of linguistic variables that fall  into
-- the currently marked rectangular area.
--
   package Edit_Find_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditFind",
             Icon       => Stock_Find,
             Relief     => Relief_None,
             Tip        =>
                (  "Select points of linguistic variables "
                &  "from the rectangle"
          )     );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record is
      new Edit_Find_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Find is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record'Class;
--
-- Get_Find_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Find_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Find;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_New -- Editing new variable button
--
-- The  button  creates  a  new linguistic variable next to the selected
-- one.  In  absence of any selection it adds the variable to the end of
-- the variables list.
--
   package Edit_New_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditNew",
             Icon       => Stock_New,
             Relief     => Relief_None,
             Tip        => "New linguistic variable"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_New_Record is
      new Edit_New_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_New is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_New_Record'Class;
--
-- Get_New_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_New_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_New;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Purge -- Purge variable(s) button
--
-- The  button purges the membership functions of the slected linguistic
-- variables.
--
   package Edit_Purge_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditPurge",
             Icon       => Stock_Clear,
             Relief     => Relief_None,
             Tip        => "Purge selected linguistic variables"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record is
      new Edit_Purge_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Purge is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record'Class;
--
-- Get_Purge_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Purge_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Purge;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Redo -- Editing redo button
--
   package Edit_Redo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditRedo",
             Icon       => Stock_Redo,
             Relief     => Relief_None,
             Tip        => "Redo edit"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record is
      new Edit_Redo_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Redo is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record'Class;
--
-- Get_Redo_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Redo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Redo;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Remove -- Editing remove button
--
   package Edit_Remove_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditRemove",
             Icon       => Stock_Remove,
             Relief     => Relief_None,
             Tip        => "Remove"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record is
      new Edit_Remove_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Remove is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record'Class;
--
-- Get_Remove_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Remove_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Remove;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Undo -- Editing undo button
--
   package Edit_Undo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditUndo",
             Icon       => Stock_Undo,
             Relief     => Relief_None,
             Tip        => "Undo edit"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record is
      new Edit_Undo_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Undo is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record'Class;
--
-- Get_Undo_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Undo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Undo;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Up -- Editing up button
--
-- The button moves selected linguistic variables up in the list.
--
   package Edit_Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditUp",
             Icon       => Stock_Go_Up,
             Relief     => Relief_None,
             Tip        => "Move linguistic variables up"
          );
   type Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record is
      new Edit_Up_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Up is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record'Class;
--
-- Get_Up_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Up_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Up;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_X -- Editing X scale
--
-- The  scrollbar is used to adjust the domain values of the points of a
-- variable or a set of variables.
--
   type Gtk_Fuzzy_Linguistic_Set_Edit_X_Record is
      new Gtk_Scrollbar_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_X is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_X_Record'Class;
--
-- Get_X_Move_Bar -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_X_Move_Bar
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_X;
--
-- Gtk_Fuzzy_Linguistic_Set_Edit_Y -- Editing Y scale
--
-- The  scrollbar is used to adjust the membership values.
--
   type Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record is
      new Gtk_Scrollbar_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Edit_Y is
      access all Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record'Class;
--
-- Get_Y_Move_Bar -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Y_Move_Bar
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Y;
--
-- Edited -- Get the modification flag
--
--    Widget - The widget
--
-- The result is true if the inidcated set was edited by the user.
--
-- Returns :
--
--    The modification flag
--
   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Boolean;
--
-- Get -- The fuzzy linguistic set of
--
--    Widget - The widget
--    Purge  - Remove redindant points of the variables
--
-- This  function  returns the set currently indicated by the widget. It
-- is  checked  for  being correct, that is it contains no duplicated or
-- improperly spelled  names  of  variables.  When  Purge  is  True  the
-- variables  of  the  result  set  are checked for redundant membership
-- points and these points are removed.
--
-- Returns :
--
--    The linguistic set
--
-- Exceptions :
--
--    Constraint_Error - Illegal names in the set
--    Name_Error       - Duplicated names in the set
--
   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Purge  : Boolean := True
            )  return Linguistic_Set;
--
-- Get -- A linguistic variable of
--
--    Widget - The widget
--    Index  - The index of the variable
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Index  : Positive
            )  return Variable;
--
-- Get_Cardinality -- The number of variables
--
--    Widget - The widget
--
-- Returns :
--
--    The number of variables
--
   function Get_Cardinality
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Natural;
--
-- Get_Domain_Column_Note -- Get the current domain column note
--
--    Widget - The widget
--
-- The  note  is  shown  next  to  the column title as controlled by the
-- resource style domain-column-title.
--
-- Returns :
--
--    The note text
--
   function Get_Domain_Column_Note
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return UTF8_String;
--
-- Get_Domain_View -- The domain widget used
--
--    Widget - The widget
--
-- Returns :
--
--    The domain view widget associated with
--
   function Get_Domain_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return not null access
                      Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
--
-- Get_Name -- Of a variable in the set
--
--    Widget - The widget
--    Index  - The index of the variable
--
-- Returns :
--
--    The name of the variable
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   function Get_Name
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Index  : Positive
            )  return UTF8_String;
--
-- Get_Tree_View -- The tree view widget used
--
--    Widget - The widget
--
-- Returns :
--
--    The tree view implementing it
--
   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Tree_View;
--
-- Gtk_New -- Factory
--
--    Widget - Tree view (output)
--    Value  - To indicate
--
-- This procedure creates the widget for  the  list  of  variables.  The
-- domain area widget is created as  well.  It  can  be  obtained  using
-- Get_Domain_View call.
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Linguistic_Set_Tree_View;
                Value  : Linguistic_Set
             );
--
-- Get_Selection -- Get current selection
--
--    Widget - The widget
--
-- Returns :
--
--    The current selection
--
   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Selection;
   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Selection_Subtype;
--
-- Image -- Output of domain values
--
--    Widget - The widget
--    Value  - The domain value
--
-- Returns :
--
--    A string representation of
--
   function Image
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Value  : Number
            )  return UTF8_String;
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget    - THe widget to initialize
--    Value     - To indicate
--    Tooltips  - The group of tooltips to use (optional)
--
   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Value  : Linguistic_Set
             );
--
-- Insert -- A linguistic variable
--
--    Widget - The widget
--    Index  - The index of the variable
--    Name   - Of the new variable
--    Value  - Of the variable
--
-- This procedure inserts a linguistic variable as if it happened  by  a
-- user action. The inserted variable is selected. Undo and redo buffers
-- are  changed  correspondingly. The parameter Index specifies where to
-- insert the variable.
--
-- Exceptions :
--
--    Constraint_Error - Index is greater than number of variables + 1
--
   procedure Insert
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable
             );
--
-- Is_Editable -- Get the editable flag
--
--    Widget - The widget
--
-- Returns :
--
--    The editable flag
--
   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Boolean;
--
-- Put -- A linguistic variable
--
--    Widget - The widget
--    Index  - The index of the variable
--    Value  - To be set
--
-- If the replaced variable had any selection of points, these  will  be
-- removed from the selection. It also erases the undo and redo buffers.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Value  : Variable
             );
--
-- Put -- A set of linguistic variables
--
--    Widget - The widget
--    Value  - To be set
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections made and erases the undo and redo buffers.
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Value  : Linguistic_Set
             );
--
-- Remove -- A linguistic variable
--
--    Widget - The widget
--    Index  - The index of the variable
--
-- This procedure removes a linguistic variable as if it happened  by  a
-- user action. Nothing happens when Index does not refer to a variable.
--
   procedure Remove
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive
             );
--
-- Replace -- A linguistic variable
--
--    Widget - The widget
--    Index  - The index of the variable
--    Name   - Of the new variable
--    Value  - Of the variable
--
-- This  procedure replaces a linguistic variable as if it happened by a
-- user action. The variable to replace is specified by its position.
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   procedure Replace
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable
             );
--
-- Select_Duplicated -- The fuzzy linguistic set of
--
--    Widget - The widget
--
-- This procedure selects duplicated variables in the widget.
--
   procedure Select_Duplicated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
             );
--
-- Set_Editable -- Change the editable flag
--
--    Widget   - The widget
--    Editable - The value of editable flag to set
--
-- This procedure changes the editable flag.
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Editable : Boolean
             );
--
-- Set_Domain_Column_Note -- Change the domain column
--
--    Widget - The widget
--    Note   - The text
--
-- This  procedure  sets  the  text  to  add  to the first column title.
-- Normally it is the measurement units used for the domain values.
--
   procedure Set_Domain_Column_Note
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Note   : UTF8_String
             );
--
-- Set_Name -- A linguistic variable
--
--    Widget - The widget
--    Index  - The index of the variable
--    Name   - To be set
--
-- The procedure changes the variable name. The name is not checked.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Set_Name
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String
             );
--
-- Update -- The tree view from a selection
--
--    Widget   - The widget
--    Selected - The variables to update
--
-- This procedure synchronizes the tree view of the linguistic sets with
-- the variables specified by Selection.
--
   procedure Update
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Selected : Selection
             );
--
-- Value -- Input of domain values
--
--    Widget - The widget
--    Source - The string containing the value
--
-- This function parses the string Source  for  a  domain  value  stored
-- there. The derived type may wish to override this function to provide
-- other editing of the domain values in the tree view.
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Constraint_Error - Value is not in range
--    Data_Error       - Unmatched text
--    End_Error        - No number matched
--    other            - Any other error, such as Unit_Error
--
   function Value
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Source : UTF8_String
            )  return Number;
--
-- Gtk_Fuzzy_Tree_View_Factory -- Abstract   factory   of  widgets.  The
--                                factory is used in  order  to  provide
--                                an automatic creation of widgets.
--
   type Gtk_Fuzzy_Tree_View_Factory is
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Create -- Widget creation
--
--    Factory - The factory object
--    Value   - To indicate
--
-- Retunrs :
--
--    The widget
--
   function Create
            (  Factory : Gtk_Fuzzy_Tree_View_Factory;
               Value   : Linguistic_Set
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View;

private
   use Fuzzy.Abstract_Edit.Named;
--
-- Edit_Action -- The  action  types  modifying  the  set,  used  as   a
--                discriminant for undo stack items.
--
   type Edit_Action is
        (  Noop,
           Delete,
           Insert,
           Move,
           Rename,
           Replace,
           Reselect,
           Slide_X,
           Slide_Y
        );
--
-- Selection_State -- The variables and points of selected
--
   type Selection_State is record
      Points    : Selection;
      Variables : Points_Indices.Set;
   end record;
   type Selection_State_Ptr is access all Selection_State;
--
-- Edit_Undo_Item -- An undo action item
--
   type Edit_Undo_Item (Action : Edit_Action := Noop) is record
      First : Boolean := True;
      case Action is
         when Noop =>
            null;
         when Delete =>
            Delete_At : Variable_Index;
         when Move =>
            Moved   : Selection;
            Move_By : Integer;
         when Slide_X =>
            Shift_X : Number'Base;
         when Slide_Y =>
            Shift_Y : Confidence;
         when Replace =>
            Replace_At : Variable_Index;
            Replaced   : Variable;
         when Insert =>
            Insert_At : Variable_Index;
            Inserted  : Variable;
            Name      : Text_Handles.Handle;
            Expand    : Boolean;
         when Rename =>
            Rename_At : Variable_Index;
            Renamed   : Text_Handles.Handle;
         when Reselect =>
            Selected  : Selection_State;
      end case;
   end record;
--
-- Edit_Undo_Stacks -- Stack for editing undo
--
   package Edit_Undo_Stacks is
      new Generic_Segmented_Stack
          (  Index_Type   => Integer,
             Object_Type  => Edit_Undo_Item,
             Null_Element => (Action => Noop, First => True)
          );
   package Edit_Stacks renames Edit_Undo_Stacks.Segmented_Stack;
   use Edit_Stacks;
--
-- Fuzzy_Linguistic_Set_Tree_View_Data
--
   type Fuzzy_Linguistic_Set_Tree_View_Data is
      new Object.Entity with
   record
      Widget     : Gtk_Fuzzy_Linguistic_Set_Tree_View;
      Tree       : Gtk_Tree_View;
      Store      : Gtk_Tree_Store;
      Set        : Gtk_Fuzzy_Linguistic_Set_Domain_Ref.Strong_Reference;
      Domain     : Gtk_Cell_Renderer_Text;
      Span       : Gtk_Cell_Renderer_Fuzzy_Boolean;
      Left       : Gtk_Cell_Renderer_Fuzzy_Boolean;
      Right      : Gtk_Cell_Renderer_Fuzzy_Boolean;
      Accumulate : Gtk_Fuzzy_Linguistic_Set_Accumulate;
      Add        : Gtk_Fuzzy_Linguistic_Set_Edit_Add;
      Copy       : Gtk_Fuzzy_Linguistic_Set_Edit_Copy;
      Down       : Gtk_Fuzzy_Linguistic_Set_Edit_Down;
      Exec       : Gtk_Fuzzy_Linguistic_Set_Edit_Exec;
      Find       : Gtk_Fuzzy_Linguistic_Set_Edit_Find;
      New_Var    : Gtk_Fuzzy_Linguistic_Set_Edit_New;
      Undo       : Gtk_Fuzzy_Linguistic_Set_Edit_Undo;
      Purge      : Gtk_Fuzzy_Linguistic_Set_Edit_Purge;
      Redo       : Gtk_Fuzzy_Linguistic_Set_Edit_Redo;
      Remove     : Gtk_Fuzzy_Linguistic_Set_Edit_Remove;
      Up         : Gtk_Fuzzy_Linguistic_Set_Edit_Up;
      X_Move     : Gtk_Fuzzy_Linguistic_Set_Edit_X;
      Y_Move     : Gtk_Fuzzy_Linguistic_Set_Edit_Y;
      Undo_Stack : Edit_Stacks.Stack;
      Redo_Stack : Edit_Stacks.Stack;
      -- Texts
      Checked     : Text_Handles.Handle := Create ("black");
      Error       : Text_Handles.Handle := Create ("red");
      Domain_Note : Text_Handles.Handle;
      -- Accumulation dialog
      Acc_Dialog  : Gtk_Dialog;
      Acc_Set     : Gtk_Fuzzy_Set;
      -- States
      Selection_Pending : Boolean := False; -- Transition state
      -- Handlers of the domain widget
      Set_Style   : Handler_Reference;
      Marked      : Handler_Reference;
   end record;
--
-- Finalize -- Destruction
--
--    Data - The widget data
--
   procedure Finalize
             (  Data : in out Fuzzy_Linguistic_Set_Tree_View_Data
             );
--
-- Get_Name -- Of a variable
--
--    Data  - The widget data
--    Index - Of the variable
--
-- Returns :
--
--    The name of, or else an empty string
--
   function Get_Name
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return UTF8_String;
--
-- Get_Row -- Of a variable
--
--    Data  - The widget data
--    Index - Of the variable
--
-- Returns :
--
--    The iterator of
--
   function Get_Row
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return Gtk_Tree_Iter;
--
-- Get_Selection -- Get current selection
--
--    Data  - The widget data
--
-- Returns :
--
--    The current selection
--
   function Get_Selection
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Selection;
   function Get_Selection
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Selection_Subtype;
--
-- Insert -- A new variable
--
--    Data     - The widget data
--    Selected - The current selection
--    Var_No   - The new variable number
--    Var      - The variable value
--
   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive
             );
   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive;
                Var      : Variable
             );
--
-- Insert -- A new membership point
--
--    Data     - The widget data
--    Selected - The current selection
--    Var_No   - The variable number
--    Position - The position to insert the point after
--
   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive;
                Position : Natural
             );
--
-- Is_Editable -- Editable property
--
--    Data - The widget data
--
-- Returns :
--
--    The property value
--
   function Is_Editable
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Boolean;
--
-- Is_Expanded -- If the variable's points are expanded in the view
--
--    Data  - The widget data
--    Index - Of the variable
--
-- Returns :
--
--    True if the variable exists and its points are expanded
--
   function Is_Expanded
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return Boolean;
--
-- Move -- Move variables
--
--    Data      - The widget data
--    Variables - The variables list
--    By        - The amount to move
--
   procedure Move
             (  Data      : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Variables : Selection;
                By        : Integer
             );
--
-- Purge -- Selected variables
--
--    Data     - The widget data
--    Selected - Selection
--
   procedure Purge
             (  Data     : in out  Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State
             );
--
-- Redo -- Restore state
--
--    Data  - The widget data
--    Item  - The action to undertake
--    First - The first action to do flag
--    Stack - To push the reverse actions onto
--
   procedure Redo
             (  Data  : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Item  : Edit_Undo_Item;
                First : in out Boolean;
                Stack : in out Edit_Stacks.Stack
             );
--
-- Remove -- Remove selected items
--
--    Data - The widget data
--
   procedure Remove (Data : in out Fuzzy_Linguistic_Set_Tree_View_Data);
--
-- Select_Duplicated -- Changes selection to duplicated variables
--
--    Data  - The widget data
--
   procedure Select_Duplicated
             (  Data : in out Fuzzy_Linguistic_Set_Tree_View_Data
             );
--
-- Set_Buttons -- Changes the buttons sensitivity upon selection
--
--    Data     - The widget data
--    Selected - Selection
--
   procedure Set_Buttons
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection
             );
--
-- Set_Domain_Column_Title -- Set the title of the first column
--
--    Data  - The widget data
--    Title - The title
--    Note  - The title note
--
   procedure Set_Domain_Column_Title
             (  Data   : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Title  : UTF8_String;
                Note   : UTF8_String
             );
--
-- Set_Name -- Of a variable
--
--    Data  - The widget data
--    Index - Of the variable
--    Name  - The name to set
--
-- The name is checked before being set.
--
   procedure Set_Name
             (  Data  : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Index : Positive;
                Name  : String
             );
--
-- Set_Value -- Of a variable point
--
--    Data     - The widget data
--    Var_No   - Of the variable
--    Point_No - The point to change
--    Value    - The value to set
--
-- Exceptions :
--
--    Constraint_Error - Wrong indices or the value
--
   procedure Set_Value
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Var_No   : Positive;
                Point_No : Positive;
                Value    : Number
             );
--
-- Set_Selection -- Set selection in the tree view
--
--    Data      - The widget data
--    Points    - The list of points to select
--    Variables - The list of variables to select
--
   procedure Set_Selection
             (  Data      : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Points    : Selection;
                Variables : Points_Indices.Set
             );

   type Fuzzy_Linguistic_Set_Tree_View_Data_Ptr is
      access Fuzzy_Linguistic_Set_Tree_View_Data'Class;
   package Edit_Data_Handles is
      new Object.Handle
          (  Fuzzy_Linguistic_Set_Tree_View_Data,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr

          );
   use Edit_Data_Handles;
--
-- Widget_Type -- The button types
--
   type Widget_Type is
        (  Accumulate_Button,
           Add_Button,
           Copy_Button,
           Down_Button,
           Exec_Button,
           Find_Button,
           New_Button,
           Undo_Button,
           Purge_Button,
           Redo_Button,
           Remove_Button,
           Up_Button,
           X_Move_Bar,
           Y_Move_Bar
        );

   type Edit_Data_Handle (Widget : Widget_Type) is
      new Edit_Data_Handles.Handle with null record;
   procedure Finalize (Ref : in out Edit_Data_Handle);
   function Ref (Object : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr)
      return Edit_Data_Handle;
   procedure Set
             (  Ref    : in out Edit_Data_Handle;
                Object : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );

   type Gtk_Fuzzy_Linguistic_Set_Accumulate_Record is
      new Accumulate_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Accumulate_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record is
      new Edit_Add_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Add_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record is
      new Edit_Copy_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Copy_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record is
      new Edit_Down_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Down_Button);
   end record;

   type Operation is abstract
      new Ada.Finalization.Controlled with null record;

   type Unary_Operation    is abstract new Operation with null record;
   type Binary_Operation   is abstract new Operation with null record;
   type Multiple_Operation is abstract new Operation with null record;

   type And_Operation is new Multiple_Operation with null record;
   type Not_Operation is new Unary_Operation    with null record;
   type Or_Operation  is new Multiple_Operation with null record;
   type Xor_Operation is new Multiple_Operation with null record;

   type Operation_Ptr is access Operation'Class;
   type Operation_Ref is new Ada.Finalization.Controlled with record
      Ptr : Operation_Ptr;
   end record;
   procedure Adjust   (Ref : in out Operation_Ref);
   procedure Finalize (Ref : in out Operation_Ref);
   procedure Set (Ref : in out Operation_Ref; Op : Operation'Class);

   package Operation_Tables is new Tables (Operation_Ref);
   use Operation_Tables;

   package Gtk_Combo_Ref is
      new GLib.Object.Strong_References (Gtk_Combo_Box_Text_Record);
   use Gtk_Combo_Ref;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record is
      new Edit_Exec_Buttons.Gtk_Style_Button_Record with
   record
      Data   : Edit_Data_Handle (Exec_Button);
      Set    : Operation_Tables.Table;
      Combo  : Gtk_Combo_Ref.Strong_Reference;
   end record;
--
-- Initialize -- To be called by any derived type
--
--    Button   - The button
--    Tooltips - The tooltips to use
--
   procedure Initialize
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record'Class
             );
   procedure Update_Combo
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record
             );

   type Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record is
      new Edit_Find_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Find_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_New_Record is
      new Edit_New_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (New_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record is
      new Edit_Purge_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Purge_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record is
      new Edit_Redo_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Redo_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record is
      new Edit_Remove_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Remove_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record is
      new Edit_Undo_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Undo_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record is
      new Edit_Up_Buttons.Gtk_Style_Button_Record with
   record
      Data : Edit_Data_Handle (Up_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Edit_X_Record is
      new Gtk_Scrollbar_Record with
   record
      Data     : Edit_Data_Handle (X_Move_Bar);
      Span     : Interval := (0.0, 1.0);
      Page     : Interval := (0.0, 1.0);
      Modified : Boolean  := False;
   end record;
   procedure Set_Adjustment
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Lower  : Number'Base;
                Upper  : Number'Base;
                From   : Number'Base;
                To     : Number'Base
             );
   procedure Set_Points
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Var_No : Variable_Index;
                From   : Point_Index;
                To     : Point_Index
             );
   procedure Set_Variables
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Selected : Selection
             );

   type Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record is
      new Gtk_Scrollbar_Record with
   record
      Data        : Edit_Data_Handle (Y_Move_Bar);
      Max         : Confidence := Confidence'First;
      Page        : Confidence := Confidence'Last;
      Modified    : Boolean := False;
      Var_No      : Positive;
      First_Point : Positive;
      Last_Point  : Positive;
   end record;
   procedure Set_Adjustment
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record;
                From   : Confidence;
                To     : Confidence
             );
   procedure Set_Points
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record;
                Var_No : Variable_Index;
                From   : Point_Index;
                To     : Point_Index
             );
--
-- Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
--
   type Gtk_Fuzzy_Linguistic_Set_Tree_View_Record is
      new Gtk_Scrolled_Window_Record with
   record
      Data : Edit_Data_Handles.Handle;
   end record;
--
-- Reset -- Erase the store and rebuild it anew
--
   procedure Reset
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Set    : Linguistic_Set
             );
--
-- Accumulate -- The clicked event's callback
--
   procedure Accumulate_Set
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Accumulate_Record'Class
             );
   package Accumulate_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Accumulate_Record
          );
--
-- Commit_Span_Edit -- The commit event callbacks
--
   procedure Commit_Span_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   procedure Commit_Left_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   procedure Commit_Right_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   package Commit_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Fuzzy_Boolean_Record,
             Gtk_Fuzzy_Linguistic_Set_Tree_View
          );
--
-- Edited -- The edited event callbacks
--
   procedure Edited
             (  Cell   : access Gtk_Cell_Renderer_Text_Record'Class;
                Params : GValues;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   package Edited_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Text_Record,
             Gtk_Fuzzy_Linguistic_Set_Tree_View
          );
--
-- Edit_Add -- The clicked event's callback
--
   procedure Edit_Add
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record'Class
             );
   package Add_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record
          );
--
-- Edit_Copy -- The clicked event's callback
--
   procedure Edit_Copy
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record'Class
             );
   package Copy_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record
          );
--
-- Edit_Down -- The clicked event's callback
--
   procedure Edit_Down
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record'Class
             );
   package Down_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record
          );
--
-- Edit_Exec -- The clicked event's callback
--
   procedure Edit_Exec
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record'Class
             );
   package Exec_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record
          );
--
-- Edit_Exec_Changed -- The changed event's callback
--
   procedure Edit_Exec_Changed
             (  Combo_Entry : access Gtk_Combo_Box_Text_Record'Class;
                Data        : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
   package Exec_Combo_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Combo_Box_Text_Record,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
          );
--
-- Edit_Find -- The clicked event's callback
--
   procedure Edit_Find
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record'Class
             );
   package Find_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record
          );
--
-- Edit_New -- The clicked event's callback
--
   procedure Edit_New
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_New_Record'Class
             );
   package New_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_New_Record
          );
--
-- Edit_Purge -- The clicked event's callback
--
   procedure Edit_Purge
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record'Class
             );
   package Purge_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record
          );
--
-- Edit_Redo -- The clicked event's callback
--
   procedure Edit_Redo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record'Class
             );
   package Redo_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record
          );
--
-- Edit_Remove -- The clicked event's callback
--
   procedure Edit_Remove
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record'Class
             );
   package Remove_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record
          );
--
-- Edit_Undo -- The clicked event's callback
--
   procedure Edit_Undo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record'Class
             );
   package Undo_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record
          );
--
-- Edit_Up -- The clicked event's callback
--
   procedure Edit_Up
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record'Class
             );
   package Up_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record
          );
--
-- Edit_X -- The value_changed event's callback
--
   procedure Edit_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
--
-- Edit_Y -- The value_changed event's callback
--
   procedure Edit_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
--
-- Key_Release -- The key_release_event event's callback
--
   function Key_Release
            (  Widget : access Gtk_Tree_View_Record'Class;
               Event  : Gdk_Event;
               Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
            )  return Boolean;
   package Key_Release_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Tree_View_Record,
             Boolean,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
          );
--
-- Selection_Changed -- The changed event callback
--
   procedure Selection_Changed
             (  Widget : access Gtk_Tree_Selection_Record'Class;
                View   : Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   package Selection_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Fuzzy_Linguistic_Set_Tree_View
          );
--
-- Style_Updated -- The style-updated event's callback
--
   procedure Style_Updated
             (  Widget : access Gtk_Tree_View_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
   package Style_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_View_Record,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
          );
--
-- Domain_Style_Updated -- Domain widget style-updated event's callback
--
-- This one is used for drawing widget's styles  which  might  influence
-- the our data.
--
   procedure Domain_Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
--
-- Domain_Marked -- Domain widget marked event's callback
--
   procedure Domain_Marked
             (  Widget : access Gtk_Widget_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             );
   package Domain_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
          );

   package Adjustment_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Adjustment_Record,
             Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
          );
--
-- Accumulation_Domain -- Specialized domain for the accumulation dialog
--
   type Accumulation_Domain
        (  Data : not null access
                  Fuzzy_Linguistic_Set_Tree_View_Data'Class
        )  is new Domain_Description with null record;
--
-- Get_Cardinality -- Overrides Fuzzy.Abstract_Edit.Named...
--
   overriding
   function Get_Cardinality
            (  Domain : Accumulation_Domain
            )  return Natural;
--
-- Get_Name -- Overrides Fuzzy.Abstract_Edit.Named...
--
   overriding
   function Get_Name
            (  Domain : Accumulation_Domain;
               Index  : Integer
            )  return String;
--
-- Put -- Overrides Fuzzy.Abstract_Edit.Named...
--
   overriding
   procedure Put
          (  Destination : in out String;
             Pointer     : in out Integer;
             Domain      : Accumulation_Domain;
             From        : Integer;
             To          : Integer;
             Field       : Natural   := 0;
             Justify     : Alignment := Left;
             Fill        : Character := ' '
          );
--
-- Selection_Monitor -- A helper type to coalesce multiple selections
--
-- When created the object freezes any reaction on  selection  changing.
-- Upon destruction it commits the selection change as necessary.
--
   type Selection_Monitor
        (  Data : not null access
                  Fuzzy_Linguistic_Set_Tree_View_Data'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Nested : Boolean := False;
   end record;
   overriding
   procedure Initialize (Monitor : in out Selection_Monitor);
   overriding
   procedure Finalize   (Monitor : in out Selection_Monitor);
--
-- Duplication_Tables -- Name to point list mapping
--
   package Duplication_Tables is new Tables (Boolean);

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View ;
