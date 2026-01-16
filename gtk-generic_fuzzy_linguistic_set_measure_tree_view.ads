--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_           Luebeck            --
--        Measure_Tree_View                        Spring, 2007       --
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
-- This  generic  package  provides  editing  and   viewing   lists   of
-- dimensioned fuzzy linguistic variables. The package has the following
-- formal parameters:
--
-- (o)  Fuzzy_Measures is an instance  of Fuzzy.Measures  which provides
--      dimensioned fuzzy numbers;
-- (o)  Fuzzy_Measure_Linguistics  is  an  instance  of  Fuzzy.Measures.
--      Linguistics (dimensioned linguistic variables);
-- (o)  Fuzzy_Measure_Linguistic_Sets  is an instance of Fuzzy.Measures.
--      Linguistics.Sets (sets of dimensioned linguistic variables);
-- (o)  Float_Edit is a compatible instance of Strings_Edit.Float_Edit;
-- (o)  Derived_Measures is  an instance of  Measures_Derived needed for
--      editing dimensioned values;
-- (o)  Irregular_Measures is an instance of Measures_Irregular used for
--      same purpose;
-- (o)  Measure_Edit is a compatible instance Measures_UTF8_Edit to edit
--      dimensioned values in UTF-8 encoding (GTK+ uses UTF-8);
-- (o)  Domain is a compatible instance of
--      Gtk.Generic_Fuzzy_Linguistic_Set_Domain;
-- (o)  Tree_View is an instance of its child
--      Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View.
--
with Gtk.Tree_View;  use Gtk.Tree_View;
with Units;          use Units;

with Fuzzy.Measures.Linguistics.Sets;
with Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View;
with Intervals.Measures;
with Measures_UTF8_Edit;
with Measures_Derived;
with Measures_Irregular;
with Strings_Edit.Float_Edit;
with Units.Base;

generic
   with package Fuzzy_Measures is new Fuzzy.Measures (<>);
   with package Fuzzy_Measure_Linguistics is
      new Fuzzy_Measures.Linguistics (<>);
   with package Fuzzy_Measure_Linguistic_Sets is
      new Fuzzy_Measure_Linguistics.Sets (<>);
   with package Float_Edit is
      new Strings_Edit.Float_Edit
          (  Fuzzy_Measures.Fuzzy_Floats.Float_Intervals_Of.Number
          );
   with package Derived_Measures is
      new Measures_Derived
          (  Fuzzy_Measures.Interval_Measures_Of.Float_Measures_Of
          );
   with package Irregular_Measures is
      new Measures_Irregular (Derived_Measures);
   with package Measure_Edit is
      new Measures_UTF8_Edit
          (  Irregular_Measures => Irregular_Measures,
             Float_Edit         => Float_Edit
          );
   with package Domain is
      new Gtk.Generic_Fuzzy_Linguistic_Set_Domain
          (  Fuzzy_Linguistics =>
                Fuzzy_Measure_Linguistics.Fuzzy_Linguistics_Of,
             Fuzzy_Linguistic_Sets =>
                Fuzzy_Measure_Linguistic_Sets.Fuzzy_Linguistic_Sets_Of,
             Float_Edit =>
                Float_Edit,
             Class_Name => <>
          );
   with package Tree_View is new Domain.Generic_Tree_View (<>);
package Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View is
   package Fuzzy_Measures_Of renames Fuzzy_Measures;
   package Fuzzy_Measure_Linguistics_Of
      renames Fuzzy_Measure_Linguistics;
   package Fuzzy_Measure_Linguistic_Sets_Of
      renames Fuzzy_Measure_Linguistic_Sets;
   package Domain_Of renames Domain;
   package Tree_View_Of renames Tree_View;
   use Fuzzy_Measures_Of.Interval_Measures_Of.Float_Measures_Of;
   use Fuzzy_Measure_Linguistics_Of;
   use Fuzzy_Measure_Linguistic_Sets_Of;
   use Fuzzy_Linguistics_Of;
   use Fuzzy_Linguistic_Sets_Of;
   use Domain_Of;
   use Tree_View_Of;
--
-- Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record -- Editing
--
   type Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record is
      new Gtk_Fuzzy_Linguistic_Set_Tree_View_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View is access
      all Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record'Class;
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Purge  : Boolean := True
            )  return Set_Measure;
--
-- Get -- The fuzzy linguistic set of
--
--    Widget - The widget
--    Value  - Dimensionless fuzzy linguistic set
--    Scale  - The scale
--    Purge  - Remove redindant points of the variables
--
-- This procedure returns the set currently indicated by the widget as a
-- dimensionless  set  and  its  scale.  The  set  is  checked for being
-- correct,  that  is  it  contains  no duplicated or improperly spelled
-- names  of  variables.  When Purge is True the variables of the result
-- set are checked for redundant membership points and these points  are
-- removed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal names in the set
--    Name_Error       - Duplicated names in the set
--
   procedure Get
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : out Linguistic_Set;
                Scale  : out Measure;
                Purge  : Boolean := True
             );
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Index  : Positive
            )  return Variable_Measure;
--
-- Get_* -- Querying the actuator widgets
--
--    Widget - The widget
--
-- Returns :
--
--    The corresponding button or scroll bar
--
   function Get_Accumulate_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Accumulate;
   function Get_Add_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Add;
   function Get_Copy_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Copy;
   function Get_Down_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Down;
   function Get_Exec_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Exec;
   function Get_Find_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Find;
   function Get_New_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_New;
   function Get_Purge_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Purge;
   function Get_Redo_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Redo;
   function Get_Remove_Button
            (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Remove;
   function Get_Undo_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Undo;
   function Get_Up_Button
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Up;
   function Get_X_Move_Bar
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_X;
   function Get_Y_Move_Bar
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Y;
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Natural;
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Gtk_Tree_View;
--
-- Get_Unit -- Get unit
--
--    Widget - The widget
--
-- Returns :
--
--    SI component
--
   function Get_Unit
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
            )  return Unit;
--
-- Gtk_New -- Factory
--
--    Widget - The widget (output)
--    Value  - To indicate
--    Scale  - The preferred unit
--
-- This procedure creates the widget as well as the domain  area  widget
-- is created as well. It can be obtained  using  Get_Domain_View  call.
-- The parameter Scale is the preferred dimension to be used to indicate
-- domain values. It must be a valid dimension specification as expected
-- by  Measures_UTF8_Edit.Value.  The  dimension must be compatible with
-- one of Value otherwise Unit_Error is  propagated.  Scale  can  be  an
-- empty  string,  in  which case the unit (Value.SI, 1.0, Value.Offset)
-- will be used as the scale.
--
-- Exceptions :
--
--    Constraint_Error - Scale's gain is not positiive
--    Data_Error       - Illegal scale
--    Unit_Error       - Incompatible scale
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View;
                Value  : Set_Measure;
                Scale  : UTF8_String  := ""
             );
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Value  : Number
            )  return UTF8_String;
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--    Value  - To indicate
--    Scale  - The preferred unit
--
-- This procedure initializes Widget and creates  all  possible  widgets
-- needed for  editing  a  linguistic  sets  indicated  by  Widget.  The
-- unnecessaty widgets can be removed using Sink.
--
-- Exceptions :
--
--    Constraint_Error - Scale's gain is not positiive
--    Data_Error       - Illegal scale
--    Unit_Error       - Incompatible scale
--
   procedure Initialize
             (  Widget : not null access
                Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record'Class;
                Value  : Set_Measure;
                Scale  : UTF8_String
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
--    Unit_Error       - Incompatible units
--
   procedure Insert
             (  Widget : not null access
                    Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable_Measure
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
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
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
--    Unit_Error       - Incompatible units
--
   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Value  : Variable_Measure
             );
--
-- Put -- A set of linguistic variables
--
--    Widget - The widget
--    Value  - To be set
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections  made  and erases the undo and redo buffers. The dimension
-- of Value must be  compatible  with  the  widget's  units.  Otherwise,
-- Constraint_Error is propagated.
--
-- Exceptions :
--
--    Ubit_Error - Incompatible units
--
   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : Set_Measure
             );
-- Put -- A set of linguistic variables
--
--    Widget - The widget
--    Value  - To be set
--    Scale  - The preferred unit
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections  made  and erases the undo and redo buffers. The parameter
-- Scale is the preferred  dimension  to  be  used  to  indicate  domain
-- values.  It  must  be  a valid dimension specification as expected by
-- Measures_UTF8_Edit.Value. The dimension must be compatible  with  one
-- of Value otherwise Unit_Error is propagated. Scale can  be  an  empty
-- string, in which case the unit (Value.SI, 1.0, Value.Offset) will  be
-- used as the scale.
--
-- Exceptions :
--
--     Unit_Error - Incompatible scale
--     Data_Error - Illegal scale
--
   procedure Put
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Value  : Set_Measure;
                Scale  : UTF8_String
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
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
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
--    Unit_Error       - Incomnpatible units
--
   procedure Replace
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable_Measure
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
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
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
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Editable : Boolean
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
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
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
                   Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
                Selected : Selection
             );
--
-- Value -- Input of domain values
--
--    Widget - The widget
--    Source - The string containing the value
--
-- This function parses the string Source  for  a  domain  value  stored
-- there.   The   in  Source  value  may  be  surrounded  by  characters
-- representing   UTF-8   encoded   code    points    from    the    set
-- Name_Tables.Blanks.  The  value  may  be  dimensioned.  When not, the
-- widget's scale is used as the dimension. The  dimension  shall  match
-- one of the scale. Whole source shall be matched. The derived type may
-- wish to override this function to provide other editing of the domain
-- values in the tree view.
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
--    Unit_Error       - Error in units
--    other            - Any other error, such as Unit_Error
--
   function Value
            (  Widget : not null access
                  Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record;
               Source : UTF8_String
            )  return Number;
--
-- Gtk_Fuzzy_Measure_Tree_View_Factory -- Abstract factory  of  widgets.
--                                        The factory is used  in  order
--                                        to    provide   an   automatic
--                                        creation of widgets.
--
   type Gtk_Fuzzy_Measure_Tree_View_Factory (Length : Natural) is
      new Gtk_Fuzzy_Tree_View_Factory with
   record
      SI     : Unit   := Units.Base.Unitless;
      Offset : Number := 1.0;
      Scale  : UTF8_String (1..Length);
   end record;
--
-- Create -- Overrides Gtk.Generic_Fuzzy_Linguistic_Set_Domain.
--                        Generic_Tree_View...
--
   overriding
   function Create
            (  Factory  : Gtk_Fuzzy_Measure_Tree_View_Factory;
               Value    : Linguistic_Set
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View;

private
--
-- Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record
--
   type Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record is
      new Gtk_Fuzzy_Linguistic_Set_Tree_View_Record with
   record
      Scale  : Measure; -- The scale of the indicated values
      Offset : Number;  -- The offset of the original values
   end record;

end Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View;
