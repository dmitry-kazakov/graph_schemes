--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Object                            Luebeck            --
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

with Gtk.Abstract_Browser;     use Gtk.Abstract_Browser;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Widget;               use Gtk.Widget;
with Persistent;               use Persistent;
with Persistent.Handle;        use Persistent.Handle;

with Generic_Doubly_Linked;
with Gtk.Generic_Style_Button;
with Gtk.Handlers;

package Gtk.Fuzzy_Object is
   pragma Elaborate_Body (Gtk.Fuzzy_Object);
--
-- Prefix -- The  prefix  of the GTK+ widget's names used by the feature
--           factories
--
   Prefix : constant String := "GtkFuzzy";
   type Hint_Type is (None, Checked, Conflicting, Erroneous);

   Picking_Error : exception;
--
-- Trace_Checks -- Storage constraint checks
--
   Trace_Checks : Boolean := False;
--
-- Install_Hints_Style_Properties -- Install common style properties
--
--    Class - The class record of a derived type
--
-- This procedure should be called upon class initialization for a  type
-- derived  from  Gtk_Fuzzy_Feature_Abstract_Factory_Record. It declares
-- style properties common for all factories.
--
-- Style properties :
--
--    none-image        - The image (stock ID) used to show a none hint.
--                        String, the default is "gtk-edit".
--    checked-image     - The image (stock ID) used to  show  a  checked
--                        hint. String, the default is "gtk-apply".
--    erroneous-image   - The  image  (stock  ID)  used to show an error
--                        hint. String, the default is "gtk-stop".
--    conflicting-image - The  image  (stock  ID)  used to show an error
--                        hint. String, the default is "gtk-cancel".
--
   procedure Install_Hints_Style_Properties (Class : Ada_GObject_Class);
--
-- Set_Hint -- Set hint
--
--    Box    - To place the hint
--    Parent - To get the styles from
--    Hint   - To indicate
--    Show   - True if the hint has to be shown
--
   procedure Set_Hint
             (  Box    : not null access Gtk_Box_Record'Class;
                Parent : not null access Gtk_Widget_Record'Class;
                Hint   : Hint_Type;
                Show   : Boolean
             );
------------------------------------------------------------------------
-- Picker_Constraint -- The constraint on the  picker  to  a  definitive
--                      storage
--
   type Picker_Constraint is private;
--
-- Check -- The constraint
--
--    Constraint - The constraint object
--    Storage    - A handle to persistent storage
--
-- Returns :
--
--    True if Storage matches the constraint
--
   function Check
            (  Constraint : Picker_Constraint;
               Storage    : Storage_Handle
            )  return Boolean;
--
-- Combine -- Two constraints
--
--    Left, Right - The constraint objects to combine
--
   procedure Combine (Left, Right : Picker_Constraint);
--
-- Create -- An object constraint
--
--    Name - The constraint name
--
-- The constraint created by this procedure has to be freed using a call
-- to Free.
--
-- Returns :
--
--    A new constraint
--
   function Create (Name : String) return Picker_Constraint;
--
-- Free -- The constraint
--
--    Constraint - To delete
--
   procedure Free (Constraint : in out Picker_Constraint);
--
-- Get -- Get the constraint
--
--    Constraint - The constraint object
--
-- Returns :
--
--    A handle to the storage to which picker is constrained or null
--
   function Get (Constraint : Picker_Constraint) return Storage_Handle;
--
-- Get_Name -- The object constraint name
--
--    Constraint - The constraint object
--
-- Returns :
--
--    Name used upon creation
--
   function Get_Name (Constraint : Picker_Constraint) return String;
--
-- Image -- The object constraint status
--
--    Constraint - The constraint object
--
-- Returns :
--
--    The string that represents the constraint state
--
   function Image (Constraint : Picker_Constraint) return String;
--
-- Reset -- Change the constraint to none
--
--    Constraint - The constraint object
--
-- This procedure is equivalent to Set with an invalid handle
--
   procedure Reset (Constraint : Picker_Constraint);
--
-- Set -- Change the constraint
--
--    Constraint - The constraint object
--    Storage    - The storage constraint to set
--
   procedure Set
             (  Constraint : Picker_Constraint;
                Storage    : Storage_Handle
             );

   Unconstrained : constant Picker_Constraint;
------------------------------------------------------------------------
-- Gtk_Object_Picker_Record -- An interface of
--
-- Signals :
--
--    state-changed - The state of a picker has been changed
--
   type Gtk_Object_Picker_Record is abstract
      new Gtk_Widget_Record with null record;
   type Gtk_Object_Picker is
      access all Gtk_Object_Picker_Record'Class;
--
-- Check -- An object path
--
--    Picker     - The object picker
--    Path       - An object path
--    Constraint - Picker constraint
--    Store      - The persistent storage of the object
--    Object     - The object
--
-- When successful, Store is a valid handle to the storage of the object
-- specified by Path.
--
-- Exceptions :
--
--    Picking_Error - Picking constraint violation
--
   procedure Check
             (  Picker     : not null access Gtk_Object_Picker_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is abstract;
--
-- Get -- The object picked
--
--    Picker     - The object picker
--    Constraint - Picker constraint
--    Store      - The persistent storage of the object
--    Object     - The object
--
-- Exceptions :
--
--    Picking_Error - Picking constraint violation
--
   procedure Get
             (  Picker     : not null access Gtk_Object_Picker_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is abstract;
--
-- Get -- The object picked
--
--    Picker     - The object picker
--    Constraint - Picker constraint
--
-- Returns :
--
--    The picked object path
--
-- Exceptions :
--
--    Picking_Error - Picking constraint violation
--
   function Get
            (  Picker     : not null access Gtk_Object_Picker_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is abstract;
--
-- Get_Parent -- The widget to take styles from
--
--    Picker - The object picker
--
-- The parent should have style button-spacing determining the  distance
-- between the buttons of the box and the entry field.
--
-- Returns :
--
--    The widget
--
   function Get_Parent
            (  Picker : not null access Gtk_Object_Picker_Record
            )  return Gtk_Widget is abstract;
--
-- Get_Path -- Get current storage path
--
--    Picker - The object picker
--    Object - A handle to
--
-- Returns :
--
--    Object's path (empty if Object is not in storage or illegal)
--
   function Get_Path
            (  Picker : not null access Gtk_Object_Picker_Record;
               Object : Deposit_Handle
            )  return Item_Path is abstract;
--
-- Initialize -- To call upon initialization
--
--    Picker - The object picker
--
   procedure Initialize
             (  Picker : not null access
                         Gtk_Object_Picker_Record'Class
             );
--
-- Set -- The object picker
--
--    Picker - The object picker
--    Store  - The persistent storage of the object
--    Object - The object
--
   procedure Set
             (  Picker : not null access Gtk_Object_Picker_Record;
                Store  : Storage_Handle;
                Object : Deposit_Handle
             )  is abstract;
   procedure Set
             (  Picker : not null access Gtk_Object_Picker_Record;
                Path   : Item_Path
             )  is abstract;
--
-- State_Changed -- Emits state-changed
--
   procedure State_Changed
             (  Picker : not null access Gtk_Object_Picker_Record
             );

------------------------------------------------------------------------
-- Gtk_Picker_Box_Record -- An entry with two buttons. The entry is used
--                          to  enter  an object path. The button on the
-- left selects currently specified path in the picker.  The  button  on
-- the right gets the path of the currently selected object.
--
   type Gtk_Picker_Box_Record is new Gtk_Hbox_Record with private;
   type Gtk_Picker_Box is access all Gtk_Picker_Box_Record'Class;
--
-- Get_Constraint -- Get the constraint of the picker box
--
--    Widget - The widget
--
-- Returns :
--
--    The entry box of the widget
--
   function Get_Constraint
            (  Widget : not null access Gtk_Picker_Box_Record'Class
            )  return Picker_Constraint;
--
-- Get_Entry -- Get the entry box
--
--    Widget - The widget
--
-- Returns :
--
--    The entry box of the widget
--
   function Get_Entry (Widget : not null access Gtk_Picker_Box_Record)
      return Gtk_Entry;
--
-- Get_Path -- Get current object path
--
--    Widget - The widget
--
-- Returns :
--
--    The path set
--
   function Get_Path (Widget : not null access Gtk_Picker_Box_Record)
      return Item_Path;
--
-- Get_Picker -- Get the object picker
--
--    Widget - The widget
--
-- Returns :
--
--    The picker
--
   function Get_Picker (Widget : not null access Gtk_Picker_Box_Record)
      return Gtk_Object_Picker;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- New widget creation
--
--    Widget       - The widget (result)
--    Name         - The widget's constraint name
--    Hint         - The hint widget associated with (result)
--    Picker       - The picker to use
--  [ Constraint ] - The constraint
--    Editable     - Initial editing flag
--    Initialize   - Set from the picker if true
--
   procedure Gtk_New
             (  Widget     : out Gtk_Picker_Box;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Storage_Handle;
                Editable   : Boolean := True;
                Initialize : Boolean := True
             );
   procedure Gtk_New
             (  Widget     : out Gtk_Picker_Box;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Editable   : Boolean := True;
                Initialize : Boolean := True
             );
--
-- Initialize -- To be called by derived type
--
--    Widget     - The widget (result)
--    Hint       - The hint widget associated with (result)
--    Picker     - The picker to use
--    Constraint - The constraint
--    Editable   - Initial editing flag
--    Initialize - Set from the picker if true
--
   procedure Initialize
             (  Widget     : not null access
                             Gtk_Picker_Box_Record'Class;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Storage_Handle;
                Editable   : Boolean;
                Initialize : Boolean
             );
--
-- Is_Editable -- Check if the widget is editable
--
--    Widget - The widget
--
-- Returns :
--
--    True if widget is editable
--
   function Is_Editable (Widget : not null access Gtk_Picker_Box_Record)
      return Boolean;
--
-- Set_Editable -- Set the widget editable or not
--
--    Widget   - The widget
--    Editable - The editable flag
--
   procedure Set_Editable
             (  Widget   : not null access Gtk_Picker_Box_Record;
                Editable : Boolean
             );
--
-- Set_Path -- Set the object path
--
--    Widget - The widget
--    Path   - To set
--
   procedure Set_Path
             (  Widget : not null access Gtk_Picker_Box_Record;
                Path   : Item_Path
             );
--
-- Set_Hint -- Set hint
--
--    Widget - The widget
--    Parent - To get the styles from
--    Hint   - To indicate
--    Show   - True if the hint has to be shown
--
   procedure Set_Hint
             (  Widget : not null access Gtk_Picker_Box_Record;
                Parent : not null access Gtk_Widget_Record'Class;
                Hint   : Hint_Type;
                Show   : Boolean
             );

   package Browse_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "BrowseButton",
             Icon       => Stock_Home,
             Tip        => "Browse the path",
             Relief     => Relief_None
          );
   use Browse_Buttons;

   package Get_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "GetButton",
             Icon       => Stock_Copy,
             Tip        => "Get currently selected",
             Relief     => Relief_None
          );
   use Get_Buttons;

   type Gtk_Null_Picker_Record is
      new Gtk_Object_Picker_Record with private;
   type Gtk_Null_Picker is access all Gtk_Null_Picker_Record'Class;

   procedure Gtk_New
             (  Picker : out Gtk_Null_Picker;
                Parent : not null access Gtk_Widget_Record'Class
             );
   procedure Initialize
             (  Picker : not null access Gtk_Null_Picker_Record'Class;
                Parent : not null access Gtk_Widget_Record'Class
             );
   overriding
   function Get_Parent
            (  Picker : not null access Gtk_Null_Picker_Record
            )  return Gtk_Widget;
   overriding
   procedure Check
             (  Picker     : not null access Gtk_Null_Picker_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
   overriding
   procedure Get
             (  Picker     : not null access Gtk_Null_Picker_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
   overriding
   function Get
            (  Picker     : not null access Gtk_Null_Picker_Record;
               Constraint : Picker_Constraint
            )  return Item_Path;
   overriding
   function Get_Path
            (  Picker : not null access Gtk_Null_Picker_Record;
               Object : Deposit_Handle
            )  return Item_Path;
   overriding
   procedure Set
             (  Picker : not null access Gtk_Null_Picker_Record;
                Store  : Storage_Handle;
                Object : Deposit_Handle
             );
   overriding
   procedure Set
             (  Picker : not null access Gtk_Null_Picker_Record;
                Path   : Item_Path
             );

   Picker_Class_Name : constant String := Prefix & "Picker";

private
   type Picker_Constraint_Object (Length : Natural) is record
      Storage : Storage_Handle;
      Name    : String (1..Length);
   end record;
   package Constraint_Lists is
      new Generic_Doubly_Linked (Picker_Constraint_Object);
   use Constraint_Lists.Doubly_Linked;

   type Picker_Constraint is new Item;

   type Gtk_Picker_Box_Record is new Gtk_Hbox_Record with record
      Edit       : Gtk_Entry;
      Hint       : Gtk_Box;
      Browse     : Browse_Buttons.Gtk_Style_Button;
      Get        : Get_Buttons.Gtk_Style_Button;
      Picker     : Gtk_Object_Picker;
      Constraint : Picker_Constraint;
         -- Signal handler reference
      Style_Handler : Handler_Reference;
      State_Handler : Handler_Reference;
   end record;
--
-- Browse -- Clicked event
--
   procedure Browse
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );
--
-- Destroy -- Event handler
--
   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );
--
-- Get -- Clicked event
--
   procedure Get
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );
--
-- Set -- Changed event
--
   procedure Set
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );
--
-- State_Changed -- Changed event
--
   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );
--
-- Set_Editable_Mode -- Set the widget editable or not
--
--    Widget   - The widget
--    Editable - The editable flag
--
-- This is done independent of the current widget state.
--
   procedure Set_Editable_Mode
             (  Widget   : not null access Gtk_Picker_Box_Record;
                Editable : Boolean
             );
--
-- Style_Updated -- Event handler
--
   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             );

   type Gtk_Null_Picker_Record is
      new Gtk_Object_Picker_Record with
   record
      Parent : Gtk_Widget;
   end record;

   package Folder_Box_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Picker_Box
          );

   Unconstrained : constant Picker_Constraint := null;

end Gtk.Fuzzy_Object;
