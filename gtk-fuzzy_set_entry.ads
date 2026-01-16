--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Set_Entry                         Luebeck            --
--  Interface                                      Spring, 2007       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
--  This   package   provides   a   widget   to   indicate  fuzzy  sets,
--  intuitionistic fuzzy sets and intuitionistic fuzzy  classifications.
--  The  widget's  visual appearance is of a combo box. The entry of the
--  combo  box  contains the fuzzy set rendered as a text. The drop down
--  part is one provided by the Gtk_Fuzzy_Set widget.
--
with Fuzzy;                      use Fuzzy;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Gdk.Event;                  use Gdk.Event;
with GLib.Values;                use GLib.Values;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Fuzzy_Boolean_Drawing;  use Gtk.Fuzzy_Boolean_Drawing;
with Gtk.Fuzzy_Set;              use Gtk.Fuzzy_Set;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;

with Fuzzy.Abstract_Edit.Handle;
with Gtk.Cell_Renderer_Fuzzy_Boolean;
with Gtk.Handlers;
with Object.Handle;

package Gtk.Fuzzy_Set_Entry is
   pragma Elaborate_Body (Gtk.Fuzzy_Set_Entry);
--
-- Abstract_Entry_Edit -- Editing and rendering parameters. This type is
--                        passed to entry and  cell  reneder  wdgets  to
-- provide  formatting  functionalities  separated   from   the   widget
-- implementation.
--
   type Abstract_Entry_Edit is
      abstract new Object.Entity with null record;
   type Abstract_Entry_Edit_Ptr is access Abstract_Entry_Edit'Class;
--
-- Get_Cardinality -- Get the cardinality of
--
--    Editor - Editing parameters
--
-- Retunrs :
--
--    The cardinality expected
--
   function Get_Cardinality
            (  Editor : Abstract_Entry_Edit
            )  return Natural is abstract;
--
-- Get_Default -- The default value used for rendering
--
--    Editor - Editing parameters
--
-- This function returns the fuzzy Boolean  value  used  for  output  of
-- intuitionistic  classifications  and sets as described in the package
-- Fuzzy.Abstract_Edit.Intuitionistic.

-- Returns :
--
--    The default fuzzy Boolean value used for intuitionistic objects
--
   function Get_Default
            (  Editor : Abstract_Entry_Edit
            )  return Fuzzy_Boolean is abstract;
--
-- Get_Domain -- The domain of the indicated value
--
--    Editor - Editing parameters
--
-- The  result  is  a  handle  to  the  formatting parameters as used by
-- Fuzzy.Abstract_Edit[.Intuitionistic].
--
-- Returns :
--
--    A handle to the description of the domain set
--
   function Get_Domain
            (  Editor : Abstract_Entry_Edit
            )  return Fuzzy.Abstract_Edit.Handle.Handle is abstract;
--
-- Image -- Rendering
--
--    Editor - Editing parameters
--    Value  - To render
--
-- These functions are used to render a value to text.
--
-- Returns :
--
--    The text
--
-- Exceptions :
--
--    Constraint_Error - Illegal value
--
   function Image
            (  Editor : Abstract_Entry_Edit;
               Value  : Fuzzy.Set
            )  return UTF8_String is abstract;
   function Image
            (  Editor : Abstract_Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Set
            )  return UTF8_String is abstract;
   function Image
            (  Editor : Abstract_Entry_Edit;
               Value  : Classification
            )  return UTF8_String is abstract;
--
-- Value -- Parsing
--
--    Editor - Editing parameters
--    Text   - To parse
--
-- These functions are used to parse a text to value.
--
-- Returns :
--
--    The text
--
-- Exceptions :
--
--    Indicate an error. Typically it is  Constraint_Error,  Data_Error,
--    End_Error, and Unit_Error
--
   function Value
            (  Editor : Abstract_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Set is abstract;
   function Value
            (  Editor : Abstract_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function Value
            (  Editor : Abstract_Entry_Edit;
               Text   : UTF8_String
            )  return Classification is abstract;
--
-- Set_Default -- Set the default value used for editing and rendering
--
--    Editor  - Editing parameters
--    Default - Fuzzy_Boolean
--
-- This  function sets the fuzzy Boolean value used for input and output
-- of intuitionistic  classifications  and  sets  as  described  in  the
-- package Fuzzy.Abstract_Edit.Intuitionistic.
--
   procedure Set_Default
             (  Editor  : in out Abstract_Entry_Edit;
                Default : Fuzzy_Boolean
             )  is abstract;
--
-- Entry_Edit_Handles -- Handles to Abstract_Entry_Edit
--
   package Entry_Edit_Handles is
      new Object.Handle
          (  Abstract_Entry_Edit'Class,
             Abstract_Entry_Edit_Ptr
          );
   use Entry_Edit_Handles;
--
-- Entry_Domain -- A handle to Abstract_Entry_Edit
--
   subtype Entry_Domain is Entry_Edit_Handles.Handle;
--
-- Entry_Edit -- An  implementation  of  Abstract_Entry_Edit  based   on
--               Fuzzy.Abstract_Edit[.Intuitionistic].
--
   type Entry_Edit is new Abstract_Entry_Edit with private;
--
-- Create -- Factory
--
--    Data    - A handle to Fuzzy.Abstract_Edit.User_Data
--    Default - The default value to use
--
-- Returns :
--
--    A handle to object
--
   function Create
            (  Data    : Fuzzy.Abstract_Edit.Handle.Handle;
               Default : Fuzzy_Boolean := Certain_True
            )  return Entry_Domain;
--
-- Implementations of abstract subprograms
--
   function Get_Cardinality (Editor : Entry_Edit) return Natural;
   function Get_Default (Editor : Entry_Edit) return Fuzzy_Boolean;
   function Get_Domain
            (  Editor : Entry_Edit
            )  return Fuzzy.Abstract_Edit.Handle.Handle;
   function Image
            (  Editor : Entry_Edit;
               Value  : Fuzzy.Set
            )  return UTF8_String;
   function Image
            (  Editor : Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Set
            )  return UTF8_String;
   function Image
            (  Editor : Entry_Edit;
               Value  : Classification
            )  return UTF8_String;
   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Set;
   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Set;
   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Classification;
   procedure Set_Default
             (  Editor  : in out Entry_Edit;
                Default : Fuzzy_Boolean
             );
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkFuzzySetEntry";
--
-- Gtk_Fuzzy_Set_Entry_Record -- The widget type
--
-- Style properties :
--
--    default-truth-value - This parameter controls the rendering of the
--                          intuitionistic fuzzy sets and intuitionistic
--                          classifications  as described in the package
--                          Fuzzy.Abstract_Edit.Intuitionistic. The type
--                          is GType_Fuzzy_Boolean. Deault Certain_True
--    has-header          - Controls if the columns  header  appears  in
--                          Gtk.Fuzzy_Set. GBoolean. Default is False.
--
   type Gtk_Fuzzy_Set_Entry_Record is
      new Gtk_Entry_Record with private;
   type Gtk_Fuzzy_Set_Entry is
      access all Gtk_Fuzzy_Set_Entry_Record'Class;
--
-- Edited -- Returns the update flag
--
--    Widget - The widget
--
-- The update flag is set when the user changes the indicated set. It is
-- reset when the value or the domain are set using  Put  or  Set_Domain
-- calls.
--
-- Returns :
--
--    True if the indicated value has been modified
--
   function Edited
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean;
--
-- Editing_Canceled -- Editing status
--
--    Widget - The unit entry widget
--
-- This  function  is  used when the widget serves as an editable widget
-- for a cell renderer.  When  the  renderer  handles  the  editing-done
-- signal it may check if the user has cancelled editing.
--
-- Retunrs :
--
--    True if the last editing was cancelled
--
   function Editing_Canceled
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean;
--
-- Get -- Indicated value
--
--    Widget - The widget
--
-- These functions return the value indicated by the  widget.  When  the
-- currently  indicated  value  has  a  type  different  from the result
-- Constraint_Error is propagated. The variant returning  a  GTK+  value
-- has the type:
--
-- (o)  GType_Set when the indicated value is a fuzzy set;
-- (o)  GType_Classification when it is a classification;
-- (o)  Intuitionistic.GType_Set when it is an instuitionistic set;
-- (o)  GType_String set to empty otherwise.
--
-- This variant does not propagate Constraint_Error.
--
-- Returns :
--
--    The indicated value
--
-- Exceptions :
--
--    Constraint_Error - No expected value indicated
--
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy.Set;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy.Intuitionistic.Set;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Classification;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return GValue;
--
-- Get_Cardinality -- Get the cardinality of
--
--    Widget - The widget
--
-- Retunrs :
--
--    The cardinality of the domain set
--
   function Get_Cardinality
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Natural;
--
-- Get_Content_Type -- Get the type of the content
--
--    Widget - The widget
--
-- Retunrs :
--
--    The type of the content rendered by the widget
--
   function Get_Content_Type
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Content_Type;
--
-- Get_Default -- The default value used for rendering
--
--    Widget - The widget
--
-- This procedure returns the domain set of the indicated value.
--
-- Returns :
--
--    CThe default value used for rendering
--
   function Get_Default
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy_Boolean;
--
-- Get_Domain -- The domain of the indicated value
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the description of the domain set
--
   function Get_Domain
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Entry_Domain;
--
-- Get_Name -- The domain element name by its index
--
--    Widget - The widget
--    Index  - The element's index
--
-- This procedure returns the domain set  element's  name  indicated  by
-- Index. The index must be in the range 1..Get_Cardinality (Widget).
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Name
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
               Index  : Positive
            )  return String;
--
-- Get_Shape -- Get the widget's shape
--
--    Widget - The widget
--
-- Returns :
--
--    The shape of
--
   function Get_Shape
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Shape;
--
-- Get_Type -- Get the type of the widget
--
-- Note  that  the  type retured refers to the tree view that implements
-- the widget.
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--    Domain - The domain set of the values to indicate
--    Value  - The initial value indicate
--
-- When Value is GValue it can have GType_Set, Intuitionistic.GType_Set,
-- GType_Classification. Undefined values equivalent to an empty sets.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of the value
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : GValue
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The result
--    Domain - The domain set of the values to indicate
--    Value  - To indicate
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of the value
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : GValue
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
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean;
--
-- Put -- Change the value indicated
--
--    Widget - The widget
--    Value  - To indicate
--
-- These procedures change the  indicated  value.  The  value  type  and
-- cardinality   shall   be   conform   to   the    domain.    Otherwise
-- Constraint_Error  is  propagated.  When  Value  is GValue it can have
-- GType_Set,  Intuitionistic.GType_Set, GType_Classification. Undefined
-- values cause Constraint_Error propagation.
--
-- Exceptions :
--
--    Constraint_Error - Wrong type, value or cardinality
--
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Fuzzy.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Classification
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : GValue
             );
--
-- Put -- Change the value indicated
--
--    Widget - The widget
--    Domain - The domain set of the values to indicate
--    Value  - To indicate
--
-- These procedures change the domain and the indicated value. The value
-- type  and  cardinality  shall  be  conform  to  the domain. Otherwise
-- Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality
--
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Classification
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : GValue
             );
--
-- Set_Default -- Set the default value used for rendering
--
--    Widget  - The widget
--    Default - Fuzzy_Boolean
--
-- This procedure sets the default value used in rendering. It overrides
-- the style property default-truth-value.
--
   procedure Set_Default
             (  Widget  : not null access Gtk_Fuzzy_Set_Entry_Record;
                Default : Fuzzy_Boolean
             );
--
-- Set_Editable -- Set the editable flag
--
--    Widget   - The result
--    Editable - The editable flag
--
   procedure Set_Editable
             (  Widget   : not null access Gtk_Fuzzy_Set_Entry_Record;
                Editable : Boolean
             );
--
-- Set_Shape -- Change the widget shape
--
--    Widget - The result
--    Form   - The shape to set
--
-- This  procedure  changes  the  shape  of  the  indicated  values.  It
-- overrides  the  widget's style property, which otherwise controls the
-- shape.
--
   procedure Set_Shape
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Form   : Shape
             );
private
   type Gtk_Fuzzy_Set_Entry_Record is new Gtk_Entry_Record with record
      Domain        : Entry_Domain;
      Popup         : Gtk_Window;
      Box           : Gtk_VBox;
      Edit          : Gtk_Entry;
      Value         : GValue;
      Selection     : Gtk_Fuzzy_Set;
      Canceled      : Boolean := True;
      Being_Edited  : Boolean := False;
      Edited        : Boolean := False;
      Default_Set   : Boolean := False;
      Being_Changed : Boolean := False;
   end record;

   procedure Activated_Entry
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             );

   function Button_Press
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean;

   function Button_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean;

   procedure Changed_Entry
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             );

   procedure Changed_Popup
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             );

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             );

   procedure Done_Popup
             (  Widget : access Gtk_Fuzzy_Set_Entry_Record'Class
             );

   function Key_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean;
--
-- Style_Updated -- The style-updated event's callback
--
   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             );

   package Edit_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Set_Entry
          );

   package Result_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Fuzzy_Set_Entry
          );
--
-- Start_Editing_Entry -- Callback on start editing
--
   type Start_Editing_Entry is access procedure
        (  Cell_Editable : System.Address;
	   Event         : Gdk_Event
        );
   pragma Convention (C, Start_Editing_Entry);

   procedure Start_Editing
             (  Cell_Editable : System.Address;
     	        Event         : Gdk_Event
             );
   pragma Convention (C, Start_Editing);

   Start_Editing_Ptr : constant Start_Editing_Entry :=
                          Start_Editing'Access;

   type Entry_Edit is new Abstract_Entry_Edit with record
      Data    : Fuzzy.Abstract_Edit.Handle.Handle;
      Default : Fuzzy_Boolean := Certain_True;
   end record;

   pragma Inline (Edited);

end Gtk.Fuzzy_Set_Entry;
