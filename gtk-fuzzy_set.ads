--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Set                               Luebeck            --
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
--  The widget's visual appearance might look as follows (when the shape
--  is bar):
--     .________.____________________.__.
--     |        |                    |/\|
--     | Domain | Value              |  |
--     |________|____________________|  |
--     |        |                    |  |
--     | Red    | |||||||| 0.5       |  |
--     |________|____________________|  |
--     |        |                    |  |
--     | Green  | |||||||||||||||1|| |  |
--     |________|____________________|  |
--     |        |                    |  |
--     | Blue   | 0                  |  |
--     |________|____________________|\/|
--     |<___________________________>|__|
--
with Fuzzy;                      use Fuzzy;
with Fuzzy.Abstract_Edit;        use Fuzzy.Abstract_Edit;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Gdk.Event;                  use Gdk.Event;
with GLib.Values;                use GLib.Values;
with Gtk.Fuzzy_Boolean_Drawing;  use Gtk.Fuzzy_Boolean_Drawing;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;

with Gtk.Cell_Renderer_Fuzzy_Boolean;
with Gtk.Handlers;

package Gtk.Fuzzy_Set is
   pragma Elaborate_Body (Gtk.Fuzzy_Set);
--
-- Class_Name - Of the widget
--
-- The widget is a scrolled window widget which decorates a  tree  view.
-- The class name applies to the tree view inside the scroll window.
--
   Class_Name : constant String := "GtkFuzzySet";
--
-- Content_Type -- The type of the content rendered by the widget
--
   type Content_Type is
        (  Plain_Set,
           Intuitionistic_Set,
           Intuitionistic_Classification
        );
--
-- Gtk_Fuzzy_Set_Record -- The widget type
--
-- Style properties :
--
--    domain-column-title - The  title  of  the  first  column.  String.
--                          Default is "Domain".
--    value-column-title  - The title  of  the  second  column.  String.
--                          Default is "Value".
--
--    Additional style properties are  same  as  ones  of  the  renderer
--    Gtk_Cell_Renderer_Fuzzy_Boolean_Record.
--
-- Signals :
--
--    changed - The widget content was changed by the user
--
   type Gtk_Fuzzy_Set_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Set is
      access all Gtk_Fuzzy_Set_Record'Class;
--
-- Edited -- Returns the update flag
--
--    Widget - The widget
--
-- The update flag is set when the user changes the indicated set. It is
-- reset when the value or the domain are set using  Put.
--
-- Returns :
--
--    True if the indicated value has been modified
--
   function Edited
            (  Widget : not null access Gtk_Fuzzy_Set_Record
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
-- (o)  GValue does not propagate Constraint_Error.
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Fuzzy.Set;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Fuzzy.Intuitionistic.Set;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Classification;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Record
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Content_Type;
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record;
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Shape;
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
            )  return Gtk_Tree_View;
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
--    Data   - Controls formatting the values of the domain set
--    Value  - The initial value indicate
--
-- When Value is GValue it can have GType_Set, Intuitionistic.GType_Set,
-- GType_Classification. Undefined values equivalent to an empty set.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality or type  of the value
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Classification
             );
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : GValue
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The result
--    Data   - Controls the formatting the values of the domain set
--    Value  - To indicate
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of the value
--
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             );
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Classification
             );
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Set_Record'Class;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
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
            (  Widget : not null access Gtk_Fuzzy_Set_Record
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
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Fuzzy.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : Classification
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Value  : GValue
             );
--
-- Put -- Change the value indicated
--
--    Widget - The widget
--    Data   - Controls formatting the values of the domain set
--    Value  - To indicate
--
-- These  procedures  change  the  domain  set  values  labeling and the
-- indicated  value  itself. The value type and the cardinality of shall
-- be  conform  to  Data.  When  it is a GTK+ value it shall be defined.
-- Otherwise Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Wrong value
--
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : Classification
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Data   : Fuzzy.Abstract_Edit.User_Data'Class;
                Value  : GValue
             );
--
-- Set_Editable -- Set the editable flag
--
--    Widget   - The result
--    Editable - The editable flag
--
   procedure Set_Editable
             (  Widget   : not null access Gtk_Fuzzy_Set_Record;
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
             (  Widget : not null access Gtk_Fuzzy_Set_Record;
                Form   : Shape
             );

private
   use Gtk.Cell_Renderer_Fuzzy_Boolean;

   type Gtk_Fuzzy_Set_Record is
      new Gtk_Scrolled_Window_Record with
   record
      Tree   : Gtk_Tree_View;
      Store  : Gtk_List_Store;
      Mode   : Content_Type := Plain_Set;
      Values : Gtk_Cell_Renderer_Fuzzy_Boolean;
      Edited : Boolean := False;
   end record;
--
-- Commit -- Handler of commit
--
   procedure Commit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Set
             );
   package Commit_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Cell_Renderer_Fuzzy_Boolean_Record,
             Gtk_Fuzzy_Set
          );
--
-- Key_Press -- Handler of key press
--
   function Key_Press
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Edit   : Gtk_Fuzzy_Set
            )  return Boolean;
--
-- Style_Updated -- Handler of style-updated
--
   procedure Style_Updated
             (  Widget : access Gtk_Fuzzy_Set_Record'Class
             );

   package Widget_Callbacks is
      new Gtk.Handlers.Callback (Gtk_Fuzzy_Set_Record);

   package Widget_Result_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Fuzzy_Set
          );

   pragma Inline (Edited);
end Gtk.Fuzzy_Set;
