--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy_Boolean             Luebeck            --
--  Interface                                      Summer, 2006       --
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
--  This  package  provides  an  editable  cell  renderer  to   indicate
--  confidence  factors  and fuzzy logical values based on GTK+ I/O. The
--  renderer's visual appearance might look as follows (when  the  shape
--  is bar):
--     ._______________________.
--     |                       |
--     |  |||||||| 0.5         |
--     |_______________________|
--
with Cairo;                      use Cairo;
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Gdk.Rectangle;              use Gdk.Rectangle;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Values;                use GLib.Values;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Fuzzy_Boolean_Drawing;  use Gtk.Fuzzy_Boolean_Drawing;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Widget;                 use Gtk.Widget;

with Gtk.Handlers;
with Gdk.Event;
with Gtk.Cell_Renderer.Abstract_Renderer;

package Gtk.Cell_Renderer_Fuzzy_Boolean is
   pragma Elaborate_Body (Gtk.Cell_Renderer_Fuzzy_Boolean);
--
-- Class_Name - Of the renderer
--
   Class_Name : constant String := "GtkCellRendererFuzzyBoolean";

   type Fuzzy_Boolean_Shape is (Bar, Bullet, Text);
   pragma Convention (C, Fuzzy_Boolean_Shape);
--
-- Gtk_Cell_Renderer_Fuzzy_Boolean_Record -- The renderer type
--
-- Properties :
--
--    confidence-value    - The  currently  indicated value. The type of
--                          the property is GType_Confidence.  When  the
--                          indicated  value   is   Fuzzy_Boolean,   its
--                          possibility  component  is  the result. When
--                          the  indicated  value is invalid, the result
--                          is False.
--    fuzzy-boolean-value - The  currently  indicated value. The type of
--                          the  property  GType_Fuzzy_Boolean. When the
--                          indicated value is Confidence, the necessity
--                          component  is  defined  as   0.   When   the
--                          indicated value is invalid,  the  result  is
--                          Certain_False.
--    editable-undefined  - This  property  determines whether undefined
--                          cells are editable. Boolean. False.
--
-- Style properties :
--
--    look              - Look and feel. Enumeration: RELIEF, FLAT
--    error-color       - The  color of impossible truth values. A value
--                        p is painted using this color when p > pos(x),
--                        p <= nec(x).
--    necessity-color   - The color of necessary truth values. A value p
--                        is painted using this color when p <=  pos(x),
--                        p <= nec(x).
--    possibility-color - The color of possible truth values. Each truth
--                        value  is  a position on the bar. A value p is
--                        painted using this color when p <= pos(x), p >
--                        nec(x).
--    shape             - The shape of. Enumeration: BAR, BULLET,  TEXT.
--                        The bar shape has a rectangular annotated with
--                        values  of  the necessity and possibility. The
--                        bullet  shape  is  circular  with  the   color
--                        reflecting the truth value and consistency.
--
--    The  style  properties  are  taken from the widget the renderer is
--    being used with. The  procedure  Install_Style_Properties  can  be
--    used to install these into the widget's class.
--
   type Gtk_Cell_Renderer_Fuzzy_Boolean_Record is
      new Gtk.Cell_Renderer.Abstract_Renderer.
          Gtk_Abstract_Renderer_Record with private;
   type Gtk_Cell_Renderer_Fuzzy_Boolean is
      access all Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
--
-- Get -- The renderer's value
--
--    Cell - The renderer
--
-- This  function  returns  the  currently  set  renderer's  value.  The
-- result's  type  is  either  GType_Confidence  or  GType_Fuzzy_Boolean
-- depending on the value set.
--
-- Returns :
--
--    The currently indicated value
--
   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return GValue;
--
-- Get -- The renderer's value
--
--    Cell - The renderer
--
-- Returns :
--
--    The currently indicated value
--
-- Exceptions :
--
--    Constraint_Error - Undefined or improperly typed value
--
   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Confidence;
   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Fuzzy_Boolean;
--
-- Get_Aligned_Area -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Aligned_Area
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget    : not null access Gtk_Widget_Record'Class;
               Flags     : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
--
-- Get_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Shape -- Get the renderer's shape
--
--    Cell   - The renderer
--    Widget - The renderer belongs to
--
-- Returns :
--
--    The shape of
--
   function Get_Shape
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Shape;
--
-- Get_Size -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Size
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget    : not null access
                           Gtk.Widget.Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
   overriding
   function Get_Size
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget    : not null access
                           Gtk.Widget.Gtk_Widget_Record'Class
            )  return Gdk_Rectangle;
--
-- Get_Type -- Get the type of the renderer
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Cell - The result
--
   procedure Gtk_New (Cell : out Gtk_Cell_Renderer_Fuzzy_Boolean);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Cell     - The renderer to initialize
--    Tooltips - The tooltips group to use
--
   procedure Initialize
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class
             );
--
-- Install_Style_Properties -- Install renderer's style properties
--
--    Class - To install properties into
--
-- This procedure installs the style properties the cell  renderer  uses
-- to determine its appearance.
--
   procedure Install_Style_Properties (Class : GObject_Class);
--
-- Install_Value_Properties -- Install renderer's value properties
--
--    Class - To install properties into
--
-- This procedure installs the value properties the cell  renderer  uses
-- to determine its value.
--
   procedure Install_Value_Properties (Class : GObject_Class);
--
-- Is_Editable_Undefined -- Get the renderer's property
--
--    Cell - The renderer
--
-- Returns :
--
--    The property value
--
   function Is_Editable_Undefined
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Boolean;
--
-- Put -- Change the value indicated
--
--    Widget - The widget
--    Value  - To indicate
--
-- This procedure changes the indicated value. The type of the indicated
-- value is also changed to correspond to the type of Value. When  Value
-- is GValue then the following types  are  expected:  GType_Confidence,
-- GType_Fuzzy_Boolean, GType_String. In the string format fuzzy logical
-- value has the syntax:
--
--    <possibility> : <necessity>
--    <necessity> .. <possibility>
--    <necessity> ... <possibility>
--
-- A confidence value has the syntax:
--
--    <confidence>
--
-- Improperly  typed or syntactically wrong values set the renderer into
-- the state of invalid value. In this state nothing is indicated.
--
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : GValue
             );
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : Fuzzy.Logic.Fuzzy_Boolean
             );
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : Confidence
             );
--
-- Render -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Render
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             );
--
-- Set_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Set_Editable_Undefined -- Change the renderer's property
--
--    Cell     - The renderer
--    Editable - The flag
--
-- This  procedure  allows or disables editing of undefined cells by the
-- renderer.
--
   procedure Set_Editable_Undefined
             (  Cell     : not null access
                           Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Editable : Boolean
             );
--
-- Set_Shape -- Change the renderer shape
--
--    Cell - The renderer
--    Form - The shape to set
--
-- This  procedure  changes  the  shape  of  the  indicated  values.  It
-- overrides  the  widget's style property, which otherwise controls the
-- shape.
--
   procedure Set_Shape
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Form : Shape
             );
--
-- Start_Editing -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Start_Editing
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Event  : Gdk.Event.Gdk_Event;
               Widget : not null access Gtk_Widget_Record'Class;
               Path            : String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget;
private
   type Gtk_Cell_Renderer_Fuzzy_Boolean_Record is
      new Gtk.Cell_Renderer.Abstract_Renderer.
          Gtk_Abstract_Renderer_Record with
   record
      Data           : aliased Fuzzy_Boolean_Data;
      Focus_Out      : Gtk.Handlers.Handler_Id;
      Edit_Undefined : Boolean := False;
   end record;
--
-- Editing_Done -- The handler of editing_done
--
   procedure Editing_Done
             (  Editor : access Gtk_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fuzzy_Boolean
             );
--
-- Focus_Out -- The handler of focus_out_event
--
   function Focus_Out
            (  Editor : access Gtk_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fuzzy_Boolean
            )  return Boolean;
--
-- Entry_Callbacks -- Handlers for editing_done
--
   package Entry_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Widget_Type => Gtk_Entry_Record,
             User_Type   => Gtk_Cell_Renderer_Fuzzy_Boolean
          );
--
-- Entry_Return_Callbacks -- Handlers for focus_out_event
--
   package Entry_Return_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Widget_Type => Gtk_Entry_Record,
             Return_Type => Boolean,
             User_Type   => Gtk_Cell_Renderer_Fuzzy_Boolean
          );

end Gtk.Cell_Renderer_Fuzzy_Boolean;
