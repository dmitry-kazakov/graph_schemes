--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Boolean                           Luebeck            --
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
--  This  package  provides  a widget to indicate confidence factors and
--  fuzzy  logical  values  based  on  GTK+  I/O.  The  widget's  visual
--  appearance might look as follows (when the shape is bar): 
--     ._______________________.
--     |                       |
--     |  |||||||| 0.5         |
--     |_______________________|
--
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Logic;                use Fuzzy.Logic;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Values;                use GLib.Values;
with Gtk.Drawing_Area;           use Gtk.Drawing_Area;
with Gtk.Fuzzy_Boolean_Drawing;  use Gtk.Fuzzy_Boolean_Drawing;
with Gtk.Widget;                 use Gtk.Widget;

with Gtk.Handlers;

package Gtk.Fuzzy_Boolean is
   pragma Elaborate_Body (Gtk.Fuzzy_Boolean);
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkFuzzyBoolean";
--
-- Gtk_Fuzzy_Boolean_Record -- The widget type
--
-- Properties :
--
--    The  properties  of  the  widget  are same as ones of the renderer
--    Gtk_Cell_Renderer_Fuzzy_Boolean_Record.
--
-- Style properties :
--
--    The syle properties of the widget are same as ones of the renderer
--    Gtk_Cell_Renderer_Fuzzy_Boolean_Record. 
--
   type Gtk_Fuzzy_Boolean_Record is
      new Gtk_Drawing_Area_Record with private;
   type Gtk_Fuzzy_Boolean is
      access all Gtk_Fuzzy_Boolean_Record'Class;
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
-- (o)  GType_Confidence when the indicated value is Confidence;
-- (o)  GType_Fuzzy_Boolean when it is a fuzzy logical value;
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
            (  Widget : not null access Gtk_Fuzzy_Boolean_Record
            )  return Confidence;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Boolean_Record
            )  return Fuzzy.Logic.Fuzzy_Boolean;
   function Get
            (  Widget : not null access Gtk_Fuzzy_Boolean_Record
            )  return GValue;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
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
            (  Widget : not null access Gtk_Fuzzy_Boolean_Record
            )  return Shape;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--    Value  - To indicate
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Boolean;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - The result
--    Value    - To indicate
--    Tooltips - The tooltips group to use
--
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record'Class;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             );
--
-- Put -- Change the value indicated
--
--    Widget - The widget
--    Value  - To indicate
--
-- This procedure changes the indicated value. The type of the indicated
-- value is also changed to correspond to the type of Value. When  Value
-- is GValue then the following types  are  expected:  GType_Confidence,
-- GType_Fuzzy_Boolean,  GType_String.  In  a  string  the fuzzy logical
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
-- Improperly  typed  or  syntactically wrong values set Widget into the
-- state of invalid value. In this state nothing is indicated. 
--
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : Confidence
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : GValue
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
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Form   : Shape
             );
private
   type Gtk_Fuzzy_Boolean_Record is
      new Gtk_Drawing_Area_Record with
   record
      Data : aliased Fuzzy_Boolean_Data;
   end record;
--
-- Get_Property -- Get property handler
--
   procedure Get_Property
             (  Widget        : access GObject_Record'Class;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Set_Property -- Get property handler
--
   procedure Set_Property
             (  Widget        : access GObject_Record'Class;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Size_Allocate -- size_allocate handler
--
   procedure Size_Allocate
             (  Widget     : access Gtk_Fuzzy_Boolean_Record'Class;
                Allocation : Gtk_Allocation_Access
             );
--
-- Allocation_Callback -- Handlers instantiation
--
   package Allocation_Callback is
      new Handlers.Callback (Gtk_Fuzzy_Boolean_Record);

   package Allocation_Marshaller is
      new Allocation_Callback.Marshallers.Generic_Marshaller
          (  Gtk_Allocation_Access,
             Get_Allocation
          );
--
-- Allocation_Callback -- Handlers for size_allocation
--
   package Return_Boolean_Callback is
      new Handlers.Return_Callback
          (  Gtk_Fuzzy_Boolean_Record,
             Boolean
          );
--
-- Size_Callback -- Handlers for size_request
--
   package Size_Callback is
      new Handlers.Callback (Gtk_Fuzzy_Boolean_Record);

end Gtk.Fuzzy_Boolean;
