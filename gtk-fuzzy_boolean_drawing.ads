--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Boolean_Drawing                   Luebeck            --
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
--  Implementation package for drawing fuzzy logical and truth values.
--
with Cairo;                      use Cairo;
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Gdk.Color;                  use Gdk.Color;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Window;                 use Gdk.Window;
with GLib;                       use GLib;
with GLib.Generic_Properties;    use GLib.Generic_Properties;
with GLib.Object;                use GLib.Object;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Values;                use GLib.Values;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Widget.Styles;          use Gtk.Widget.Styles;
with Pango.Layout;               use Pango.Layout;

with Ada.Finalization;
with Gtk.Widget.Styles.Generic_Enumeration;

package Gtk.Fuzzy_Boolean_Drawing is
   pragma Elaborate_Body (Gtk.Fuzzy_Boolean_Drawing);
   Fuzzy_Domain : constant String := "Fuzzy";
--
-- Shape -- Of the representation
--
-- Possible shapes are bar, bullet or plain text
--
   type Shape is (Bar, Bullet, Text);
   pragma Convention (C, Shape);
   for Shape'Size use GInt'Size;
--
-- Shape_Propertiy -- GTK+ property of Shape
--
   package Shape_Property is
      new Generic_Enumeration_Property ("GtkFuzzyBooleanShape", Shape);
--
-- Shape_Style -- GTK+ style property of Shape
--
   package Shape_Style is new Generic_Enumeration (Shape_Property);
--
-- Look_And_Feel -- Of the representation: relief vs. flat
--
   type Look_And_Feel is (Relief, Flat);
   pragma Convention (C, Look_And_Feel);
   for Look_And_Feel'Size use GInt'Size;
--
-- Look_And_Feel_Property -- GTK+ property of Look_And_Feel
--
   package Look_And_Feel_Property is
      new Generic_Enumeration_Property
          (  "GtkFuzzyBooleanLookAndFeel",
             Look_And_Feel
          );
--
-- Look_And_Feel_Style -- GTK+ style property of Look_And_Feel
--
   package Look_And_Feel_Style is
      new Generic_Enumeration (Look_And_Feel_Property);

   type RGB_Pixel is record
      Red   : GUChar;
      Green : GUChar;
      Blue  : GUChar;
      Alpha : GUChar;
   end record;
   pragma Convention (C, RGB_Pixel);

   function To_RGB_Pixel (Color : Gdk_Color) return RGB_Pixel;
   function To_RGB_Pixel (Color : Gdk_RGBA) return RGB_Pixel;
   pragma Inline (To_RGB_Pixel);

   type RGB_Image is array (GInt range <>, GInt range <>) of RGB_Pixel;
   pragma Convention (C, RGB_Image);
   type RGB_Image_Ptr is access RGB_Image;
--
-- Properties of the drawing state
--
   Intuitionistic_Value_ID : constant Property_ID := 1;
   Truth_Value_ID          : constant Property_ID := 2;
--
-- Fuzzy_Boolean_Drawing
--
   type Fuzzy_Boolean_Data is
      new Ada.Finalization.Controlled with
   record
      Value       : Fuzzy_Boolean := Certain_False;
      Updated     : Boolean       := True;
      Invalid     : Boolean       := True;
      Shape_Set   : Boolean       := False;
      Shape_Fixed : Boolean       := False;
      Confidence  : Boolean       := False;
      Form        : Shape         := Bar;
      Possibility : Pango_Layout;
      Necessity   : Pango_Layout;
      Image       : RGB_Image_Ptr;
   end record;
--
-- Adjust -- Assignment
--
   procedure Adjust (Data : in out Fuzzy_Boolean_Data);
--
-- Draw -- The state
--
--    Data       - The data of the renderer or widget
--    Context    - The drawing context
--    Widget     - The widget
--    Area       - The area to draw into
--    Background - The background color
--    State      - The cell renderer state (optional)
--
   procedure Draw
             (  Data       : in out Fuzzy_Boolean_Data;
                Context    : Cairo_Context;
                Widget     : not null access Gtk_Widget_Record'Class;
                Area       : Gdk_Rectangle;
                Background : Gdk_RGBA;
                State      : Gtk_Cell_Renderer_State := 0
             );
--
-- Finalize -- Destruction
--
   procedure Finalize (Data : in out Fuzzy_Boolean_Data);
--
-- Get -- The value of the state
--
--    Data - The data of the renderer or widget
--
-- Returns :
--
--    The value of
--
   function Get (Data : Fuzzy_Boolean_Data) return GValue;
--
-- Get_Shape -- Of the state
--
--    Data   - The data of the renderer or widget
--    Widget - The widget
--
-- Returns :
--
--    The current shape (maybe defined by the style of Widget)
--
   function Get_Shape
            (  Data   : not null access Fuzzy_Boolean_Data;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Shape;
--
-- Get_Property -- Property request implementation
--
   procedure Get_Property
             (  Data          : in out Fuzzy_Boolean_Data;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Size -- get_size request implementation
--
   function Get_Size
            (  Data    : Fuzzy_Boolean_Data;
               Form    : Shape;
               Area    : Gdk_Rectangle;
               X_Align : GFloat;
               Y_Align : GFloat;
               X_Pad   : GUInt;
               Y_Pad   : GUInt
            )  return Gdk_Rectangle;
   function Get_Size
            (  Data  : Fuzzy_Boolean_Data;
               Form  : Shape;
               X_Pad : GUInt;
               Y_Pad : GUInt
            )  return Gdk_Rectangle;
--
-- Parse -- A string value
--
--    Source - The string value
--    Data   - To set the value into
--    Retype - Allow type change
--
-- Exceptions :
--
--    Constraint_Error - Type error (when not Retype)
--
   procedure Parse
             (  Source : String;
                Data   : in out Fuzzy_Boolean_Data;
                Retype : Boolean
             );
--
-- Put -- Set a value
--
--    Data  - The data of the renderer or widget
--    Value - To set
--
-- The  type  is  changed as appropriate. When GValue does not specify a
-- valid value, Data is set to invalid value.
--
   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : Fuzzy_Boolean
             );
   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : Confidence
             );
   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : GValue
             );
--
-- Put_Undefined_Confidence -- Change the type and set undefined value
--
--    Data  - The data of the renderer or widget
--
   procedure Put_Undefined_Confidence
             (  Data : in out Fuzzy_Boolean_Data
             );
--
-- Put_Undefined_Fuzzy_Boolean -- Change the type and set undefined value
--
--    Data  - The data of the renderer or widget
--
   procedure Put_Undefined_Fuzzy_Boolean
             (  Data : in out Fuzzy_Boolean_Data
             );
--
-- Set_Property -- Property set implementation
--
   procedure Set_Property
             (  Data          : in out Fuzzy_Boolean_Data;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Update -- Preparation to rendering
--
--    Data   - The data of the renderer or widget
--    Widget - The widget
--
-- This procedure is called before each  rendering  or  size  estimation
-- requests.
--
   procedure Update
             (  Data   : in out Fuzzy_Boolean_Data;
                Widget : not null access Gtk_Widget_Record'Class
             );

end Gtk.Fuzzy_Boolean_Drawing;
