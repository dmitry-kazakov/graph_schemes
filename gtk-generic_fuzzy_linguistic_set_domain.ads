--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain     Luebeck            --
--  Interface                                      Winter, 2007       --
--                                                                    --
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
--  The package provides a widget for indicating the domain of a set  of
--  linguistic variables. The widget looks as:
--
--    ._____________________.
--    |   |<--[]-------->|__|
--    |   | /\           |/\|
--    |   |/  \___       |[]|
--    | 0 |_______\______|\/|
--    |     5  10  15  20   |
--    |_____________________|
--
-- The  widget  is  a  drawing  area that contains the lingustic set. It
-- optionally  can  have an annotation of the vertical (truth value) and
-- the  horizontal  axes  (the  domain  values).  The widget has no user
-- interface of its own. So it has to be controlled from outside.
--
-- The package is generic. It has the following parameters
--
-- (o)  Class_Name  is  the  prefix for the names of the GTK+ classed of
--      the widgets;
-- (o)  Fuzzy_Linguistics is an instance of Fuzzy.Linguistics providing
--      the linguistic varibles of the sets;
-- (o)  Fuzzy_Linguistic_Sets  is  a  compatible instance of the package
--      Fuzzy.Linguistics.Sets, that provides the sets of variables;
-- (o)  Domain_Edit is an instance of Strings_Edit.Float_Edit to be used
--      for I/O of the domain values.
--
-- Note that the package has to be instantiated at the library level.
--
with Cairo;               use Cairo;
with Confidence_Factors;  use Confidence_Factors;
with Fuzzy.Logic;         use Fuzzy.Logic;
with GLib;                use GLib;
with Gdk.Color.IHLS;      use Gdk.Color.IHLS;
with Gdk.Color;           use Gdk.Color;
with Gdk.Event;           use Gdk.Event;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Gdk.Pixbuf.Image;    use Gdk.Pixbuf.Image;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.RGBA;            use Gdk.RGBA;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Drawing_Area;    use Gtk.Drawing_Area;
with Gtk.Event_Box;       use Gtk.Event_Box;
with Gtk.Fixed;           use Gtk.Fixed;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Missed;          use Gtk.Missed;
with Gtk.Scrollbar;       use Gtk.Scrollbar;
with Gtk.Widget;          use Gtk.Widget;
with Pango.Layout;        use Pango.Layout;

with Ada.Unchecked_Deallocation;
with Fuzzy.Linguistics.Sets;
with Generic_Set;
with Generic_Map;
with Generic_Unbounded_Array;
with GLib.Object.Strong_References;
with Object.Handle;
with Strings_Edit.Generic_Scale;
with Strings_Edit.Float_Edit;

generic
   Class_Name : String;
   with package Fuzzy_Linguistics     is new Fuzzy.Linguistics (<>);
   with package Fuzzy_Linguistic_Sets is new Fuzzy_Linguistics.Sets;
   use Fuzzy_Linguistics.Fuzzy_Floats.Float_Intervals;
   with package Float_Edit is new Strings_Edit.Float_Edit (Number);
package Gtk.Generic_Fuzzy_Linguistic_Set_Domain is
   package Fuzzy_Linguistics_Of     renames Fuzzy_Linguistics;
   package Fuzzy_Linguistic_Sets_Of renames Fuzzy_Linguistic_Sets;
   package Float_Edit_Of            renames Float_Edit;

   use Fuzzy_Linguistics_Of;
   use Fuzzy_Linguistic_Sets_Of;
   use Fuzzy_Floats_Of;
   use Float_Intervals_Of;
--
-- Zoom_X_Factor -- The range of zoom factors
-- Zoom_Y_Factor
--
-- The factor Zoom_X_Range'First corresponds  to  no  zoom.  All  domain
-- points  of  the  set  of variables are visible. Similarly, The factor
-- Zoom_Y_Range'First corresponds to no zoom along the vertical axis.
--
   type Zoom_X_Factor is new Float;
   subtype Zoom_X_Range is Zoom_X_Factor range 1.0..100_000.0;
   type Zoom_Y_Factor is new Float;
   subtype Zoom_Y_Range is Zoom_Y_Factor range 1.0..1_000.0;
--
-- Points_Indices -- The set of selected points
--
   type Point_Index is new Natural;
   package Points_Indices is new Generic_Set (Point_Index, 0);
   use Points_Indices;
--
-- Selections -- Selection of points and variables, see Selection
--
   type Variable_Index is new Positive;
   package Selections is
      new Generic_Map
          (  Key_Type    => Variable_Index,
             Object_Type => Points_Indices.Set
          );
   use Selections;
--
-- Selection -- The variables and points selected
--
-- A  selection  is  a  map  of  variables indices to the sets of points
-- indices. If a variable  is  present  in  the  map  it  is  considered
-- selected. All points of the variable present is the corresponding set
-- of point are selected as well.
--
   subtype Selection is Selections.Map;
--
-- Selection_Mode -- Specific types of selections
--
   type Selection_Mode is
        (  Empty,
           Single_Point,
           Points_Range,
           All_Points,
           Single_Variable,
           Variables_Range,
           Complete_Variables,
           Complex
        );
--
-- Selection_Subtype -- Specific values of a selection
--
   type Selection_Subtype (Mode : Selection_Mode) is record
      case Mode is
         when Single_Variable =>
            Variable : Variable_Index;
         when Single_Point =>
            Point_At : Variable_Index;
            Point_No : Point_Index;
         when Variables_Range =>
            From_Variable : Variable_Index;
            To_Variable   : Variable_Index;
         when Points_Range | All_Points =>
            Range_At   : Variable_Index;
            From_Point : Point_Index;
            To_Point   : Point_Index;
         when Complete_Variables | Complex =>
            Selected : Selection;
         when Empty =>
            null;
      end case;
   end record;
--
-- Gtk_Fuzzy_Linguistic_Set_Domain_Record -- Linguistic set domain
--
-- Style properties :
--
--    actions-merge-timeout - The timeout in seconds  within  which  two
--                            consequent  zoom  or  scroll  actions  are
--                            merged into one.  An undo  would roll back
--                            both  of them. The timeout is specified in
--                            seconds. GDouble. Default is 0.3.
--    background-color      - The  color  of the background. The default
--                            is white.
--    first-color           - The  color  of  the  first  variable.  The
--                            default is reddish.
--    line-color            - The color used  to draw  annotation lines.
--                            Default is black
--    major-tick-length     - The  length  of  a  major  tick in pixels.
--                            GInt. Default is 10.
--    minor-tick-length     - The  length  of  a  minor  tick in pixels.
--                            GInt. Default is 5.
--    tick-gap              - The gap  between  a  major  tick  and  its
--                            annotation under  or  left  of  it.  GInt.
--                            Default is 4.
--    selection-color       - The color of selection circle. Default  is
--                            red.
--    selection-radius      - The  radius  of  point  selection  circle.
--                            GInt. Default is 20.
--    x-move-tip            - The tip of X-move  scale  slider.  String.
--                            Default "Move along X-axis".
--    x-tick-step           - The minimal  step  of  horizontal ticks in
--                            pixels. GInt. Default is 50.
--    x-zoom-tip            - The tip of X-zoom  scale  slider.  String.
--                            Default "Zoom X-axis".
--    y-move-tip            - The tip of Y-move  scale  slider.  String.
--                            Default "Move along Y-axis".
--    y-tick-step           - The minimal  step  of  vertical  ticks  in
--                            pixels. GInt. Default is 50.
--    y-zoom-tip            - The tip of Y-zoom  scale  slider.  String.
--                            Default "Zoom Y-axis".
--
-- Signals :
--
--    Zoomed - The   visible   part   of   the  widget  has  changed its
--             magnification and/or has been scrolled.
--
   type Gtk_Fuzzy_Linguistic_Set_Domain_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Domain is
      access all Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
--
-- Deselect_All -- Selection removing
--
--    Widget - The widget
--
-- This procedure removes any selections of the variables and points.
--
   procedure Deselect_All
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             );
--
-- Get -- Variable by its index
--
--    Widget - The widget
--    Index  - Of the variable oo get
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record;
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
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Natural;
--
-- Get_Domain_Note -- The current domain note text
--
--    Widget - The widget
--
-- Returns :
--
--    The domain note text
--
   function Get_Domain_Note
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return UTF8_String;
--
-- Get_Selection -- The current selection
--
--    Widget - The widget
--
-- The result of this function is either a raw selection or a  selection
-- classified to one of its narrower classes.
--
-- Returns :
--
--    The current selection
--
   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Selection;
   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Selection_Subtype;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Get_X_Adjustment -- The adjustment of X-axis scroll bar
--
--    Widget - The widget
--
-- Returns :
--
--    The adjustment or null
--
   function Get_X_Adjustment
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Gtk_Adjustment;
--
-- Get_X_Zoom -- X-axis zoom factor
--
--    Widget - The widget
--
-- Returns :
--
--    The current zoom factor
--
   function Get_X_Zoom
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Zoom_X_Range;
--
-- Get_Y_Adjustment -- The adjustment of Y-axis scroll bar
--
--    Widget - The widget
--
-- Returns :
--
--    The adjustment or null
--
   function Get_Y_Adjustment
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Gtk_Adjustment;
--
-- Get_Y_Zoom -- Y-axis zoom factor
--
--    Widget - The widget
--
-- Returns :
--
--    The current zoom factor
--
   function Get_Y_Zoom
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Zoom_Y_Range;
--
-- Gtk_New -- Factory
--
--    Widget      - The result
--    Value       - To indicate
--    Annotated   - Show the X- and Y-axis annotation if true
--    Domain_Note - The domain axis text
--    X_Scroll    - Has horizontal scroll
--    Y_Scroll    - Has vertical scroll
--
-- The  parameter  Annotated  controls  appearance  of the X- and Y-axis
-- annotation. When annotated, Domain_Note is the text shown next to the
-- annotation  of  the  last  major  tick  at the X-axis. The parameters
-- X_Scroll and Y_Scroll control appearance of the scroll  bars  of  the
-- corresponding axes.
--
   procedure Gtk_New
             (  Widget      : out Gtk_Fuzzy_Linguistic_Set_Domain;
                Value       : Linguistic_Set;
                Annotated   : Boolean     := True;
                Domain_Note : UTF8_String := "";
                X_Scroll    : Boolean     := True;
                Y_Scroll    : Boolean     := True
             );
--
-- Hide_Accumulated -- Set
--
--    Widget - The widget
--
-- This  procedures hides accumulated Set, if present. Otherwise it does
-- nothing. This procedure is equivalent to  setting  an  empty  set  to
-- accumulate. See Set_Accumulated.
--
   procedure Hide_Accumulated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget      - The result
--    Value       - To indicate
--    Annotated   - Show the X- and Y-axis annotation if true
--    Domain_Note - The domain axis text
--    X_Scroll    - Has horizontal scroll
--    Y_Scroll    - Has vertical scroll
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
                Value       : Linguistic_Set;
                Annotated   : Boolean;
                Domain_Note : UTF8_String;
                X_Scroll    : Boolean;
                Y_Scroll    : Boolean
             );
--
-- Is_Annotated -- Get the axes annotation flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if the axes are annotated
--
   function Is_Annotated
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Boolean;
--
-- Marked -- Emit zoomed signal
--
--    Widget - The widget
--
-- This procedure emits "marked" signal. The signal is  used  to  inform
-- about setting / removing a rectangular selection area of the widget.
--
   procedure Marked
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             );
--
-- Put -- Replace the widget content
--
--    Widget - The widget
--    Value  - The set of linguistic variables
--
-- This  procedure  replaces  the widget content with the variables from
-- Value.
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Value  : Linguistic_Set
             );
--
-- Refresh -- Refresh
--
--    Widget - The widget
--
-- This procedure queues necessary redraw requests  if  there  were  any
-- changes made.
--
   procedure Refresh
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             );
--
-- Scroll -- To the value
--
--    Widget - The widget
--    Value  - The range of values to make visible
--    Level  - The range of confidence to make visible
--
-- This procedure changes zooming  if  necessary.
--
-- Signals :
--
--    "zoomed" signal if the appearance of the widget has been changed
--
   procedure Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Value  : Interval;
                Level  : Fuzzy_Boolean
             );
--
-- Select_Point -- Selection setting
--
--    Widget   - The widget
--    Index    - Of the variable to select 1..
--    Point    - The point of the variable 1..
--    Selected - Selection flag
--
-- This  procedure  adds  or removes selection of a point in the widget.
-- Adding a point selection automatically selects  the  variable  of.  A
-- selected variable is highlighted with a more intensive  color.  Index
-- is  the  number  of the variable to select. Point is the index of the
-- point.  Selected  indicates  whether the selection should be added or
-- removed.
--
-- Exceptions :
--
--    Constraint_Error - Invalid Index or Point
--
   procedure Select_Point
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Index    : Positive;
                Point    : Positive;
                Selected : Boolean
             );
--
-- Select_Variable -- Selection setting
--
--    Widget   - The widget
--    Index    - Of the variable to select 1..
--    Selected - Selection flag
--
-- This procedure adds or removes a  selection  of  a  variable  in  the
-- widget.  A  selected  variable  is  highlighted with a more intensive
-- color. Index  is  the  number  of  the  variable  to  select.Selected
-- indicates whether the selection should be added or removed.
--
   procedure Select_Variable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Index    : Positive;
                Selected : Boolean
             );
--
-- Set_Annotated -- Set the axes annotation
--
--    Widget    - The widget
--    Annotated - The annotation flag
--
-- This procedure enables or disables annotation of the axes.
--
   procedure Set_Annotated
             (  Widget    : not null access
                            Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Annotated : Boolean
             );
--
-- Set_Domain_Note -- The current domain note text
--
--    Widget      - The widget
--    Domain_Note - The text to set
--
-- This procedure changes the domain note text.
--
   procedure Set_Domain_Note
             (  Widget      : not null access
                              Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Domain_Note : UTF8_String
             );
--
-- Set_Selection -- Selection setting
--
--    Widget   - The widget
--    Selected - Selection to set
--
-- This  procedure  sets new selection. The parameter Selected detemines
-- the selected variables and points. All other variables and points are
-- deselected.
--
   procedure Set_Selection
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Selected : Selection
             );
--
-- Set_X_Scroll -- Enable / Disable horizontal scroll bar
--
--    Widget - The widget
--    Scroll - Enable flag
--
   procedure Set_X_Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Scroll : Boolean
             );
--
-- Set_Y_Scroll -- Enable / Disable vertical scroll bar
--
--    Widget   - The widget
--    Scroller - Enable flag
--
   procedure Set_Y_Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Scroll : Boolean
             );
--
-- Show_Accumulated -- Set
--
--    Widget - The widget
--    Set    - The set to accumulate
--
-- This procedures  causes  Widget  to  indicate  accumulated  Set.  The
-- cardinality of Set has  to  be  equal  to  the  number  of  variables
-- indicated by Widget.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of Set
--
   procedure Show_Accumulated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Set    : Fuzzy.Set
             );
--
-- Zoom -- Zooming the widget in and out
--
--    Widget   - The widget
--    X_Factor - The X-axis zooming factor
--    Y_Factor - The Y-axis zooming factor
--
-- This procedure zooms the widget. The zooming factors are specified as
-- absolute values. The value Zoom_*_Factor'First corresponds to minimal
-- possible resolution. The value Zoom_*_Factor'Last correspond  to  the
-- maximal   resolution.   When  a  factor  parameter  is  omitted,  the
-- resolution of the corresponding axis is not changed.
--
-- Signals :
--
--    "zoomed" signal if the appearance of the widget has been changed
--
   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                X_Factor : Zoom_X_Range;
                Y_Factor : Zoom_Y_Range
             );
   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                X_Factor : Zoom_X_Range
             );
   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Y_Factor : Zoom_Y_Range
             );
--
-- Zoomed -- Emit zoomed signal
--
--    Widget    - The widget
--    Immediate - Check if the appearance has been changed
--
-- When Immediate is true the signal is emitted only if  the  appearance
-- has been changed but not yet updated on the screen. In that  case  it
-- also queues necessary redraw action (see Refresh).
--
   procedure Zoomed
             (  Widget    : not null access
                            Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Immediate : Boolean := False
             );

private
   type Truth is new Float;
--
-- Scale -- Confidence to Truth conversion
--
--    Value - The value to convert
--
-- Returns :
--
--    A value in 0.0..1.0 range
--
   function Scale (Value : Confidence) return Truth;
   pragma Inline (Scale);
--
-- To_Confidence -- Truth to confidence conversion
--
--    Value - The value to convert
--
-- Returns :
--
--    A value in Confidence'Range
--
   function To_Confidence (Value : Truth) return Confidence;
   pragma Inline (To_Confidence);

   package X_Axis_Ticks is
      new Strings_Edit.Generic_Scale (Number'Base);
   package Y_Axis_Ticks is
      new Strings_Edit.Generic_Scale (Truth);
--
-- Zoom_Undo_Item
--
   type Zoom_Undo_Item is record
      X_Gain   : Number'Base;
      X_Offset : Number'Base;
      Y_Gain   : Truth;
      Y_Offset : Truth;
   end record;

   type Color_Point is record
      Y     : Y_Axis;
      Color : Gdk_Color;
   end record;
   function "<" (Left, Right : Color_Point) return Boolean;
--
-- Selection_Color -- Get selection color
--
--    Color - Of a variable
--
-- Returns :
--
--    The corresponding selection color
--
   function Selection_Color (Color : Gdk_Color) return Gdk_Color;

   type Color_Position is new Integer;
   package Color_Position_Sets is
      new Generic_Set
          (  Object_Type  => Color_Position,
             Null_Element => -1
          );
   use Color_Position_Sets;

   package Color_Maps is
      new Generic_Set
          (  Object_Type  => Color_Point,
             Null_Element => (0, Null_Color)
          );
   use Color_Maps;

   type Color_Item is record
      Color    : Gdk_Color;
      Position : Color_Position;
   end record;
   type Color_Item_Array is array (Natural range <>) of Color_Item;

   package Color_Item_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Natural,
             Object_Type       => Color_Item,
             Object_Array_Type => Color_Item_Array,
             Null_Element      => (Null_Color, -1)
          );
   use Color_Item_Arrays;

   function "not" (Pixel : RGB_Pixel) return RGB_Pixel;
   pragma Inline ("not");
--
-- Drawing_Area -- Client-side image with the variables drawn on it
--
   type Drawing_Area is record
      X_Gain   : Number'Base := 1.0;
      X_Offset : Number'Base := 0.0;
      Y_Gain   : Truth       := 1.0;
      Y_Offset : Truth       := 0.0;
      Order    : Color_Maps.Set;
      Pixels   : RGB_Image;
   end record;
--
-- Draw_Area -- Render the area
--
--    Area        - The drawing area
--    From_X      - The first position to draw
--    To_X        - The last position to draw
--    Cardinality - The number of variables
--    Set         - The variables array
--    Selected    - The list of selected variables
--    Accumulated - A selected variable index (additionally to Selected)
--    Palette     - The colors of variables
--    Background  - The background color
--    Opague      - Background erasing flag
--
   procedure Draw_Area
             (  Area        : in out Drawing_Area;
                From_X      : X_Axis;
                To_X        : X_Axis;
                Cardinality : Natural;
                Set         : Unbounded_Arrays.Unbounded_Array;
                Selected    : Selection;
                Accumulated : Natural;
                Palette     : Color_Item_Arrays.Unbounded_Array;
                Background  : Gdk_Color;
                Opaque      : Boolean
             );
   procedure Draw_Variable
             (  Area   : in out Drawing_Area;
                From_X : X_Axis;
                To_X   : X_Axis;
                Var    : Variable;
                Color  : Gdk_Color
             );
--
-- From_X -- Co-ordinates conversion
--
--    Area - The drawing area
--    X    - Horizontal position
--
-- Visible horizontal positions of Area are 1..Area.X_Size.
--
-- Returns :
--
--    Domain value of (center of the interval)
--
   function From_X (Area : Drawing_Area; X : X_Axis)
      return Number'Base;
--
-- From_Y -- Co-ordinates conversion
--
--    Area - The drawing area
--    Y    - Vertical position
--
-- Visible vertical positions of Area are 1..Area.Y_Size bottom-up.
--
-- Returns :
--
--    Confidence (center of the interval)
--
   function From_Y (Area : Drawing_Area; Y : Y_Axis)
      return Truth;
--
-- Get -- The current state of the drawing area
--
--    Area - The drawing area
--
-- Returns :
--
--    Zoom_Undo_Item
--
   function Get (Area : Drawing_Area) return Zoom_Undo_Item;
--
-- Get_Height -- Y-axis size
--
--    Area - The drawing area
--
-- Returns :
--
--    Y-axis size
--
   function Get_Height (Area : Drawing_Area) return Y_Axis;
--
-- Get_Width -- X-axis size
--
--    Area - The drawing area
--
-- Returns :
--
--    X-axis size
--
   function Get_Width (Area : Drawing_Area) return X_Axis;
--
-- Get_X_Size -- X-axis size
--
--    Area - The drawing area
--
-- Returns :
--
--    X-axis size
--
   function Get_X_Size (Area : Drawing_Area) return Number'Base;
--
-- Get_Y_Size -- Y-axis size
--
--    Area - The drawing area
--
-- Returns :
--
--    Y-axis size
--
   function Get_Y_Size (Area : Drawing_Area) return Truth;
--
-- Middle - X-axis value of the center point
--
--    Area - The drawing area
--
-- Returns :
--
--    The domain value of the central point
--
   function Middle_X (Area : Drawing_Area) return Number'Base;
--
-- Middle - Y-axis value of the center point
--
--    Area - The drawing area
--
-- Returns :
--
--    Confidence of the central point
--
   function Middle_Y (Area : Drawing_Area) return Truth;

   type Clip_Type is (None, Above, Below);

   function To_X
            (  Area : Drawing_Area;
               X    : Number'Base
            )  return X_Axis;
   procedure To_X
             (  Area    : Drawing_Area;
                X       : Number'Base;
                Result  : out X_Axis;
                Cut_Off : out Clip_Type
             );
   function To_Y
            (  Area : Drawing_Area;
               Y    : Confidence
            )  return Y_Axis;
   function To_Y
            (  Area : Drawing_Area;
               Y : Truth
            )  return Y_Axis;
   procedure To_Y
             (  Area    : Drawing_Area;
                Y       : Truth;
                Result  : out Y_Axis;
                Cut_Off : out Clip_Type
             );
   function To_Number
            (  Area : Drawing_Area;
               X    : X_Axis
            )  return Interval;
   function To_Necessity
            (  Area : Drawing_Area;
               Y    : Y_Axis
            )  return Truth;
   function To_Possibility
            (  Area : Drawing_Area;
               Y    : Y_Axis
            )  return Truth;

   type Square_Selection_State is (None, Inactive, Active);
   type Square_Selection is record
      State  : Square_Selection_State := None;
      X1, X2 : X_Axis;
      Y1, Y2 : Y_Axis;
   end record;
--
-- Text_Body -- Varying texts
--
   type Text_Body (Length : Natural) is
      new Object.Entity with
   record
      Text : UTF8_String (1..Length);
   end record;
   type Text_Ptr is access Text_Body'Class;
   package Text_Handles is new Object.Handle (Text_Body, Text_Ptr);
   use Text_Handles;

   type Extension_Interface is limited interface;
   procedure Finalized
             (  Extension : in out Extension_Interface
             )  is null;
   type Extension_Data_Ptr is access all Extension_Interface'Class;
--
-- Create -- A variable string
--
--    Text - The string
--
-- Returns :
--
--    A handle to the string containing Text
--
   function Create (Text : UTF8_String) return Text_Handles.Handle;
--
-- Linguistic_Set_Data -- The widget's data
--
   type Linguistic_Set_Data is new Object.Entity with record
      Cardinality : Natural  := 0;
      Accumulated : Natural  := 0; -- No accumulated sets
      X_Range     : Interval := (1.0, 2.0);
      Updated     : Boolean  := True;  -- Change the background
      Repaint     : Boolean  := True;  -- Change the palette
      Annotated   : Boolean  := True;  -- Has grid
      Zoomed_X    : Boolean  := True;  -- X-axis has been zoomed
      Zoomed_Y    : Boolean  := True;  -- Y-axis has been zoomed
      New_Small_Y : Boolean  := False; -- Y-axis small has been zoomed
      Scrolled_X  : Boolean  := True;  -- X-axis has been scrolled
      Scrolled_Y  : Boolean  := True;  -- Y-axis has been scrolled
      Set         : Unbounded_Arrays.Unbounded_Array;
      -- Colors used for the variables
      Palette     : Color_Item_Arrays.Unbounded_Array;
      First_Color : Gdk_IHLS_Color;
      Background  : Gdk_Color := RGB (0.5, 0.5, 0.5);
      Line_Color  : Gdk_Color := RGB (0.0, 0.0, 0.0);
      Free_Colors : Color_Position_Sets.Set;
      Last_Used   : Color_Position := -1;
      -- Drawing area and a selection in it
      Area           : Drawing_Area;
      Area_Selection : Square_Selection;
      Text           : Pango_Layout;
      Note_Text      : Text_Handles.Handle;
      -- Offsets of the area within the allocation box
      Left_Offs      : X_Axis := 0;
      Top_Offs       : Y_Axis := 0;
      Bottom_Offs    : Y_Axis := 0;
      -- Selection
      Selected       : Selection;
      -- The measurements of the ticks
      Major_Tick_Length : GInt    := 10;  -- Pixels
      Minor_Tick_Length : GInt    :=  5;  -- Pixels
      Tick_Gap          : GInt    :=  4;  -- Pixels
      X_Tick_Step       : GInt    := 50;  -- Pixels per horizontal tick
      Y_Tick_Step       : GInt    := 50;  -- Pixels per vertical tick
      X_Tracker_Small   : Integer :=  0;  -- Resolution for X axis
      X_Power           : Integer :=  0;  -- Power of the X axis scale
      X_Ticks           : X_Axis_Ticks.Scale;
      Y_Ticks           : Y_Axis_Ticks.Scale;
      -- The measurements of the selections
      Selection_Length  : GInt      := 20;
      Selection_Color   : Gdk_Color := RGB (1.0, 0.0, 0.0);
      -- Repetive actions coalescing
      Merge_Threshold   : Duration  := 0.3;
      -- Backward references
      Zooming : Extension_Data_Ptr;
   end record;
   type Linguistic_Set_Data_Ptr is access Linguistic_Set_Data'Class;

   overriding
      procedure Finalize (Data : in out Linguistic_Set_Data);
--
-- Create_Palette -- Generate the color palette
--
--    Data - The widget data
--
   procedure Create_Palette
             (  Data : in out Linguistic_Set_Data
             );
--
-- Draw_Annotation -- Draws annotation axes
--
--    Widget - The widget
--    Context - Drawing context
--
   procedure Draw_Annotation
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             );
--
-- Draw_Area_Selection -- Draw selection rectangle
--
--    Data     - The widget data
--    Context  - Drawing context
--    Inversed - If true use inversed colors of the area
--    Y_Offs   - The vertical offset
--
   procedure Draw_Area_Selection
             (  Data     : in out Linguistic_Set_Data;
                Context  : Cairo_Context;
                Inversed : Boolean;
                Y_Offs   : Y_Axis := 0
             );
--
-- Draw_X_Annotation -- Draws X annotation
--
--    Data    - The widget data
--    Context - Drawing context
--    ...
--
   procedure Draw_X_Annotation
             (  Data    : Linguistic_Set_Data;
                Context : Cairo_Context;
                X_Offs  : X_Axis;
                Y_Offs  : Y_Axis;
                From    : X_Axis;
                To      : X_Axis;
                Major   : Interval;
                Color   : Gdk_RGBA
             );
--
-- Draw_Domain -- Draws the domain area
--
--    Data    - The widget data
--    Context - Drawing context
--    Height  - Widget height
--
   procedure Draw_Domain
             (  Data    : in out Linguistic_Set_Data;
                Context : Cairo_Context;
                Height  : Y_Axis
             );
--
-- Draw_Line -- Draws a horizontal line
--
--    Context - Drawing context
--    X1, X2  - Start and end points
--    Y       - Vertical co-ordinate
--    Color   - Color
--
-- This procedure draws a line.
--
   procedure Draw_Line
             (  Context : Cairo_Context;
                X1, X2  : X_Axis;
                Y       : Y_Axis;
                Color   : Gdk_Color
             );
--
-- Draw_Line -- Draws a vertical line
--
--    Context - Drawing context
--    X       - Horizontcal co-ordinate
--    Y1, Y2  - Start and end points
--    Color   - Color
--
-- This procedure draws a line.
--
   procedure Draw_Line
             (  Context : Cairo_Context;
                X       : X_Axis;
                Y1, Y2  : Y_Axis;
                Color   : Gdk_Color
             );
--
-- Draw_Rectangle -- Draws a rectangle
--
--    Context - Drawing context
--    X1, X2  - Horizontal bounds
--    Y1, Y2  - Vertical bounds
--    Color   - The color
--
   procedure Draw_Rectangle
             (  Context : Cairo_Context;
                X1, X2  : X_Axis;
                Y1, Y2  : Y_Axis;
                Color   : Gdk_Color
             );
--
-- Draw_Rectangle_Area -- Draws an inversed rectangle
--
--    Data     - The widget data
--    Context  - Drawing context
--    X1, X2   - Horizontal bounds
--    Y1, Y2   - Vertical bounds
--    Inversed - Inverse if True
--
-- This  procedure  draws  the  rectangle with the content of the pixels
-- rendered by the domain representation or their inverses.
--
   procedure Draw_Rectangle_Area
             (  Data     : in out Linguistic_Set_Data;
                Context  : Cairo_Context;
                X1, X2   : X_Axis;
                Y1, Y2   : Y_Axis;
                Inversed : Boolean
             );
--
-- Draw_Selected -- Draws the selection points
--
--    Data    - The widget data
--    Context - Drawing context
--    Y_Offs  - Vertical offset
--
   procedure Draw_Selected
             (  Data    : in out Linguistic_Set_Data;
                Context : Cairo_Context;
                Y_Offs  : Y_Axis := 0
             );
--
-- Draw_Selected -- Draws the selection points
--
--    Area     - The widget data
--    Context  - Drawing context
--    X        - Horizontal position
--    Min, Max - Vertical positions
--    Radius   - Of the circle
--    Y_Offs   - Vertical offset
--
   procedure Draw_Selected
             (  Area    : Drawing_Area;
                Context : Cairo_Context;
                X       : X_Axis;
                Min     : Confidence;
                Max     : Confidence;
                Radius  : GInt;
                Y_Offs  : Y_Axis
             );
--
-- Draw_Selected -- Draws the selection points
--
--    Area     - The widget data
--    Context  - Drawing context
--    X        - Horizontal position
--    Y        - List of vertical positions
--    Radius   - Of the circle
--    Distance - Between two points
--
   type Ordinates is array (1..4) of Confidence;
   procedure Draw_Selected
             (  Area     : Drawing_Area;
                Context  : Cairo_Context;
                X        : X_Axis;
                Y        : Ordinates;
                Radius   : GInt;
                Distance : Confidence;
                Y_Offs   : Y_Axis
             );
--
-- Draw_X_Ticks -- Draws the selection points
--
--    Data    - The widget data
--    Context - Drawing context
--    Top     - Vertical offset
--    Major   - Interval
--
   procedure Draw_X_Ticks
             (  Data    : Linguistic_Set_Data;
                Context : Cairo_Context;
                Major   : out Interval
             );
--
-- Free_Color -- Free color of a variable
--
--    Data  - The widget data
--    Color - The position of
--
   procedure Free_Color
             (  Data  : in out Linguistic_Set_Data;
                Color : Color_Position
             );
--
-- Get_Color -- Get a color for a new variable
--
--    Data  - The widget data
--    Color - The position of (result)
--
   procedure Get_Color
             (  Data  : in out Linguistic_Set_Data;
                Color : out Color_Position
             );
--
-- Get_Domain_Note -- Get domain note for a value
--
--    Data  - The widget data
--    Power - The power to use
--
-- This  function  returns  the text of the domain values note. The text
-- consists of a decimal multiplier according to the  scale  factor  and
-- the  domain note text. The scale factor is specified by the parameter
-- Power.
--
-- Returns :
--
--    The annotation text for a domain value
--
   function Get_Domain_Note
            (  Data  : Linguistic_Set_Data;
               Power : Integer
            )  return String;
--
-- Get_Selection -- Get specific selections
--
--    Data     - The widget data
--    Selected - A selection
--
-- Returns :
--
--    The specific selection value
--
   function Get_Selection
            (  Data     : Linguistic_Set_Data;
               Selected : Selection
            )  return Selection_Subtype;
--
-- Get_X_Zoom -- X-axis zoom factor
--
--    Data - The widget data
--
-- Returns :
--
--    X-axis zoom scroll bar position
--
   function Get_X_Zoom (Data : Linguistic_Set_Data) return Zoom_X_Range;
--
-- Get_Y_Zoom -- X-axis zoom factor
--
--    Data - The widget data
--
-- The  Y-axis  bar  is  reverse.  Thus   the   value   Zoom_Range'First
-- corresponds to the most zoomed in state.
--
-- Returns :
--
--    Y-axis zoom scroll bar position
--
   function Get_Y_Zoom (Data : Linguistic_Set_Data) return Zoom_Y_Range;
--
-- Hide_Accumulated -- Remove accumulated set if any
--
--    Data - The widget area
--
   procedure Hide_Accumulated (Data : in out Linguistic_Set_Data);
--
-- Insert -- A variable
--
--    Data  - The widget area
--    Index - Of the variable to insert
--    Value - Of the variable
--
   procedure Insert
             (  Data  : in out Linguistic_Set_Data;
                Index : Positive;
                Value : Variable
             );
--
-- Move -- A variable from one position to another
--
--    Data - The widget area
--    From - Of the variable to insert
--    To   - Of the variable
--
   procedure Move
             (  Data : in out Linguistic_Set_Data;
                From : Positive;
                To   : Positive
             );
--
-- Remove -- A variable
--
--    Data  - The widget area
--    Index - Of the variable to remove
--
   procedure Remove
             (  Data  : in out Linguistic_Set_Data;
                Index : Positive
             );
--
-- Rescale_X -- Set X-axis scale
--
--    Data   - The widget area
--    Span   - The desired range of visible values
--    Scroll - The scroll bar adjustment to set or null
--    Whole  - Span is the whole range when True
--
-- The  procedure prevents ranges of too high resolution to be set, when
-- the parameter Whole is False.
--
   procedure Rescale_X
             (  Data   : in out Linguistic_Set_Data;
                Span   : Interval;
                Scroll : Gtk_Adjustment;
                Whole  : Boolean
             );
--
-- Rescale_X -- Set X-axis scale
--
--    Data   - The widget area
--    Scroll - The scroll bar adjustment to set or null
--
-- The  procedure evaluates the whole range of values of the current set
-- and calls Rescale_X with Whole set to True.
--
   procedure Rescale_X
             (  Data   : in out Linguistic_Set_Data;
                Scroll : Gtk_Adjustment
             );
--
-- Rescale_Y -- Set Y-axis scale
--
--    Data   - The widget area
--    From   - The desired range of visible membership values
--    To     - The desired range of visible membership values
--    Scroll - The scroll bar adjustment to set or null
--
-- The  procedure prevents ranges of too high resolution to be set, when
-- the parameter Whole is False.
--
   procedure Rescale_Y
             (  Data   : in out Linguistic_Set_Data;
                From   : Truth;
                To     : Truth;
                Scroll : Gtk_Adjustment
             );
--
-- Rescale_Y -- Set Y-axis scale
--
--    Data   - The widget area
--    Scroll - The scroll bar adjustment to set or null
--
-- It calls Rescale_Y with Whole set to True.
--
   procedure Rescale_Y
             (  Data   : in out Linguistic_Set_Data;
                Scroll : Gtk_Adjustment
             );
--
-- Resize -- The drawing area
--
--    Data     - The widget area
--    X_Size   - New width
--    Y_Size   - New height
--    X_Scroll - The scroll bar adjustment or null
--    Y_Scroll - The scroll bar adjustment or null
--    Resized  - True if the drawing area was resized or created
--
-- The drawing area keeps the range of values it currently  showns.  The
-- positions of the adjustments (if any) don't change.
--
   procedure Resize
             (  Data     : in out Linguistic_Set_Data;
                X_Size   : X_Axis;
                Y_Size   : Y_Axis;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment;
                Resized  : out Boolean
             );
--
-- Scale -- Set a zoom state of the drawing area
--
--    Data     - The widget area
--    Item     - The state of zooming
--    X_Scroll - The scroll bar adjustment or null
--    Y_Scroll - The scroll bar adjustment or null
--
   procedure Scale
             (  Data     : in out Linguistic_Set_Data;
                Item     : Zoom_Undo_Item;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             );
--
-- Scroll -- To the value
--
--    Data     - The widget area
--    Value    - The range of values to make visible
--    Level    - The range of confidence to make visible
--    X_Scroll - The scroll bar adjustment or null
--    Y_Scroll - The scroll bar adjustment or null
--
-- This procedure changes zooming if necessary.
--
   procedure Scroll
             (  Data     : in out Linguistic_Set_Data;
                Value    : Interval;
                Level    : Fuzzy_Boolean;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             );
--
-- Scroll_Left -- To the value on the left
--
--    Data   - The widget area
--    Left   - The value to be the leftmost in the drawing area
--    Scroll - The scroll bar adjustment or null
--
-- The  procedure  does  not affect the horizontal scroll bar if any. It
-- also does not check Left for being in the range.
--
   procedure Scroll_Left
             (  Data   : in out Linguistic_Set_Data;
                Left   : Number'Base;
                Scroll : Gtk_Adjustment
             );
--
-- Scroll_Top -- To the value on the top
--
--    Data   - The widget area
--    Top    - The value to be the topmost in the drawing area
--    Scroll - The scroll bar adjustment or null
--
-- The procedure does not affect the vertical scroll bar if any. It also
-- does not check for Top for being in the range.
--
   procedure Scroll_Top
             (  Data   : in out Linguistic_Set_Data;
                Top    : Truth;
                Scroll : Gtk_Adjustment
             );
--
-- Scroll_X -- Scroll to range without zooming
--
--    Data   - The widget area
--    Left   - The left point to scroll to
--    Right  - The right point to scroll to
--    Scroll - The scroll bar adjustment to set or null
--
   procedure Scroll_X
             (  Data   : in out Linguistic_Set_Data;
                Left   : Number'Base;
                Right  : Number'Base;
                Scroll : Gtk_Adjustment
             );
--
-- Scroll_Y -- Scroll to range without zooming
--
--    Data   - The widget area
--    Top    - The top point to scroll to
--    Bottom - The bottom point to scroll to
--    Scroll - The scroll bar adjustment to set or null
--
   procedure Scroll_Y
             (  Data   : in out Linguistic_Set_Data;
                Top    : Truth;
                Bottom : Truth;
                Scroll : Gtk_Adjustment
             );
--
-- Set_Border -- Of the drawing area
--
--    Data   - The widget area
--    Widget - For which resize is requested
--
-- Evaluate border parameters such as length of the ticks and decoration
-- of the drawing area.
--
   procedure Set_Border
             (  Data   : in out Linguistic_Set_Data;
                Widget : not null access Gtk_Widget_Record'Class
             );
--
-- Set_Size -- Set drawing area size
--
--    Data   - The widget data
--    X_Size - Width
--    Y_Size - Height
--
-- The  old  drawing  area is freed if exists. The newly created drawing
-- area has to be scaled. One way to do this is to  call  Rescale_X  and
-- Rescale_Y.
--
   procedure Set_Size
             (  Data   : in out Linguistic_Set_Data;
                X_Size : X_Axis;
                Y_Size : Y_Axis
             );
--
-- Set_X_Scale -- Adjust X-scale
--
--    Data   - The widget area
--    Gain   - The gain to set
--    Offset - The offset to set
--    Scroll - The scroll bar to set or null
--
   procedure Set_X_Scale
             (  Data   : in out Linguistic_Set_Data;
                Gain   : Number'Base;
                Offset : Number'Base;
                Scroll : Gtk_Adjustment
             );
--
-- Set_Y_Scale -- Adjust X-scale
--
--    Data   - The widget area
--    Gain   - The gain to set
--    Offset - The offset to set
--    Scroll - The scroll bar to set or null
--
   procedure Set_Y_Scale
             (  Data   : in out Linguistic_Set_Data;
                Gain   : Truth;
                Offset : Truth;
                Scroll : Gtk_Adjustment
             );
--
-- Show_Accumulated -- Set
--
--    Data - The widget area
--    Var  - The result of accumulation
--
-- This procedures changes accumulated Set being indicated.
--
   procedure Show_Accumulated
             (  Data : in out Linguistic_Set_Data;
                Var  : Variable
             );
--
-- Zoom -- Change zoom factors
--
--    Data     - The widget area
--    X_Factor - The X zoom factor
--    Y_Factor - The Y zoom factor
--    X_Scroll - The scroll bar adjustment or null
--    Y_Scroll - The scroll bar adjustment or null
--
   procedure Zoom
             (  Data     : in out Linguistic_Set_Data;
                X_Factor : Zoom_X_Factor;
                Y_Factor : Zoom_Y_Factor;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             );
--
-- Data_Handles -- Handles to Linguistic_Set_Data
--
   package Data_Handles is
      new Object.Handle (Linguistic_Set_Data, Linguistic_Set_Data_Ptr);
   use Data_Handles;
--
-- Gtk_Fuzzy_Linguistic_Set_Domain_Record -- The widget implementation
--
--    Top_Left   X_Scroll
--       .___________________.
--       |  |<--[]------->|__|
--       |----------------|/\|
--       |                |[]| Y_Scroll
--       |     View       |\/|
--       |                |--|
--       |________________|__| Bottom_Right
--
   type Gtk_Fuzzy_Linguistic_Set_Domain_Record is
      new Gtk_Event_Box_Record with
   record
      Data    : Data_Handles.Handle;
      Content : Gtk_Fixed;
      -- Drawing areas
      View         : Gtk_Drawing_Area;
      Top_Left     : Gtk_Drawing_Area;
      Bottom_Right : Gtk_Drawing_Area;
      -- The scrollbars
      X_Scroll : Gtk_Scrollbar;
      Y_Scroll : Gtk_Scrollbar;
      -- The geometry
      Width  : GInt := 0;
      Height : GInt := 0;
   end record;
--
-- Destroy -- event's callback
--
   procedure Destroy
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             );
--
-- Draw -- The expose_event's handler
--
--    Area    - The widget being srawn
--    Context - The context to draw into
--    Widget  - The widget
--
-- This procedure is an event callback of draw signal.
--
   function Draw
            (  Object  : access GObject_Record'Class;
               Context : Cairo_Context;
               Widget  : Gtk_Fuzzy_Linguistic_Set_Domain
            )  return Boolean;
--
-- Draw_Bottom_Right -- Draw the widget's bottom-right drawing area
--
--    Widget  - The widget
--    Context - The context to draw into
--
   procedure Draw_Bottom_Right
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             );
--
-- Draw_Top_Left -- Draw the widget's top-left drawing area
--
--    Widget  - The widget
--    Context - The context to draw into
--
   procedure Draw_Top_Left
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             );
--
-- Resize -- Change the widget size
--
--    Widget - The widget
--    Width  - The new widget's width
--    Height - The new widget's height
--    Force  - Relocation forcing
--
-- This  procedure causes the widget to change the drawing area size and
-- location as well as ones of the scroll bars if  any.  When  Froce  is
-- false it does nothing if the drawing area size already corresponds to
-- the widget size. Otherwise it might not change the area size, but  it
-- moves it and the scroll bars to the positions are evaluated from  the
-- size.
--
   procedure Resize
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Width  : GInt;
                Height : GInt;
                Force  : Boolean
             );
--
-- Style_Updated -- The style-update event's callback
--
   procedure Style_Updated
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             );
--
-- Instantiations of the callback handlers
--
   package Widget_Callback is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Domain_Record
          );
   package Return_Callback is
      new Gtk.Handlers.Return_Callback
          (  Gtk_Fuzzy_Linguistic_Set_Domain_Record,
             Boolean
          );
   package Return_Boolean_Callback is
      new Gtk.Handlers.User_Return_Callback
          (  GObject_Record,
             Boolean,
             Gtk_Fuzzy_Linguistic_Set_Domain
          );
   package Allocation_Marshaller is
      new Widget_Callback.Marshallers.Generic_Marshaller
          (  Gtk_Allocation_Access,
             Get_Allocation
          );
   package Gtk_Fuzzy_Linguistic_Set_Domain_Ref is
      new GLib.Object.Strong_References
          (  Gtk_Fuzzy_Linguistic_Set_Domain_Record
          );
   use Gtk_Fuzzy_Linguistic_Set_Domain_Ref;

   pragma Inline (From_X);
   pragma Inline (From_Y);
   pragma Inline (Get_X_Size);
   pragma Inline (To_Necessity);
   pragma Inline (To_Possibility);
   pragma Inline (To_Number);
   pragma Inline (To_X);
   pragma Inline (To_Y);

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain;
