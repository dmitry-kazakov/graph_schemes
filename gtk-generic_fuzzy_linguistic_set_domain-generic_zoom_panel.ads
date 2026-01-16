--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain.    Luebeck            --
--        Generic_Zoom_Panel                       Winter, 2007       --
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
--  Zoom panel and buttons for Gtk_Fuzzy_Linguistic_Set_Domain widget:
--
--            .______. .______.
--       |    |      | |      |
--       |    | Undo | |Zoom +|
--       |    |______| |______|
--       |    .______. .______.
--       |    |      | |      |
--       |    |Zoom -| | Redo |
--      [ ]   |______| |______|
--    .______.
--    |      |
--    |Zoom 0| []-------------
--    |______|
--
-- This widget is used with Gtk_Fuzzy_Linguistic_Set_Domain  to  provide
-- zooming and scrolling functionality.  The  widget  has  vertical  and
-- horizontal  zoom  sliders.  It  connects to the signals of the scroll
-- bars  of  the  Gtk_Fuzzy_Linguistic_Set   widget   making   scrolling
-- possible.  It  also  connects to the mouse events of the drawing area
-- supporting visual selection of the content. When a square area in the
-- drawing area is selected, zoom+ and zoom- buttons can be used to zoom
-- in our out of this area. Otherwise these buttons zoom  incrementally.
-- Further  the  widget supports undo and redo buffers for scrolling and
-- zooming actions.
--
-- The elements of the widget can also be used without  the  widget.  In
-- this case they are created using Gtk_New and then can be placed  into
-- customized containers or dereferenced.
--
-- The package is generic it has  to  be  instantiated  at  the  library
-- level.
--
with Ada.Calendar;             use Ada.Calendar;
with Gdk.Event;                use Gdk.Event;
with Gdk.Window;               use Gdk.Window;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scale;                use Gtk.Scale;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Table;                use Gtk.Table;
with Gtk.Widget;               use Gtk.Widget;

with Ada.Finalization;
with Generic_Segmented_Stack;
with Gtk.Generic_Style_Button;
with Object.Handle;

generic
package Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Zoom_Panel is
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record -- Zooming / scrolling
--
-- Style properties :
--
--    column-spacings - The  spacing  between  the  columns  in  pixels.
--                      GUInt. Default is 3.
--    row-spacings    - The spacing between the rows in  pixels.  GUInt.
--                      Default is 3.
--
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Panel is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record'Class;
--
-- Gtk_Fuzzy_Linguistic_Set_Membership -- Y tracker scale
--
   type Gtk_Fuzzy_Linguistic_Set_Membership_Record is
      new Gtk_Label_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Membership is
      access all Gtk_Fuzzy_Linguistic_Set_Membership_Record'Class;
--
-- Get_Y_Tracker -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Y_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Membership;
--
-- Gtk_Fuzzy_Linguistic_Set_Value -- X tracker scale
--
   type Gtk_Fuzzy_Linguistic_Set_Value_Record is
      new Gtk_Label_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Value is
      access all Gtk_Fuzzy_Linguistic_Set_Value_Record'Class;
--
-- Get_X_Tracker -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_X_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Value;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_100 -- Zoom 100% button
--
   package Zoom_100_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Zoom100",
             Icon       => Stock_Zoom_100,
             Relief     => Relief_None,
             Tip        => "Zoom 100%"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record is
      new Zoom_100_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_100 is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record'Class;
--
-- Get_Zoom_100_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_100_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_100;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Fit -- Zoom fit button
--
   package Zoom_Fit_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomFit",
             Icon       => Stock_Zoom_Fit,
             Relief     => Relief_None,
             Tip        => "Scale to fit"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record is
      new Zoom_Fit_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Fit is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record'Class;
--
-- Get_Zoom_Fit_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_Fit_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Fit;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_In -- Zoom in button
--
   package Zoom_In_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomIn",
             Icon       => Stock_Zoom_In,
             Relief     => Relief_None,
             Tip        => "Zoom in"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record is
      new Zoom_In_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_In is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record'Class;
--
-- Get_Zoom_In_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_In_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_In;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Out -- Zoom out button
--
   package Zoom_Out_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomOut",
             Icon       => Stock_Zoom_Out,
             Relief     => Relief_None,
             Tip        => "Zoom out"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record is
      new Zoom_Out_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Out is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record'Class;
--
-- Get_Zoom_Out_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_Out_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Out;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Redo -- Zoom / scroll redo button
--
   package Zoom_Redo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomRedo",
             Icon       => Stock_Redo,
             Relief     => Relief_None,
             Tip        => "Redo"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record is
      new Zoom_Redo_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Redo is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record'Class;
--
-- Get_Zoom_Redo_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_Redo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Redo;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Undo -- Zoom / scroll undo button
--
   package Zoom_Undo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomUndo",
             Icon       => Stock_Undo,
             Relief     => Relief_None,
             Tip        => "Undo"
          );
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record is
      new Zoom_Undo_Buttons.Gtk_Style_Button_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Undo is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record'Class;
--
-- Get_Zoom_Undo_Button -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_Undo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Undo;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_X -- Zoom X scale
--
   type Gtk_Fuzzy_Linguistic_Set_Zoom_X_Record is
      new Gtk_Scale_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_X is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_X_Record'Class;
--
-- Get_Zoom_X_Scale -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_X_Scale
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_X;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Y -- Zoom Y scale
--
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Y_Record is
      new Gtk_Scale_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Y is
      access all Gtk_Fuzzy_Linguistic_Set_Zoom_Y_Record'Class;
--
-- Get_Zoom_Y_Scale -- Factory
--
--    Widget - The widget to use with
--
-- Returns :
--
--    The widget (created if necessary)
--
   function Get_Zoom_Y_Scale
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Y;
------------------------------------------------------------------------
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
                        Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record
            )  return not null access
                      Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Panel  - The result
--    Widget - The widget to use with
--
   procedure Gtk_New
             (  Panel  : out Gtk_Fuzzy_Linguistic_Set_Zoom_Panel;
                Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Panel  - To intitialize
--    Widget - The widget to use with the panel
--
-- Exceptions :
--
--    Constraint_Error - Widget is null
--
   procedure Initialize
             (  Panel  : not null access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record'Class;
                Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_REcord'Class
             );
private
--
-- Zoom_Undo_Stacks -- Stack for zooming undo
--
   package Zoom_Undo_Stacks is
      new Generic_Segmented_Stack
          (  Index_Type   => Integer,
             Object_Type  => Zoom_Undo_Item,
             Null_Element => (1.0, 0.0, 1.0, 0.0)
          );
   package Zoom_Undo renames Zoom_Undo_Stacks.Segmented_Stack;
   use Zoom_Undo;
--
-- Fuzzy_Linguistic_Set_Zoom_Data
--
   type Fuzzy_Linguistic_Set_Zoom_Data is
      new Object.Entity and Extension_Interface with
   record
      Set        : Gtk_Fuzzy_Linguistic_Set_Domain_Ref.Strong_Reference;
      Undo_Stack : Zoom_Undo.Stack;
      Redo_Stack : Zoom_Undo.Stack;
      Last_Undo  : Time; -- The last time Undo was pushed on to
      Pending_Zoom : Boolean := False;

      Undo     : Gtk_Fuzzy_Linguistic_Set_Zoom_Undo;
      Redo     : Gtk_Fuzzy_Linguistic_Set_Zoom_Redo;
      Zoom_100 : Gtk_Fuzzy_Linguistic_Set_Zoom_100;
      Zoom_Fit : Gtk_Fuzzy_Linguistic_Set_Zoom_Fit;
      Zoom_In  : Gtk_Fuzzy_Linguistic_Set_Zoom_In;
      Zoom_Out : Gtk_Fuzzy_Linguistic_Set_Zoom_Out;
      Zoom_X   : Gtk_Fuzzy_Linguistic_Set_Zoom_X;
      Zoom_Y   : Gtk_Fuzzy_Linguistic_Set_Zoom_Y;
      Value    : Gtk_Fuzzy_Linguistic_Set_Value;
      Level    : Gtk_Fuzzy_Linguistic_Set_Membership;
         -- Handlers
      Style_Updated  : Handler_Reference;
      Leave_Notify   : Handler_Reference;
      Button_Press   : Handler_Reference;
      Button_Release : Handler_Reference;
      Motion_Notify  : Handler_Reference;
      Zoomed         : Handler_Reference;
   end record;
   type Fuzzy_Linguistic_Set_Zoom_Data_Ptr is
      access all Fuzzy_Linguistic_Set_Zoom_Data'Class;
--
-- Finalize -- Destruction
--
   procedure Finalize (Data : in out Fuzzy_Linguistic_Set_Zoom_Data);
--
-- Get_Data -- Get zooming data object
--
--    Widget - The linguistic set widget
--
-- Returns :
--
--    The data object, created if necessary
--
   function Get_Data
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Fuzzy_Linguistic_Set_Zoom_Data_Ptr;


   package Zoom_Data_Handles is
      new Object.Handle
          (  Fuzzy_Linguistic_Set_Zoom_Data,
             Fuzzy_Linguistic_Set_Zoom_Data_Ptr
          );
   use Zoom_Data_Handles;

   type Widget_Type is
        (  Undo_Button, Redo_Button, Zoom_In_Button, Zoom_Out_Button,
           Zoom_100_Button, Zoom_Fit_Button, Zoom_X_Scale, Zoom_Y_Scale,
           Membership_Tracker, Value_Tracker
        );
   type Zoom_Data_Handle (Widget : Widget_Type) is
      new Zoom_Data_Handles.Handle with null record;
   procedure Finalize (Ref : in out Zoom_Data_Handle);
   function Ref (Object : Fuzzy_Linguistic_Set_Zoom_Data_Ptr)
      return Zoom_Data_Handle;
   procedure Set
             (  Ref    : in out Zoom_Data_Handle;
                Object : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );

   type Gtk_Fuzzy_Linguistic_Set_Membership_Record is
      new Gtk_Label_Record with
   record
      Data : Zoom_Data_Handle (Membership_Tracker);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Value_Record is
      new Gtk_Label_Record with
   record
      Data : Zoom_Data_Handle (Value_Tracker);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record is
      new Zoom_100_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Zoom_100_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record is
      new Zoom_Fit_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Zoom_Fit_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record is
      new Zoom_In_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Zoom_In_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record is
      new Zoom_Out_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Zoom_Out_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record is
      new Zoom_Undo_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Undo_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record is
      new Zoom_Redo_Buttons.Gtk_Style_Button_Record with
   record
      Data : Zoom_Data_Handle (Redo_Button);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_X_Record is
      new Gtk_Scale_Record with
   record
      Data : Zoom_Data_Handle (Zoom_X_Scale);
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Zoom_Y_Record is
      new Gtk_Scale_Record with
   record
      Data : Zoom_Data_Handle (Zoom_Y_Scale);
   end record;
--
-- Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record
--
   type Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record is
      new Gtk_Table_Record with
   record
      Data : Zoom_Data_Handles.Handle;
   end record;
--
-- Button_Press -- The button_press_event's callback
--
   function Button_Press
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean;
--
-- Button_Release -- The button_release_event's callback
--
   function Button_Release
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean;
--
-- Leave_Notify -- The leave_notify_event's callback
--
   function Leave_Notify
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean;
--
-- Motion_Notify -- The motion_event's callback
--
   function Motion_Notify
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean;
--
-- Scroll_X -- The value_changed event's callback
--
   procedure Scroll_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );
--
-- Scroll_Y -- The value_changed event's callback
--
   procedure Scroll_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );
--
-- Style_Updated -- The style-updated event's callback
--
   procedure Style_Updated
             (  Widget : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record'Class
             );
   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );
   package Style_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Fuzzy_Linguistic_Set_Zoom_Data_Ptr
          );
--
-- Instantiations of the callback handlers
--
   package Zoom_Callbacks is
      new Gtk.Handlers.Callback
             (Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record);
--
-- Zoomed_External -- The zoomed event's handler
--
   procedure Zoomed
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );
   package Zoomed_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Fuzzy_Linguistic_Set_Domain_Record,
             Fuzzy_Linguistic_Set_Zoom_Data_Ptr
          );
--
-- Zoomed_100 -- The clicked event's callback
--
   procedure Zoomed_100
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record'Class
             );
   package Zoom_100_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record
          );
--
-- Zoomed_Fit -- The clicked event's callback
--
   procedure Zoomed_Fit
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record'Class
             );
   package Zoom_Fit_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record
          );
--
-- Zoomed_In -- The clicked event's callback
--
   procedure Zoomed_In
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record'Class
             );
   package Zoom_In_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record
          );
--
-- Zoomed_Out -- The clicked event's callback
--
   procedure Zoomed_Out
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record'Class
             );
   package Zoom_Out_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record
          );
--
-- Zoomed_Redo -- The clicked event's callback
--
   procedure Zoomed_Redo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record'Class
             );
   package Redo_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record
          );
--
-- Zoomed_Undo -- The clicked event's callback
--
   procedure Zoomed_Undo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record'Class
             );
   package Undo_Callbacks is
      new Gtk.Handlers.Callback
          (  Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record
          );
--
-- Zoomed_X -- The value_changed event's callback
--
   procedure Zoomed_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );
--
-- Zoomed_Y -- The value_changed event's callback
--
   procedure Zoomed_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             );

   package Adjustment_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Gtk_Adjustment_Record,
             Fuzzy_Linguistic_Set_Zoom_Data_Ptr
          );

   package Area_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Drawing_Area_Record,
             Boolean,
             Fuzzy_Linguistic_Set_Zoom_Data_Ptr
          );
--
-- Zoom_Undo_State -- A helper controlled type to manage undo stack
--
-- Upon initialization the current  gain and offset are stored into  the
-- object.  After that within the object scope the axes can be magnified
-- and scrolled to some desired state.  Upon  finalization,  the  object
-- will check if something has been changed and modify the undo and redo
-- stacks as necessary.
--
   type Zoom_Undo_State
        (  Data : not null access Fuzzy_Linguistic_Set_Zoom_Data'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Item     : Zoom_Undo_Item;
      Inactive : Boolean := False; -- Other pending zoom
   end record;
   overriding
      procedure Initialize (State : in out Zoom_Undo_State);
   overriding
      procedure Finalize   (State : in out Zoom_Undo_State);

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Zoom_Panel;
