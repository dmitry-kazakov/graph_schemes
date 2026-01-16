--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy                     Luebeck            --
--  Interface                                      Summer, 2007       --
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
--  fuzzy sets, intuitionistic fuzzy sets and classifications.
--
with Cairo;                      use Cairo;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with GLib.Properties.Creation;   use GLib.Properties.Creation;
with GLib.Values;                use GLib.Values;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Fuzzy_Set_Entry;        use Gtk.Fuzzy_Set_Entry;
with Gtk.Widget;                 use Gtk.Widget;
with Pango.Layout;               use Pango.Layout;

with Fuzzy.Abstract_Edit.Handle;
with GLib.Values.Fuzzy;
with Gtk.Cell_Renderer.Abstract_Renderer;
with Gtk.Handlers;

package Gtk.Cell_Renderer_Fuzzy is
   pragma Elaborate_Body (Gtk.Cell_Renderer_Fuzzy);
--
-- Class_Name - Of the renderer
--
   Class_Name : constant String := "GtkCellRendererFuzzy";

   type Fuzzy_Boolean_Shape is (Bar, Bullet, Text);
   pragma Convention (C, Fuzzy_Boolean_Shape);
--
-- Gtk_Cell_Renderer_Fuzzy_Record -- The renderer type
--
-- Properties :
--
--    classification-value     - The currently indicated value. The type
--                               of   the   property  is  intuitionistic
--                               classification  (GType_Classification).
--                               When the indicated value is not of this
--                               type,   then   the  result  is  set  to
--                               undefined.
--    default-truth-value      - The  default truth value which controls
--                               input  and  output  of   intuitionistic
--                               fuzzy  classifications  and  sets,   as
--                               described   in    the    I/O    package
--                               Fuzzy.Abstract_Edit.Intuitionistic.
--                               GType_Fuzzy_Boolean,  The  default   is
--                               certain true.
--    editable-undefined       - This    property   determines   whether
--                               undefined cells are editable.  Boolean.
--                               False.
--    set-value                - The currently indicated value. The type
--                               of the  property  GType_Set.  When  the
--                               indicated value is not  of  this  type,
--                               then the result is set to undefined.
--    intuitionistic-set-value - The currently indicated value. The type
--                               of  the  property  is of intuitionistic
--                               set  value  (Intuitionistic.GType_Set).
--                               When the indicated value is not of this
--                               type,   then   the  result  is  set  to
--                               undefined.
--    prefix-text              - The  text  shown  in  front  of a fuzzy
--                               value. GType_String. The deafaulr is an
--                               empty string.
--
   type Gtk_Cell_Renderer_Fuzzy_Record is
      new Gtk.Cell_Renderer.Abstract_Renderer.
          Gtk_Abstract_Renderer_Record with private;
   type Gtk_Cell_Renderer_Fuzzy is
      access all Gtk_Cell_Renderer_Fuzzy_Record'Class;
--
-- Finalize -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
             );
--
-- Get -- The renderer's value
--
--    Cell - The renderer
--
-- This  function  returns  the  currently  set  renderer's  value.  The
-- result's type is either of:
--
--    GLib.Values.Fuzzy.GType_Set
--    GLib.Values.Fuzzy.Intuitionistic.GType_Classification
--    GLib.Values.Fuzzy.Intuitionistic.GType
--
-- depending on the value set.
--
-- Returns :
--
--    The currently indicated value
--
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
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
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Set;
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Intuitionistic.Classification;
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Intuitionistic.Set;
--
-- Get_Aligned_Area -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Aligned_Area
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Fuzzy_Record;
               Widget    : not null access Gtk_Widget_Record'Class;
               Flags     : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
--
-- Get_Cardinality -- Get the cardinality of
--
--    Cell - The renderer
--
-- Retunrs :
--
--    The cardinality of the domain set
--
   function Get_Cardinality
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Natural;
--
-- Get_Domain -- The domain of the indicated value
--
--    Cell - The widget
--
-- Returns :
--
--    A handle to the description of the domain set
--
   function Get_Domain
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Entry_Domain;
--
-- Get_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Size -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
   overriding
   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
               Widget : not null access Gtk_Widget_Record'Class
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
--    Cell       - The result
--    Domain     - The domain set of the values to indicate
--    Value_Type - The initial type of values to show
--
-- The  parameter  Domain  determines  the  domain  set  of  the  object
-- rendered. The domain set is a crisp set of named members.
--
   procedure Gtk_New
             (  Cell       : out Gtk_Cell_Renderer_Fuzzy;
                Domain     : Entry_Domain;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Cell       - The renderer to initialize
--    Domain     - The domain set of the values to indicate
--    Cell_Type  - The renderer's type
--    Value_Type - The initial type of values to show
--
-- The parameter Cell_Type is used by a derived type when the renderer's
-- GTK type differs.
--
   procedure Initialize
             (  Cell       : not null access
                             Gtk_Cell_Renderer_Fuzzy_Record'Class;
                Domain     : Entry_Domain;
                Cell_Type  : Gtk_Type;
                Value_Type : Gtk_Type
             );
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
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Boolean;
--
-- Put -- Change the value indicated
--
--    Cell   - The renderer
--    Value  - To indicate
--
-- This procedure changes the indicated value. The type of the indicated
-- value is also changed to correspond to the type of Value. When  Value
-- is   GValue   then  the  following  types  are  expected:  GType_Set,
-- GType_Classification, Intuitionistic.GType_Set, GType_String. When it
-- is  a  string then it has to have a format used by the function Value
-- of   Fuzzy.Abstract_Edit.Intuitionistic.    Improperly    typed    or
-- syntactically wrong values set the renderer into the state of invalid
-- value. In this state nothing is indicated.
--
   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : GValue
             );
   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Set
             );
   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Intuitionistic.Classification
             );
--
-- Put -- Change the value indicated
--
--    Cell   - The renderer
--    Domain - The domain set of the values to indicate
--    Value  - To indicate
--
-- This  procedure  changes the domain and the indicated value. The type
-- of  the  indicated value is also changed to correspond to the type of
-- Value. When Value is GValue then the following  types  are  expected:
-- GType_Set,      GType_Classification,       Intuitionistic.GType_Set,
-- GType_String. When it is a string then it has to have a  format  used
-- by   the   function   Value   of  Fuzzy.Abstract_Edit.Intuitionistic.
-- Improperly  typed or syntactically wrong values set the renderer into
-- the state of invalid value. In this state nothing is indicated.
--
   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : GValue
             );
   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             );
   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Cell   : access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             );
--
-- Render -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Render
             (  Cell            : not null access
                                  Gtk_Cell_Renderer_Fuzzy_Record;
                Context         : Cairo_Context;
                Widget          : not null access
                                  Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             );
--
-- Set_Domain -- Change the domain and set value undefined
--
--    Cell   - The renderer
--    Domain - The domain set of the values to indicate
--
-- This procedure  changes  the  domain.  The  indicated  value  is  set
-- undefined.
--
   procedure Set_Domain
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain
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
                           Gtk_Cell_Renderer_Fuzzy_Record;
                Editable : Boolean
             );
--
-- Set_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Start_Editing -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Start_Editing
            (  Cell            : not null access
                                 Gtk_Cell_Renderer_Fuzzy_Record;
               Event           : Gdk_Event;
               Widget          : not null access
                                 Gtk_Widget_Record'Class;
               Path            : String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget;

private
   pragma Inline (Get_Alignment);
   pragma Inline (Get_Cardinality);

   Editable_Undefined_ID : constant Property_ID := 1;
   Set_ID                : constant Property_ID := 2;
   Intuitionistic_Set_ID : constant Property_ID := 3;
   Classification_ID     : constant Property_ID := 4;
   Default_ID            : constant Property_ID := 5;
   Prefix_ID             : constant Property_ID := 6;

   type String_Ptr is access String;
   type Gtk_Cell_Renderer_Fuzzy_Record is
      new Gtk.Cell_Renderer.Abstract_Renderer.
          Gtk_Abstract_Renderer_Record with
   record
      Domain         : Entry_Domain;
      Value          : GValue;
      Text           : Pango_Layout;
      Focus_Out      : Gtk.Handlers.Handler_Id;
      Prefix         : String_Ptr    := new String'("");
      Prefix_Length  : Natural       := 0;
      Default        : Fuzzy_Boolean := Certain_True;
      Edit_Undefined : Boolean       := False;
      Updated        : Boolean       := True;
   end record;
--
-- Editing_Done -- The handler of editing_done
--
   procedure Editing_Done
             (  Editor : access Gtk_Fuzzy_Set_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fuzzy
             );
--
-- Focus_Out -- The handler of focus_out_event
--
   function Focus_Out
            (  Editor : access Gtk_Fuzzy_Set_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fuzzy
            )  return Boolean;
--
-- Entry_Callbacks -- Handlers for editing_done
--
   package Entry_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Widget_Type => Gtk_Fuzzy_Set_Entry_Record,
             User_Type   => Gtk_Cell_Renderer_Fuzzy
          );
--
-- Entry_Return_Callbacks -- Handlers for focus_out_event
--
   package Entry_Return_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Widget_Type => Gtk_Fuzzy_Set_Entry_Record,
             Return_Type => Boolean,
             User_Type   => Gtk_Cell_Renderer_Fuzzy
          );

   procedure Update
             (  Cell   : not null access
                         Gtk_Cell_Renderer_Fuzzy_Record'Class;
                Widget : not null access
                         Gtk_Widget_Record'Class
             );
--
-- Class_Init -- The class initialization procedure
--
--    Class - The renderer's class record
--
-- This procedure is called upon class initialization. It shall be  used
-- in Gtk.Cell_Renderer.Abstract_Renderer.Register  either  directly  or
-- called form its replacement of the derived type.
--
   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

end Gtk.Cell_Renderer_Fuzzy;
