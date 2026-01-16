--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy.                    Luebeck            --
--        Feature_Value                            Spring, 2006       --
--  Interface                                                         --
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
--
--  This   package   provides  Gtk_Cell_Renderer_Fuzzy_Feature_Value  to
--  render fuzzy sets, intuitionistic sets and classifications.
--
with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;

package Gtk.Cell_Renderer_Fuzzy.Feature_Value is
--
-- Class_Name - Of the renderer
--
   Class_Name : constant String := "GtkCellRendererFeatureValue";
--
-- Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record -- The renderer type
--
-- Properties :
--
--    feature-value  - The  value  to render into the cell. The expected
--                     type  is GType_Feature_Value, which can be set to
--                     a    fuzzy    set,    intuitionistic    set    or
--                     classification. When the value is set  undefined,
--                     it is rendered as empty.
--
   type Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record is
      new Gtk_Cell_Renderer_Fuzzy_Record with private;
   type Gtk_Cell_Renderer_Fuzzy_Feature_Value is
      access all Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record'Class;
--
-- Get -- Overrides Gtk.Cell_Renderer_Fuzzy...
--
   overriding
   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record
            )  return GValue;
--
-- Get_Feature -- Get the feature
--
--    Cell - The renderer
--
-- Returns :
--
--    The feature handle
--
   function Get_Feature
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record
            )  return Feature_Handle;
--
-- Get_Property -- Overrides Gtk.Cell_Renderer_Fuzzy...
--
   overriding
   procedure Get_Property
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Type -- Get the type of cell renderer
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
--    Input      - Input parameters
--    Output     - Rendering parameters
--    Value_Type - The initial type of values to show
--
   procedure Gtk_New
             (  Cell       : out Gtk_Cell_Renderer_Fuzzy_Feature_Value;
                Input      : Input_Parameters'Class  := Input_Defaults;
                Output     : Output_Parameters'Class := Output_Defaults;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Cell       - The renderer to initialize
--    Input      - Input parameters
--    Output     - Rendering parameters
--    Value_Type - The initial type of values to show
--
   procedure Initialize
             (  Cell : not null access
                   Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record'Class;
                Input      : Input_Parameters'Class;
                Output     : Output_Parameters'Class;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             );
--
-- Put -- Overrides Gtk.Cell_Renderer_Fuzzy...
--
   overriding
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Value : GValue
             );
--
-- Set_Feature -- Set the feature
--
--    Cell    - The renderer
--    Feature - To set
--
-- The value is set undefined
--
   procedure Set_Feature
             (  Cell    : not null access
                          Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Feature : Feature_Handle
             );
--
-- Set_Property -- Overrides Gtk.Cell_Renderer_Fuzzy...
--
   procedure Set_Property
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );

private
   type Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record is
      new Gtk_Cell_Renderer_Fuzzy_Record with null record;
--
-- Class_Init -- The class initialization procedure
--
--    Class - The renderer's class record
--
-- When  the  derived  wish  to  extend  the  class record by calling to
-- Gtk.Cell_Renderer.Abstract_Renderer.Register   it   shall  call  this
-- procedure from it class initialization one.
--
   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

end Gtk.Cell_Renderer_Fuzzy.Feature_Value;
