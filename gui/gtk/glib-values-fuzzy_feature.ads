--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy_Feature                   Luebeck            --
--  Interface                                      Spring, 2006       --
--                                                                    --
--                                Last revision :  21:25 10 Nov 2009  --
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
--  This package provides a GTK+ type GType_Feature can be used to place
--  a feature into a GTK+ value.
--
with Fuzzy.Feature;         use Fuzzy.Feature;
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;

with GLib.Values.Handle;

package GLib.Values.Fuzzy_Feature is
--
-- Get_Feature -- Get feature from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- GType_Feature.
--
-- Returns :
--
--    A handle to the feature
--
-- Exceptions :
--
--    Constraint_Error - The value is not a feature
--
   function Get_Feature (Value : GValue) return Feature_Handle;
--
-- GType_Feature -- The GTK+ type of feature
--
   function GType_Feature return GType;
--
-- Set_Feature -- Set a value
--
--    Value   - To set
--    Feature - A handle to the feature
--
-- This procedure sets feature into GTK+  value,  previously  initialized
-- using Init with the parameter GType_Feature.
--
-- Exceptions :
--
--    Constraint_Error - Not an object value, invalid handle
--
   procedure Set_Feature
             (  Value   : in out GValue;
                Feature : Feature_Handle
             );
private
--
-- Gtk_Values -- Interfacing of features to GTK+ values
--
   package Gtk_Values is
      new GLib.Values.Handle
          (  Type_Name       => "GFuzzyFeature",
             Object_Type     => Feature_Object,
             Object_Type_Ptr => Feature_Object_Ptr,
             Handle_Type     => Feature_Handle
          );
   function GType_Feature return GType
      renames Gtk_Values.Get_Type;
   function Get_Feature (Value : GValue) return Feature_Handle
      renames Gtk_Values.Get_Handle;
   procedure Set_Feature
             (  Value   : in out GValue;
                Feature : Feature_Handle
             )  renames Gtk_Values.Set_Handle;

end GLib.Values.Fuzzy_Feature;
