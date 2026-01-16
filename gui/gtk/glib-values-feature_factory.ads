--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Feature_Factory                 Luebeck            --
--  Interface                                      Spring, 2007       --
--                                                                    --
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

with Gtk.Fuzzy_Feature;  use Gtk.Fuzzy_Feature;
with System;             use System;

package GLib.Values.Feature_Factory is
--
-- Class_Name -- The name of GTK+ type
--
   Class_Name : constant String := "GtkAbstractFeatureFactory";
--
-- Get_Factory -- From a GTK+ value
--
--    Value - Of the type returned by Get_Type
--
-- A  factory  can  be  put  into  a  GTK+  value  using  the  procedure
-- Set_Factory.
--
-- Returns :
--
--    The factory or null
--
   function Get_Factory (Value : GValue)
      return Gtk_Fuzzy_Feature_Abstract_Factory;
--
-- Get_Type -- The type of GTK+ factory values
--
-- Note that this type might differ from the actual type of the  factory
-- widget, the GTK+ value might refer to.
--
-- Returns :
--
--    The type of
--
   function Get_Type return GType;
--
-- Set_Factory -- Into a GTK+ value
--
--    Value   - Initialized with the type returned by Get_Type
--    Factory - The factory widget to be set into Value
--
-- The  value  set  by  Set_Factory can be passed around and counts as a
-- reference to the factory widget until Unset is called on it.
--
-- Exceptions :
--
--    Constraint_Error - Not  a  properly initialized object value, null
--                       pointer
--
   procedure Set_Factory
             (  Value   : in out GValue;
                Factory : not null access
                   Gtk_Fuzzy_Feature_Abstract_Factory_Record'Class
             );
private
   function Copy_Factory (Boxed : Address) return Address;
   pragma Convention (C, Copy_Factory);

   procedure Free_Factory (Boxed : Address);
   pragma Convention (C, Free_Factory);

   Copy : constant Boxed_Copy := Copy_Factory'Access;
   Free : constant Boxed_Free := Free_Factory'Access;

end GLib.Values.Feature_Factory;
