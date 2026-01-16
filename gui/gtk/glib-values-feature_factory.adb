--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Feature_Factory                 Luebeck            --
--  Implementation                                 Winter, 2007       --
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

with Ada.Unchecked_Conversion;

package body GLib.Values.Feature_Factory is

   Gtk_Type : GType := GType_Invalid;

   pragma Assert
          (  Address'Size
          >= Gtk_Fuzzy_Feature_Abstract_Factory'Size
          );

   function To_Object_Ptr is
      new Ada.Unchecked_Conversion
          (  Address,
             Gtk_Fuzzy_Feature_Abstract_Factory
          );

   function Copy_Factory (Boxed : Address) return Address is
   begin
      if Boxed /= Null_Address then
         Ref (To_Object_Ptr (Boxed));
      end if;
      return Boxed;
   end Copy_Factory;

   procedure Free_Factory (Boxed : Address) is
   begin
      if Boxed /= Null_Address then
         Unref (To_Object_Ptr (Boxed));
      end if;
   end Free_Factory;

   function Get_Factory (Value : GValue)
      return Gtk_Fuzzy_Feature_Abstract_Factory is
      Object_Address : Address;
   begin
      if Value.G_Type = Get_Type then
         Object_Address := Get_Boxed (Value);
         if Object_Address = Null_Address then
            return null;
         else
            return To_Object_Ptr (Object_Address);
         end if;
      else
         return null;
      end if;
   end Get_Factory;

   function Get_Type return GType is
   begin
      if Gtk_Type = GType_Invalid then
         Gtk_Type :=
            Boxed_Type_Register_Static (Class_Name, Copy, Free);
      end if;
      return Gtk_Type;
   end Get_Type;

   procedure Set_Factory
             (  Value   : in out GValue;
                Factory : not null access
                   Gtk_Fuzzy_Feature_Abstract_Factory_Record'Class
             )  is
   begin
      if Value.G_Type /= Get_Type then
         raise Constraint_Error;
      else
         Set_Boxed (Value, Factory.all'Address);
      end if;
   end Set_Factory;

end GLib.Values.Feature_Factory;
