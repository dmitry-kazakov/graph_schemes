--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Confidence_Factors              Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  12:27 10 Jun 2003  --
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

package body GLib.Values.Confidence_Factors is

   Gtk_Confidence_Type : GType := GType_Invalid;

   type Confidence_Value is record
      Value   : Confidence := Confidence'First;
      Defined : Boolean    := False;
   end record;
   pragma Pack (Confidence_Value);
   pragma Assert (Confidence_Value'Size <= Address'Size);

   function To_Address is
      new Ada.Unchecked_Conversion (Confidence_Value, Address);
   function From_Address is
      new Ada.Unchecked_Conversion (Address, Confidence_Value);

   function Copy_Confidence (Boxed : Address) return Address is
   begin
      return Boxed;
   end Copy_Confidence;

   procedure Free_Confidence (Boxed : Address) is
   begin
      null;
   end Free_Confidence;

   function Get (Value : GValue) return Confidence is
      Result : Confidence_Value;
   begin
      if Value.G_Type = GType_Confidence then
         Result := From_Address (Get_Boxed (Value));
         if Result.Defined then
            return Result.Value;
         end if;
      end if;
      raise Constraint_Error;
   end Get;

   function GType_Confidence return GType is
   begin
      if Gtk_Confidence_Type = GType_Invalid then
         Gtk_Confidence_Type :=
            Boxed_Type_Register_Static
            (  Confidence_Type_Name,
               Copy_Confidence'Access,
               Free_Confidence'Access
            );
      end if;
      return Gtk_Confidence_Type;
   end GType_Confidence;

   function Is_Defined (Value : GValue) return Boolean is
   begin
      return
      (  Value.G_Type = GType_Confidence
      and then
         From_Address (Get_Boxed (Value)).Defined
      );
   end Is_Defined;

   procedure Set
             (  Value : in out GValue;
                Level : Confidence
             )  is
   begin
      if Value.G_Type /= Gtk_Confidence_Type then
         raise Constraint_Error;
      else
         Set_Boxed (Value, To_Address ((Level, True)));
      end if;
   end Set;

   procedure Set_Undefined (Value : in out GValue) is
   begin
      if Value.G_Type /= Gtk_Confidence_Type then
         raise Constraint_Error;
      else
         Set_Boxed (Value, To_Address ((Confidence'First, False)));
      end if;
   end Set_Undefined;

end GLib.Values.Confidence_Factors;
