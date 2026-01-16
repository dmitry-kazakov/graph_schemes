--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy.Logic                     Luebeck            --
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

with Confidence_Factors;  use Confidence_Factors;

with Ada.Unchecked_Conversion;

package body GLib.Values.Fuzzy.Logic is

   Gtk_Boolean_Type : GType := GType_Invalid;

   type Fuzzy_Boolean_Value is record
      Possibility : Confidence := Confidence'First;
      Necessity   : Confidence := Confidence'First;
      Defined     : Boolean    := False;
   end record;
   pragma Pack (Fuzzy_Boolean_Value);
   pragma Assert (Fuzzy_Boolean_Value'Size <= Address'Size);

   function To_Address is
      new Ada.Unchecked_Conversion (Fuzzy_Boolean_Value, Address);
   function From_Address is
      new Ada.Unchecked_Conversion (Address, Fuzzy_Boolean_Value);

   function Copy_Fuzzy_Boolean (Boxed : Address) return Address is
   begin
      return Boxed;
   end Copy_Fuzzy_Boolean;

   procedure Free_Fuzzy_Boolean (Boxed : Address) is
   begin
      null;
   end Free_Fuzzy_Boolean;

   function Get (Value : GValue) return Fuzzy_Boolean is
      Result : Fuzzy_Boolean_Value;
   begin
      if Value.G_Type = Gtk_Boolean_Type then
         Result := From_Address (Get_Boxed (Value));
         if Result.Defined then
            return
            (  Possibility => Result.Possibility,
               Necessity   => Result.Necessity
            );
         end if;
      end if;
      raise Constraint_Error;
   end Get;

   function GType_Fuzzy_Boolean return GType is
   begin
      if Gtk_Boolean_Type = GType_Invalid then
         Gtk_Boolean_Type :=
            Boxed_Type_Register_Static
            (  Fuzzy_Boolean_Type_Name,
               Copy_Fuzzy_Boolean'Access,
               Free_Fuzzy_Boolean'Access
            );
      end if;
      return Gtk_Boolean_Type;
   end GType_Fuzzy_Boolean;

   function Is_Defined (Value : GValue) return Boolean is
   begin
      return
      (  Value.G_Type = GType_Fuzzy_Boolean
      and then
         From_Address (Get_Boxed (Value)).Defined
      );
   end Is_Defined;

   procedure Set (Value : in out GValue; Level : Fuzzy_Boolean) is
   begin
      if Value.G_Type /= GType_Fuzzy_Boolean then
         raise Constraint_Error;
      else
         Set_Boxed
         (  Value,
            To_Address
            (  (  Possibility => Level.Possibility,
                  Necessity   => Level.Necessity,
                  Defined     => True
         )  )  );
      end if;
   end Set;

   procedure Set_Undefined (Value : in out GValue) is
   begin
      if Value.G_Type /= GType_Fuzzy_Boolean then
         raise Constraint_Error;
      else
         Set_Boxed
         (  Value,
            To_Address ((Confidence'First, Confidence'First, False))
         );
      end if;
   end Set_Undefined;

end GLib.Values.Fuzzy.Logic;
