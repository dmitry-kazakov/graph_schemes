--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy.Intuitionistic            Luebeck            --
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

with Ada.Exceptions;  use Ada.Exceptions;

package body GLib.Values.Fuzzy.Intuitionistic is
   use Intuitionistic_Set_Values;
   use Classification_Values;

   function Get (Value : GValue)
      return Classification is
      This : constant Classification_Object_Ptr := Get_Ptr (Value);
   begin
      if This /= null then
         return This.Value;
      else
         if Value.G_Type /= GType_Classification then
            raise Constraint_Error with
                  "Type is not " &
                  Intuitionistic_Classification_Type_Name;
         else
            raise Constraint_Error with "Undefined";
         end if;
      end if;
   end Get;

   function Get (Value : GValue)
      return Standard.Fuzzy.Intuitionistic.Set is
      This : constant Intuitionistic_Set_Object_Ptr := Get_Ptr (Value);
   begin
      if This /= null then
         return This.Value;
      else
         if Value.G_Type /= Intuitionistic_Set_Values.Get_Type then
            raise Constraint_Error with
                  "Type is not " & Intuitionistic_Set_Type_Name;
         else
            raise Constraint_Error with "Undefined";
         end if;
      end if;
   end Get;

   function Get_Cardinality (Value : GValue) return Natural is
      This : constant GType := Value.G_Type;
   begin
      if This = GLib.Values.Fuzzy.GType_Set then
         declare
            This : constant Fuzzy_Set_Object_Ptr :=
                   Fuzzy_Set_Values.Get_Ptr (Value);
         begin
            if This /= null then
               return This.Value'Length;
            else
               raise Constraint_Error with "Undefined";
            end if;
         end;
      elsif This = GType_Set then
         declare
            This : constant Intuitionistic_Set_Object_Ptr :=
                   Get_Ptr (Value);
         begin
            if This /= null then
               return This.Cardinality;
            else
               raise Constraint_Error with "Undefined";
            end if;
         end;
--       return Intuitionistic_Set_Values.Get_Ptr (Value).Cardinality;
      elsif This = GType_Classification then
         declare
            This : constant Classification_Object_Ptr :=
                   Get_Ptr (Value);
         begin
            if This /= null then
               return This.Cardinality;
            else
               raise Constraint_Error with "Undefined";
            end if;
         end;
      else
         raise Constraint_Error with
               "Type is not " &
               Fuzzy_Set_Type_Name & ", " &
               Intuitionistic_Set_Type_Name & ", " &
               Intuitionistic_Classification_Type_Name;
      end if;
   end Get_Cardinality;

   function Is_Classification (Value : GValue) return Boolean is
   begin
      return Classification_Values.Get_Ptr (Value) /= null;
   end Is_Classification;

   function Is_Set (Value : GValue) return Boolean is
   begin
      return Intuitionistic_Set_Values.Get_Ptr (Value) /= null;
   end Is_Set;

   function Is_Defined (Value : GValue) return Boolean is
      This : constant GType := Value.G_Type;
   begin
      return
      (  Fuzzy_Set_Values.Get_Ptr (Value) /= null
      or else
         Intuitionistic_Set_Values.Get_Ptr (Value) /= null
      or else
         Classification_Values.Get_Ptr (Value) /= null
      );
   end Is_Defined;

   procedure Set
             (  Value : in out GValue;
                Data  : Classification
             )  is
   begin
      if Value.G_Type = GType_Classification then
         declare
            This : Classification_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This = null
               or else
                  This.Value.Cardinality /= Data.Cardinality
               or else
                  This.Use_Count > 1
               )
            then
               This := new Classification_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            This.Value := Data;
         end;
      else
         raise Constraint_Error with
               "Type is not " & Intuitionistic_Classification_Type_Name;
      end if;
   end Set;

   procedure Set
             (  Value : in out GValue;
                Data  : Standard.Fuzzy.Intuitionistic.Set
             )  is
   begin
      if Value.G_Type = GType_Set then
         declare
            This : Intuitionistic_Set_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This = null
               or else
                  This.Value.Cardinality /= Data.Cardinality
               or else
                  This.Use_Count > 1
               )
            then
               This := new Intuitionistic_Set_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            This.Value := Data;
         end;
      else
         raise Constraint_Error with
               "Type is not " & Intuitionistic_Set_Type_Name;
      end if;
   end Set;

   procedure Set_Undefined (Value : in out GValue) is
      This : constant GType := Value.G_Type;
   begin
      if This = GLib.Values.Fuzzy.GType_Set then
         Fuzzy_Set_Values.Set_Ptr (Value, null);
      elsif This = GType_Set then
         Intuitionistic_Set_Values.Set_Ptr (Value, null);
      elsif This = GType_Classification then
         Classification_Values.Set_Ptr (Value, null);
      else
         raise Constraint_Error with
               "Type is not " &
               Fuzzy_Set_Type_Name & ", " &
               Intuitionistic_Set_Type_Name & ", " &
               Intuitionistic_Classification_Type_Name;
      end if;
   end Set_Undefined;

end GLib.Values.Fuzzy.Intuitionistic;
