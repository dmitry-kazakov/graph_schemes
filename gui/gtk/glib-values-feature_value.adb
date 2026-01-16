--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Feature_Value                   Luebeck            --
--  Implementation                                 Summer 2007       --
--                                                                    --
--                                Last revision :  11:45 29 May 2020  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;           use GLib.Messages;

package body GLib.Values.Feature_Value is
   use Feature_Values;

   function Where (Name : String) return String is
   begin
      return " in GLib.Values.Feature_Value." & Name;
   end Where;

   function Get (Value : GValue) return Fuzzy.Set is
   begin
      return Set_Object (Get_Ptr (Value).all).Value;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Get")
         );
         raise;
   end Get;

   function Get (Value : GValue) return Fuzzy.Intuitionistic.Set is
   begin
      return Intuitionistic_Set_Object (Get_Ptr (Value).all).Value;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Get")
         );
         raise;
   end Get;

   function Get (Value : GValue) return Classification is
   begin
      return Classification_Object (Get_Ptr (Value).all).Value;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Get")
         );
         raise;
   end Get;

   function Get_Feature (Value : GValue) return Feature_Handle is
   begin
      return Get_Ptr (Value).Feature;
   end Get_Feature;

   function Get_Cardinality (Value : GValue) return Natural is
   begin
      return Get_Ptr (Value).Cardinality;
   end Get_Cardinality;

   function Is_Defined (Value : GValue) return Boolean is
   begin
      return Get_Ptr (Value) /= null;
   end Is_Defined;

   function Is_Feature_Value (Value : GValue) return Boolean is
   begin
      return
      (  Value.G_Type = GType_Feature_Value
      and then
         Get_Ptr (Value) /= null
      );
   end Is_Feature_Value;

   function Is_Classification (Value : GValue) return Boolean is
      This : constant Value_Object_Ptr := Get_Ptr (Value);
   begin
      return
      (  This /= null
      and then
         This.all in Classification_Object'Class
      );
   end Is_Classification;

   function Is_Intuitionistic_Set (Value : GValue) return Boolean is
      This : constant Value_Object_Ptr := Get_Ptr (Value);
   begin
      return
      (  This /= null
      and then
         This.all in Intuitionistic_Set_Object'Class
      );
   end Is_Intuitionistic_Set;

   function Is_Set (Value : GValue) return Boolean is
      This : constant Value_Object_Ptr := Get_Ptr (Value);
   begin
      return This /= null and then This.all in Set_Object'Class;
   end Is_Set;

   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Fuzzy.Set
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This = null
               or else
                  This.Cardinality /= Data'Length
               or else
                  This.Use_Count > 1
               or else
                  This.all not in Set_Object'Class
               )
            then
               This := new Set_Object (Data'Length);
               Set_Ptr (Value, This);
            end if;
            Set_Object (This.all).Value := Data;
            This.Feature := Feature;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Fuzzy.Intuitionistic.Set
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This = null
               or else
                  This.Cardinality /= Data.Cardinality
               or else
                  This.Use_Count > 1
               or else
                  This.all not in Intuitionistic_Set_Object'Class
               )
            then
               This :=
                  new Intuitionistic_Set_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            Intuitionistic_Set_Object (This.all).Value := Data;
            This.Feature := Feature;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Classification
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This = null
               or else
                  This.Cardinality /= Data.Cardinality
               or else
                  This.Use_Count > 1
               or else
                  This.all not in Classification_Object'Class
               )
            then
               This := new Classification_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            Classification_Object (This.all).Value := Data;
            This.Feature := Feature;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set
             (  Value : in out GValue;
                Data  : Fuzzy.Set
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This /= null
               and then
                  (  This.all not in Set_Object'Class
                  or else
                     This.Cardinality /= Data'Length
               )  )
            then
               raise Constraint_Error;
            elsif This = null or else This.Use_Count > 1 then
               This := new Set_Object (Data'Length);
               Set_Ptr (Value, This);
            end if;
            Set_Object (This.all).Value := Data;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set
             (  Value : in out GValue;
                Data  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This /= null
               and then
                  (  This.all not in Intuitionistic_Set_Object'Class
                  or else
                     This.Cardinality /= Data.Cardinality
               )  )
            then
               raise Constraint_Error;
            elsif This = null or else This.Use_Count > 1 then
               This :=
                  new Intuitionistic_Set_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            Intuitionistic_Set_Object (This.all).Value := Data;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set
             (  Value : in out GValue;
                Data  : Classification
             )  is
   begin
      if Value.G_Type = GType_Feature_Value then
         declare
            This : Value_Object_Ptr := Get_Ptr (Value);
         begin
            if (  This /= null
               and then
                  (  This.Cardinality /= Data.Cardinality
                  or else
                     This.all not in Classification_Object'Class
               )  )
            then
               raise Constraint_Error;
            elsif This = null or else This.Use_Count > 1 then
               This := new Classification_Object (Data.Cardinality);
               Set_Ptr (Value, This);
            end if;
            Classification_Object (This.all).Value := Data;
         end;
      else
         raise Constraint_Error;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            "Fault: " & Exception_Information (Error) & Where ("Set")
         );
         raise;
   end Set;

   procedure Set_Undefined (Value : in out GValue) is
   begin
      if Value.G_Type = GType_Feature_Value then
         Set_Ptr (Value, null);
      else
         raise Constraint_Error with "Wrong type";
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Undefined")
         )  );
         raise;
   end Set_Undefined;

end GLib.Values.Feature_Value;
