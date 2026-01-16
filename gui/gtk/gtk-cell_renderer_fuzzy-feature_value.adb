--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy.                    Luebeck            --
--        Feature_Value                            Summer, 2007       --
--  Implementation                                                    --
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
with Glib.Messages;           use Glib.Messages;
with Gtk.Fuzzy_Feature;       use Gtk.Fuzzy_Feature;
with Gtk.Missed;              use Gtk.Missed;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;

with GLib.Values.Feature_Value;
with GLib.Values.Fuzzy.Intuitionistic;
with Gtk.Fuzzy_Boolean_Drawing;

package body Gtk.Cell_Renderer_Fuzzy.Feature_Value is
   use Entry_Edit_Handles;

   Renderer_Type : GType := GType_Invalid;
   Any_Set_ID    : constant Property_ID := 7;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Cell_Renderer_Fuzzy.Feature_Value." & Name;
   end Where;

   procedure Class_Init (Class : GObject_Class) is
   begin
      Gtk.Cell_Renderer_Fuzzy.Class_Init (Class);
      Class_Install_Property
      (  Class,
         Any_Set_ID,
         Gnew_Boxed
         (  Name       => "feature-value",
            Boxed_Type => GLib.Values.Feature_Value.GType_Feature_Value,
            Nick       => "Fuzzy feature value",
            Blurb      => "A value of some fuzzy feature"
      )  );
   end Class_Init;

   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record
            )  return GValue is
      Value : GValue;
   begin
      Init (Value, GLib.Values.Feature_Value.GType_Feature_Value);
      GLib.Values.Feature_Value.Set_Undefined (Value);
      if Ptr (Cell.Domain).all in Feature_Entry_Edit'Class then
         if GLib.Values.Fuzzy.Is_Set (Cell.Value) then
            GLib.Values.Feature_Value.Set
            (  Value,
               Feature_Entry_Edit'Class
               (  Ptr (Cell.Domain).all
               ) .Feature,
               GLib.Values.Fuzzy.Get (Cell.Value)
            );
         elsif GLib.Values.Fuzzy.Intuitionistic.Is_Set (Cell.Value) then
            GLib.Values.Feature_Value.Set
            (  Value,
               Feature_Entry_Edit'Class
               (  Ptr (Cell.Domain).all
               ) .Feature,
               Fuzzy.Intuitionistic.Set'
               (  GLib.Values.Fuzzy.Intuitionistic.Get
                  (  Cell.Value
            )  )  );
         elsif GLib.Values.Fuzzy.Intuitionistic.
               Is_Classification (Cell.Value) then
            GLib.Values.Feature_Value.Set
            (  Value,
               Feature_Entry_Edit'Class
               (  Ptr (Cell.Domain).all
               ) .Feature,
               Fuzzy.Intuitionistic.Classification'
               (  GLib.Values.Fuzzy.Intuitionistic.Get
                  (  Cell.Value
            )  )  );
         end if;
      end if;
      return Value;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
         return Value;
   end Get;

   function Get_Feature
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record
            )  return Feature_Handle is
   begin
      if Ptr (Cell.Domain).all in Feature_Entry_Edit'Class then
         return
             Feature_Entry_Edit'Class (Ptr (Cell.Domain).all).Feature;
      else
         return No_Feature;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Feature")
         )  );
         return No_Feature;
   end Get_Feature;

   procedure Get_Property
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
      Feature : Feature_Handle;
   begin
      case Param_ID is
         when Any_Set_ID =>
            Value := Get (Cell);
         when others =>
            Get_Property
            (  Gtk_Cell_Renderer_Fuzzy_Record (Cell.all)'Access,
               Param_ID,
               Value,
               Property_Spec
            );
      end case;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Property")
         )  );
   end Get_Property;

   function Get_Type return Gtk_Type is
   begin
     if Renderer_Type = GType_Invalid then
        Renderer_Type :=
           Gtk.Cell_Renderer.Abstract_Renderer.Register
           (  Class_Name,
              Class_Init'Access
           );
     end if;
     return Renderer_Type;
   end Get_Type;

   procedure Gtk_New
             (  Cell       : out Gtk_Cell_Renderer_Fuzzy_Feature_Value;
                Input      : Input_Parameters'Class  := Input_Defaults;
                Output     : Output_Parameters'Class := Output_Defaults;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             )  is
   begin
      Cell := new Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
      begin
         Initialize (Cell, Input, Output, Value_Type);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Cell.Unref;
            Cell := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Cell : not null access
                   Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record'Class;
                Input      : Input_Parameters'Class;
                Output     : Output_Parameters'Class;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             )  is
      Domain : constant Entry_Domain := Ref (new Feature_Entry_Edit);
      Data   : Feature_Entry_Edit renames
                  Feature_Entry_Edit (Ptr (Domain).all);
   begin
      Data.Input  := Input_Parameters  (Input);
      Data.Output := Output_Parameters (Output);
      Initialize (Cell, Domain, Get_Type, Value_Type);
   end Initialize;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Value : GValue
             )  is
      use GLib.Values.Feature_Value;
   begin
      if (  Is_Feature_Value (Value)
         and then
            Ptr (Cell.Domain).all in Feature_Entry_Edit'Class
         )
      then
         --
         -- Handle valid cases of Feature_Value here
         --
         declare
            Data : Feature_Entry_Edit'Class renames
                      Feature_Entry_Edit'Class (Ptr (Cell.Domain).all);
         begin
            if Data.Feature /= Get_Feature (Value) then
               --
               -- The feature differs, so we change it too
               --
               if Ptr (Cell.Domain).Use_Count > 1 then
                  Cell.Domain :=
                     Create
                     (  Get_Feature (Value),
                        Data.Input,
                        Data.Output
                     );
               else
                  Data.Feature := Get_Feature (Value);
               end if;
            end if;
         end;
         if Is_Intuitionistic_Set (Value) then
            Put (Cell, Fuzzy.Intuitionistic.Set'(Get (Value)));
            return;
         elsif Is_Classification (Value) then
            Put
            (  Cell,
               Fuzzy.Intuitionistic.Classification'(Get (Value))
            );
            return;
         elsif Is_Set (Value) then
            Put (Cell, Fuzzy.Set'(Get (Value)));
            return;
         end if;
      end if;
      Put (Gtk_Cell_Renderer_Fuzzy_Record (Cell.all)'Access, Value);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Set_Feature
             (  Cell    : not null access
                          Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Feature : Feature_Handle
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      declare
         Data : Feature_Entry_Edit'Class renames
                Feature_Entry_Edit'Class (Ptr (Cell.Domain).all);
      begin
         if Data.Feature /= Feature then
            --
            -- The feature differs, so we change it too
            --
            if Ptr (Cell.Domain).Use_Count > 1 then
               Cell.Domain :=
                  Create (Feature, Data.Input, Data.Output);
            else
               Data.Feature := Feature;
            end if;
         end if;
         Set_Undefined (Cell.Value);
         Cell.Updated := True;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Feature")
         )  );
   end Set_Feature;

   procedure Set_Property
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Any_Set_ID =>
            Put (Cell, Value);
         when others =>
            Set_Property
            (  Gtk_Cell_Renderer_Fuzzy_Record (Cell.all)'Access,
               Param_ID,
               Value,
               Property_Spec
            );
      end case;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Property")
         )  );
   end Set_Property;

end Gtk.Cell_Renderer_Fuzzy.Feature_Value;
