--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy.feature                           Luebeck            --
--  Implementation                                 Spring, 2007       --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Fuzzy.Abstract_Edit.Handle;  use Fuzzy.Abstract_Edit.Handle;
with Fuzzy.Abstract_Edit.Named;   use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Gtk_Icon_Factory;      use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;               use GLib.Messages;
with Gtk.Widget;                  use Gtk.Widget;

package body Gtk.Fuzzy_Feature is
   use Entry_Edit_Handles;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature." & Name;
   end Where;

   function Create
            (  Feature : Feature_Handle;
               Input   : Input_Parameters'Class  := Input_Defaults;
               Output  : Output_Parameters'Class := Output_Defaults
            )  return Entry_Domain is
      Result : constant Entry_Domain := Ref (new Feature_Entry_Edit);
      Data   : Feature_Entry_Edit renames
                  Feature_Entry_Edit (Ptr (Result).all);
   begin
      if not Feature.Is_Valid then
         raise Constraint_Error;
      end if;
      Data.Feature := Feature;
      Data.Input   := Input_Parameters (Input);
      Data.Output  := Output_Parameters (Output);
      return Result;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Create")
         )  );
         return Result;
   end Create;

   function Get_Cardinality (Editor : Feature_Entry_Edit)
      return Natural is
   begin
      return Get_Cardinality (Editor.Feature);
   end Get_Cardinality;

   function Get_Default (Editor : Feature_Entry_Edit)
      return Fuzzy_Boolean is
   begin
      return Editor.Output.Default;
   end Get_Default;

   function Get_Domain
            (  Editor : Feature_Entry_Edit
            )  return Fuzzy.Abstract_Edit.Handle.Handle is
      Domain : constant Fuzzy.Abstract_Edit.Handle.Handle :=
                        Ref (new Domain_Description);
   begin
      Get_Domain
      (  Editor.Feature,
         Domain_Description (Ptr (Domain).all),
         Editor.Output
      );
      return Domain;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Domain")
         )  );
         return Domain;
   end Get_Domain;

   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Set
            )  return UTF8_String is
   begin
      return Image (Editor.Feature, Value, Editor.Output);
   end Image;

   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Set
            )  return UTF8_String is
   begin
      return Image (Editor.Feature, Value, Editor.Output);
   end Image;

   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Classification
            )  return UTF8_String is
   begin
      return Image (Editor.Feature, Value, Editor.Output);
   end Image;

   procedure Set_Button_Spacing
             (  Widget : not null access
                         Gtk_Fuzzy_Feature_Abstract_Factory_Record;
                Spacing : GUInt
             )  is
   begin
      null;
   end Set_Button_Spacing;

   procedure Set_Default
             (  Editor  : in out Feature_Entry_Edit;
                Default : Fuzzy_Boolean
             )  is
   begin
      Editor.Input.Default  := Default;
      Editor.Output.Default := Default;
   end Set_Default;

   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Set is
   begin
      return Value (Text, Editor.Feature, Editor.Input);
   end Value;

   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return Value (Text, Editor.Feature, Editor.Input);
   end Value;

   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return Value (Text, Editor.Feature, Editor.Input);
   end Value;

end Gtk.Fuzzy_Feature;
