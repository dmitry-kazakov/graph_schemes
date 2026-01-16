--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature_Entry                     Luebeck            --
--  Implementation                                 Summer, 2007       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
with Fuzzy.Gtk_Icon_Factory;      use Fuzzy.Gtk_Icon_Factory;
with Glib.Messages;               use Glib.Messages;
with Gtk.Fuzzy_Feature;           use Gtk.Fuzzy_Feature;

with GNAT.Traceback.Symbolic;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature_Entry is
   use Gtk.Fuzzy_Set_Entry.Entry_Edit_Handles;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature_Entry." & Name;
   end Where;

   function Get_Feature
            (  Widget : not null access Gtk_Fuzzy_Feature_Entry_Record
            )  return Feature_Handle is
   begin
      return
         Feature_Entry_Edit'Class
         (  Ptr (Get_Domain (Widget)).all
         ) .Feature;
   end Get_Feature;

   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Widget := new Gtk_Fuzzy_Feature_Entry_Record;
      begin
         Initialize (Widget, Feature, Value, Input, Output);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Feature_Entry)")
            )  );
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Widget := new Gtk_Fuzzy_Feature_Entry_Record;
      begin
         Initialize (Widget, Feature, Value, Input, Output);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Feature_Entry)")
            )  );
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Widget := new Gtk_Fuzzy_Feature_Entry_Record;
      begin
         Initialize (Widget, Feature, Value, Input, Output);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Feature_Entry)")
            )  );
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Widget := new Gtk_Fuzzy_Feature_Entry_Record;
      begin
         Initialize (Widget, Feature, Value, Input, Output);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Feature_Entry)")
            )  );
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Initialize
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Initialize
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Initialize
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Initialize
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Initialize;

--     function Image
--              (  Widget : not null access
--                          Gtk_Fuzzy_Set_Entry_Record;
--                 Value  : Fuzzy.Set
--              )  return UTF8_String is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Image (Data.Feature, Value, Data.Output);
--     end Image;
--
--     function Image
--              (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
--                 Value  : Fuzzy.Intuitionistic.Set
--              )  return UTF8_String is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Image (Data.Feature, Value, Data.Output);
--     end Image;
--
--     function Image
--              (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
--                 Value  : Classification
--              )  return UTF8_String is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Image (Data.Feature, Value, Data.Output);
--     end Image;

   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Put
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Put;

   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Put
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Put;

   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Put
      (  Widget,
         Create (Feature, Input, Output),
         Value
      );
   end Put;

   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             )  is
   begin
      Put
      (  Widget,
         Create (feature, Input, Output),
         Value
      );
   end Put;

--     function Value
--              (  Widget : not null access
--                          Gtk_Fuzzy_Feature_Entry_Record;
--                 Text   : UTF8_String
--              )  return Fuzzy.Set is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Value (Text, Data.Feature, Data.Input);
--     end Value;
--
--     function Value
--              (  Widget : not null access
--                          Gtk_Fuzzy_Feature_Entry_Record;
--                 Text   : UTF8_String
--              )  return Fuzzy.Intuitionistic.Set is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Value (Text, Data.Feature, Data.Input);
--     end Value;
--
--     function Value
--              (  Widget : not null access
--                          Gtk_Fuzzy_Feature_Entry_Record;
--                 Text   : UTF8_String
--              )  return Classification is
--        Data : Feature_Entry_Edit'Class renames
--               Feature_Entry_Edit'Class (Ptr (Get_Domain (Widget)).all);
--     begin
--        return Value (Text, Data.Feature, Data.Input);
--     end Value;

end Gtk.Fuzzy_Feature_Entry;
