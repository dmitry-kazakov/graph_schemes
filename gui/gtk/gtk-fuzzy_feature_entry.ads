--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature_Entry                     Luebeck            --
--  Interface                                      Summer, 2007       --
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
--
--  This   package   provides   a   widget   to   indicate  fuzzy  sets,
--  intuitionistic fuzzy sets and intuitionistic fuzzy  classifications.
--  The  widget's  visual appearance is of a combo box. The entry of the
--  combo  box  contains the fuzzy set rendered as a text. The drop down
--  part is one provided by the Gtk_Fuzzy_Set widget.
--
with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with GLib.Values;                use GLib.Values;
with Gtk.Fuzzy_Set_Entry;        use Gtk.Fuzzy_Set_Entry;

package Gtk.Fuzzy_Feature_Entry is
   pragma Elaborate_Body (Gtk.Fuzzy_Feature_Entry);
--
-- Gtk_Fuzzy_Feature_Entry -- The widget type
--
   type Gtk_Fuzzy_Feature_Entry_Record is
      new Gtk_Fuzzy_Set_Entry_Record with private;
   type Gtk_Fuzzy_Feature_Entry is
      access all Gtk_Fuzzy_Feature_Entry_Record'Class;
--
-- Get_Feature -- The feature of
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the widget's feature
--
   function Get_Feature
            (  Widget : not null access Gtk_Fuzzy_Feature_Entry_Record
            )  return Feature_Handle;
--
-- Gtk_New -- Factory
--
--    Widget  - The result
--    Feature - The feature (a handle to)
--    Value   - The initial value indicate
--
-- When Value is GValue it can have GType_Set, Intuitionistic.GType_Set,
-- GType_Classification. Undefined values equivalent to an empty sets.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of the value
--
   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Feature_Entry;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget  - The result
--    Feature - The feature (a handle to)
--    Value   - To indicate
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of the value
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record'Class;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
--
-- Put -- Change the value indicated
--
--    Widget  - The widget
--    Feature - The feature (a handle to)
--    Value   - To indicate
--
-- These procedures change the domain and the indicated value. The value
-- type  and  cardinality  shall  be  conform  to  the domain. Otherwise
-- Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality
--
   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Fuzzy.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Fuzzy.Intuitionistic.Set;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : Classification;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
   procedure Put
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Entry_Record;
                Feature : Feature_Handle;
                Value   : GValue;
                Input   : Input_Parameters'Class  := Input_Defaults;
                Output  : Output_Parameters'Class := Output_Defaults
             );
private
   type Gtk_Fuzzy_Feature_Entry_Record is
      new Gtk_Fuzzy_Set_Entry_Record with null record;

end Gtk.Fuzzy_Feature_Entry;
