--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Generalization_Combo             Luebeck            --
--  Interface                                      Autumn, 2008       --
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

with Fuzzy.Classifier;  use Fuzzy.Classifier;
with Gtk.Combo_Box;     use Gtk.Combo_Box;
with Gtk.Fuzzy_Object;  use Gtk.Fuzzy_Object;
with Gtk.List_Store;    use Gtk.List_Store;
with Gtk.Widget;        use Gtk.Widget;

with Gtk.Handlers;

package Gtk.Fuzzy_Generalization_Combo is
--
-- Class_Name -- The name of the widget class
--
   Class_Name : constant String := Prefix & "Generalization";
--
-- Gtk_Fuzzy_Generalization_Combo_Record -- The widget type
--
-- Style properties:
--
--    generalization-none    - The  label  of the no generalization mode
--                             selection  choice. String, the default is
--                             "None".
--    generalization-nearest - The label of the selection choice for the
--                             nearest  neighbour  generalization  mode.
--                             String, the default is "Nearest".
--    generalization-linear  - The  label of the selection choice of the
--                             linear  generalization  mode. String, the
--                             default is "Linear".
--
-- The widget  can  be  made  read-only  using  Set_Sensitive  with  the
-- parameter false.
--
   type Gtk_Fuzzy_Generalization_Combo_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Generalization_Combo is
      access all Gtk_Fuzzy_Generalization_Combo_Record'Class;
--
-- Get -- The widget state
--
--    Widget - The widget
--
-- Returns :
--
--    The generalization mode selected in the widget
--
   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Generalization_Combo_Record
            )  return Generalization_Mode;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--    Mode   - The initial generalization mode
--
   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Generalization_Combo;
                Mode   : Generalization_Mode
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--    Mode   - The initial generalization mode
--
-- When  a  new  type is derived from the base, this procedure has to be
-- called as a part of widget object initialization.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Generalization_Combo_Record'Class;
                Mode   : Generalization_Mode
             );
--
-- Set -- Change the widget state
--
--    Widget - The widget
--    Mode   - The generalization mode to set
--
-- Returns :
--
--    The generalization mode selected in the widget
--
   procedure Set
             (  Widget : not null access
                         Gtk_Fuzzy_Generalization_Combo_Record;
                Mode   : Generalization_Mode
             );
private
   type Gtk_Fuzzy_Generalization_Combo_Record is
      new Gtk_Combo_Box_Record with
   record
      List : Gtk_List_Store;
   end record;
--
-- Handlers -- Event handlers
--
   package Handlers is
       new Gtk.Handlers.Callback
           (  Gtk_Fuzzy_Generalization_Combo_Record
           );
--
-- Style_Updated -- Event handler
--
   procedure Style_Updated
             (  Widget : access
                         Gtk_Fuzzy_Generalization_Combo_Record'Class
             );

end Gtk.Fuzzy_Generalization_Combo;
