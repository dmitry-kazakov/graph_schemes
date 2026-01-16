--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Integer_Factory                          Winter, 2007       --
--  Interface                                                         --
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
--
--  This package provides a  GTK+  widget  for  viewing/editing  integer
--  fuzzy features. It has the look:
--      .________._________.________.
--      |        |         |        |
--      |  From  |  Value  | Status |
--      |________|_________|________|
--      |        |         |        |
--      |    To  |  Value  | Status |
--      |________|_________|________|
--
with Gtk.Fuzzy_Object;  use Gtk.Fuzzy_Object;
with Gtk.Box;           use Gtk.Box;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Label;         use Gtk.Label;

with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Integer_Factory is

   Integer_Factory_Class_Name : constant UTF8_String :=
                                   Prefix & "IntegerFeatureFactory";
--
-- Gtk_Integer_Factory_Record -- The widget type
--
-- Style properties :
--
--    bounds-error           - The error text shown when the lower range
--                             bound is  greater  than  the  upper  one.
--                             String, the default is  "The  field  from
--                             cannot exceed to".
--    from-label             - The label of the lower bound. String, the
--                             default is "From".
--    missing-from-error     - The error text shown when the lower bound
--                             is  missing.  String, the default is "The
--                             field   from   must  contain  an  integer
--                             number".
--    missing-to-error       - The error text shown when the upper bound
--                             is  missing.  String, the default is "The
--                             field to must contain an integer number".
--    non-numeric-from-error - The  eror text shown when the upper bound
--                             field does not contain  a  valid  integer
--                             number. String, the default is "The field
--                             from must contain an integer number".
--    non-numeric-to-error   - The  eror text shown when the lower bound
--                             field does not contain  a  valid  integer
--                             number. String, the default is "The field
--                             to must contain an integer number".
--    overflow-from-error    - The error text shown when the lower bound
--                             is too big. String, the default  is  "The
--                             field from contains a too large number".
--    overflow-to-error      - The error text shown when the upper bound
--                             is too big. String, the default  is  "The
--                             field from contains a too large number".
--    to-label               - The label of the upper bound. String, the
--                             default is "To".
--
--  + Styles from Install_Hints_Style_Properties
--
   type Gtk_Integer_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Integer_Factory is
      access all Gtk_Integer_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Integer_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Integer_Factory_Record
            )  return Boolean;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Factory
--
--    Widget   - The result
--    Feature  - To initialize the fields of
--    Editable - Initial state
--
-- The  parameter  Feature  can  be  an invalid handle. In that case the
-- fields  left  uninitialized.  When  Feature is valid it must refer an
-- integer feature, otherwise Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Gtk_New
             (  Widget   : out Gtk_Integer_Factory;
                Feature  : Feature_Handle := No_Feature;
                Editable : Boolean        := True
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - The widget to initialize
--    Feature  - To initialize the fields of
--    Editable - Initial state
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Initialize
             (  Widget   : not null access
                           Gtk_Integer_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget : not null access Gtk_Integer_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access Gtk_Integer_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Integer_Factory_Record
             );
private
   type Gtk_Integer_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      From_Label : Gtk_Label;
      From_Entry : Gtk_Entry;
      From_Hint  : Gtk_Box;
      To_Label   : Gtk_Label;
      To_Entry   : Gtk_Entry;
      To_Hint    : Gtk_Box;
      Modified   : Boolean := False;
   end record;

   procedure Changed
             (  Edit   : access Gtk_Entry_Record'Class;
                Widget : Gtk_Integer_Factory
             );

   procedure Get
             (  Widget : access Gtk_Integer_Factory_Record;
                From   : out Integer;
                To     : out Integer
             );

   procedure Style_Updated
             (  Widget : access Gtk_Integer_Factory_Record'Class
             );

   package Handlers is
      new Gtk.Handlers.Callback (Gtk_Integer_Factory_Record);

   package Entry_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Entry_Record,
             Gtk_Integer_Factory
          );
end Gtk.Fuzzy_Feature.Integer_Factory;
