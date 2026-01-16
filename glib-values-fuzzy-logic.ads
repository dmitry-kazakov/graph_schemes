--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy.Logic                     Luebeck            --
--  Interface                                      Summer, 2006       --
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
--
--  This package provides confidence factors values for GTK+.  The  GTK+
--  type of the value is GType_Fuzzy_Boolean.
--
with Fuzzy.Logic;  use Fuzzy.Logic;

package GLib.Values.Fuzzy.Logic is
   pragma Elaborate_Body (GLib.Values.Fuzzy.Logic);
--
-- Get -- Get from a GTK+ value
--
--    Value - A GTK+ value
--
-- Returns :
--
--    The value of
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   function Get (Value : GValue) return Fuzzy_Boolean;
--
-- GType_Fuzzy_Boolean -- The GTK+ type used with Fuzzy_Booleans
--
   function GType_Fuzzy_Boolean return GType;
--
-- Is_Defined -- Check if the value is defined
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value of the designated type and defined
--
   function Is_Defined (Value : GValue) return Boolean;
--
-- Is_Fuzzy_Boolean -- Check the value type
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value is defined and of the designated type
--
   function Is_Fuzzy_Boolean (Value : GValue) return Boolean;
--
-- Set -- Set into a GTK+ value
--
--    Value - A GTK+ value
--    Level - Truth value to set
--
-- The value should have been initialized with GType_Fuzzy_Boolean  type
-- prior to the call.
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set (Value : in out GValue; Level : Fuzzy_Boolean);
--
-- Set_Undefined -- GTK+ value undefined
--
--    Value - A GTK+ value
--
-- The  value  should  have  been initialized with GType_Confidence type
-- prior to the call.
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set_Undefined (Value : in out GValue);
--
-- Fuzzy_Boolean_Type_Name -- The GTK+ type name
--
   Fuzzy_Boolean_Type_Name : constant String := "GFuzzyBoolean";

private
   use System;

   function Copy_Fuzzy_Boolean (Boxed : Address) return Address;
   pragma Convention (C, Copy_Fuzzy_Boolean);

   procedure Free_Fuzzy_Boolean (Boxed : Address);
   pragma Convention (C, Free_Fuzzy_Boolean);

   function Is_Fuzzy_Boolean (Value : GValue) return Boolean
      renames Is_Defined;

   pragma Inline (Is_Defined);
   pragma Inline (Is_Fuzzy_Boolean);
   pragma Inline (Set);
   pragma Inline (Set_Undefined);

end GLib.Values.Fuzzy.Logic;
