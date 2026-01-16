--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy                           Luebeck            --
--  Interface                                      Spring, 2007       --
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

with Fuzzy;  use Fuzzy;

with GLib.Values.Handle;
with Object.Handle;

package GLib.Values.Fuzzy is
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
--    Constraint_Error - Type error, undefined value
--
   function Get (Value : GValue) return Standard.Fuzzy.Set;
--
-- Get_Cardinality -- From a GTK+ value
--
--    Value - A GTK+ value
--
-- Returns :
--
--    The cardinality of
--
-- Exceptions :
--
--    Constraint_Error - Type error, undefined value
--
   function Get_Cardinality (Value : GValue) return Natural;
--
-- GType_Set -- The GTK+ type used with fuzzy sets
--
   function GType_Set return GType;
--
-- Is_Defined -- Check fuzzy set for being defined
--
--    Value - A GTK+ value
--
-- Undefined  values  are  set  using Set_Undefined procedure. These are
-- also initial values of.
--
-- Returns :
--
--    True if the value is a defined fuzzy set
--
   function Is_Defined (Value : GValue) return Boolean;
--
-- Is_Set -- Check the value type
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value is defined and of the designated type
--
   function Is_Set (Value : GValue) return Boolean;
--
-- Set -- Set into a GTK+ value
--
--    Value - A GTK+ value
--    Data  - The fuzzy set
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set (Value : in out GValue; Data : Standard.Fuzzy.Set);
--
-- Set_Undefined -- Set into a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  should have been initialized with GType_Set type prior to
-- the call.
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set_Undefined (Value : in out GValue);
--
-- Fuzzy_Set_Type_Name -- The GTK+ type name
--
   Fuzzy_Set_Type_Name : constant String := "GFuzzySet";

private
   pragma Inline (Get);
   pragma Inline (Get_Cardinality);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Set);
   pragma Inline (Set);
   pragma Inline (Set_Undefined);
--
-- Fuzzy_Set_Object -- Reference  counted  object  to be referenced as a
--                     GValue
--
   type Fuzzy_Set_Object (Cardinality : Positive) is
      new Standard.Object.Entity with
   record
      Value : Standard.Fuzzy.Set (1..Cardinality);
   end record;
   type Fuzzy_Set_Object_Ptr is access Fuzzy_Set_Object'Class;

   package Fuzzy_Set_Handles is
      new Standard.Object.Handle
          (  Fuzzy_Set_Object,
             Fuzzy_Set_Object_Ptr
          );
   package Fuzzy_Set_Values is
      new GLib.Values.Handle
          (  Type_Name       => Fuzzy_Set_Type_Name,
             Object_Type     => Fuzzy_Set_Object,
             Object_Type_Ptr => Fuzzy_Set_Object_Ptr,
             Handle_Type     => Fuzzy_Set_Handles.Handle,
             Ptr             => Fuzzy_Set_Handles.Ptr,
             Ref             => Fuzzy_Set_Handles.Ref
          );
   function GType_Set return GType
      renames Fuzzy_Set_Values.Get_Type;

end GLib.Values.Fuzzy;
