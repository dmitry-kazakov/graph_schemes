--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy.Intuitionistic            Luebeck            --
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

with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;

package GLib.Values.Fuzzy.Intuitionistic is
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
   function Get (Value : GValue) return Classification;
   function Get (Value : GValue)
      return Standard.Fuzzy.Intuitionistic.Set;
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
-- GType_Set -- The GTK+ type used with intuitionistic sets
--
   function GType_Set return GType;
--
-- GType_Classification -- The GTK+ type used with classifications
--
   function GType_Classification return GType;
--
-- Is_Classification -- Check the value type
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value is defined and of the designated type
--
   function Is_Classification (Value : GValue) return Boolean;
--
-- Is_Defined -- Check intuitionistic fuzzy set  or  classification  for
--               being defined
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
   procedure Set
             (  Value : in out GValue;
                Data  : Classification
             );
   procedure Set
             (  Value : in out GValue;
                Data  : Standard.Fuzzy.Intuitionistic.Set
             );
--
-- Set_Undefined -- Set into a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  should  have   been   initialized   with   GType_Set   or
-- GType_Classification type prior to the call.
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set_Undefined (Value : in out GValue);
--
-- Intuitionistic_Set_Type_Name            -- The GTK+ type names
-- Intuitionistic_Classification_Type_Name
--
   Intuitionistic_Set_Type_Name : constant String :=
      "GIntuitionisticFuzzySet";
   Intuitionistic_Classification_Type_Name : constant String :=
      "GIntuitionisticFuzzyClassification";

private
   pragma Inline (Get);
   pragma Inline (Get_Cardinality);
   pragma Inline (Is_Classification);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Set);
   pragma Inline (Set);
   pragma Inline (Set_Undefined);
--
-- Intuitionistic_Set_Object
--
   type Intuitionistic_Set_Object (Cardinality : Positive) is
      new Standard.Object.Entity with
   record
      Value : Standard.Fuzzy.Intuitionistic.Set (Cardinality);
   end record;
   type Intuitionistic_Set_Object_Ptr is
      access Intuitionistic_Set_Object'Class;

   package Intuitionistic_Set_Handles is
      new Standard.Object.Handle
          (  Intuitionistic_Set_Object,
             Intuitionistic_Set_Object_Ptr
          );
   package Intuitionistic_Set_Values is
      new GLib.Values.Handle
          (  Type_Name       => Intuitionistic_Set_Type_Name,
             Object_Type     => Intuitionistic_Set_Object,
             Object_Type_Ptr => Intuitionistic_Set_Object_Ptr,
             Handle_Type     => Intuitionistic_Set_Handles.Handle,
             Ptr             => Intuitionistic_Set_Handles.Ptr,
             Ref             => Intuitionistic_Set_Handles.Ref
          );
   function GType_Set return GType
      renames Intuitionistic_Set_Values.Get_Type;
--
-- Classification_Object
--
   type Classification_Object (Cardinality : Positive) is
      new Standard.Object.Entity with
   record
      Value : Classification (Cardinality);
   end record;
   type Classification_Object_Ptr is
      access Classification_Object'Class;

   package Classification_Handles is
      new Standard.Object.Handle
          (  Classification_Object,
             Classification_Object_Ptr
          );
   package Classification_Values is
      new GLib.Values.Handle
          (  Type_Name       => Intuitionistic_Classification_Type_Name,
             Object_Type     => Classification_Object,
             Object_Type_Ptr => Classification_Object_Ptr,
             Handle_Type     => Classification_Handles.Handle,
             Ptr             => Classification_Handles.Ptr,
             Ref             => Classification_Handles.Ref
          );
   function GType_Classification return GType
      renames Classification_Values.Get_Type;

end GLib.Values.Fuzzy.Intuitionistic;
