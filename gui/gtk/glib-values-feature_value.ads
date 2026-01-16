--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Feature_Value                   Luebeck            --
--  Interface                                      Summer 2007       --
--                                                                    --
--                                Last revision :  12:57 12 Aug 2010  --
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
--  This package provides a GTK+ type GType_Feature_Value can be used to
--  place  a  fuzzy set, an intuitionistic fuzzy set or a classification
--  of any feature into a GTK+ value. This  is  a  kind  of  polymorphic
--  value which knows its domain set, from the  feature  of.  When  set,
--  both the value and the feature are provided.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;

with GLib.Values.Handle;
with Object.Handle;

package GLib.Values.Feature_Value is
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
   function Get (Value : GValue) return Fuzzy.Set;
   function Get (Value : GValue) return Fuzzy.Intuitionistic.Set;
   function Get (Value : GValue) return Classification;
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
-- Get_Feature -- Get feature from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- GType_Feature.
--
-- Returns :
--
--    A handle to the feature
--
-- Exceptions :
--
--    Constraint_Error - Type error, undefined value
--
   function Get_Feature (Value : GValue) return Feature_Handle;
--
-- GType_Feature_Value -- The GTK+ type used with fuzzy sets
--
   function GType_Feature_Value return GType;
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
-- Is_Feature_Value -- Check the value type
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value is of the designated type
--
   function Is_Feature_Value (Value : GValue) return Boolean;
--
-- Is_Classification -- Check the value type
-- Is_Intuitionisitc_Set
-- Is_Set
--
--    Value - A GTK+ value
--
-- Returns :
--
--    True if the value is defined and of the designated type
--
   function Is_Classification     (Value : GValue) return Boolean;
   function Is_Intuitionistic_Set (Value : GValue) return Boolean;
   function Is_Set                (Value : GValue) return Boolean;
--
-- Set -- Set into a GTK+ value
--
--    Value     - A GTK+ value
--  [ Feature ] - The feature (a handle to)
--    Data      - The fuzzy value
--
-- These procedures change the  value.  When the  parameter  Feature  is
-- omitted then the operation changes only the value. Otherwise it  sets
-- both the value and the feature. In the case of setting only the value
-- the  replaced  one  has  to  be defined and the new value cardinality
-- shall match  one  of  the  old  value  feature.  Constraint_Error  is
-- propagated otherwise.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, wrong Data cardinality
--
   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Fuzzy.Set
             );
   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Fuzzy.Intuitionistic.Set
             );
   procedure Set
             (  Value   : in out GValue;
                Feature : Feature_Handle;
                Data    : Classification
             );
   procedure Set
             (  Value : in out GValue;
                Data  : Fuzzy.Set
             );
   procedure Set
             (  Value : in out GValue;
                Data  : Fuzzy.Intuitionistic.Set
             );
   procedure Set
             (  Value : in out GValue;
                Data  : Classification
             );
--
-- Set_Undefined -- Set into a GTK+ value
--
--    Value - A GTK+ value
--
-- The value should have been initialized with GType_Feature_Value  type
-- prior to the call.
--
-- Exceptions :
--
--    Constraint_Error - Type error
--
   procedure Set_Undefined (Value : in out GValue);

private
   pragma Inline (Get);
   pragma Inline (Get_Cardinality);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Feature_Value);
   pragma Inline (Is_Classification, Is_Intuitionistic_Set, Is_Set);
   pragma Inline (Set_Undefined);
--
-- Fuzzy_Value_Object -- Reference  counted object to be referenced as a
--                       GValue
--
   type Value_Object (Cardinality : Positive) is
      new Standard.Object.Entity with
   record
      Feature : Feature_Handle;
   end record;
   type Value_Object_Ptr is access Value_Object'Class;

   package Value_Handles is
      new Standard.Object.Handle (Value_Object, Value_Object_Ptr);
   package Feature_Values is
      new GLib.Values.Handle
          (  Type_Name       => "GFeatureValue",
             Object_Type     => Value_Object,
             Object_Type_Ptr => Value_Object_Ptr,
             Handle_Type     => Value_Handles.Handle,
             Ptr             => Value_Handles.Ptr,
             Ref             => Value_Handles.Ref
          );
   function GType_Feature_Value return GType
      renames Feature_Values.Get_Type;

   type Set_Object (Cardinality : Positive) is
      new Value_Object (Cardinality) with
   record
      Value : Fuzzy.Set (1..Cardinality);
   end record;

   type Intuitionistic_Set_Object (Cardinality : Positive) is
      new Value_Object (Cardinality) with
   record
      Value : Fuzzy.Intuitionistic.Set (Cardinality);
   end record;

   type Classification_Object (Cardinality : Positive) is
      new Value_Object (Cardinality) with
   record
      Value : Classification (Cardinality);
   end record;

end GLib.Values.Feature_Value;
