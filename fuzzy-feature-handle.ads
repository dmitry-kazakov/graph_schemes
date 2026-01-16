--                                                                    --
--  package Fuzzy.Feature.Handle    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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
--  This package defines the type Feature_Handle which is used to access
--  fuzzy  feature  objects.  It  defines  the public interface of fuzzy
--  features. A fuzzy feature is something that can be measured and then
--  used for classification and learning. The set of classes is  also  a
--  feature. The domain set of a feature is the set  of  values  it  can
--  take. A classification or estimation of a feature is the pair of two
--  fuzzy sets. The first set is the distribution of possibilities  over
--  the  feature  domain.  The second is the distribution of necessities
--  over the domain.
--
with Deposit_Handles;

package Fuzzy.Feature.Handle is
   pragma Elaborate_Body (Fuzzy.Feature.Handle);
--
-- Feature_Handle -- A handle to feature
--
   type Feature_Handle is tagged private;
   type Feature_Array is array (Integer range <>) of Feature_Handle;
--
-- Create_Constraint -- Create a new feature constraint in a context
--
--    Feature - A handle to the feature
--    Context - To put the constraint in
--    Allowed - The initial state of the constraint
--
-- This procedure is used to put a new constraint on the feature  values
-- in the context. The parameter Allowed specifies the initial state  of
-- the  constraint.  If  True,  then  the  constraint  allows all values
-- allowed at the moment. Otherwise it  disallows  all  feature  values.
-- Once  created  a  constraint can be modified using Set_Constraint and
-- Set_Range procedures. Some values can be disallowed, others saturated
-- to definite bounds. Normally constraints are used to  refine  results
-- in  case of dependent features. For instance, a constraint can be set
-- to   limit   the   number   of  variants  taken  into  consideration.
-- Set_Constraint and Set_Range create a constraint  when  necessary  by
-- calling Create_Constraint.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, like No_Feature
--
   procedure Create_Constraint
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             );
--
-- Delete -- Feature deletion request
--
--    Feature - A handle to the feature
--
-- This  procedure  requests  deletion  of  Feature.  Feature becomes an
-- invalid  handle.  The  feature itself is deleted if possible. Nothing
-- happens if Feature is not a valid handle.
--
   procedure Delete (Feature : in out Feature_Handle);
--
-- Get_Cardinality -- Get cardinality of a feature
--
--    Feature - A handle to the feature
--
-- The  cardinality of a feature is one of the feature domain set. It is
-- the number of elements in the set, or else the  number  of  different
-- elements there.
--
-- Returns :
--
--    The cardinality of the feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, like No_Feature
--
   function Get_Cardinality (Feature : Feature_Handle)
      return Positive;
--
-- Get_Class -- Get the feature class
--
--    Feature - The feature handle
--
-- Returns :
--
--    The feature class
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Class (Feature : Feature_Handle) return String;
--
-- Get_Constraint -- On the feature domain values
--
--    Feature - The feature which values constraint to change
--    Context - The constraint belongs to
--
-- This function returns the current constraint on the  feature  values.
-- That  is  a  Boolean  array  (1..Cardinality)  in which for a feature
-- domain value index True is set when the domain value  is  allowed  by
-- the constraint.
--
-- Returns :
--
--    The current constraint
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Constraint
            (  Feature : Feature_Handle;
               Context : access Context_Object'Class
            )  return Domain_Subset;
--
-- Get_ID -- Get feature ID
--
--    Feature - The feature handle
--
-- Each feature has an unique identification number.
--
-- Returns :
--
--    The ID of the feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, like No_Feature
--
   function Get_ID (Feature : Feature_Handle) return Feature_ID;
--
-- Get_Name -- Get feature name
--
--    Feature - The feature handle
--
-- Returns :
--
--    The name of the feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, like No_Feature
--
   function Get_Name (Feature : Feature_Handle) return String;
--
-- Invalidate -- Detach handle from the object
--
--    Feature - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Feature : in out Feature_Handle);
--
-- Is_Computed - Check if a feature is computed from on another
--
--    Feature - A feature to test on being computed
--    Source  - The source feature
--
-- This  function  can be used to test whether the values of Feature are
-- computed  from  the values of Source. For any feature X it is assumed
-- that Is_Computed (X, X) is true.
--
-- Returns :
--
--    True if Feature values are computed from Source
--
-- Exceptions :
--
--    Constraint_Error - Either handle is invalid
--
   function Is_Computed
            (  Feature : Feature_Handle;
               Source  : Feature_Handle
            )  return Boolean;
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Feature - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Feature : Feature_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Feature - The handle
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Feature : Feature_Handle) return Feature_Object_Ptr;
--
-- Ref -- Get handle to a feature object
--
--    Feature - The feature object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Feature : Feature_Object_Ptr) return Feature_Handle;
--
-- Ref -- Set a handle to a feature object
--
--    Handle  - The handle to set
--    Feature - The feature object
--
   procedure Ref
             (  Handle  : in out Feature_Handle;
                Feature : Feature_Object_Ptr
             );
--
-- Set_Constraint -- On a feature domain value
--
--    Feature - The feature which values constraint to change
--    Context - The constraint belongs to
--    Value   - The domain value which constraint to change
--    Allowed - Allows the domain value if True, otherwise disallows it
--
-- The effect of this procedure is changing the  feature  constraint  so
-- that it will or will not  allow  Value  according  to  the  parameter
-- Allowed.  When  Allow  is  True,  the  stack of active constraints is
-- inspected and the value is allowed only of it is  not  disallowed  by
-- any of the contsraints. If no constraint was imposed on  the  feature
-- it  is created using Create_Constraint with the parameter Allowed set
-- to the value not Allowed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal domain value or handle
--
   procedure Set_Constraint
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                Value   : Positive;
                Allowed : Boolean := False
             );
--
-- Set_Constraint_Range -- On a feature domain value
--
--    Feature - The feature which values constraint to change
--    Context - The constraint belongs to
--    From    - The first domain value which constraint to change
--    To      - The lasst domain value which constraint to change
--    Allowed - Allows the range if True, otherwise disallows it
--
-- This procedure is a variant of Set_Constraint provided for ranges  of
-- domain values. When the range is empty, nothing happens.
--
-- Exceptions :
--
--    Constraint_Error - Illegal handle or a domain value in the range
--
   procedure Set_Constraint_Range
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             );
--
-- Set_Name -- Set feature name
--
--    Feature - The feature
--    Name    - The new name to set
--
-- Exceptions :
--
--    Constraint_Error - Illegal handle
--
   procedure Set_Name
             (  Feature : Feature_Handle;
                Name    : String
             );
--
-- Set_Range -- For the feature domain values
--
--    Feature - The feature which values constraint to change
--    Context - The constraint belongs to
--    From    - The lower bound of the domain values
--    To      - The upper bound of the domain values
--
-- The effect of this procedure is changing the  feature  constraint  so
-- that  all  values  outside  From..To will be saturated to the closest
-- boundary. If no constraint was imposed on the feature it  is  created
-- using Create_Constraint with the parameter Allowed => True.
--
-- Exceptions :
--
--    Constraint_Error - Illegal domain values range or invalid handle
--
   procedure Set_Range
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             );
--
-- To_Deposit_Handle -- Pointer conversion
--
--    Feature - A handle to
--
-- Returns :
--
--    Handle to archived object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Deposit_Handle (Feature : Feature_Handle)
      return Deposit_Handles.Handle;
--
-- To_Feature_Handle -- Handle conversion
--
--    Feature - A handle to
--
-- Returns :
--
--    A handle to the feature
--
-- Exceptions :
--
--    Constraint_Error - The object is not a feature, or invalid handle
--
   function To_Feature_Handle
            (  Feature : Deposit_Handles.Handle
            )  return Feature_Handle;
--
-- <, <=, =, >=, > -- Comparisons
--
--    Left  - The first argument
--    Right - The second argument
--
-- Only  valid  handles  are comparable. However it is exception-safe to
-- compare No_Feature for equality. In all other cases  Constraint_Error
-- is propagated.
--
-- Returns :
--
--    The result of comparison of the objects
--
-- Exceptions :
--
--    Constraint_Error - One of arguments is an invalid handle
--
   function "<"  (Left, Right : Feature_Handle) return Boolean;
   function "<=" (Left, Right : Feature_Handle) return Boolean;
   function "="  (Left, Right : Feature_Handle) return Boolean;
   function ">=" (Left, Right : Feature_Handle) return Boolean;
   function ">"  (Left, Right : Feature_Handle) return Boolean;
--
-- No_Feature -- An invalid feature handle
--
   No_Feature : constant Feature_Handle;

private
   pragma Inline (Create_Constraint);
   pragma Inline (Get_Cardinality);
   pragma Inline (Get_Class);
   pragma Inline (Get_Constraint);
   pragma Inline (Get_ID);
   pragma Inline (Get_Name);
   pragma Inline (Invalidate);
   pragma Inline (Is_Computed);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Ref);
   pragma Inline (Set_Constraint);
   pragma Inline (Set_Constraint_Range);
   pragma Inline (Set_Name);
   pragma Inline (Set_Range);
   pragma Inline (To_Deposit_Handle);
   pragma Inline (To_Feature_Handle);
   pragma Inline ("<=", "<", "=", ">", ">=");

   type Feature_Handle is new Handles.Handle with null record;

   No_Feature : constant Feature_Handle :=
                   (Handles.Handle with null record);

end Fuzzy.Feature.Handle;
