--                                                                    --
--  package Fuzzy.Feature           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
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
--
-- This  package  defines  the abstract base type Feature_Object for all
-- fuzzy  feature  types. All descendants of the type should be private,
-- because feature objects are accessed using  handles  defined  in  the
-- child package Fuzzy.Feature.Handle.
--
with Object.Archived;  use Object.Archived;
with Fuzzy.Logic;      use Fuzzy.Logic;
with Strings_Edit;     use Strings_Edit;

with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Generic_Segmented_Stack;
with Generic_Unbounded_Ptr_Array;
with Object.Handle;
with Stack_Storage.Mark_And_Release;
with Units;

package Fuzzy.Feature is
   pragma Elaborate_Body (Fuzzy.Feature);
--
-- Image_Type -- The basic characteristic images of an event
--
--    Has_In      - it has the original of a domain value in P(x(d)|p)
--    Has_Out     - its complement has the original P(x(d)|not p)
--    Has_Not     - it has the complement of the original P(not x(d)|p)
--    Has_Not_Out - its complement has the complement of the original
--                  P(not x(d)| not p)
--
   type Image_Type is (Has_In, Has_Out, Has_Not, Has_Not_Out);
--
-- Feature_Class -- The prefix of all feature object classes
--
   Feature_Class : constant String := "Feature.";
--
-- Set_Ptr -- Pointer to a fuzzy set
--
   type Set_Ptr is access Set;
   type String_Ptr is access String;
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);
--
-- Feature_ID -- A unique feature identifier
--
   type Feature_ID is new Integer;
   type Feature_Object;
--
-- Feature_Object -- The abstract base type for all feature type
--
--    Cardinality - Of the feature domain set
--
-- The fields of the object:
--
--    ID   - The unique ID of the feature
--    Self - A pointer to the object
--    Name - The name of
--
   type Feature_Object_Ptr is access Feature_Object'Class;
   for Feature_Object_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
   type Feature_Object (Cardinality : Positive) is abstract
      new Deposit with
   record
      ID       : Feature_ID;
      Self     : Feature_Object_Ptr;
      Name     : String_Ptr;
      Modified : Boolean := False;
   end record;
--
-- Feature_Data -- Context dependent feature data
--
-- The base type for feature-specific types containing writable data. An
-- instance of Feature_Data's descendant  can  be  used  to  store  some
-- context dependent data related to a  feature.  For  instance,  cached
-- values or current constraints of dependent features.
--
   type Feature_Data is abstract
      new Ada.Finalization.Limited_Controlled with private;
   type Feature_Data_Ptr is access Feature_Data'Class;
--
-- Context_Object -- Context dependent data
--
-- An   instance   of  Context_Object  contains  data  of  the  features
-- referenced in a context.
--
   type Context_Object is abstract new Object.Entity with private;
   type Context_Object_Ptr is access Context_Object;
--
-- Domain_Subset -- A crisp subset of the feature domain
--
-- It is implemented as a boolean array, which for each domain value has
-- true if it is the subset.
--
   type Domain_Subset is array (Positive range <>) of Boolean;
   pragma Pack (Domain_Subset);
--
-- Context_Stack -- The pool used to allocate frames of context data
--
   Context_Stack : Stack_Storage.Pool
                   (  Initial_Size => 2048,
                      Items_Number => 16
                   );
--
-- Context_Stack_Objects -- Mark and release stack
--
   package Context_Stack_Objects is
      new Stack_Storage.Mark_And_Release
          (  Stack_Storage.Pool'Class (Context_Stack)
          );
--
-- Value_Constraint -- A constraint imposed on the feature value(s)
--
-- The  feature  value  constraints  are  allocated  on  a  stack  pool.
-- Therefore  they should not be explicitly deallocated. Instead of this
-- one should create a Context_Snap object which will automatically save
-- and restore the context state upon its construction and destruction.
--
   type Value_Constraint (<>) is
      new Context_Stack_Objects.Pool_Object with private;
   type Value_Constraint_Ptr is access Value_Constraint'Class;
   for Value_Constraint_Ptr'Storage_Pool use Context_Stack;
--
-- Context_Snap -- Contexts snap shot
--
-- When  created  an  object  of Context_Snap memorizes the state of the
-- context  stack  pool and restores it upon destruction by deleting all
-- allocated there objects.
--
   subtype Context_Snap is Context_Stack_Objects.Pool_Mark;
--
-- Input_Parameters -- Used during feature values input
--
--    Base        - Of the numbers
--    Default     - The default truth values
--    Mode        - The character set
--    Get_Units   - Dimensioned values require a unit specification
--    Quote_Units - Unit specification to be put into []
--
-- Any parameter is ignored when not applicable.
--
   type Input_Parameters is tagged record
      Base        : NumberBase     := 10;
      Default     : Fuzzy_Boolean  := Certain_True;
      Mode        : Units.Code_Set := Units.UTF8_Set;
      Get_Units   : Boolean        := False;
      Quote_Units : Boolean        := False;
   end record;
--
-- Output_Parameters -- Used during feature values output
--
--    Base        - Of the numbers
--    Default     - The default truth values
--    Mode        - The character set
--    Abs_Small   - Absolute small for floating-point output
--    Rel_Small   - Relative small for floating-point output
--    Put_Plus    - Forces + before positive values
--    Put_Units   - Forces unit specification
--    Quote_Units - Unit specification to be put into []
--    Use_SI      - Forces output in SI units
--    Use_Derived - Allows use of drived units in output
--
-- Any parameter is ignored when not applicable.
--
   type Output_Parameters is tagged record
      Base        : NumberBase     := 10;
      Default     : Fuzzy_Boolean  := Certain_True;
      Mode        : Units.Code_Set := Units.UTF8_Set;
      Abs_Small   : Integer        :=-MaxSmall;
      Rel_Small   : Positive       := MaxSmall;
      Put_Units   : Boolean        := False;
      Put_Plus    : Boolean        := False;
      Quote_Units : Boolean        := False;
      Use_SI      : Boolean        := False;
      Use_Derived : Boolean        := True;
   end record;
--
-- Create_Constraint -- Create a new feature constraint in a context
--
--    Feature - The feature
--    Context - To put the constraint in
--    Allowed - The initial state of the constraint
--
-- This procedure is used to put a new constraint on the feature  values
-- in a context. It is dispatching on the parameter Feature  so  that  a
-- feature implementation might override it. The standard implementation
-- creates a new constraint. The parameter Allowed specifies the initial
-- state of the constraint. If True,  then  the  constraint  allows  all
-- values  allowed  at  the  moment.  Otherwise it disallows all feature
-- values.   Once   created   a   constraint   can   be  modified  using
-- Set_Constraint and Set_Range procedures.
--
   procedure Create_Constraint
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             );
--
-- Create_Data -- Create feature data
--
--    Feature - The feature
--
-- This  function  is  used  allocate  a  data block associated with the
-- feature. The data block is allocated when necessary. It can  be  used
-- to  store feature-specific data which have to be stored in a context.
-- The caller is responsible to free the allocated block. It is legal to
-- return null if no data needed for the feature.
--
-- Returns :
--
--    A pointer to the data in the data block or null
--
   function Create_Data (Feature : Feature_Object)
      return Feature_Data_Ptr is abstract;
--
-- Finalize -- Destructor
--
--    Feature - The feature
--
-- The feature ID is released, so that it can be reused.
--
   procedure Finalize (Feature : in out Feature_Object);
--
-- Finalize -- Destructor
--
--    Constraint - The constraint to be removed
--
-- The constraint is removed from the context it was applied.
--
   procedure Finalize (Constraint : in out Value_Constraint);
--
-- Free -- Fuzzy set using its pointer
--
   procedure Free is new Ada.Unchecked_Deallocation (Set, Set_Ptr);
--
-- Get -- Get image of a value in the feature space
--
--    Feature - The feature
--    Context - The context of the data
--    Image   - The characteristic to get
--
-- This function returns a fuzzy subset of the feature domain  set.  The
-- subset  corresponds  to the characteristic specified by the parameter
-- Image.
--
-- Returns :
--
--    Fuzzy set
--
-- Exceptions :
--
--    Constraint_Error - No feature data present
--
    function Get
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Set is abstract;
--
-- Get -- Get image of a feature (one domain value)
--
--    Feature - The feature
--    Value   - The feature domain value
--    Context - The context of the data
--    Image   - The characteristic to get
--
-- This function is an equivalent to:
--
--    Get (Feature, Data, Image) (Value)
--
-- Returns :
--
--    The possibility (value of the membership function)
--
-- Exceptions :
--
--    Constraint_Error - Illegal value or no data in the context
--
   function Get
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence is abstract;
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
   function Get_Constraint
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset;
--
-- Get_Data -- Get the feature data from a context
--
--    Context - To get the data from
--    Feature - The feature
--
-- This function is used to get  the  data  block  associated  with  the
-- feature.   The   data   block   is  allocated  when  necessary  using
-- Create_Data. So this function should never  fail.  This  function  is
-- dispatching on the context parameter.
--
-- Returns :
--
--    A pointer to the data in the context
--
   function Get_Data
            (  Context : not null access Context_Object;
               Feature : Feature_Object'Class
            )  return Feature_Data_Ptr is abstract;
--
-- Get_ID -- Get feature ID
--
--    Feature - The feature
--
-- Returns :
--
--    The ID of the feature
--
   function Get_ID (Feature : Feature_Object'Class) return Feature_ID;
--
-- Get_Name -- Get feature name
--
--    Feature - The feature
--
-- Returns :
--
--    The name of the feature
--
   function Get_Name (Feature : Feature_Object) return String;
--
-- Get_Number_Of_Features -- Get total number of features
--
-- Returns :
--
--   The number of all features existing at the moment
--
   function Get_Number_Of_Features return Natural;
--
-- Get_Range -- Get the range of the feature domain values
--
--    Source     - The string to be processed
--    Pointer    - The current position in the string
--    Feature    - The feature
--    From       - The index of the lower bound of the range
--    To         - The index of the upper bound
--    Exclusive  - The exclusive, non-overlapping range
--    Parameters - The parameters controlling string parsing
--
-- This procedure is called to get a range of feature  values  from  the
-- string Source. The process starts from Source (Pointer).  Pointer  is
-- advanced to the string position following the value. The procedure is
-- abstract dispatching in the parameter Feature. The result of input is
-- returned through the parameters From and To. From..To is the interval
-- of feature values recognized in Source. From =  To  if  the  interval
-- consists of one value. Any implementation should return From <= To <=
-- Cardinality  of  the  feature.  The  parameter  Exclusive  is true if
-- From..To  may not overlap with other ranges. This is usually the case
-- for nominal features and otherwise for numeric ones.  For  linguistic
-- variables named ranges are exclusive, the  numeric  ranges  from  the
-- domain  set are not. Parameters controls parsing process. It is up to
-- an implementation to decide which fields of Parameters should be used
-- and in which way.
--
-- Exceptions:
--
--    Constraint_Error - A value or cardinality is not in range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--    Unit_Error       - Error in units
--
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is abstract;
--
-- Initialize -- Constructor
--
--    Feature - The feature
--
-- The feature receives a unique ID.
--
   procedure Initialize (Feature : in out Feature_Object);
--
-- Is_Computed - Check if a feature is computed from on another
--
--    Feature - A feature to test on being computed
--    Source  - The source feature
--
-- This  function  can be used to test whether the values of Feature are
-- computed  from  the values of Source. For any feature X it is assumed
-- that Is_Computed (X, X) is true. An implementation should also ensure
-- its  transitivity:  Is_Computed  (X,  Y)  and  Is_Computed  (Y, Z) =>
-- Is_Computed (X, Z).
--
-- Returns :
--
--    True if Feature values are computed from Source
--
   function Is_Computed
            (  Feature : Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is abstract;
--
-- Is_Defined - Check if a context defines an image
--
--    Feature - The feature
--    Context - The context of the data
--    Image   - The characteristic to check
--
-- This function can be used to query whether  the  specified  image  is
-- defined by  the  context.  An  undefined  image  when  requested  are
-- returned as all 1.
--
-- Returns :
--
--    True if defined
--
-- Exceptions :
--
--    Constraint_Error - No feature data present
--
   function Is_Defined
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is abstract;
--
-- Is_Known - Check if an image is known
--
--    Feature - The feature
--    Context - The context of the data
--    Image   - The characteristic to check
--
-- This  function checks if the specified image is unknown, i.e. has all
-- values of the membership function 1.
--
-- Returns :
--
--    False if Get (Feature, Context, Image) would return all 1
--
-- Exceptions :
--
--    Constraint_Error - No feature data in the context
--
   function Is_Known
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is abstract;
--
-- Is_Modified -- Overrides Object.Archived...
--
   function Is_Modified (Feature : Feature_Object) return Boolean;
--
-- Put_Range -- Put a feature values range into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Feature     - The feature
--    From        - The index of the lower bound of the range
--    To          - The index of the upper bound
--    Parameters  - The output parameters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places  a  range of feature values From..To into the
-- output string Destination. Usually singletons are put not  as  ranges
-- but  as  singletons.  The string is written starting from Destination
-- (Pointer).  Pointer  is  advanced  to  position  after the end of the
-- output  field.  Parameters  controls  the output. The parameter Field
-- determines the width of  the  output  field.  Zero  width  means  the
-- minimal  possible  width. If Field is not zero Justify determines the
-- way the  value  should  be  aligned  within  the  output  field.  The
-- parameter Fill  is  then  the  fill  character.  Constraint_Error  is
-- propagated  when From..To is not in 1..Feature cardinality.
--
-- Exceptions:
--
--    Constraint_Error - Wrong cardinality
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is abstract;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   procedure Reset_Modified (Feature : in out Feature_Object);
--
-- Set_Constraint -- On a feature domain value
--
--    Feature - The feature which values constraint to change
--    Context - The constraint belongs to
--    Value   - The domain value which constraint to change
--    Allowed - Allows the domain value if True, otherwise disallows it
--
-- The effect of this procedure is changing the  feature  constraint  so
-- that it will or not allow Value. When Allow is  True,  the  stack  of
-- active constraints is inspected and the value is allowed only  of  it
-- is  not  disallowed  by  any  of  the  contsraints.  The procedure is
-- dispatching on the parameter  Feature  to  support  feature-dependent
-- semantics.  If no constraint was imposed on the feature it is created
-- using Create_Constraint with the parameter Allowed => not Allowed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal domain value
--
   procedure Set_Constraint
             (  Feature : Feature_Object;
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
--    Constraint_Error - Illegal domain value in the range
--
   procedure Set_Constraint_Range
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             );
--
-- Set_Data -- Set the feature data in a context
--
--    Context - To get the data from
--    Feature - The feature
--    Data    - The data to be set (a pointer to)
--
-- This  procedure places a newly allocated data block of a feature into
-- a context. The context is responsible to free the object  pointed  by
-- Data upon context destruction. This procedure has to  be  implemented
-- by  a type derived from Context_Object. The object pointed by Data is
-- destroyed if any exceptions propagates.
--
-- Exceptions :
--
--    Program_Error - The  context  already  has  a  data  block for the
--                    feature
--
   procedure Set_Data
             (  Context : in out Context_Object;
                Feature : Feature_Object'Class;
                Data    : in out Feature_Data_Ptr
             )  is abstract;
--
-- Set_Name -- Set feature name
--
--    Feature - The feature
--    Name    - The new name to set
--
   procedure Set_Name
             (  Feature : in out Feature_Object;
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
--    Constraint_Error - Illegal domain values range
--
   procedure Set_Range
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             );
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Feature_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Feature_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a feature
--
   function To_Feature_Object_Ptr (Ptr : Deposit_Ptr)
      return Feature_Object_Ptr;
--
-- Undefine -- Undefine a value of a feature in a context
--
--    Data    - The feature data
--    Context - The context
--
-- This  procedure is used to undefine a feature in the context. Defined
-- feature images are usually taken directly from the context. Undefined
-- images  are  requested  from  the  teaching  set  associated with the
-- context.
--
   procedure Undefine
             (  Data    : in out Feature_Data;
                Context : in out Context_Object'Class
             )  is abstract;
--
-- < -- Comparison of pointers
--
-- Pointers  to  feature  objects  are ordered as the identifiers of the
-- objects they point to. Null is less than any valid pointer.
--
   function "<" (Left, Right : Feature_Object_Ptr) return Boolean;

private
   pragma Inline (Create_Constraint);
   pragma Inline (Get_Constraint);
   pragma Inline (Get_ID);
   pragma Inline (Get_Name);
   pragma Inline (Set_Constraint);
   pragma Inline (Set_Constraint_Range);
   pragma Inline (Set_Range);
   pragma Inline (To_Feature_Object_Ptr);
   pragma Inline ("<");
--
-- Feature_Data -- Implementation
--
-- The field Constraint points to an  array  of  bits,  which  for  each
-- domain value index contains True if this value belongs to the current
-- constraint.
--
   type Feature_Data is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Constraint : Value_Constraint_Ptr;
   end record;

   type Feature_Data_Ptr_Array is
      array (Feature_ID range <>) of Feature_Data_Ptr;

   package Feature_Data_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Feature_ID,
             Object_Type           => Feature_Data'Class,
             Object_Ptr_Type       => Feature_Data_Ptr,
             Object_Ptr_Array_Type => Feature_Data_Ptr_Array
          );

   type Context_Object is abstract new Object.Entity with
   record
      Data : Feature_Data_Arrays.Unbounded_Ptr_Array;
   end record;

   type Value_Constraint
        (  Cardinality : Positive;
           Data        : not null access Feature_Data'Class
        )  is new Context_Stack_Objects.Pool_Object with
   record
      Parent   : Value_Constraint_Ptr;
      From     : Positive := 1;
      To       : Positive := Cardinality;
      Allowed  : Domain_Subset (1..Cardinality);
   end record;
--
-- Handles -- To Feature_Objects
--
   package Handles is
      new Object.Handle (Feature_Object, Feature_Object_Ptr);

   package ID_Stacks is
      new Generic_Segmented_Stack (Positive, Feature_ID, 0);

end Fuzzy.Feature;
