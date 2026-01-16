--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier.Generic_Handle.      Autumn, 2005       --
--           Generic_Output                                           --
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

with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
        Generic_Handle.Generic_Output is
   pragma Elaborate_Body
         (  Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
            Generic_Handle.Generic_Output
         );
--
-- Output_Class -- The class of name
--
   Output_Class : constant String := Feature_Class & "Output." & Suffix;
--
-- Create -- Create an output feature
--
--    Name    - Of the feature
--    Source  - A handle to the feature to defuzzify
--    Method  - The defuzzification method
--    Default - The default used when defuzzification fails
--
-- The  parameter  Name  is  the  feature  name. Source is a handle to a
-- feature  with the floating-point domain. The newly created feature is
-- exactly Source but also supports defuzzification by  the  defuzzifier
-- which handle is Method. Default is the defuzzification default.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or not a float feature
--    Unit_Error       - Incompatible units of Source and Default
--
   function Create
            (  Name    : String;
               Feature : Feature_Handle;
               Method  : Defuzzifier_Handle;
               Default : Measure
            )  return Feature_Handle;
--
-- Defuzzify -- A fuzzy set
--
--    Feature   - Object or a handle to it
--    Value     - The value to be accumulated
--  [ Default ] - The returned on defuzzification errors
--
-- These functions evaluate the accumulated result  of  a  fuzzy  domain
-- subset.  Value is the subset. Constraint_Error is propagated when the
-- cardinality of Value differs from one of Feature. When the  parameter
-- Default  specified  it  overrides  the  default value of the feature.
-- Constraint_Error is propagated when Default has a dimension different
-- from one of Feature.
--
-- Returns :
--
--    A dimensioned variable representing the accumulated result
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--
   function Defuzzify
            (  Feature : Feature_Object'Class;
               Value   : Fuzzy.Set
            )  return Measure;
   function Defuzzify
            (  Feature : Feature_Object'Class;
               Value   : Fuzzy.Set;
               Default : Measure
            )  return Measure;
   function Defuzzify
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set
            )  return Measure;
   function Defuzzify
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set;
               Default : Measure
            )  return Measure;
--
-- Get_Default -- Get the defuzzification default
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The default value
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--
   function Get_Default
            (  Feature : Feature_Object'Class
            )  return Measure;
   function Get_Default
            (  Feature : Feature_Handle
            )  return Measure;
--
-- Get_Defuzzifier -- Get the defuzzification method
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    A handle to the defuzzifier
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--
   function Get_Defuzzifier
            (  Feature : Feature_Object'Class
            )  return Defuzzifier_Handle;
   function Get_Defuzzifier
            (  Feature : Feature_Handle
            )  return Defuzzifier_Handle;
--
-- Get_Source -- Get the source feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    A handle to the source feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--
   function Get_Source
            (  Feature : Feature_Object'Class
            )  return Float_Handles.Handle;
   function Get_Source
            (  Feature : Feature_Handle
            )  return Float_Handles.Handle;
--
-- Is_Output -- Check if a handle refers to an output feature
--
--    Feature - Object or a handle to it
--
-- For an invalid handle, false is the result.
--
-- Returns :
--
--    True if Feature refers to a lingustic feature
--
   function Is_Output (Feature : Feature_Handle)
      return Boolean;
   function Is_Output (Feature : Feature_Object'Class)
      return Boolean;
--
-- Set_Default -- Set the defuzzification default
--
--    Feature - Object or a handle to it
--    Default - The value to set
--
-- Returns :
--
--    The default value
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--    Unit_Error       - Incompatible dimension of the default
--
   procedure Set_Default
             (  Feature : in out Feature_Object'Class;
                Default : Measure
             );
   procedure Set_Default
             (  Feature : in out Feature_Handle;
                Default : Measure
             );
--
-- Set_Defuzzifier -- Set the defuzzification method
--
--    Feature - Object or a handle to it
--    Method  - A handle to
--
-- Returns :
--
--    The default value
--
-- Exceptions :
--
--    Constraint_Error - Invalid, object or value
--
   procedure Set_Defuzzifier
             (  Feature : in out Feature_Object'Class;
                Method  : Defuzzifier_Handle
             );
   procedure Set_Defuzzifier
             (  Feature : in out Feature_Handle;
                Method  : Defuzzifier_Handle
             );

private
   pragma Inline (Is_Output);
--
-- Output_Feature_Object -- The feature object
--
   type Output_Feature_Object is new Domain_Feature_Object with record
      Source  : Float_Handles.Handle;
      Method  : Defuzzifier_Handle;
      Default : Measure;
   end record;
--
-- Accumulate -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Accumulate
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measures.Variable_Measure;
--
-- Classify -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Create_Constraint -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Create_Constraint
             (  Feature : Output_Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             );
--
-- Create_Data -- Overrides Fuzzy.Feature...
--
   overriding
   function Create_Data (Feature : Output_Feature_Object)
      return Feature_Data_Ptr;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Output_Feature_Object)
      return String;
--
-- Get_Constraint -- Overrides Fuzzy.Feature...
--
   overriding
   function Get_Constraint
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset;
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Output_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Feature : Output_Feature_Object;
                List    : in out Deposit_Container'Class
             );
--
-- Get_Scale -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale (Feature : Output_Feature_Object)
      return Measure;
--
-- Get_Scale_Text -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale_Text
            (  Feature    : Output_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Get_Unit -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Unit (Feature : Output_Feature_Object)
      return Unit;
--
-- Get_Variable -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Variable
            (  Feature : Output_Feature_Object;
               Value   : Positive
            )  return Variable_Measure;
--
-- Is_Computed -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Computed
            (  Feature : Output_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean;
--
-- Is_Defined - Overrides Fuzzy.Feature...
--
   overriding
   function Is_Defined
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Overrides Fuzzy.Feature...
--
   overriding
   function Is_Known
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Domain_Linguistic -- Overrides
--                         Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Is_Domain_Linguistic (Feature : Output_Feature_Object)
      return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Output_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Output_Feature_Object
             );
--
-- To_Set -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

   pragma Inline (Accumulate);
   pragma Inline (Classify);
   pragma Inline (Create_Constraint);
   pragma Inline (Create_Data);
   pragma Inline (Get);
   pragma Inline (Get_Class);
   pragma Inline (Get_Constraint);
   pragma Inline (Get_Range);
   pragma Inline (Get_Scale);
   pragma Inline (Get_Scale_Text);
   pragma Inline (Get_Unit);
   pragma Inline (Get_Variable);
   pragma Inline (Is_Computed);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Known);
   pragma Inline (Put_Range);
   pragma Inline (To_Set);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle.Generic_Output;
