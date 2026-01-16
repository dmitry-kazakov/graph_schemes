--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Binary                        Luebeck            --
--  Interface                                      Spring, 2002       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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
-- This package defines Binary_Feature_Object which is a derived feature
-- of some other feature. The domain of a binary feature is {0,1}. There
-- are n different binary features of  a  source  feature.  Where  n  is
-- binary logarithm of the feature domain cardinality.
--
with Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Bounded_Arrays;
with Strings_Edit;
with System;

use Fuzzy.Feature.Handle;
use Fuzzy.Feature.Handle.Bounded_Arrays;

package Fuzzy.Feature.Binary is
   pragma Elaborate_Body (Fuzzy.Feature.Binary);
--
-- Binary_Class -- Class name
--
   Binary_Class : constant String := Feature_Class & "Binary";
--
-- Create -- Create a set of binary features
--
--    Feature - A feature handle
--
-- This function creates all possible derived  binary  features  of  the
-- feature  specified  by  the  parameter  Feature.  The  newly  created
-- features will have names of the source feature with the  suffix  ".n"
-- added, where n is the order of the created feature.
--
-- Returns :
--
--    The set of handles to created derived binary features
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create (Feature : Feature_Handle) return Bounded_Array;
--
-- Create -- Create a binary feature
--
--    Name    - Of the result
--    Feature - A feature handle
--
-- This  function  creates  a  derived  binary  feature.  When   Feature
-- indicates  a  non-binary  feature,  the  result  is  the first binary
-- feature derived from it. When feature indicates a binary feature, the
-- result  is  the  binary  feature  second  to  it.  The parameter Name
-- specifies the name of the result. End_Error is  propagated  when  the
-- feature cannot  be  created  because  Feature  is  already  the  last
-- possible binary feature.
--
-- Returns :
--
--    A handle to the created derived binary feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - No more binary features
--
   function Create
            (  Name    : String;
               Feature : Feature_Handle
            )  return Feature_Handle;
--
-- Create -- Create a binary feature
--
--    Name         - Of the result
--    Feature      - A feature handle
--    Bit_Position - The position 0 is MSB
--
-- This  function creates a derived binary feature. Corresponding to the
-- Bit_Position. The parameter Name specifies the name  of  the  result.
-- End_Error  is  propagated  when the feature cannot be created because
-- Bit is less than LSB.
--
-- Returns :
--
--    A handle to the created derived binary feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - No binary feature for Bit_Position
--
   function Create
            (  Name         : String;
               Feature      : Feature_Handle;
               Bit_Position : Natural
            )  return Feature_Handle;
--
-- Get_Bit_Position -- Corresponding to the feature
--
--    Feature - Object or a handle to it
--
-- Constraint_Error is propagated if Feature is not a binary feature.
--
-- Returns :
--
--    The bit position as in Create
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle or object
--
   function Get_Bit_Position (Feature : Feature_Handle)
      return Natural;
   function Get_Bit_Position (Feature : Feature_Object'Class)
      return Natural;
--
-- Get_Source -- Get the feature the binary one depends on
--
--    Feature - Object or a handle to it
--
-- Constraint_Error is propagated if Feature is not a binary feature.
--
-- Returns :
--
--    A handle to the source feature
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle or object
--
   function Get_Source (Feature : Feature_Handle)
      return Feature_Handle;
   function Get_Source (Feature : Feature_Object'Class)
      return Feature_Handle;
--
-- Is_Binary -- Check if a handle refers a binary feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature refers a binary feature
--
   function Is_Binary (Feature : Feature_Handle) return Boolean;
   function Is_Binary (Feature : Feature_Object'Class) return Boolean;

private
   pragma Inline (Get_Source);
   pragma Inline (Is_Binary);

   type Binary_Feature_Object;
   type Binary_Feature_Object_Ptr is
      access Binary_Feature_Object'Class;

   type Mask is mod System.Max_Binary_Modulus;

   type Binary_Feature_Object is new Feature_Object with record
      Source : Feature_Handle;
      Bit    : Mask;  -- The bit reflected by the feature
   end record;
--
-- Create_Constraint -- Overrides Fuzzy.Feature...
--
   procedure Create_Constraint
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             );
--
-- Create_Data -- Overrides Fuzzy.Feature...
--
   function Create_Data (Feature : Binary_Feature_Object)
      return Feature_Data_Ptr;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence;
--
-- Get_Constraint -- Overrides Fuzzy.Feature...
--
   overriding
   function Get_Constraint
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Binary_Feature_Object)
      return String;
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Binary_Feature_Object;
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
             (  Feature : Binary_Feature_Object;
                List    : in out Deposit_Container'Class
             );
--
-- Is_Computed -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Computed
            (  Feature : Binary_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean;
--
-- Is_Defined - Overrides Fuzzy.Feature...
--
   overriding
   function Is_Defined
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Overrides Fuzzy.Feature...
--
   overriding
   function Is_Known
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Binary_Feature_Object;
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
-- Set_Constraint -- Overrides Fuzzy.Feature...
--
   procedure Set_Constraint
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                Value   : Positive;
                Allowed : Boolean := False
             );
--
-- Set_Constraint_Range -- Overrides Fuzzy.Feature...
--
   procedure Set_Constraint_Range
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             );
--
-- Set_Range - Overrides Fuzzy.Feature...
--
   procedure Set_Range
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             );
--
-- Store -- Overrides Object.Archived...
--
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Binary_Feature_Object
             );
--
-- Get_Bit -- The mask of the lowest order bit
--
--    Source - The source feature
--
-- Returns :
--
--    The mask of the lowest order bit (one of the highest order is 1)
--
   function Get_Bit (Source : Feature_Object'Class) return Mask;
--
-- Log2 -- Binary logarithm rounded
--
--    Value - Of the function
--
-- Returns :
--
--    Upper bound of log2
--
   function Log2 (Value : Positive) return Positive;

   pragma Inline (Create_Constraint);
   pragma Inline (Get_Constraint);
   pragma Inline (Get_Class);
   pragma Inline (Set_Constraint);
   pragma Inline (Set_Constraint_Range);

end Fuzzy.Feature.Binary;
