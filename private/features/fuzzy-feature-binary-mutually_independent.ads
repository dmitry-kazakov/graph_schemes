--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Binary.                       Luebeck            --
--        Mutually_Independent                     Winter, 2005       --
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
-- This package defines Bit_Feature_Object which is a derived feature of
-- some  other  feature.  The domain of a binary feature is {0,1}. There
-- are n different binary features of  a  source  feature.  Where  n  is
-- binary  logarithm  of  the  feature  domain  cardinality. Unlikely to
-- Binary_Feature_Object values  of  Bit_Feature_Object  are  considered
-- mutually independent.
--
package Fuzzy.Feature.Binary.Mutually_Independent is
   pragma Elaborate_Body (Fuzzy.Feature.Binary.Mutually_Independent);
--
-- Independent_Binary_Class -- Class name
--
   Independent_Binary_Class : constant String :=
      Feature_Class & "Binary.Independent";
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
-- Is_Bit -- Check if a handle refers a binary feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature refers a binary feature
--
   function Is_Bit (Feature : Feature_Handle) return Boolean;
   function Is_Bit (Feature : Feature_Object'Class) return Boolean;

private
   pragma Inline (Get_Source);
   pragma Inline (Is_Bit);

   type Bit_Feature_Object is
      new Binary_Feature_Object with null record;
--
-- Get_Constraint -- Overrides Fuzzy.Feature...
--
   overriding
   function Get_Constraint
            (  Feature : Bit_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Bit_Feature_Object)
      return String;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Feature : Bit_Feature_Object;
                List    : in out Deposit_Container'Class
             );
--
-- Is_Computed -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Computed
            (  Feature : Bit_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean;
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
-- Set_Range - Overrides Fuzzy.Feature...
--
   overriding
   procedure Set_Range
             (  Feature : Bit_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             );
   pragma Inline (Get_Constraint);
   pragma Inline (Get_Class);

end Fuzzy.Feature.Binary.Mutually_Independent;
