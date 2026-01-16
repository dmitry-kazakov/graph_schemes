--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Integer.       Luebeck            --
--        Generic_Integer                          Summer, 2002       --
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
--  This generic package is used to create integer  feature  types.  The
--  formal generic parameters are a package implementing fuzzy integers,
--  i.e.   an   instantiation  of  Fuzzy.Integers  and  an  instance  of
--  Integer_Edit based on it:
--
--      Suffix         - Of the feature class name
--      Fuzzy_Integers - An instance of Fuzzy.Integers
--      Integer_Edit   - An instance of Strings_Edit.Integer_Edit
--
--  A fuzzy integer feature is a fuzzy set defined over  some  range  of
--  integer  values.  If this range is From..To then the domain consists
--  of  {From},  {From+1},..,  {To-1}, {To}. Thus the cardinality of the
--  feature is To - From + 1.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;

generic
package Fuzzy.Feature.Generic_Domain_Integer.Generic_Integer is
--
-- Integer_Class -- The class name
--
   Integer_Class : constant String :=
      Feature_Class & "Integer." & Suffix;
--
-- Create -- Create an integer feature
--
--    Name - Of the feature
--    From - The lower boundary of the feature values range
--    To   - The upper boundary
--
-- The parameters From and To determine the range of the feature domain.
-- Its cardinality will be To - From + 1.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    End_Error - Empty domain (To < From)
--
   function Create
            (  Name : String;
               From : Domain_Integer;
               To   : Domain_Integer
            )  return Feature_Handle;
--
-- Get_From -- Get the lower bound of the values
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle or object
--
   function Get_From (Feature : Feature_Handle) return Domain_Integer;
   function Get_From (Feature : Feature_Object'Class)
      return Domain_Integer;
--
-- Get_To -- Get the upper bound of the values
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle or object
--
   function Get_To (Feature : Feature_Handle) return Domain_Integer;
   function Get_To (Feature : Feature_Object'Class)
      return Domain_Integer;
--
-- Is_Integer -- Check if a handle refers an integer feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature is a valid integer feature (handle)
--
   function Is_Integer (Feature : Feature_Handle) return Boolean;
   function Is_Integer (Feature : Feature_Object'Class) return Boolean;

private
   use Independent_Features;

   pragma Inline (Get_From);
   pragma Inline (Get_To);
   pragma Inline (Is_Integer);
--
-- Integer_Feature -- The feature object
--
   type Integer_Feature_Object is
      new Independent_Feature_Object with
   record
      From : Domain_Integer;
   end record;
--
-- Classify -- Overrides Fuzzy.Feature.Generic_Domain_Integer...
--
   overriding
   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Integer_Feature_Object)
      return String;
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Integer_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Integer_Feature_Object;
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
                Feature     : Integer_Feature_Object
             );
--
-- To_Set -- Overrides Fuzzy.Feature.Generic_Domain_Integer...
--
   overriding
   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Set;

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

   pragma Inline (Get_Class);

end Fuzzy.Feature.Generic_Domain_Integer.Generic_Integer;
