--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Float.                           Autumn, 2005       --
--           Generic_Isosceles_Trapezoids                             --
--                                                                    --
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
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Units;                 use Units;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Float.
        Generic_Isosceles_Trapezoids is
   pragma Elaborate_Body
          (  Fuzzy.Feature.Generic_Domain_Float.Generic_Float.
             Generic_Isosceles_Trapezoids
          );
--
-- Isosceles_Trapezoid_Class -- The prefix of all isosceles trapezoids
--                              feature classes
--
   Isosceles_Trapezoid_Class : constant String :=
      Feature_Class & "Isosceles_Trapezoid." & Suffix;
--
-- Create -- Create a linguistic isosceles trapezoids feature
--
--    Name        - Of the feature
--    Cardinality - Of the feature (number of elementary intervals)
--    From        - The lower boundary of the feature values range
--    To          - The upper boundary
--    Shoulder    - The length of the trapezoid's shoulders
--    Scale       - The scale of the feature values
--  [ Mode ]      - Scale character set
--
-- The  feature  domain  of  created  linguistic feature consists of two
-- shoulders  and  Cardinality  -  2 isosceles trapezoids. The following
-- figure illustrates the case for the Cardinality = 4.
--
--                            Variable names
--    _____
--         \                  L
--          \______________
--         ____
--        /    \              T1
--    ___/      \__________
--             ____
--            /    \          T2
--    _______/      \______
--                 ________
--                /           R
--    ___________/
--         |     | |
--         |   ->| |<- Shoulder
--       From      To
--
-- The parameter Scale is the feature  values  scale.  It  is  specified
-- either as a measure  or  as  a  string  which  is  then  parsed.  The
-- parameter  Mode is the character set used for parsing a string Scale.
-- The  scale  shall  have  positive  gain,  Unit_Error  is   propagated
-- otherwise. When From < To, Cardinality should be  at  least  3.  When
-- From = To, then Cardinality should be 2.  Otherwise  Constraint_Error
-- is propagated. It is also propagated when Shoulder is negative.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Constraint_Error - From = To and Cardinality is less than 2
--    End_Error        - Empty domain (To < From)
--    Unit_Error       - Wrong Scale
--
   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Shoulder    : Domain_Float;
               Scale       : Measure := Float_Measures.Np
            )  return Feature_Handle;
   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Shoulder    : Domain_Float;
               Scale       : String;
               Mode        : Code_Set := UTF8_Set
            )  return Feature_Handle;
--
-- Get_Shoulder -- Get shoulder
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The shoulder
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or object
--
   function Get_Shoulder (Feature : Feature_Handle) return Measure;
   function Get_Shoulder (Feature : Feature_Object'Class)
      return Measure;
--
-- Get_Trapezoid_Parameters -- Get the feature
--
--    Feature  - Object or a handle to it
--    From     - The rightmost value of L
--    To       - The leftmost value of R
--    Shoulder - The value of
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle or object
--
   procedure Get_Trapezoid_Parameters
             (  Feature  : Feature_Handle;
                From     : out Measure;
                To       : out Measure;
                Shoulder : out Measure
             );
   procedure Get_Trapezoid_Parameters
             (  Feature : Feature_Object'Class;
                From     : out Measure;
                To       : out Measure;
                Shoulder : out Measure
             );
--
-- Is_Isosceles_Trapezoid -- Check  if  a  handle  refers  an  isosceles
--                           trapezoid feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature is a valid float feature (handle)
--
   function Is_Isosceles_Trapezoid (Feature : Feature_Handle)
      return Boolean;
   function Is_Isosceles_Trapezoid (Feature : Feature_Object'Class)
      return Boolean;

private
   pragma Inline (Get_Shoulder);
   pragma Inline (Get_Trapezoid_Parameters);
   pragma Inline (Is_Isosceles_Trapezoid);

   use Independent_Features;
--
-- Isosceles_Trapezoid_Feature_Object -- The feature object
--
   type Isosceles_Trapezoid_Feature_Object is
      new Float_Feature_Object with
   record
      Domain   : Linguistic_Set;
      Shoulder : Domain_Float'Base;
   end record;
--
-- Accumulate -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Accumulate
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measures.Variable_Measure;
--
-- Classify -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Isosceles_Trapezoid_Feature_Object)
      return String;
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Isosceles_Trapezoid_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Variable -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Variable
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Positive
            )  return Variable_Measure;
--
-- Is_Domain_Linguistic -- Overrides
--                         Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Is_Domain_Linguistic
            (  Feature : Isosceles_Trapezoid_Feature_Object
            )  return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Isosceles_Trapezoid_Feature_Object;
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
                Feature     : Isosceles_Trapezoid_Feature_Object
             );
--
-- To_Set -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

   pragma Inline (Get_Class);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Float.
    Generic_Isosceles_Trapezoids;
