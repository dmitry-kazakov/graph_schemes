--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Linguistic                       Autumn, 2005       --
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
--  The domain of a linguistic feature is a fuzzy set defined over  some
--  set of fuzzy subsets of dimensioned real numbers. These subsets  are
--  called linguistic variables. So a linguistic feature has  a  set  of
--  linguistic variables as the domain set.  In  this  implementation  a
--  membership  function  of  a  linguistic  variable  is contiguous and
--  linear on a finite number of segments.
--
--  This generic package is used to create linguistic feature types.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Units;                 use Units;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Linguistic is
   pragma Elaborate_Body
          (Fuzzy.Feature.Generic_Domain_Float.Generic_Linguistic);
--
-- Linguistic_Class -- The prefix of all linguistic feature classes
--
   Linguistic_Class : constant String :=
      Feature_Class & "Linguistic." & Suffix;
--
-- Create -- Create a linguistic feature
--
--    Name   - Of the feature
--    Domain - Of the domain set of the feature
--    Scale  - Of the feature values
--  [ Mode ] - Character set used in dimension unit specification
--
-- The  parameter Domain is the list of feature domain having the syntax
-- described  in  Fuzzy.Generic_Linguistics.Edit. The whole string shall
-- be   matched.   Otherwise,  Data_Error  is  propagated.  It  is  also
-- propagated if Domain contains duplicated element names. Alternatively
-- Domain is a set of linguistic variables. The parameter Scale  is  the
-- feature values scale. It is specified either as  a  string  which  is
-- parsed using the function Value or as a measure. The scale shall have
-- positive gain, Unit_Error is propagated otherwise. The parameter Mode
-- is the character set used in Scale.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Constraint_Error - A number is out of range
--    Data_Error       - Syntax error of the string Domain
--    End_Error        - No elements found in the string Domain
--    Unit_Error       - Wrong Scale
--
   function Create
            (  Name   : String;
               Domain : String;
               Scale  : Measure := Float_Measures.Np
            )  return Feature_Handle;
   function Create
            (  Name   : String;
               Domain : String;
               Scale  : String;
               Mode   : Code_Set := UTF8_Set
            )  return Feature_Handle;
   function Create
            (  Name   : String;
               Domain : Variable_Sets.Linguistic_Set;
               Scale  : Measure := Float_Measures.Np
            )  return Feature_Handle;
   function Create
            (  Name   : String;
               Domain : Variable_Sets.Linguistic_Set;
               Scale  : String;
               Mode   : Code_Set := UTF8_Set
            )  return Feature_Handle;
--
-- Is_Linguistic -- Check if a handle refers a linguistic feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature is a valid linguistic feature (handle)
--
   function Is_Linguistic (Feature : Feature_Handle)
      return Boolean;
   function Is_Linguistic (Feature : Feature_Object'Class)
      return Boolean;
   pragma Inline (Is_Linguistic);

private
   use Independent_Features;
--
-- Linguistic_Feature_Object -- The feature object
--
   type Linguistic_Feature_Object
        (  Cardinality : Positive;
           SI          : Unit;
           Unit_Length : Natural
        )  is new Independent_Feature_Object (Cardinality) with
   record
      Domain    : Linguistic_Set;
      Gain      : Domain_Float'Base;
      Offset    : Domain_Float'Base;
      Dimension : String (1..Unit_Length);
   end record;
--
-- Accumulate -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Accumulate
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measures.Variable_Measure;
--
-- Classify -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Linguistic_Feature_Object)
      return String;
   pragma Inline (Get_Class);
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Linguistic_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Scale -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale (Feature : Linguistic_Feature_Object)
      return Measure;
--
-- Get_Scale_Text -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale_Text
            (  Feature    : Linguistic_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Get_Unit -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Unit (Feature : Linguistic_Feature_Object)
      return Unit;
--
-- Get_Variable -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Variable
            (  Feature : Linguistic_Feature_Object;
               Value   : Positive
            )  return Variable_Measure;
--
-- Is_Domain_Linguistic -- Overrides
--                         Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Is_Domain_Linguistic (Feature : Linguistic_Feature_Object)
      return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Linguistic_Feature_Object;
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
                Feature     : Linguistic_Feature_Object
             );
--
-- To_Set -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;
--
-- Check_Names -- Check the names of a lingustic variables set
--
--    Domain - To be checked
--
-- This  procedure  checks  the names of Domain variables for conformity
-- with a linguistic feature domain values names.
--
-- Exceptions :
--
--    Data_Error - Domain contains an illegal name
--
   procedure Check_Names (Domain : Variable_Sets.Linguistic_Set);

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

end Fuzzy.Feature.Generic_Domain_Float.Generic_Linguistic;
