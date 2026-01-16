--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Integer        Luebeck            --
--  Interface                                      Summer, 2002       --
--                                                                    --
--                                Last revision :  12:48 16 Oct 2010  --
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
--  This generic package is the root for all integer features  based  of
--  same  type.  It  defines  the  interface  of  such  features   which
--  particular packages implements. The formal parameters are:
--
--  Number         - The type used for domain values
--  Suffix         - The class name suffix used for  the  features  that
--                   are based upon
--  Interval_Index - The index type used to count intervals
--  Interval_Map   - The array type mapping index to confidence
--  To_Confidence  - Interval_Index->Confidence. It shall be an array of
--                   ascending   values.   The  last  value  has  to  be
--                   Confidence'Last.
--
with Fuzzy.Feature.Generic_Independent;
with Fuzzy.Integers;
with Fuzzy.Intuitionistic;
with Intervals.Integers;
with Strings_Edit.Integer_Edit;

generic
   type Number is range <>;
   Suffix : String;
   type Interval_Index is (<>);
   type Interval_Map is array (Interval_Index) of Confidence;
   To_Confidence : Interval_Map;
package Fuzzy.Feature.Generic_Domain_Integer is
   subtype Domain_Integer is Number;
   package Integer_Intervals is
      new Intervals.Integers (Domain_Integer'Base);
   package Fuzzy_Integers is
      new Fuzzy.Integers
          (  Integer_Intervals => Integer_Intervals,
             Interval_Index    => Interval_Index,
             Interval_Map      => Interval_Map,
             To_Confidence     => To_Confidence
          );
   package Integer_Edit is
      new Strings_Edit.Integer_Edit (Domain_Integer'Base);

   use Integer_Intervals;
   use Fuzzy_Integers;
--
-- Domain_Feature_Object -- The  abstract  base  type  for  all  domain-
--                          valued features
--
   type Domain_Feature_Object is abstract
      new Feature_Object with null record;
--
-- Classify -- Get a classification from the value
--
--    Feature - The feature
--    Value   - An integer number, interval, fuzzy number
--
-- The  result  is  a fuzzy intuitionistic classification of the feature
-- corresponding to the given value.
--
-- Returns :
--
--    The classification
--
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Classification is abstract;
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Classification is abstract;
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Classification is abstract;
--
-- To_Set -- Get an intuitionistic set from the value
--
--    Feature - The feature
--    Value   - An integer number, interval, fuzzy number
--
-- The  result  is  a  fuzzy intuitionistic subset of the feature domain
-- corresponding to the given value.
--
-- Returns :
--
--    The set
--
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Set is abstract;
--
-- Independent_Features -- Independent domain-valued features
--
   package Independent_Features is
      new Generic_Independent (Domain_Feature_Object);

end Fuzzy.Feature.Generic_Domain_Integer;
