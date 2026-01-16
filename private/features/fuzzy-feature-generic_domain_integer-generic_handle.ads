--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Integer        Luebeck            --
--        Generic_Handle                           Autumn, 2005       --
--  Interface                                                         --
--                                Last revision :  21:30 10 Nov 2009  --
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
package Fuzzy.Feature.Generic_Domain_Integer.Generic_Handle is
--
-- Classify -- Fuzzify a value using the domain set
--
--    Feature - A handle to
--    Value   - An integer number, interval, fuzzy number
--
-- The  result  is  a classification of the value. It tells how possible
-- and  necessary  the  value is contained by the subsets of the feature
-- domain set. The parameter Value is either a number, an interval or  a
-- fuzzy number. The cardinality of the result is one of the feature.  
--
-- Returns :
--
--    Fuzzified value
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Classify
            (  Feature : Feature_Handle;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Classification;
   function Classify
            (  Feature : Feature_Handle;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Classification;
   function Classify
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Classification;
   pragma Inline (Classify);
--
-- Is_Domain_Integer -- Check if a handle refers an integer feature
--
--    Feature - A handle to
--
-- Returns :
--
--    True if Feature is a valid float feature (handle)
--
   function Is_Domain_Integer (Feature : Feature_Handle) return Boolean;
   pragma Inline (Is_Domain_Integer);
--
-- To_Set -- Get an intuitionistic set from the value
--
--    Feature - A handle to
--    Value   - The value to be converted
--
-- The result is an intuitionistic fuzzy set of the value. It tells  how
-- possible  and necessary is that the value contains the subsets of the
-- feature domain set. The  parameter  Value  is  either  a  number,  an
-- interval  or  a fuzzy number. The cardinality of the result is one of
-- the feature. 
--
-- Returns :
--
--    Set value
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Set;
   pragma Inline (To_Set);

end Fuzzy.Feature.Generic_Domain_Integer.Generic_Handle;
