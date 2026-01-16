--                                                                    --
--  package Fuzzy.Integers          Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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
--  This generic package provides integer fuzzy numbers. The package has
--  the following formal parameters: 
--
--     Integer_Intervals - An instantiation of Intervals.Integers
--     Interval_Index    - The index type used to count intervals
--     Interval_Map      - The array type mapping index to confidence
--     To_Confidence     - Interval_Index->Confidence.  It  shall be an
--                         array of ascending values.  The  last  value
--                          has to be Confidence'Last. 
--
with Fuzzy.Logic;  use Fuzzy.Logic;
with Intervals;    use Intervals;

with Intervals.Integers;
with Fuzzy.Numbers;

generic
   with package Integer_Intervals is new Intervals.Integers (<>);
   type Interval_Index is (<>);
   type Interval_Map is array (Interval_Index) of Confidence;
   To_Confidence : Interval_Map;
package Fuzzy.Integers is
   use Integer_Intervals;

   package Fuzzy_Numbers is
      new Fuzzy.Numbers
          (  Interval_Index => Interval_Index,
             Interval_Map   => Interval_Map,
             Interval       => Interval,
             Number         => Number,
             To_Confidence  => To_Confidence
          );
   subtype Fuzzy_Integer is Fuzzy_Numbers.Fuzzy_Number;
--
-- Equal -- Renames Fuzzy.Numbers...
--
   function Equal (Left, Right : Fuzzy_Integer) return Boolean
      renames Fuzzy_Numbers.Equal;
--
-- Is_In -- Renames Fuzzy.Numbers...
--
   function Is_In (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
--
-- Necessity -- Renames Fuzzy.Numbers...
--
   function Necessity (Left, Right : Fuzzy_Integer) return Confidence
      renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Number; Right : Fuzzy_Integer)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Interval; Right : Fuzzy_Integer)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Fuzzy_Integer; Right : Number)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Fuzzy_Integer; Right : Interval)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
--
-- Possibility -- Renames Fuzzy.Numbers...
--
   function Possibility (Left, Right : Fuzzy_Integer) return Confidence
      renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Number; Right : Fuzzy_Integer)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Interval; Right : Fuzzy_Integer)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Fuzzy_Integer; Right : Number)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Fuzzy_Integer; Right : Interval)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
--
-- To_Fuzzy -- Renames Fuzzy.Numbers...
--
   function To_Fuzzy (Left : Number) return Fuzzy_Integer
      renames Fuzzy_Numbers.To_Fuzzy;
   function To_Fuzzy (Left : Interval) return Fuzzy_Integer
      renames Fuzzy_Numbers.To_Fuzzy;
--
-- abs -- Renames Fuzzy.Numbers...
--
   function "abs" (Left : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."abs";
--
-- + -- Renames Fuzzy.Numbers...
--
   function "+" (Left : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."+";
--
-- - -- Renames Fuzzy.Numbers...
--
   function "-" (Left : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."-";
--
-- + -- Renames Fuzzy.Numbers...
--
   function "+" (Left, Right : Fuzzy_Integer) return Fuzzy_Integer
         renames Fuzzy_Numbers."+";
   function "+" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."+";
   function "+" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."+";
   function "+" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."+";
   function "+" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."+";
--
-- - -- Renames Fuzzy.Numbers...
--
   function "-" (Left, Right : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."-";
   function "-" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."-";
   function "-" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."-";
   function "-" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."-";
   function "-" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."-";
--
-- * -- Renames Fuzzy.Numbers...
--
   function "*" (Left, Right : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."*";
   function "*" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."*";
   function "*" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."*";
   function "*" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."*";
   function "*" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."*";
--
-- / -- Renames Fuzzy.Numbers...
--
   function "/" (Left, Right : Fuzzy_Integer) return Fuzzy_Integer
      renames Fuzzy_Numbers."/";
   function "/" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."/";
   function "/" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."/";
   function "/" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."/";
   function "/" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."/";
--
-- ** -- Renames Fuzzy.Numbers...
--
   function "**" (Left : Fuzzy_Integer; Right : Natural)
      return Fuzzy_Integer
         renames Fuzzy_Numbers."**";
--
-- >, >=, =, /=, <, <= -- Renames Fuzzy.Numbers...
--
   function ">" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers.">";
   function ">" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";

   function ">=" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers.">=";
   function ">=" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";

   function "=" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers."=";
   function "=" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" ( Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" ( Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";

   function "/=" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers."/=";
   function "/=" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";

   function "<" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers."<";
   function "<" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";

   function "<=" (Left, Right : Fuzzy_Integer) return Fuzzy_Boolean
      renames Fuzzy_Numbers."<=";
   function "<=" (Left : Fuzzy_Integer; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Number; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Fuzzy_Integer; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Interval; Right : Fuzzy_Integer)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";

end Fuzzy.Integers;
