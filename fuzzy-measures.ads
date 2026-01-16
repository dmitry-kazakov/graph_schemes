--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures                              Luebeck            --
--  Interface                                      Spring, 2005       --
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
--  This  package provides dimensioned fuzzy floating-point numbers. The
--  package is generic, the formal parameter are:
--
--  (o)  Interval_Measures is dimeansioned Floating-point intervals
--  (o)  Fuzzy_Floats is compatible fuzzy numbers
--
--  The behavior of the operations on  fuzzy  numbers  on  overflows  is
--  determined  by  one  of  the  underlying  floating-point  type.  See
--  T'Machine_Overflows.
--
with Fuzzy.Logic;  use Fuzzy.Logic;
with Intervals;    use Intervals;
with Units;        use Units;

with Fuzzy.Floats;
with Intervals.Measures;
with Units.Base;

generic
   with package Interval_Measures is new Intervals.Measures (<>);
   with package Fuzzy_Floats is
      new Fuzzy.Floats
          (  Float_Intervals => Interval_Measures.Float_Intervals_Of,
             others => <>
          );
package Fuzzy.Measures is
   package Fuzzy_Floats_Of      renames Fuzzy_Floats;
   package Fuzzy_Numbers        renames Fuzzy_Floats_Of.Fuzzy_Numbers;
   package Interval_Measures_Of renames Interval_Measures;
   package Intervals_Of renames Interval_Measures.Float_Intervals_Of;
   package Measures_Of  renames Interval_Measures.Float_Measures_Of;

--   use Fuzzy_Floats_Of;
--   use Float_Intervals_Of;
--   use Interval_Measures_Of;
--   use Float_Measures_Of;
--
-- Number -- This is probably a  compiler  bug.  There  is  no  need  to
--           introduce   Number,   as   it   should   be   visible  from
--           Interval_Measures_Of.  Nevertheless,  this  workaround   is
--           required to get instantiations of this package compiled.
--
   subtype Number is Fuzzy_Floats_Of.Float_Intervals_Of.Number_Of;
   function "abs" (Right : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."abs";
   function "+" (Left, Right : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."+";
   function "+" (Left : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."+";
   function "-" (Left, Right : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."-";
   function "-" (Left : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."-";
   function "*" (Left, Right : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."*";
   function "/" (Left, Right : Number) return Number
      renames Fuzzy_Floats_Of.Float_Intervals_Of."/";
   function "=" (Left, Right : Number) return Boolean
      renames Fuzzy_Floats_Of.Float_Intervals_Of."=";
   function "<" (Left, Right : Number) return Boolean
      renames Fuzzy_Floats_Of.Float_Intervals_Of."<";
   function "<=" (Left, Right : Number) return Boolean
      renames Fuzzy_Floats_Of.Float_Intervals_Of."<=";
   function ">=" (Left, Right : Number) return Boolean
      renames Fuzzy_Floats_Of.Float_Intervals_Of.">=";
   function ">" (Left, Right : Number) return Boolean
      renames Fuzzy_Floats_Of.Float_Intervals_Of.">";
--
-- Interval
--
   subtype Interval is Intervals_Of.Interval;
   function Distance (Left, Right : Interval) return Number
      renames Intervals_Of.Distance;
   function Distance (Left : Number; Right : Interval) return Number
      renames Intervals_Of.Distance;
   function Distance (Left : Interval; Right : Number) return Number
      renames Intervals_Of.Distance;
   function From (Left : Interval) return Number
      renames Intervals_Of.From;
   function Is_In (Left, Right : Interval) return Boolean
      renames Intervals_Of.Is_In;
   function Is_In (Left : Number; Right : Interval) return Boolean
      renames Intervals_Of.Is_In;
   function Is_Negative (Left : Interval) return Boolean
      renames Intervals_Of.Is_Negative;
   function Is_Positive (Left : Interval) return Boolean
      renames Intervals_Of.Is_Positive;
   function Length (Left : Interval) return Number
      renames Intervals_Of.Length;
   function To (Left : Interval) return Number
      renames Intervals_Of.To;
   function To_Interval (Left : Number) return Interval
      renames Intervals_Of.To_Interval;
   function To_Interval (Left, Right : Number) return Interval
      renames Intervals_Of.To_Interval;
   function "abs" (Left : Interval) return Interval
      renames Intervals_Of."abs";
   function "+" (Left : Interval) return Interval
      renames Intervals_Of."+";
   function "-" (Left : Interval) return Interval
      renames Intervals_Of."-";
   function "&" (Left, Right : Interval) return Boolean
      renames Intervals_Of."&";
   function "+" (Left, Right : Interval) return Interval
      renames Intervals_Of."+";
   function "+" (Left : Interval; Right : Number) return Interval
      renames Intervals_Of."+";
   function "+" (Left : Number;  Right : Interval) return Interval
      renames Intervals_Of."+";
   function "-" (Left, Right : Interval) return Interval
      renames Intervals_Of."-";
   function "-" (Left : Interval; Right : Number) return Interval
      renames Intervals_Of."-";
   function "-" (Left : Number; Right : Interval) return Interval
      renames Intervals_Of."-";
   function "*" (Left : Interval; Right : Number) return Interval
      renames Intervals_Of."*";
   function "*" (Left : Number; Right : Interval) return Interval
      renames Intervals_Of."*";
   function "*" (Left, Right : Interval) return Interval
      renames Intervals_Of."*";
   function "/" (Left : Interval; Right : Number) return Interval
      renames Intervals_Of."/";
   function "/" (Left : Number; Right : Interval) return Interval
      renames Intervals_Of."/";
   function "/" (Left, Right : Interval) return Interval
      renames Intervals_Of."/";
   function "**" (Left : Interval; Right : Natural) return Interval
      renames Intervals_Of."**";
   function ">" (Left, Right : Interval) return Logical
      renames Intervals_Of.">";
   function ">" (Left : Interval; Right : Number) return Logical
      renames Intervals_Of.">";
   function ">" (Left : Number;   Right : Interval) return Logical
      renames Intervals_Of.">";
   function ">=" (Left, Right : Interval) return Logical
      renames Intervals_Of.">=";
   function ">=" (Left : Interval; Right : Number) return Logical
      renames Intervals_Of.">=";
   function ">=" (Left : Number;   Right : Interval) return Logical
      renames Intervals_Of.">=";
   function "<=" (Left, Right : Interval) return Logical
      renames Intervals_Of."<=";
   function "<=" (Left : Interval; Right : Number) return Logical
      renames Intervals_Of."<=";
   function "<=" (Left : Number;   Right : Interval) return Logical
      renames Intervals_Of."<=";
   function "<" (Left, Right : Interval) return Logical
      renames Intervals_Of."<";
   function "<" (Left : Interval; Right : Number) return Logical
      renames Intervals_Of."<";
   function "<" (Left : Number;   Right : Interval) return Logical
      renames Intervals_Of."<";
--
-- Measure
--
   subtype Scaled is Measures_Of.Scaled;
   subtype Measure is Measures_Of.Measure;
   function "abs" (Right : Measure) return Measure
      renames Measures_Of."abs";
   function "+" (Right : Measure) return Measure
      renames Measures_Of."+";
   function "-" (Right : Measure) return Measure
      renames Measures_Of."-";
   function "**" (Left : Measure; Right : Integer) return Measure
      renames Measures_Of."**";
   function "*" (Left, Right : Measure) return Measure
      renames Measures_Of."*";
   function "*" (Left : Number'Base; Right : Measure) return Measure
      renames Measures_Of."*";
   function "*" (Left : Measure; Right : Number'Base) return Measure
      renames Measures_Of."*";
   function "/" (Left, Right : Measure) return Measure
      renames Measures_Of."/";
   function "/" (Left : Number'Base; Right : Measure) return Measure
      renames Measures_Of."/";
   function "/" (Left : Measure; Right : Number'Base) return Measure
      renames Measures_Of."/";
   function "and" (Left : Measure; Right : Number'Base) return Measure
      renames Measures_Of."and";
   function "+"  (Left, Right : Measure) return Measure
      renames Measures_Of."+";
   function "-"  (Left, Right : Measure) return Measure
      renames Measures_Of."-";
   function ">" (Left, Right : Measure) return Boolean
      renames Measures_Of.">";
   function "<" (Left, Right : Measure) return Boolean
      renames Measures_Of."<";
   function "="  (Left, Right : Measure) return Boolean
      renames Measures_Of."=";
   function ">=" (Left, Right : Measure) return Boolean
      renames Measures_Of.">=";
   function "<=" (Left, Right : Measure) return Boolean
      renames Measures_Of."<=";
   function Get_Value (Value : Measure) return Number
      renames Measures_Of.Get_Value;
   function Get_Value_As (Value, Scale : Measure) return Number
      renames Measures_Of.Get_Value_As;
   function Get_Unit (Value : Measure) return Unit
      renames Measures_Of.Get_Unit;
   function Convert (Value, Scale : Measure) return Measure
      renames Measures_Of.Convert;
   function Normalize (Value : Measure) return Measure
      renames Measures_Of.Normalize;
   function Shift (Value : Measure; Shift : Number'Base) return Measure
      renames Measures_Of.Shift;
   function To_Measure (Value : Number) return Measure
      renames Measures_Of.To_Measure;
--
-- Interval_Measure
--
   subtype Interval_Measure is Interval_Measures_Of.Interval_Measure;
   function Convert (Value : Interval_Measure; Scale : Measure)
      return Interval_Measure renames Interval_Measures_Of.Convert;
   function Distance (Left, Right : Interval_Measure)
      return Measure renames Interval_Measures_Of.Distance;
   function Distance (Left : Measure; Right : Interval_Measure)
      return Measure renames Interval_Measures_Of.Distance;
   function Distance (Left : Interval_Measure; Right : Measure)
      return Measure renames Interval_Measures_Of.Distance;
   function From (Left : Interval_Measure) return Measure
      renames Interval_Measures_Of.From;
   function Get_Value (Value : Interval_Measure) return Interval
      renames Interval_Measures_Of.Get_Value;
   function Get_Value_As (Value : Interval_Measure; Scale : Measure)
      return Interval renames Interval_Measures_Of.Get_Value_As;
   function Get_Unit (Value : Interval_Measure) return Unit
      renames Interval_Measures_Of.Get_Unit;
   function Is_In (Left, Right : Interval_Measure) return Boolean
      renames Interval_Measures_Of.Is_In;
   function Is_In (Left : Measure; Right : Interval_Measure)
      return Boolean renames Interval_Measures_Of.Is_In;
   function Is_Negative (Left : Interval_Measure) return Boolean
      renames Interval_Measures_Of.Is_Negative;
   function Is_Positive (Left : Interval_Measure) return Boolean
      renames Interval_Measures_Of.Is_Positive;
   function Length (Left : Interval_Measure) return Measure
      renames Interval_Measures_Of.Length;
   function Normalize (Value : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of.Normalize;
   function Shift (Value : Interval_Measure; Shift : Number'Base)
      return Interval_Measure renames Interval_Measures_Of.Shift;
   function To (Left : Interval_Measure) return Measure
      renames Interval_Measures_Of.To;
   function To_Interval_Measure (Left, Right : Measure)
      return Interval_Measure
         renames Interval_Measures_Of.To_Interval_Measure;
   function To_Interval_Measure (Left, Right : Number)
      return Interval_Measure
         renames Interval_Measures_Of.To_Interval_Measure;
   function To_Interval_Measure (Left : Measure)
      return Interval_Measure
         renames Interval_Measures_Of.To_Interval_Measure;
   function To_Interval_Measure (Left : Number)
      return Interval_Measure
         renames Interval_Measures_Of.To_Interval_Measure;
   function To_Interval_Measure (Left : Interval)
      return Interval_Measure
         renames Interval_Measures_Of.To_Interval_Measure;
   function "abs" (Left : Interval_Measure) return Interval_Measure
      renames Interval_Measures_Of."abs";
   function "+" (Left : Interval_Measure) return Interval_Measure
      renames Interval_Measures_Of."+";
   function "-" (Left : Interval_Measure) return Interval_Measure
      renames Interval_Measures_Of."-";
   function "&" (Left, Right : Interval_Measure) return Boolean
      renames Interval_Measures_Of."&";
   function "+" (Left, Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."+";
   function "+" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."+";
   function "+" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."+";
   function "-" (Left, Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."-";
   function "-" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."-";
   function "-" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."-";
   function "*" (Left, Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Measure; Right : Interval)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "*" (Left : Interval; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."*";
   function "/" (Left, Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Measure; Right : Interval)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "/" (Left : Interval; Right : Measure)
      return Interval_Measure renames Interval_Measures_Of."/";
   function "**" (Left : Interval_Measure; Right : Natural)
      return Interval_Measure renames Interval_Measures_Of."**";
   function ">" (Left, Right : Interval_Measure)
      return Logical renames Interval_Measures_Of.">";
   function ">" (Left : Interval_Measure; Right : Measure)
      return Logical renames Interval_Measures_Of.">";
   function ">" (Left : Measure; Right : Interval_Measure)
      return Logical renames Interval_Measures_Of.">";
   function ">=" (Left, Right : Interval_Measure)
      return Logical renames Interval_Measures_Of.">=";
   function ">=" (Left : Interval_Measure; Right : Measure)
      return Logical renames Interval_Measures_Of.">=";
   function ">=" (Left : Measure; Right : Interval_Measure)
      return Logical renames Interval_Measures_Of.">=";
   function "<=" (Left, Right : Interval_Measure)
      return Logical renames Interval_Measures_Of."<=";
   function "<=" (Left : Interval_Measure; Right : Measure)
      return Logical renames Interval_Measures_Of."<=";
   function "<=" (Left : Measure; Right : Interval_Measure)
      return Logical renames Interval_Measures_Of."<=";
   function "<"  (Left, Right : Interval_Measure)
      return Logical renames Interval_Measures_Of."<";
   function "<"  (Left : Interval_Measure; Right : Measure)
      return Logical renames Interval_Measures_Of."<";
   function "<"  (Left : Measure; Right : Interval_Measure)
      return Logical renames Interval_Measures_Of."<";
--
-- Fuzzy_Float
--
   subtype Fuzzy_Float is Fuzzy_Floats_Of.Fuzzy_Float;
   function Equal (Left, Right : Fuzzy_Float) return Boolean
      renames Fuzzy_Numbers.Equal;
   function Is_In (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Is_In (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.Is_In;
   function Necessity (Left, Right : Fuzzy_Float) return Confidence
      renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Number; Right : Fuzzy_Float)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Interval; Right : Fuzzy_Float)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Fuzzy_Float; Right : Number)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Necessity (Left : Fuzzy_Float; Right : Interval)
      return Confidence
         renames Fuzzy_Numbers.Necessity;
   function Possibility (Left, Right : Fuzzy_Float) return Confidence
      renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Number; Right : Fuzzy_Float)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Interval; Right : Fuzzy_Float)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Fuzzy_Float; Right : Number)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function Possibility (Left : Fuzzy_Float; Right : Interval)
      return Confidence
         renames Fuzzy_Numbers.Possibility;
   function To_Fuzzy (Left : Number) return Fuzzy_Float
      renames Fuzzy_Numbers.To_Fuzzy;
   function To_Fuzzy (Left : Interval) return Fuzzy_Float
      renames Fuzzy_Numbers.To_Fuzzy;
   function "abs" (Left : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."abs";
   function "+" (Left : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."+";
   function "-" (Left : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."-";
   function "+" (Left, Right : Fuzzy_Float) return Fuzzy_Float
         renames Fuzzy_Numbers."+";
   function "+" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Float
         renames Fuzzy_Numbers."+";
   function "+" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."+";
   function "+" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Float
         renames Fuzzy_Numbers."+";
   function "+" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."+";
   function "-" (Left, Right : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."-";
   function "-" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Float
         renames Fuzzy_Numbers."-";
   function "-" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."-";
   function "-" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Float
         renames Fuzzy_Numbers."-";
   function "-" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."-";
   function "*" (Left, Right : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."*";
   function "*" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Float
         renames Fuzzy_Numbers."*";
   function "*" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."*";
   function "*" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Float
         renames Fuzzy_Numbers."*";
   function "*" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."*";
   function "/" (Left, Right : Fuzzy_Float) return Fuzzy_Float
      renames Fuzzy_Numbers."/";
   function "/" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Float
         renames Fuzzy_Numbers."/";
   function "/" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."/";
   function "/" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Float
         renames Fuzzy_Numbers."/";
   function "/" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Float
         renames Fuzzy_Numbers."/";
   function "**" (Left : Fuzzy_Float; Right : Natural)
      return Fuzzy_Float
         renames Fuzzy_Numbers."**";
   function ">" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers.">";
   function ">" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">";
   function ">=" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers.">=";
   function ">=" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function ">=" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers.">=";
   function "=" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers."=";
   function "=" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" ( Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "=" ( Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."=";
   function "/=" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers."/=";
   function "/=" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "/=" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."/=";
   function "<" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers."<";
   function "<" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<";
   function "<=" (Left, Right : Fuzzy_Float) return Fuzzy_Boolean
      renames Fuzzy_Numbers."<=";
   function "<=" (Left : Fuzzy_Float; Right : Number)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Number; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Fuzzy_Float; Right : Interval)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
   function "<=" (Left : Interval; Right : Fuzzy_Float)
      return Fuzzy_Boolean
         renames Fuzzy_Numbers."<=";
--
-- Fuzzy_Measure -- Dimensioned fuzzy number
--
--    SI     - The dimension unit
--    Gain   - The value of, in the dimension units
--    Offset - The dimension unit shift
--
   type Fuzzy_Measure (SI : Unit := Units.Base.Unitless) is record
      Gain   : Fuzzy_Float;
      Offset : Number'Base := 0.0;
   end record;
--
-- Convert -- Measure conversion
--
--    Value - The fuzzy measurement to convert
--    Scale - The measure of the result
--
-- This function is used  to  convert  a  fuzzy  measure  Value  to  the
-- measurement units specified by the parameter Scale. When  offsets  of
-- Value and Scale are same this is the null operation.
--
--    Convert (T, Celsius)  -- Temperature in Celsius degrees
--
-- Returns :
--
--    Scale equivalent of Value
--
-- Exceptions :
--
--    Unit_Error -- Item and Scale have different units
--
   function Convert (Value : Fuzzy_Measure; Scale : Measure)
      return Fuzzy_Measure;
--
-- Equal -- Equality
--
--    Left  - The first argument
--    Right - The first argument
--
-- Returns :
--
--    True if Left is identic to Right
--
   function Equal (Left, Right : Fuzzy_Measure) return Boolean;
--
-- Get_Value -- Get SI value
--
--    Value  - The fuzzy measure
--
-- Returns :
--
--    SI equivalent of Value as a fuzzy number
--
   function Get_Value (Value : Fuzzy_Measure) return Fuzzy_Float;
--
-- Get_Value_As -- Get value measured in non-SI units
--
--    Value  - The fuzzy measure
--    Scale  - The measure of the result
--
-- This  function  is  used to get the value in units other than SI. For
-- instance:
--
--    Get_Value_As (T, Celsius)  -- Temperatures in Celsius degrees
--
-- Returns :
--
--    Scale equivalent of Value in as a fuzzy number
--
-- Exceptions :
--
--    Unit_Error -- Value and Scale have different units
--
   function Get_Value_As (Value : Fuzzy_Measure; Scale : Measure)
      return Fuzzy_Float;
--
-- Get_Unit -- Get unit
--
--    Value  - The measure
--
-- Returns :
--
--    SI component
--
   function Get_Unit (Value : Fuzzy_Measure) return Unit;
--
-- Is_In -- Membership test
--
--    Left  - The first argument
--    Right - The second argument
--
-- One of the parameters shall be a fuzzy measure. Other can be interval
-- measure, measure or fuzzy measure. Tests can be applied to parameters
-- of different units, in which case the result is Certain_False.
--
-- Returns :
--
--    ( Possibility (Right | Left), Necessity (Right | Left) )
--
   function Is_In (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function Is_In (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function Is_In (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function Is_In (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function Is_In (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
--
-- Necessity -- The possibility theory
--
--    Left  - A measure / interval measure / fuzzy measure
--    Right - A measure / interval measure / fuzzy measure
--
-- The result tells how  necessary  is  Left  given  Right.  It  is  the
-- inversed confidence level of the most nested interval  of  Right  not
-- intersecting  Left.  The  arguments  may  have  different shifts. The
-- result is a lower estimation of the precise result.
--
-- Returns :
--
--    Necessity (Left | Right}
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function Necessity (Left, Right : Fuzzy_Measure)
      return Confidence;
   function Necessity (Left : Measure; Right : Fuzzy_Measure)
      return Confidence;
   function Necessity (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Confidence;
   function Necessity (Left : Fuzzy_Measure; Right : Measure)
      return Confidence;
   function Necessity (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Confidence;
--
-- Normalize -- Shift removing
--
--    Value - Fuzzy measure
--
-- This  function  is  used  to  convert  Value  to  to  its   unshifted
-- equivalent.
--
-- Returns :
--
--    Unshifted equivalent of Value
--
   function Normalize (Value : Fuzzy_Measure) return Fuzzy_Measure;
--
-- Possibility -- The possiblity theory
--
--    Left  - A measure / interval measure / fuzzy measure
--    Right - A measure / interval measure / fuzzy measure
--
-- The  result  tells  how  possible  is  Left  given  Right.  It is the
-- confidence  level  of  the most nested interval of Right intersecting
-- Left. The arguments may have different shifts.
--
-- Returns :
--
--    Possibility (Left | Right}
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function Possibility (Left, Right : Fuzzy_Measure)
      return Confidence;
   function Possibility (Left : Measure; Right : Fuzzy_Measure)
      return Confidence;
   function Possibility (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Confidence;
   function Possibility (Left : Fuzzy_Measure; Right : Measure)
      return Confidence;
   function Possibility (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Confidence;
--
-- Shift -- Non-destructive shift
--
--    Value  - Fuzzy measure
--    Shift  - Shift
--
-- This function is used to convert Value to to its shifted equivalent.
--
-- Returns :
--
--    Shifted equivalent of Value
--
   function Shift
            (  Value : Fuzzy_Measure;
               Shift : Number'Base
            )  return Fuzzy_Measure;
--
-- To_Fuzzy -- Conversion to fuzzy
--
--    Left - Number, measure, interval, interval measure, fuzzy number
--
-- Returns :
--
--    Fuzzy equivalent
--
   function To_Fuzzy_Measure (Left : Number)
      return Fuzzy_Measure;
   function To_Fuzzy_Measure (Left : Measure)
      return Fuzzy_Measure;
   function To_Fuzzy_Measure (Left : Interval)
      return Fuzzy_Measure;
   function To_Fuzzy_Measure (Left : Interval_Measure)
      return Fuzzy_Measure;
   function To_Fuzzy_Measure (Left : Fuzzy_Float)
      return Fuzzy_Measure;
--
-- abs -- Absolute value
--
--    Left - The argument
--
-- Returns :
--
--    Absolute value
--
   function "abs" (Left : Fuzzy_Measure) return Fuzzy_Measure;
--
-- + -- Unary plus
--
--    Left - The operand
--
-- Returns :
--
--    Left
--
   function "+" (Left : Fuzzy_Measure) return Fuzzy_Measure;
--
-- - -- Unary minus
--
--    Left - The operand
--
-- Returns :
--
--    -Left
--
   function "-" (Left : Fuzzy_Measure) return Fuzzy_Measure;
--
-- + -- Addition
--
--    Left  - A measure / interval measure / fuzzy measure
--    Right - A measure / interval measure / fuzzy measure
--
-- Returns :
--
--      Left + Right
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "+" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "+" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure;
   function "+" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "+" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure;
   function "+" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
--
-- - -- Subtraction
--
--    Left  - A measure / interval measure / fuzzy measure
--    Right - A measure / interval measure / fuzzy measure
--
-- Returns :
--
--      Left - Right
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "-" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "-" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure;
   function "-" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "-" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure;
   function "-" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
--
-- * -- Multiplication
--
--    Left  - First argument
--    Right - Second argument
--
-- One  of  the  argument  is  a  fuzzy measure another is either of the
-- following types:  fuzzy  measure,  fuzzy  number,  number,  interval,
-- interval  measure.  Additionally  multiplication  is  defined  on   a
-- dimensioned number or interval and a  fuzzy  number  resulting  in  a
-- dimensioned fuzzy number.
--
-- Returns :
--
--    Left * Right
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--    Unit_Error       - Incompatible units
--
   function "*" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Measure; Right : Number)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Measure; Right : Interval)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "*" (Left : Number; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Interval; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Float; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "*" (Left : Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "*" (Left : Interval_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Float; Right : Measure)
      return Fuzzy_Measure;
   function "*" (Left : Fuzzy_Float; Right : Interval_Measure)
      return Fuzzy_Measure;
--
-- / -- Division
--
--    Left  - First argument
--    Right - Second argument
--
-- One  of  the  argument  is  a  fuzzy measure another is either of the
-- following types:  fuzzy  measure,  fuzzy  number,  number,  interval,
-- interval  measure.  Additionally division is defined on a dimensioned
-- number or interval and a fuzzy  number  resulting  in  a  dimensioned
-- fuzzy number.
--
-- Returns :
--
--    Left / Right
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--    Unit_Error       - Incompatible units
--
   function "/" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Measure; Right : Number)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Measure; Right : Interval)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "/" (Left : Number; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Interval; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Float; Right : Fuzzy_Measure)
      return Fuzzy_Measure;
   function "/" (Left : Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "/" (Left : Interval_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Float; Right : Measure)
      return Fuzzy_Measure;
   function "/" (Left : Fuzzy_Float; Right : Interval_Measure)
      return Fuzzy_Measure;
--
-- ** -- Exponentiation
--
--    Left  - First argument
--    Right - Second argument (natural power)
--
-- Returns :
--
--    Left ** Right
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--
   function "**" (Left : Fuzzy_Measure; Right : Natural)
      return Fuzzy_Measure;
--
-- >, >=, =, /=, <, <= -- Comparisons
--
--    Left  - A measure / interval measure / fuzzy measure
--    Right - A measure / interval measure / fuzzy measure
--
-- Comparisons are allowed for differently shifted arguments. One of the
-- arguments can be measure or plain interval.
--
-- Returns :
--
--    The result of comparison
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function ">" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function ">" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function ">" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function ">" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function ">" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

   function ">=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function ">=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function ">=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function ">=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function ">=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

   function "=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function "=" ( Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function "=" ( Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

   function "/=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "/=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function "/=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "/=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function "/=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

   function "<" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "<" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function "<" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "<" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function "<" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

   function "<=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "<=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean;
   function "<=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;
   function "<=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean;
   function "<=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean;

private
   pragma Inline (Convert);
   pragma Inline (Get_Value);
   pragma Inline (Get_Value_As);
   pragma Inline (Get_Unit);
   pragma Inline (Normalize);
   pragma Inline (Shift);
   pragma Inline (To_Fuzzy_Measure);
end Fuzzy.Measures;
