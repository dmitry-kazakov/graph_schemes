--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures.Linguistics                  Luebeck            --
--  Implementation                                 Autumn, 2006       --
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

with Units.Base;  use Units.Base;

package body Fuzzy.Measures.Linguistics is

   procedure Append
             (  Var   : in out Variable_Measure;
                Value : Measure;
                Level : Confidence
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      elsif Var.Offset = Value.Offset then
         Append (Var.Gain, Value.Gain, Level);
      else
         Append (Var.Gain, Get_Value (Value) - Var.Offset, Level);
      end if;
   end Append;

   function Convert (Value : Variable_Measure; Scale : Measure)
      return Variable_Measure is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      elsif Value.Offset = Scale.Offset then
         return Value;
      else
         return
         (  SI     => Value.SI,
            Gain   => Value.Gain + (Value.Offset - Scale.Offset),
            Offset => Scale.Offset
         );
      end if;
   end Convert;

   function Empty (Scale : Measure) return Variable_Measure is
   begin
      return
      (  SI     => Scale.SI,
         Gain   => Empty,
         Offset => Scale.Offset
      );
   end Empty;

   function Empty (SI : Unit; Offset : Number'Base := 0.0)
      return Variable_Measure is
   begin
      return
      (  SI     => SI,
         Gain   => Empty,
         Offset => Offset
      );
   end Empty;

   procedure Erase (Var : in out Variable_Measure) is
   begin
      Erase (Var.Gain);
   end Erase;

   function Equal
            (  A, B : Variable_Measure;
               Eps  : Number'Base := 0.0
            )  return Boolean is
   begin
      return
      (  A.SI = B.SI
      and then
         abs (A.Offset - B.Offset) <= Eps
      and then
         Equal (A.Gain, B.Gain, Eps)
      );
   end Equal;

   function Equal
            (  A   : Variable_Measure;
               B   : Pair_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean is
   begin
      return
      (  A.SI = B.Value.SI
      and then
         abs (A.Offset - B.Value.Offset) <= Eps
      and then
         Equal (A.Gain, Pair'(B.Value.Gain, B.Level), Eps)
      );
   end Equal;

   function Equal
            (  A   : Pair_Measure;
               B   : Variable_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean is
   begin
      return Equal (B, A, Eps);
   end Equal;

   function Equal
            (  A   : Pair_Measure;
               B   : Pair_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean is
   begin
      return
      (  A.Value.SI = B.Value.SI
      and then
         abs (A.Value.Offset - B.Value.Offset) <= Eps
      and then
         Equal
         (  Pair'(A.Value.Gain, A.Level),
            Pair'(B.Value.Gain, B.Level),
            Eps
      )  );
   end Equal;

   procedure Find
             (  Var   : Variable_Measure;
                Span  : Interval_Measure;
                From  : out Positive;
                To    : out Natural
             )  is
   begin
      if Var.SI /= Span.SI then
         raise Unit_Error;
      else
         declare
            Shift : constant Number'Base :=
                       Span.Offset - Var.Offset;
         begin
            Find
            (  Var.Gain,
               (Span.From + Shift, Span.To + Shift),
               From,
               To
            );
         end;
      end if;
   end Find;

   procedure Get
             (  Var   : Variable_Measure;
                Value : Measure;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      else
         Get
         (  Var.Gain,
            Value.Gain + Value.Offset - Var.Offset,
            Left,
            Min,
            Max,
            Right
         );
      end if;
   end Get;

   procedure Get_Point
             (  Var   : Variable_Measure;
                Index : Positive;
                Value : out Measure;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             )  is
      Abscissa : Number;
   begin
      Get_Point
      (  Var.Gain,
         Index,
         Abscissa,
         Left,
         Min,
         Max,
         Right
      );
      Value := (SI => Var.SI, Gain => Abscissa, Offset => Var.Offset);
   end Get_Point;

   function Get_Points_Number (Var : Variable_Measure) return Natural is
   begin
      return Get_Points_Number (Var.Gain);
   end Get_Points_Number;

   function Get_Span (Var : Variable_Measure) return Interval_Measure is
      Span : constant Interval := Get_Span (Var.Gain);
   begin
      return
      (  SI     => Var.SI,
         From   => Span.From,
         To     => Span.To,
         Offset => Var.Offset
      );
   end Get_Span;

   function Get_Value (Var : Variable_Measure; Index : Positive)
      return Measure is
   begin
      return
      (  SI     => Var.SI,
         Gain   => Get_Value (Var.Gain, Index),
         Offset => Var.Offset
      );
   end Get_Value;

   function Get_Value (Value : Variable_Measure) return Variable is
   begin
      return Value.Gain + Value.Offset;
   end Get_Value;

   function Get_Value_As (Value : Variable_Measure; Scale : Measure)
      return Variable is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return
            (Value.Gain + (Value.Offset - Scale.Offset)) / Scale.Gain;
      end if;
   end Get_Value_As;

   function Get_Unit (Value : Variable_Measure) return Unit is
   begin
      return Value.SI;
   end Get_Unit;

   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      else
         Insert_Point
         (  Var.Gain,
            Index,
            Value.Gain + Value.Offset - Var.Offset,
            Left,
            Min,
            Max,
            Right,
            Purge
         );
      end if;
   end Insert_Point;

   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      else
         Insert_Point
         (  Var.Gain,
            Index,
            Value.Gain + Value.Offset - Var.Offset,
            Left,
            Right,
            Purge
         );
      end if;
   end Insert_Point;

   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Level : Confidence;
                Purge : Boolean := True
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      else
         Insert_Point
         (  Var.Gain,
            Index,
            Value.Gain + Value.Offset - Var.Offset,
            Level,
            Purge
         );
      end if;
   end Insert_Point;

   function Is_Empty (Var : Variable_Measure) return Boolean is
   begin
      return Is_Empty (Var.Gain);
   end Is_Empty;

   function Is_In (A, B : Variable_Measure) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Variable_Measure;
               B : Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_Interval (Var : Variable_Measure) return Boolean is
   begin
      return Is_Interval (Var.Gain);
   end Is_Interval;

   function Is_Singleton (Var : Variable_Measure) return Boolean is
   begin
      return Is_Singleton (Var.Gain);
   end Is_Singleton;

   procedure Move_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Purge : Boolean := True
             )  is
   begin
      if Var.SI /= Value.SI then
         raise Unit_Error;
      else
         Move_Point
         (  Var.Gain,
            Index,
            Value.Gain + Value.Offset - Var.Offset,
            Purge
         );
      end if;
   end Move_Point;

   function Necessity (A : Variable_Measure) return Confidence is
   begin
      return Necessity (A.Gain);
   end Necessity;

   function Necessity (A, B : Variable_Measure) return Confidence is
   begin
      if A.SI /= B.SI then
         return not Possibility (B);
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, B.Gain);
      else
         return Necessity (A.Gain, B.Gain + (B.Offset - A.Offset));
      end if;
   end Necessity;

   function Necessity
            (  A : Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return not Possibility (B);
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, B.Gain);
      else
         return Necessity (A.Gain + (A.Offset - B.Offset), B.Gain);
      end if;
   end Necessity;

   function Necessity
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return not Possibility (B);
      elsif A.Offset = B.Offset then
         return Necessity (Interval'(A.From, A.To), B.Gain);
      else
         return
            Necessity
            (  (Interval'(A.From, A.To) + A.Offset) - B.Offset,
               B.Gain
            );
      end if;
   end Necessity;

   function Necessity
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return not Possibility (B);
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, B.Gain);
      else
         return Necessity (A.Gain + (A.Offset - B.Offset), B.Gain);
      end if;
   end Necessity;

   function Necessity
            (  A : Variable_Measure;
               B : Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, B.Gain);
      else
         return Necessity (A.Gain, B.Gain + (B.Offset - A.Offset));
      end if;
   end Necessity;

   function Necessity
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, Interval'(B.From, B.To));
      else
         return
            Necessity
            (  A.Gain,
               (Interval'(B.From, B.To) + B.Offset) - A.Offset
            );
      end if;
   end Necessity;

   function Necessity
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Necessity (A.Gain, B.Gain);
      else
         return Necessity (A.Gain, B.Gain + (B.Offset - A.Offset));
      end if;
   end Necessity;

   function Normalize (Value : Variable_Measure)
      return Variable_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Value.Gain + Value.Offset,
         Offset => 0.0
      );
   end Normalize;

   function Possibility (A : Variable_Measure) return Confidence is
   begin
      return Possibility (A.Gain);
   end Possibility;

   function Possibility (A, B : Variable_Measure) return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Possibility (A.Gain, B.Gain);
      else
         return
            Possibility (A.Gain, B.Gain + (B.Offset - A.Offset));
      end if;
   end Possibility;

   function Possibility
            (  A : Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      return Possibility (B, A);
   end Possibility;

   function Possibility
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      return Possibility (B, A);
   end Possibility;

   function Possibility
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Confidence is
   begin
      return Possibility (B, A);
   end Possibility;

   function Possibility
            (  A : Variable_Measure;
               B : Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Possibility (A.Gain, B.Gain);
      else
         return
            Possibility
            (  A.Gain,
               (Interval'(B.Gain, B.Gain) + B.Offset) - A.Offset
            );
      end if;
   end Possibility;

   function Possibility
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Possibility (A.Gain, Interval'(B.From, B.To));
      else
         return
            Possibility
            (  A.Gain,
               (Interval'(B.From, B.To) + B.Offset) - A.Offset
            );
      end if;
   end Possibility;

   function Possibility
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Confidence is
   begin
      if A.SI /= B.SI then
         return Confidence'First;
      elsif A.Offset = B.Offset then
         return Possibility (A.Gain, B.Gain);
      else
         return
            Possibility
            (  A.Gain,
               (B.Gain + B.Offset) - A.Offset
            );
      end if;
   end Possibility;

   procedure Purge
             (  Var       : in out Variable_Measure;
                Keep_Ends : Boolean := False
             )  is
   begin
      Purge (Var.Gain, Keep_Ends);
   end Purge;

   procedure Remove_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Purge : Boolean := True
             )  is
   begin
      Remove_Point (Var.Gain, Index, Purge);
   end Remove_Point;

   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Set_Point (Var.Gain, Index, Left, Min, Max, Right, Purge);
   end Set_Point;

   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Set_Point (Var.Gain, Index, Left, Right, Purge);
   end Set_Point;

   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Level : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Set_Point (Var.Gain, Index, Level, Purge);
   end Set_Point;

   function Shift
            (  Value : Variable_Measure;
               Shift : Number'Base
            )  return Variable_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Value.Gain + (-Shift),
         Offset => Value.Offset + Shift
      );
   end Shift;

   function To_Interval_Measure (Value : Variable_Measure)
      return Interval_Measure is
      Result : constant Interval := To_Interval (Value.Gain);
   begin
      return
      (  SI     => Value.SI,
         From   => Result.From,
         To     => Result.To,
         Offset => Value.Offset
      );
   end To_Interval_Measure;

   function To_Measure (Value : Variable_Measure) return Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => To_Number (Value.Gain),
         Offset => Value.Offset
      );
   end To_Measure;

   function To_Variable_Measure (Value : Number)
      return Variable_Measure is
   begin
      return
      (  SI     => Units.Base.Unitless,
         Gain   => To_Variable (Value),
         Offset => 0.0
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Interval)
      return Variable_Measure is
   begin
      return
      (  SI     => Units.Base.Unitless,
         Gain   => To_Variable (Value),
         Offset => 0.0
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Fuzzy_Float)
      return Variable_Measure is
   begin
      return
      (  SI     => Units.Base.Unitless,
         Gain   => To_Variable (Value),
         Offset => 0.0
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Variable)
      return Variable_Measure is
   begin
      return
      (  SI     => Units.Base.Unitless,
         Gain   => Value,
         Offset => 0.0
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Measure)
      return Variable_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => To_Variable (Value.Gain),
         Offset => Value.Offset
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Interval_Measure)
      return Variable_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => To_Variable ((Value.From, Value.To)),
         Offset => Value.Offset
      );
   end To_Variable_Measure;

   function To_Variable_Measure (Value : Fuzzy_Measure)
      return Variable_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => To_Variable (Value.Gain),
         Offset => Value.Offset
      );
   end To_Variable_Measure;

   procedure Trim (Var : in out Variable_Measure) is
   begin
      Trim (Var.Gain);
   end Trim;

   function "+" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "-" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "*" (Left : Variable_Measure; Right : Number)
      return Variable_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
            (  SI     => Left.SI * Right.SI,
               Gain   => Left.Gain * Right.Gain,
               Offset => Right.Offset
            );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
            (  SI     => Left.SI,
               Gain   => Left.Gain * Right.Gain,
               Offset => Left.Offset
            );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Variable; Right : Measure)
      return Variable_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Right.Gain,
         Offset => Right.Offset
      );
   end "*";

   function "/" (Left : Variable_Measure; Right : Number)
      return Variable_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure is
   begin
      if (  Right.Offset = 0.0
         and then
            (  Left.Offset = 0.0
            or else
               Right.SI = Unitless
         )  )
      then
         return
         (  SI     => Left.SI / Right.SI,
            Gain   => Left.Gain / Right.Gain,
            Offset => Left.Offset
         );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Variable; Right : Measure)
      return Variable_Measure is
   begin
      if Right.Offset = 0.0 then
         return
         (  SI     => Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
      raise Unit_Error;
   end "/";

   function "=" (A, B : Variable_Measure) return Fuzzy_Boolean is
   begin
      if A.SI /= B.SI then
         return Certain_False;
      else
         return
         (  Possibility => Possibility (A, B),
            Necessity   => Necessity (A, B) and Necessity (B, A)
         );
      end if;
   end "=";

   function "=" (A : Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return B = A;
   end "=";

   function "=" (A : Interval_Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return B = A;
   end "=";

   function "=" (A : Fuzzy_Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return B = A;
   end "=";

   function "=" (A : Variable_Measure; B : Measure)
      return Fuzzy_Boolean is
   begin
      if A.SI /= B.SI then
         return Certain_False;
      else
         return
         (  Possibility => Possibility (A, B),
            Necessity   => Necessity (A, B) and Necessity (B, A)
         );
      end if;
   end "=";

   function "=" (A : Variable_Measure; B : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if A.SI /= B.SI then
         return Certain_False;
      else
         return
         (  Possibility => Possibility (A, B),
            Necessity   => Necessity (A, B) and Necessity (B, A)
         );
      end if;
   end "=";

   function "=" (A : Variable_Measure; B : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if A.SI /= B.SI then
         return Certain_False;
      else
         return
         (  Possibility => Possibility (A, B),
            Necessity   => Necessity (A, B) and Necessity (B, A)
         );
      end if;
   end "=";

   function "/=" (A, B : Variable_Measure) return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Interval_Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Fuzzy_Measure; B : Variable_Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable_Measure; B : Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable_Measure; B : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable_Measure; B : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "&" (Left : Pair_Measure; Right : Pair_Measure)
      return Variable_Measure is
   begin
      if (  Left.Value.SI /= Right.Value.SI
         or else
            Left.Value.Offset /= Right.Value.Offset
         )
      then
         raise Unit_Error;
      else
         declare
            Result : Variable_Measure (Left.Value.SI);
         begin
            Result.Offset := Left.Value.Offset;
            Append (Result.Gain, Left.Value.Gain,  Left.Level);
            Append (Result.Gain, Right.Value.Gain, Right.Level);
            return Result;
         end;
      end if;
   end "&";

   function "&" (Left : Variable_Measure; Right : Pair_Measure)
      return Variable_Measure is
   begin
      if Left.SI /= Right.Value.SI then
         raise Unit_Error;
      else
         declare
            Result : Variable_Measure := Left;
         begin
            if Left.Offset = Right.Value.Offset then
               Append (Result.Gain, Right.Value.Gain, Right.Level);
            else
               Append
               (  Result.Gain,
                  Right.Value.Gain + Right.Value.Offset - Left.Offset,
                  Right.Level
               );
            end if;
            return Result;
         end;
      end if;
   end "&";

   function "and" (A, B : Variable_Measure) return Variable_Measure is
   begin
      if A.SI /= B.SI then
         raise Unit_Error;
      elsif A.Offset = B.Offset then
         return
         (  SI     => A.SI,
            Gain   => A.Gain and B.Gain,
            Offset => A.Offset
         );
      else
         return
         (  SI     => A.SI,
            Gain   => A.Gain and B.Gain + (B.Offset - A.Offset),
            Offset => A.Offset
         );
      end if;
   end "and";

   function "and" (A : Variable_Measure; B : Confidence)
      return Variable_Measure is
   begin
      return
      (  SI     => A.SI,
         Gain   => A.Gain and B,
         Offset => A.Offset
      );
   end "and";

   function "and" (A : Confidence; B : Variable_Measure)
      return Variable_Measure is
   begin
      return B and A;
   end "and";

   function "not" (A : Variable_Measure) return Variable_Measure is
   begin
      return
      (  SI     => A.SI,
         Gain   => not A.Gain,
         Offset => A.Offset
      );
   end "not";

   function "or" (A, B : Variable_Measure) return Variable_Measure is
   begin
      if A.SI /= B.SI then
         raise Unit_Error;
      elsif A.Offset = B.Offset then
         return
         (  SI     => A.SI,
            Gain   => A.Gain or B.Gain,
            Offset => A.Offset
         );
      else
         return
         (  SI     => A.SI,
            Gain   => A.Gain or B.Gain + (B.Offset - A.Offset),
            Offset => A.Offset
         );
      end if;
   end "or";

   function "or" (A : Variable_Measure; B : Confidence)
      return Variable_Measure is
   begin
      return
      (  SI     => A.SI,
         Gain   => A.Gain or B,
         Offset => A.Offset
      );
   end "or";

   function "or" (A : Confidence; B : Variable_Measure)
      return Variable_Measure is
   begin
      return B or A;
   end "or";

   function "xor" (A, B : Variable_Measure) return Variable_Measure is
   begin
      if A.SI /= B.SI then
         raise Unit_Error;
      elsif A.Offset = B.Offset then
         return
         (  SI     => A.SI,
            Gain   => A.Gain xor B.Gain,
            Offset => A.Offset
         );
      else
         return
         (  SI     => A.SI,
            Gain   => A.Gain xor B.Gain + (B.Offset - A.Offset),
            Offset => A.Offset
         );
      end if;
   end "xor";

   function "xor" (A : Variable_Measure; B : Confidence)
      return Variable_Measure is
   begin
      return
      (  SI     => A.SI,
         Gain   => A.Gain xor B,
         Offset => A.Offset
      );
   end "xor";

   function "xor" (A : Confidence; B : Variable_Measure)
      return Variable_Measure is
   begin
      return B xor A;
   end "xor";

end Fuzzy.Measures.Linguistics;
