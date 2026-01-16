--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures                              Luebeck            --
--  Implementation                                 Spring, 2005       --
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

package body Fuzzy.Measures is

   function Shift (Left, Right : Number'Base) return Interval is
      pragma Inline (Shift);
   begin
      return (Left, Left) - Right;
   end Shift;

   function Equal (Left, Right : Fuzzy_Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         return False;
      elsif Left.Offset = Right.Offset then
         return Equal (Left.Gain, Right.Gain);
      else
         return
            Equal
            (  Left.Gain + Shift (Left.Offset, Right.Offset),
               Right.Gain
            );
      end if;
   end Equal;

   function Convert (Value : Fuzzy_Measure; Scale : Measure)
      return Fuzzy_Measure is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return
         (  SI     => Value.SI,
            Gain   => Value.Gain + Shift (Value.Offset, Scale.Offset),
            Offset => Scale.Offset
         );
      end if;
   end Convert;

   function Get_Value (Value : Fuzzy_Measure) return Fuzzy_Float is
   begin
      return Value.Gain + Value.Offset;
   end Get_Value;

   function Get_Value_As (Value : Fuzzy_Measure; Scale : Measure)
      return Fuzzy_Float is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return
         (  (  Value.Gain
            +  (  (Value.Offset, Value.Offset)
               -  Scale.Offset
            )  )
         /  Scale.Gain
         );
      end if;
   end Get_Value_As;

   function Get_Unit (Value : Fuzzy_Measure) return Unit is
   begin
      return Value.SI;
   end Get_Unit;

   function Is_In (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Is_In (Left.Gain, Right.Gain);
      else
         return     -- All computations on the right side to esure a
            Is_In   -- upper possibility and lower necessity
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Is_In;

   function Is_In (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Is_In (Left.Gain, Right.Gain);
      else
         return
            Is_In
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Is_In;

   function Is_In (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Is_In (Interval'(Left.From, Left.To), Right.Gain);
      else
         return
            Is_In
            (  Interval'(Left.From, Left.To),
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Is_In;

   function Is_In (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Is_In (Left.Gain, Right.Gain);
      else
         return
            Is_In
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Is_In;

   function Is_In (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Is_In (Left.Gain, Interval'(Right.From, Right.To));
      else
         return
            Is_In
            (  Left.Gain,
               (  Interval'(Right.From, Right.To)
               +  Shift (Right.Offset, Left.Offset)
            )  );
      end if;
   end Is_In;

   function Necessity (Left, Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Necessity (Left.Gain, Right.Gain);
      else
         return           -- To be a lower estimation all computations
            Necessity     -- have to made on the right side
            (  Left.Gain,
               (Right.Gain + Right.Offset) - Left.Offset
            );
      end if;
   end Necessity;

   function Necessity (Left : Measure; Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Necessity (Left.Gain, Right.Gain);
      else
         return
            Necessity
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Necessity;

   function Necessity (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Necessity (Interval'(Left.From, Left.To), Right.Gain);
      else
         return
            Necessity
            (  Interval'(Left.From, Left.To),
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Necessity;

   function Necessity (Left : Fuzzy_Measure; Right : Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Necessity (Left.Gain, Right.Gain);
      else
         return
            Necessity
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Necessity;

   function Necessity (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Necessity (Left.Gain, Interval'(Right.From, Right.To));
      else
         return
            Necessity
            (  Left.Gain,
               (  Interval'(Right.From, Right.To)
               +  Shift (Right.Offset, Left.Offset)
            )  );
      end if;
   end Necessity;

   function Normalize (Value : Fuzzy_Measure) return Fuzzy_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Value.Gain + Value.Offset,
         Offset => 0.0
      );
   end Normalize;

   function Possibility (Left, Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Possibility (Left.Gain, Right.Gain);
      else
         return
            Possibility
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Possibility;

   function Possibility (Left : Measure; Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Possibility (Left.Gain, Right.Gain);
      else
         return
            Possibility
            (  Left.Gain + Shift (Left.Offset, Right.Offset),
               Right.Gain
            );
      end if;
   end Possibility;

   function Possibility (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Possibility (Interval'(Left.From, Left.To), Right.Gain);
      else
         return
            Possibility
            (  (  Interval'(Left.From, Left.To)
               +  Shift (Left.Offset, Right.Offset)
               ),
               Right.Gain
            );
      end if;
   end Possibility;

   function Possibility (Left : Fuzzy_Measure; Right : Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Possibility (Left.Gain, Right.Gain);
      else
         return
            Possibility
            (  Left.Gain,
               Right.Gain + Shift (Right.Offset, Left.Offset)
            );
      end if;
   end Possibility;

   function Possibility (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Confidence is
   begin
      if Left.SI /= Right.SI then
         return Confidence'First;
      elsif Left.Offset = Right.Offset then
         return Possibility (Left.Gain, Interval'(Right.From, Right.To));
      else
         return
            Possibility
            (  Left.Gain,
               (  Interval'(Right.From, Right.To)
               +  Shift (Right.Offset, Left.Offset)
            )  );
      end if;
   end Possibility;

   function Shift
            (  Value : Fuzzy_Measure;
               Shift : Number'Base
            )  return Fuzzy_Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Value.Gain   - Shift,
         Offset => Value.Offset + Shift
      );
   end Shift;

   function To_Fuzzy_Measure (Left : Number) return Fuzzy_Measure is
   begin
      return
      (  SI     => Unitless,
         Gain   => To_Fuzzy (Left),
         Offset => 0.0
      );
   end To_Fuzzy_Measure;

   function To_Fuzzy_Measure (Left : Measure) return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => To_Fuzzy (Left.Gain),
         Offset => Left.Offset
      );
   end To_Fuzzy_Measure;

   function To_Fuzzy_Measure (Left : Interval) return Fuzzy_Measure is
   begin
      return
      (  SI     => Unitless,
         Gain   => To_Fuzzy (Left),
         Offset => 0.0
      );
   end To_Fuzzy_Measure;

   function To_Fuzzy_Measure (Left : Interval_Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => To_Fuzzy ((Left.From, Left.To)),
         Offset => Left.Offset
      );
   end To_Fuzzy_Measure;

   function To_Fuzzy_Measure (Left : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Unitless,
         Gain   => Left,
         Offset => 0.0
      );
   end To_Fuzzy_Measure;

   function "abs" (Left : Fuzzy_Measure) return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => abs Left.Gain,
         Offset => Left.Offset
      );
   end "abs";

   function "+" (Left : Fuzzy_Measure) return Fuzzy_Measure is
   begin
      return Left;
   end "+";

   function "-" (Left : Fuzzy_Measure) return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => -Left.Gain,
         Offset => Left.Offset
      );
   end "-";

   function "+" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Interval'(Right.From, Right.To),
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Interval'(Left.From, Left.To) + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "-" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Interval'(Left.From, Left.To) - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Interval'(Right.From, Right.To),
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "*" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure is
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

   function "*" (Left : Fuzzy_Measure; Right : Number)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Fuzzy_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure is
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

   function "*" (Left : Fuzzy_Measure; Right : Interval)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
            (  SI     => Left.SI * Right.SI,
               Gain   => Left.Gain * Interval'(Right.From, Right.To),
               Offset => Right.Offset
            );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
            (  SI     => Left.SI,
               Gain   => Left.Gain * Interval'(Right.From, Right.To),
               Offset => Left.Offset
            );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Number; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Right.Gain,
         Offset => Right.Offset
      );
   end "*";

   function "*" (Left : Fuzzy_Float; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Right.Gain,
         Offset => Right.Offset
      );
   end "*";

   function "*" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
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

   function "*" (Left : Interval; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Right.Gain,
         Offset => Right.Offset
      );
   end "*";

   function "*" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
            (  SI     => Left.SI * Right.SI,
               Gain   => Interval'(Left.From, Left.To) * Right.Gain,
               Offset => Right.Offset
            );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
            (  SI     => Left.SI,
               Gain   => Interval'(Left.From, Left.To) * Right.Gain,
               Offset => Left.Offset
            );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Fuzzy_Float; Right : Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Right.Gain,
         Offset => Right.Offset
      );
   end "*";

   function "*" (Left : Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Interval_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Interval'(Left.From, Left.To) * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left : Fuzzy_Float; Right : Interval_Measure)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Left * Interval'(Right.From, Right.To),
         Offset => Right.Offset
      );
   end "*";

   function "/" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Measure is
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

   function "/" (Left : Fuzzy_Measure; Right : Number)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Fuzzy_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Measure is
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

   function "/" (Left : Fuzzy_Measure; Right : Interval)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Measure is
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
            Gain   => Left.Gain / Interval'(Right.From, Right.To),
            Offset => Left.Offset
         );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Number; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
   end "/";

   function "/" (Left : Fuzzy_Float; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
   end "/";

   function "/" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
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

   function "/" (Left : Interval; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
   end "/";

   function "/" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Measure is
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
            Gain   => Interval'(Left.From, Left.To) / Right.Gain,
            Offset => Left.Offset
         );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Interval_Measure; Right : Fuzzy_Float)
      return Fuzzy_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Interval'(Left.From, Left.To) / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/" (Left : Fuzzy_Float; Right : Measure)
      return Fuzzy_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
   end "/";

   function "/" (Left : Fuzzy_Float; Right : Interval_Measure)
      return Fuzzy_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Interval'(Right.From, Right.To),
            Offset => 0.0
         );
      end if;
   end "/";

   function "**" (Left : Fuzzy_Measure; Right : Natural)
      return Fuzzy_Measure is
   begin
      if Left.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  Left.SI ** Right,
            Left.Gain ** Right,
            0.0
         );
      end if;
   end "**";

   function ">" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain > Right.Gain;
      else
         return
         (  Left.Gain
         >  Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end ">";

   function ">" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain > Right.Gain;
      else
         return
         (  Left.Gain
         >  Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end ">";

   function ">" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain > Right.Gain;
      else
         return
         (  Left.Gain + Shift (Left.Offset, Right.Offset)
         >  Right.Gain
         );
      end if;
   end ">";

   function ">" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain > Interval'(Right.From, Right.To);
      else
         return
         (  Left.Gain
         >  (  Interval'(Right.From, Right.To)
            +  Shift (Right.Offset, Left.Offset)
         )  );
      end if;
   end ">";

   function ">" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Interval'(Left.From, Left.To) > Right.Gain;
      else
         return
         (  (  Interval'(Left.From, Left.To)
            +  Shift (Left.Offset, Right.Offset)
            )
         >  Right.Gain
         );
      end if;
   end ">";

   function ">=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain >= Right.Gain;
      else
         return
         (  Left.Gain
         >= Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end ">=";

   function ">=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain >= Right.Gain;
      else
         return
         (  Left.Gain
         >= Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end ">=";

   function ">=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain >= Right.Gain;
      else
         return
         (  Left.Gain + Shift (Left.Offset, Right.Offset)
         >= Right.Gain
         );
      end if;
   end ">=";

   function ">=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Left.Gain >= Interval'(Right.From, Right.To);
      else
         return
         (  Left.Gain
         >= (  Interval'(Right.From, Right.To)
            +  Shift (Right.Offset, Left.Offset)
         )  );
      end if;
   end ">=";

   function ">=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      elsif Left.Offset = Right.Offset then
         return Interval'(Left.From, Left.To) >= Right.Gain;
      else
         return
         (  (  Interval'(Left.From, Left.To)
            +  Shift (Left.Offset, Right.Offset)
            )
         >= Right.Gain
         );
      end if;
   end ">=";

   function "=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Left.Gain = Right.Gain;
      else
         return
         (  Left.Gain
         =  Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end "=";

   function "=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Left.Gain = Right.Gain;
      else
         return
         (  Left.Gain
         =  Right.Gain + Shift (Right.Offset, Left.Offset)
         );
      end if;
   end "=";

   function "=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Left.Gain = Right.Gain;
      else
         return
         (  Left.Gain + Shift (Left.Offset, Right.Offset)
         =  Right.Gain
         );
      end if;
   end "=";

   function "=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Left.Gain = Interval'(Right.From, Right.To);
      else
         return
         (  Left.Gain
         =  (  Interval'(Right.From, Right.To)
            +  Shift (Right.Offset, Left.Offset)
         )  );
      end if;
   end "=";

   function "=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      if Left.SI /= Right.SI then
         return Certain_False;
      elsif Left.Offset = Right.Offset then
         return Interval'(Left.From, Left.To) = Right.Gain;
      else
         return
         (  (  Interval'(Left.From, Left.To)
            +  Shift (Left.Offset, Right.Offset)
            )
         =  Right.Gain
         );
      end if;
   end "=";

   function "/=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "<" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right > Left;
   end "<";

   function "<=" (Left, Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Fuzzy_Measure; Right : Measure)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Fuzzy_Measure; Right : Interval_Measure)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Interval_Measure; Right : Fuzzy_Measure)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

end Fuzzy.Measures;
