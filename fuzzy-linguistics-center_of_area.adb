--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.                          Luebeck            --
--        Center_Of_Area                           Autumn, 2005       --
--  Implementation                                                    --
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
--  GNAT 7.1.1 20170622 workaround,  no qualified expression  should  be
--  needed for literals, like Number'(0.0)
--
with Intervals;

function Fuzzy.Linguistics.Center_Of_Area (A : Variable)
   return Number is
   function Get_Area (Index : Integer) return Interval is
      pragma Inline (Get_Area);
      Left  : Point renames Get (A.Data, Index);
      Right : Point renames Get (A.Data, Index + 1);
   begin
      return
      (  (  To_Interval (Number (Left.Right))
         +  Number (Right.Left)
         )
      *  (To_Interval (Right.Value) - Left.Value)
      /  Number'(2.0)
      );
   end;

   Size  : constant Natural := Get_Points_Number (A);
   Left  : Natural  := 1;
   Right : Natural  := Size;
   DS    : Interval := (0.0, 0.0);   -- Right - left side area
begin
   if (  Size < 2
      or else
         Get (A.Data, 1).Left /= 0.0
      or else
         Get (A.Data, Size).Right /= 0.0
      )
   then
      raise Constraint_Error;
   end if;
   while Left < Right loop
      case DS > Number'(0.0) is
         when Intervals.True =>
            DS := DS - Get_Area (Left);
            Left := Left + 1;
         when Intervals.False | Intervals.Uncertain =>
            Right := Right - 1;
            DS := DS + Get_Area (Right);
      end case;
   end loop;
   case DS > Number'(0.0) is
      when Intervals.True =>
         DS := DS - Get_Area (Right);
         Right := Right + 1;
      when Intervals.False =>
         Left := Left - 1;
         DS := DS + Get_Area (Left);
      when Intervals.Uncertain =>
         return Get (A.Data, Left).Value;
   end case;
   --
   -- At this stage we have to find the center of area of  a  trapezoid.
   -- Whose  right side is weighted with DS (the difference in the areas
   -- on the right and left sides).
   --
   --                . Y2    S1 = 2*S2 + DS
   --               /|       X = 0.5*(X1+X2+T)
   --              / |
   --             /S2|      S1 = Y1*T + 0.5*tgﬂ*T**2
   --        ..../___|    2*S2 = tgﬂ*(0.5*(DX-T))**2
   --        :  /|   |      DS = RS - LS
   --        : / |   |     tgﬂ = (Y2-Y1)/(X2-X1)
   --        :/  |   |
   --        /   |   |   -------------------------------------------
   --       /|   |   |       DY*T**2 + B*T - C = 0
   --      / |   |   |
   --  Y1 /  |   |   |    DY = Y2-Y1
   --    |ﬂ  |   |   |    DX = X2-X2
   --    | S1|   |   |    B  = 2*DX*(Y1+Y2)
   --   -|---|---|---|    C  = DX*(DX*DY + 4.0*DS)
   --  X1|<T>|   X   X2
   --
   -- The  quadratic  equation  will  be solved using sections method
   -- instead  of  direct  formula, because A and B might be close or
   -- zero. The methods performs iterations as follows:
   --
   --    f(T)=0; T in [L, U]; sign f(L) /= sign f(U)
   --
   --    M = L - f(L) * (U-L) / (f(U)-f(L)), which with a quadratic f
   --                                        above gives:
   --
   --    M = (A*L*U + C) / (A*(L+U) + B)
   --
   declare
      use Intervals;
      From : Point renames Get (A.Data, Left);
      To   : Point renames Get (A.Data, Right);
      DX : constant Interval := To_Interval (To.Value) - From.Value;
      Y1 : constant Number := Number (From.Right);
      Y2 : constant Number := Number (To.Left);
      DY : constant Interval     := To_Interval (Y2) - Y1;
      B  : constant Interval     := Number'(2.0) * DX * (Y1 + Y2);
      C  : constant Interval     := DX * (DX * DY + Number'(4.0) * DS);
      L  : Interval := To_Interval (0.0);
      U  : Interval := DX;
      T  : Interval;
      Positive_Left : constant Boolean :=
                               (C < Number'(0.0)) = Intervals.True;
   begin
      loop
         T := (DY*L*U + C) / (DY*(L+U) + B);
         exit when T & U or else T & L;
         case (DY*T + B)*T - C > Number'(0.0) is
            when Intervals.True =>
               if Positive_Left then
                  L := T;
               else
                  U := T;
               end if;
            when Intervals.False =>
               if Positive_Left then
                  U := T;
               else
                  L := T;
               end if;
            when Intervals.Uncertain =>
               L := To_Interval (T.From);
               U := To_Interval (T.To);
         end case;
      end loop;
      T := ((T + To.Value) + From.Value) / Number'(2.0);
      declare
         Result : constant Number := (T.From + T.To) / 2.0;
      begin
         if Result'Valid then
            return Result;
         end if;
      end;
      raise Constraint_Error;
   end;
end Fuzzy.Linguistics.Center_Of_Area;
