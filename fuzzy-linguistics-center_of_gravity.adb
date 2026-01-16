--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.                          Luebeck            --
--        Center_Of_Gravity                        Autumn, 2005       --
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
--  needed for literals, like Number'(1.5)
--
function Fuzzy.Linguistics.Center_Of_Gravity (A : Variable)
   return Number is
   Size   : constant Natural := Get_Points_Number (A);
   YS, XS : Interval := (0.0, 0.0);
   DX     : Interval;
   Y1, Y2 : Interval;
   Left   : Point;
   Right  : Point;
begin
   if Size < 2 then
      raise Constraint_Error;
   end if;
   Left := Get (A.Data, 1);
   if Left.Left /= 0.0 or else Get (A.Data, Size).Right /= 0.0 then
      raise Constraint_Error;
   end if;
   for Index in 2..Size loop
      Right := Get (A.Data, Index);
      DX := Right.Value - To_Interval (Left.Value);
      Y1 := To_Interval (Number (Left.Right));
      Y2 := To_Interval (Number (Right.Left));
      YS :=
         (  YS
         +  (  DX
            *  (  ((Y2 - Y1) * DX) / Number'(1.5)
               +  Y1 * Right.Value
               +  Y2 * Left.Value
         )  )  );
      XS := XS + (Y1 + Y2) * DX;
      Left := Right;
   end loop;
   YS := YS / XS;
   declare
      Result : constant Number := (YS.From + YS.To) / 2.0;
   begin
      if Result'Valid then
         return Result;
      end if;
   end;
   raise Constraint_Error;
end Fuzzy.Linguistics.Center_Of_Gravity;
