--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.                          Luebeck            --
--        Discrete_Center_Of_Gravity               Autumn, 2005       --
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

function Fuzzy.Linguistics.Discrete_Center_Of_Gravity (A : Variable)
   return Number is
   Size : constant Natural := Get_Points_Number (A);
   Sum  : Interval := (0.0, 0.0);
   Norm : Interval := (0.0, 0.0);
begin
   if Size < 1 then
      raise Constraint_Error;
   end if;
   for Index in 1..Size loop
      declare
         This : Point renames Get (A.Data, Index);
      begin
         if This.Left /= 0.0 or else This.Right /= 0.0 then
            raise Constraint_Error;
         end if;
         Sum :=
            Sum  + Number (This.Max) * To_Interval (This.Value);
         Norm := Norm + Number (This.Max);
      end;
   end loop;
   Sum := Sum / Norm;
   declare
      Result : constant Number := (Sum.From + Sum.To) / 2.0;
   begin
      if Result'Valid then
         return Result;
      end if;
   end;
   raise Constraint_Error;
end Fuzzy.Linguistics.Discrete_Center_Of_Gravity;
