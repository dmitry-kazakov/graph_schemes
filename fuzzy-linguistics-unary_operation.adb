--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Unary_Operation           Luebeck            --
--  Separate body implementation                   Summer, 2003       --
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

separate (Fuzzy.Linguistics)
   procedure Unary_Operation
             (  Data : in out User_Data;
                A    : Variable
             )  is
      Size : constant Natural := Get_Size (A.Data);
      This : Point;
      Left : Pair;

      procedure Do_Singleton is
      begin
         Do_Point (Data, This.Value, This.Left);
         if This.Min /= This.Left then
            Do_Point (Data, This.Value, This.Min);
         end if;
         if This.Max /= This.Min then
            Do_Point (Data, This.Value, This.Max);
         end if;
         if This.Right /= This.Max then
            Do_Point (Data, This.Value, This.Right);
         end if;
      end Do_Singleton;

begin
   if Size = 0 then
      Do_Empty (Data);
   else
      This := Get (A.Data, 1);
      Do_Singleton;
      Left.Value := This.Value;
      Left.Level := This.Right;
      for Index in 2..Size loop
         This := Get (A.Data, Index);
         Do_Interval
         (  Data,
            Left.Value,
            Left.Level,
            This.Value,
            This.Left
         );
         Do_Singleton;
         Left.Value := This.Value;
         Left.Level := This.Right;
      end loop;
   end if;
end Unary_Operation;
