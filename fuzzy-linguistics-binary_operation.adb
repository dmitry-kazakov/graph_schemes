--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Binary_Operation          Luebeck            --
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
   procedure Binary_Operation
             (  Data : in out User_Data;
                A, B : Variable
             )  is
   A_First : Positive;
   B_First : Positive;
   A_Last  : Natural;
   B_Last  : Natural;

   procedure Do_Singleton
             (  Data : in out User_Data;
                X    : Number;
                A    : Operand;
                B    : Operand
             )  is
      pragma Inline (Do_Singleton);
   begin
      Do_Point (Data, X, A.This.Left, B.This.Left);
      if (  A.This.Min /= A.This.Left
         or else
            B.This.Min /= B.This.Left
         )
      then
         Do_Point (Data, X, A.This.Min, B.This.Min);
      end if;
      if (  A.This.Max /= A.This.Min
         or else
            B.This.Max /= B.This.Min
         )
      then
         Do_Point (Data, X, A.This.Max, B.This.Max);
      end if;
      Do_Point (Data, X, A.This.Right, B.This.Right);
   end Do_Singleton;

   procedure Do_Singleton
             (  Data : in out User_Data;
                X    : Number;
                A    : Operand;
                B    : Confidence
             )  is
      pragma Inline (Do_Singleton);
   begin
      Do_Point (Data, X, A.This.Left, B);
      if A.This.Min /= A.This.Left then
         Do_Point (Data, X, A.This.Min, B);
      end if;
      if A.This.Max /= A.This.Min then
         Do_Point (Data, X, A.This.Max, B);
      end if;
      if A.This.Right /= A.This.Max then
         Do_Point (Data, X, A.This.Right, B);
      end if;
   end Do_Singleton;

   procedure Do_Singleton
             (  Data : in out User_Data;
                X    : Number;
                A    : Confidence;
                B    : Operand
             )  is
      pragma Inline (Do_Singleton);
   begin
      Do_Point (Data, X, A, B.This.Left);
      if B.This.Min /= B.This.Left then
         Do_Point (Data, X, A, B.This.Min);
      end if;
      if B.This.Max /= B.This.Min then
         Do_Point (Data, X, A, B.This.Max);
      end if;
      if B.This.Right /= B.This.Max then
         Do_Point (Data, X, A, B.This.Right);
      end if;
   end Do_Singleton;

   function Get
            (  X    : Number'Base;
               Left : Number'Base;
               Data : Operand
            )  return Confidence is
      pragma Inline (Get);
   begin
      if Data.Index = 1 then
         return Data.Left;
      else
         return
            Interpolate
            (  X  => X,
               X1 => Left,
               Y1 => Data.Left,
               X2 => Data.This.Value,
               Y2 => Data.This.Left
            );
      end if;
   end Get;

begin
   Trim (A, A_First, A_Last);
   Trim (B, B_First, B_Last);
   if A_First > A_Last then
      Do_Empty (Data, B);
   elsif B_First > B_Last then
      Do_Empty (Data, A);
   else
      declare
         A_Copy : aliased Points.Set := A.Data;
         B_Copy : aliased Points.Set := B.Data;
         A_Data : Operand (A_Last, A_Copy'Access);
         B_Data : Operand (B_Last, B_Copy'Access);
         Left   : Number := Number'First;
      begin
         A_Data.This := Get (A.Data, A_First);
         A_Data.Left := A_Data.This.Left;
         B_Data.This := Get (B.Data, B_First);
         B_Data.Left := B_Data.This.Left;
         loop
            if A_Data.Index > A_Data.Size then
               exit when B_Data.Index > B_Data.Size;
               --
               -- No more  points  in  A.  The  rightmost  value  of  is
               -- extrapolated to to the right.
               --
               if B_Data.Index > B_First then
                  Do_Interval
                  (  Data => Data,
                     X1   => Left,
                     A1   => A_Data.Left,
                     B1   => B_Data.Left,
                     X2   => B_Data.This.Value,
                     A2   => A_Data.Left,
                     B2   => B_Data.This.Left
                  );
               end if;
               Left := B_Data.This.Value;
               Do_Singleton (Data, Left, A_Data.Left, B_Data);
               Next (B_Data);
            elsif B_Data.Index > B_Data.Size then
               --
               -- No more  points  in  B.  The  rightmost  value  of  is
               -- extrapolated to to the right.
               --
               if A_Data.Index > A_First then
                  Do_Interval
                  (  Data => Data,
                     X1   => Left,
                     A1   => A_Data.Left,
                     B1   => B_Data.Left,
                     X2   => A_Data.This.Value,
                     A2   => A_Data.This.Left,
                     B2   => B_Data.Left
                  );
               end if;
               Left := A_Data.This.Value;
               Do_Singleton (Data, Left, A_Data, B_Data.Left);
               Next (A_Data);
            else
               --
               -- Both A and B have points. The least one is chosen.
               --
               if A_Data.This.Value < B_Data.This.Value then
                  --
                  -- B2 is the interpolated value of B in the point
                  --
                  declare
                     B2 : constant Confidence :=
                             Get (A_Data.This.Value, Left, B_Data);
                  begin
                     if A_Data.Index > A_First then
                        Do_Interval
                        (  Data => Data,
                           X1   => Left,
                           A1   => A_Data.Left,
                           B1   => B_Data.Left,
                           X2   => A_Data.This.Value,
                           A2   => A_Data.This.Left,
                           B2   => B2
                        );
                     end if;
                     B_Data.Left := B2;
                     Left := A_Data.This.Value;
                     Do_Singleton (Data, Left, A_Data, B2);
                     Next (A_Data);
                  end;
               elsif A_Data.This.Value > B_Data.This.Value then
                  --
                  -- A2 is the interpolated value of A in the point
                  --
                  declare
                     A2 : constant Confidence :=
                             Get (B_Data.This.Value, Left, A_Data);
                  begin
                     if B_Data.Index > B_First then
                        Do_Interval
                        (  Data => Data,
                           X1   => Left,
                           A1   => A_Data.Left,
                           B1   => B_Data.Left,
                           X2   => B_Data.This.Value,
                           A2   => A2,
                           B2   => B_Data.This.Left
                        );
                     end if;
                     A_Data.Left := A2;
                     Left := B_Data.This.Value;
                     Do_Singleton (Data, Left, A2, B_Data);
                     Next (B_Data);
                  end;
               else
                  --
                  -- A and B share the point
                  --
                  if (  A_Data.Index > A_First
                     or else
                        B_Data.Index > B_First
                     )
                  then
                     Do_Interval
                     (  Data => Data,
                        X1   => Left,
                        A1   => A_Data.Left,
                        B1   => B_Data.Left,
                        X2   => A_Data.This.Value,
                        A2   => A_Data.This.Left,
                        B2   => B_Data.This.Left
                     );
                  end if;
                  Left := A_Data.This.Value;
                  Do_Singleton (Data, Left, A_Data, B_Data);
                  Next (A_Data);
                  Next (B_Data);
               end if;
            end if;
         end loop;
      end;
   end if;
end Binary_Operation;
