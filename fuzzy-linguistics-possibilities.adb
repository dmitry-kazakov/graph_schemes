--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Possibilities             Luebeck            --
--  Separate body implementation                   Winter, 2003       --
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

separate (Fuzzy.Linguistics) package body Possibilities is

   function P (A : Variable) return Confidence is
      Result : Confidence := Confidence'First;
   begin
      for Index in 1..Get_Size (A.Data) loop
         Result := Result or Get (A.Data, Index).Max;
      end loop;
      return Result;
   end P;

   function P (A : Number; B : Variable) return Confidence is
   begin
      return Is_In (A, B, Find (B.Data, A)).Possibility;
   end P;

   function P (A : Interval; B : Variable) return Confidence is
   begin
      if A.From = A.To then
         --
         -- Only one point in the interval
         --
         return Is_In (A.From, B, Find (B.Data, A.From)).Possibility;
      end if;
      --
      -- Several points in the interval
      --
      declare
         Left   : Integer := Find (B.Data, A.From);
         Right  : Integer := Find (B.Data, A.To);
         Result : Confidence :=
                  (  Is_In (A.From, B, Left ).Possibility
                  or Is_In (A.To,   B, Right).Possibility
                  );
      begin
         --
         -- Result now contains data obtained from the interval ends. To
         -- complete  it,  we  have  to  go  through  all  points in the
         -- interval.
         --
         if Left < 0 then
            Left := -Left;
         else
            Left := Left + 1;
         end if;
         if Right <= 0 then
            Right := - Right - 1;
         else
            Right := Right - 1;
         end if;
         for Index in Left..Right loop
            Result := Result or Get (B.Data, Index).Max;
         end loop;
         return Result;
      end;
   end P;

   function P (A : Fuzzy_Number; B : Variable) return Confidence is
      Result : Confidence;
      Left   : Number;
      Right  : Number;
      Slice  : Interval :=
                  Get_Interval (A, Fuzzy_Floats.Interval_Index'Last);
   begin
      Result := P (Slice, B);
      Left   := Slice.From;
      Right  := Slice.To;
      for Index in reverse Fuzzy_Floats.Interval_Index'First
                        .. Fuzzy_Floats.Interval_Index'Pred
                              (Fuzzy_Floats.Interval_Index'Last)
      loop
         Slice := Get_Interval (A, Index);
         if Left /= Slice.From then
            Result :=
               (  Result
               or (  P (Interval'(Slice.From, Left), B)
                  and
                     Fuzzy_Floats.To_Confidence (Index)
               )  );
            Left := Slice.From;
         end if;
         if Right /= Slice.To then
            Result :=
               (  Result
               or (  P (Interval'(Right, Slice.To), B)
                  and
                     Fuzzy_Floats.To_Confidence (Index)
               )  );
            Right := Slice.To;
         end if;
      end loop;
      return Result;
   end P;

   procedure Do_Empty
             (  Result : in out Confidence;
                A      : Variable
             )  is
   begin
      null;
   end Do_Empty;

   procedure Do_Point
             (  Result : in out Confidence;
                X      : Number'Base;
                A, B   : Confidence
             )  is
   begin
      Result := Result or (A and B);
   end Do_Point;

   procedure Do_Interval
             (  Result : in out Confidence;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             )  is
       X : constant Number'Base := Root (X1, A1, B1, X2, A2, B2);
   begin
       if X > X1 and then X < X2 then
          Result := Result or Interpolate (X, X1, A1, X2, A2);
       end if;
   end Do_Interval;

   procedure Do_Possibility is
      new Binary_Operation
          (  User_Data   => Confidence,
             Do_Empty    => Do_Empty,
             Do_Point    => Do_Point,
             Do_Interval => Do_Interval
          );

   function P (A, B : Variable) return Confidence is
      Result : Confidence := Confidence'First;
   begin
      Do_Possibility (Result, A, B);
      return Result;
   end P;

end Possibilities;
