--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Necessities               Luebeck            --
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

separate (Fuzzy.Linguistics) package body Necessities is

   function N (A : Variable) return Confidence is
      Result : Confidence := Confidence'Last;
   begin
      for Index in 1..Get_Size (A.Data) loop
         Result := Result and Get (A.Data, Index).Min;
      end loop;
      return Result;
   end N;

   function N (A : Variable; B : Number) return Confidence is
   begin
      return Is_In (B, A, Find (A.Data, B)).Necessity;
   end N;

   function N (A : Number; B : Variable) return Confidence is
      Point : constant Integer := Find (B.Data, A);
   begin
      if Point > 0 then
         --
         -- There  is a joining point in B. The result is a min over all
         -- joining points except it.
         --
         declare
            Result : Confidence := Confidence'Last;
         begin
            for Index in 1..Point - 1 loop
               Result := Result and Get (B.Data, Index).Min;
            end loop;
            for Index in Point..Get_Size (B.Data) loop
               Result := Result and Get (B.Data, Index).Min;
            end loop;
            return Result;
         end;
      else
         --
         -- The result is N(B)
         --
         return N (B);
      end if;
   end N;

   function N (A : Variable; B : Interval) return Confidence is
   begin
      if B.From = B.To then
         --
         -- Only one point in the interval
         --
         return Is_In (B.From, A, Find (A.Data, B.From)).Necessity;
      end if;
      --
      -- Several points in the interval
      --
      declare
         Left   : Integer := Find (A.Data, B.From);
         Right  : Integer := Find (A.Data, B.To);
         Result : Confidence :=
                  (  Is_In (B.From, A, Left ).Necessity
                  and
                     Is_In (B.To,   A, Right).Necessity
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
            Result := Result and Get (A.Data, Index).Min;
         end loop;
         return Result;
      end;
   end N;

   function N (A : Interval; B : Variable) return Confidence is
   begin
      if A.From = A.To then
         --
         -- Only one point in the interval
         --
         return N (A.From, B);
      end if;
      --
      -- Several points in the interval
      --
      declare
         Left   : Integer    := Find (B.Data, A.From);
         Right  : Integer    := Find (B.Data, A.To);
         Result : Confidence :=
                  (  Is_In (A.From, B, Left ).Necessity
                  and
                     Is_In (A.To,   B, Right).Necessity
                  );
      begin
         --
         -- Result now contains data obtained from the interval ends. To
         -- complete  it,  we  have  to  go  through  all  points in the
         -- interval.
         --
         if Left < 0 then
            Left := -Left - 1;
         end if;
         if Right <= 0 then
            Right := -Right;
         end if;
         for Index in 1..Left loop
            Result := Result and Get (B.Data, Index).Min;
         end loop;
         for Index in Right..Get_Size (B.Data) loop
            Result := Result and Get (B.Data, Index).Min;
         end loop;
         return Result;
      end;
   end N;

   function N (A : Variable; B : Fuzzy_Number) return Confidence is
      Result : Confidence;
      Left   : Number;
      Right  : Number;
      Slice  : Interval :=
                  Get_Interval (B, Fuzzy_Floats.Interval_Index'Last);
   begin
      Result := N (A, Slice);
      Left   := Slice.From;
      Right  := Slice.To;
      for Index in reverse Fuzzy_Floats.Interval_Index'First
                        .. Fuzzy_Floats.Interval_Index'Pred
                              (Fuzzy_Floats.Interval_Index'Last)
      loop
         Slice := Get_Interval (B, Index);
         if Left /= Slice.From then
            Result :=
               (  Result
               and
                  (  N (A, Interval'(Slice.From, Left))
                  or not Fuzzy_Floats.To_Confidence (Index)
               )  );
            Left := Slice.From;
         end if;
         if Right /= Slice.To then
            Result :=
               (  Result
               and
                  (  N (A, Interval'(Right, Slice.To))
                  or not Fuzzy_Floats.To_Confidence (Index)
               )  );
            Right := Slice.To;
         end if;
      end loop;
      return Result;
   end N;

   function N (A : Fuzzy_Number; B : Variable) return Confidence is
      Result : Confidence;
      Left   : Number;
      Right  : Number;
      Slice  : Interval :=
                  Get_Interval (A, Fuzzy_Floats.Interval_Index'First);
   begin
      Result := N (Slice, B);
      Left   := Slice.From;
      Right  := Slice.To;
      for Index in Fuzzy_Floats.Interval_Index'Succ
                      (Fuzzy_Floats.Interval_Index'First)
                .. Fuzzy_Floats.Interval_Index'Last
      loop
         Slice := Get_Interval (A, Index);
         if Left /= Slice.From then
            Result :=
               (  Result
               and
                  (  not Possibility (B, Interval'(Slice.From, Left))
                  or Fuzzy_Floats.To_Confidence (Index)
               )  );
            Left := Slice.From;
         end if;
         if Right /= Slice.To then
            Result :=
               (  Result
               and
                  (  not Possibility (B, Interval'(Right, Slice.To))
                  or Fuzzy_Floats.To_Confidence (Index)
               )  );
            Right := Slice.To;
         end if;
      end loop;
      return Result;
   end N;

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
      Result := Result and (A or not B);
   end Do_Point;

   procedure Do_Interval
             (  Result : in out Confidence;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             )  is
       X : constant Number'Base :=
              Root (X1, A1, not B1, X2, A2, not B2);
   begin
       if X > X1 and then X < X2 then
          Result := Result and Interpolate (X, X1, A1, X2, A2);
       end if;
   end Do_Interval;

   procedure Do_Necessity is
      new Binary_Operation
          (  User_Data   => Confidence,
             Do_Empty    => Do_Empty,
             Do_Point    => Do_Point,
             Do_Interval => Do_Interval
          );

   function N (A, B : Variable) return Confidence is
      Result : Confidence := Confidence'Last;
   begin
      Do_Necessity (Result, A, B);
      return Result;
   end N;

end Necessities;
