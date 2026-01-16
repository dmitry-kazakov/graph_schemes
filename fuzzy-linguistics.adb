--                                                                    --
--  package Fuzzy.Linguistics       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Fuzzy.Linguistics is
   use Fuzzy_Floats.Fuzzy_Numbers;

   type Operand
        (  Size : Positive;
           Set  : access Points.Set
        )  is limited
   record
      Index : Positive := 1;
      Left  : Confidence;
      This  : Point;
   end record;

   procedure Next (Data : in out Operand) is
   begin
      Data.Left  := Data.This.Right;
      Data.Index := Data.Index + 1;
      if Data.Index <= Data.Size then
         Data.This := Get (Data.Set.all, Data.Index);
      end if;
   end Next;

   function "=" (Left, Right : Point) return Boolean is
   begin
      return Left.Value = Right.Value;
   end "=";

   function "<" (Left, Right : Point) return Boolean is
   begin
      return Left.Value < Right.Value;
   end "<";
--
-- Append_Middle -- Add extreme point on an interval
--
--    Result - A variable to be modified
--    X1     - The left bound of the interval
--    A1, B1 - The values of linear functions there
--    X2     - The right bound of the interval
--    A2, B2 - The values of linear functions there
--
-- This procedure adds a point where A and B intersect. It does  nothing
-- if this point does not belong to ]X1,X2[.
--
   procedure Append_Middle
             (  Result : in out Variable;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             );
--
-- Append_Xor_Middle -- Add extreme points on an interval
--
--    Result - A variable to be modified
--    X1     - The left bound of the interval
--    A1, B1 - The values of linear functions there
--    X2     - The right bound of the interval
--    A2, B2 - The values of linear functions there
--
-- This procedure adds from one to two points where A and B,  or  A  and
-- not B intersect. It does nothing for a  point  out  of  ]X1,X2[.  The
-- value added in a point is A xor B.
--
   procedure Append_Xor_Middle
             (  Result : in out Variable;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             );
--
-- Find -- A value in a set of points
--
--    List  - The set
--    Value - The value to search for
--
-- Returns :
--
--    When  positive  it  is  the  index  of the value found in the set.
--    Otherwise it is a negated index of the place the value should be.
--
   function Find (List : Points.Set; Value : Number)
      return Integer;
   pragma Inline (Find);
--
-- Is_In -- Get min-max for a point
--
--    Value - The point
--    Var   - Variable
--    Index - The index of the interval containing Value
--
-- Returns :
--
--    (P(Var|Value), N(Var|Value))
--
   function Is_In
            (  Value : Number'Base;
               Var   : Variable;
               Index : Integer
            )  return Fuzzy_Boolean;

   function Is_Singular (Dot : Point) return Boolean is
   begin
      return Dot.Min = Dot.Max;
   end Is_Singular;

   function Root
            (  X1     : Number;
               A1, B1 : Confidence;
               X2     : Number;
               A2, B2 : Confidence
            )  return Number'Base is
      DX : constant Number'Base := X2 - X1;
      D1 : constant Number'Base :=
              Number'Base (B1) - Number'Base (A1);
      D2 : constant Number'Base :=
              Number'Base (B2) - Number'Base (A2);
      DD : constant Number'Base := D1 - D2;
   begin
      if abs D1 < abs DD then
         return X1 + D1 * DX / DD;
      else
         return X1;
      end if;
   end Root;

   procedure Append_Middle
             (  Result : in out Variable;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             )  is
       X : constant Number'Base := Root (X1, A1, B1, X2, A2, B2);
   begin
       if X > X1 and then X < X2 then
          Append (Result, X, Interpolate (X, X1, A1, X2, A2));
       end if;
   end Append_Middle;

   procedure Append_Xor_Middle
             (  Result : in out Variable;
                X1     : Number'Base;
                A1, B1 : Confidence;
                X2     : Number'Base;
                A2, B2 : Confidence
             )  is
      procedure Do_X (X : Number'Base; Inversed : Boolean) is
         Y : constant Confidence := Interpolate (X, X1, A1, X2, A2);
      begin
         if X > X1 and then X < X2 then
            if Inversed then
               Append (Result, X, Y or not Y);
            else
               Append (Result, X, Y and not Y);
            end if;
         end if;
      end Do_X;

      X     : constant Number'Base :=
                 Root (X1, A1, B1, X2, A2, B2);
      Not_X : constant Number'Base :=
                 Root (X1, A1, not B1, X2, A2, not B2);
   begin
       if X < Not_X then
          Do_X (X,     False);
          Do_X (Not_X, True );
       else
          Do_X (Not_X, True );
          Do_X (X,     False);
       end if;
   end Append_Xor_Middle;

   function Find (List : Points.Set; Value : Number)
      return Integer is
   begin
      return Find
             (  List,
                Point'(Value => Value, others => Confidence'Last)
             );
   end Find;

   procedure Find
             (  Var  : Variable;
                Span : Interval;
                From : out Positive;
                To   : out Natural
             )  is
      Index : Integer;
   begin
      Index := Find (Var.Data, Span.From);
      if Index > 0 then
         From := Index;
      else
         From := -Index;
      end if;
      Index := Find (Var.Data, Span.To);
      if Index > 0 then
         To := Index;
      else
         To := - Index - 1;
      end if;
   end Find;

   procedure Get
             (  Var   : Variable;
                Value : Number'Base;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             )  is
      Index : constant Integer := Find (Var.Data, Value);
   begin
      if Index > 0 then
         declare
            This : Point renames Get (Var.Data, Index);
         begin
            Left  := This.Left;
            Right := This.Right;
            Min   := This.Min;
            Max   := This.Max;
         end;
      else
         declare
            This : constant Fuzzy_Boolean := Is_In (Value, Var, Index);
         begin
            Left  := This.Possibility;
            Right := This.Possibility;
            Min   := This.Possibility;
            Max   := This.Possibility;
         end;
      end if;
   end Get;

   procedure Get_Point
             (  Var   : Variable;
                Index : Positive;
                Value : out Number;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             )  is
      This : constant Point := Get (Var.Data, Index);
   begin
      Value := This.Value;
      Left  := This.Left;
      Right := This.Right;
      Min   := This.Min;
      Max   := This.Max;
   end Get_Point;

   function Get_Points_Number (Var : Variable) return Natural is
   begin
      return Get_Size (Var.Data);
   end Get_Points_Number;

   function Get_Span (Var : Variable) return Interval is
   begin
      return
      (  Get (Var.Data, 1).Value,
         Get (Var.Data, Get_Size (Var.Data)).Value
      );
   end Get_Span;

   function Get_Value (Var : Variable; Index : Positive)
      return Number is
   begin
      return Get (Var.Data, Index).Value;
   end Get_Value;

   function Interpolate
            (  X  : Number;
               X1 : Number;
               Y1 : Confidence;
               X2 : Number;
               Y2 : Confidence
            )  return Confidence is
   begin
      if Y1 = Y2 then
         return Y1;
      else
         declare
            Y : constant Number'Base :=
               (  Number'Base (Y1)
               +  (  (Number'Base (Y2) - Number'Base (Y1))
                  *  ((X - X1) / (X2 - X1))
               )  );
         begin
            if Y <= Number'Base (Confidence'First) then
               return Confidence'First;
            elsif Y >= Number'Base (Confidence'Last) then
               return Confidence'Last;
            else
               return Confidence (Y);
            end if;
         end;
      end if;
   end Interpolate;

   procedure Append
             (  Var   : in out Variable;
                Value : Number;
                Level : Confidence
             )  is
      Size : constant Natural := Get_Size (Var.Data);
   begin
      if Size = 0 then
         Add (Var.Data, Point'(Value => Value, others => Level));
      else
         declare
            Last : Point := Get (Var.Data, Size);
         begin
            if Last.Value > Value then
               raise Data_Error;
            elsif Last.Value = Value then
               if Last.Right /= Level then
                  Last.Right := Level;
                  Last.Max   := Confidence'Max (Last.Max, Level);
                  Last.Min   := Confidence'Min (Last.Min, Level);
                  Replace (Var.Data, Last);
               end if;
            else
               Add (Var.Data, Point'(Value => Value, others => Level));
               --
               -- Purge the points before the added one. The added point
               -- cannot  be touched, because another Append may follow,
               -- which  would need the added point. The example is left
               -- shoulder of a trapezoid. The third  point  when  added
               -- could be otherwise purged.
               --
               Purge (Var, Size, 2, True);
            end if;
         end;
      end if;
   end Append;

   procedure Erase (Var : in out Variable) is
   begin
      Erase (Var.Data);
   end Erase;

   procedure Binary_Operation
             (  Data : in out User_Data;
                A, B : Variable
             )  is separate;
   procedure Unary_Operation
             (  Data : in out User_Data;
                A    : Variable
             )  is separate;

   function Is_Empty (Var : Variable) return Boolean is
   begin
      return Possibility (Var) = Confidence'First;
   end Is_Empty;

   function Is_In
            (  Value : Number'Base;
               Var   : Variable;
               Index : Integer
            )  return Fuzzy_Boolean is
      Size : constant Natural := Get_Size (Var.Data);
   begin
      if Size = 0 then
         return Certain_False;
      end if;
      if Index > 0 then
         --
         -- There is a point for the argument. The result is  the  value
         -- in  this  point.  Max  gives  the possibility. Min gives the
         -- necessity.
         --
         declare
            This : Point renames Get (Var.Data, Index);
         begin
            return (Possibility => This.Max, Necessity => This.Min);
         end;
      else
         --
         -- There is no such point we have to interpolate or extrapolate
         -- the membership function.
         --
         if Index >= -1 then
            --
            -- Extrapolate to the left
            --
            declare
               Result : constant Confidence :=
                  Get (Var.Data, 1).Left;
            begin
               return (Possibility | Necessity => Result);
            end;
         elsif Index = -Size - 1 then
            --
            -- Extrapolate to the right
            --
            declare
               Result : constant Confidence :=
                  Get (Var.Data, Get_Size (Var.Data)).Right;
            begin
               return (Possibility | Necessity => Result);
            end;
         else
            --
            -- Interpolate between two points
            --
            declare
               Left   : Point renames Get (Var.Data, -Index - 1);
               Right  : Point renames Get (Var.Data, -Index);
               Result : Confidence;
            begin
               Result :=
                  Interpolate
                  (  Value,
                     Left.Value,
                     Left.Right,
                     Right.Value,
                     Right.Left
                  );
               return (Possibility | Necessity => Result);
            end;
         end if;
      end if;
   end Is_In;

   function Is_Interval
            (  Var   : Variable;
               First : Positive;
               Last  : Natural
            )  return Boolean is
   begin
      if First + 1 = Last then
         declare
            Left  : Point renames Get (Var.Data, First);
            Right : Point renames Get (Var.Data, Last);
         begin
            return
            (  Left.Left = Confidence'First
            and then
               Left.Min = Confidence'First
            and then
               Left.Max = Confidence'Last
            and then
               Left.Right = Confidence'Last
            and then
               Right.Left = Confidence'Last
            and then
               Right.Min = Confidence'First
            and then
               Right.Max = Confidence'Last
            and then
               Right.Right = Confidence'First
            );
         end;
      else
         return False;
      end if;
   end Is_Interval;

   function Is_Interval (Var : Variable) return Boolean is
      First : Positive;
      Last  : Natural;
   begin
      Trim (Var, First, Last);
      return Is_Interval (Var, First, Last);
   end Is_Interval;

   function Is_Singleton
            (  Var   : Variable;
               First : Positive;
               Last  : Natural
            )  return Boolean is
   begin
      if First = Last then
         declare
            This : Point renames Get (Var.Data, First);
         begin
            return
            (  This.Left = Confidence'First
            and then
               This.Min = Confidence'First
            and then
               This.Max = Confidence'Last
            and then
               This.Right = Confidence'First
            );
         end;
      else
         return False;
      end if;
   end Is_Singleton;

   function Is_Singleton (Var : Variable) return Boolean is
      First : Positive;
      Last  : Natural;
   begin
      Trim (Var, First, Last);
      return Is_Singleton (Var, First, Last);
   end Is_Singleton;

   function Equal (A, B : Point; Eps : Number'Base := 0.0)
      return Boolean is
   begin
      return
      (  A.Left = B.Left
      and then
         A.Min = B.Min
      and then
         A.Max = B.Max
      and then
         A.Right = B.Right
      and then
         abs (A.Value - B.Value) <= Eps
      );
   end Equal;

   function Equal
            (  A, B : Pair;
               Eps  : Number'Base := 0.0
            )  return Boolean is
   begin
      return A.Level = B.Level and then abs (A.Value - B.Value) <= Eps;
   end Equal;

   function Equal
            (  A   : Variable;
               B   : Pair;
               Eps : Number'Base := 0.0
            )  return Boolean is
      This : Point;
   begin
      for Index in 1..Get_Size (A.Data) loop
         This := Get (A.Data, Index);
         if This.Min /= B.Level or else This.Max /= B.Level then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   function Equal
            (  A   : Pair;
               B   : Variable;
               Eps : Number'Base := 0.0
            )  return Boolean is
   begin
      return Equal (B, A, Eps);
   end Equal;

   function Equal (A, B : Variable; Eps : Number'Base := 0.0)
      return Boolean is
      Size_1  : constant Natural := Get_Size (A.Data);
      Size_2  : constant Natural := Get_Size (B.Data);
      Index_1 : Positive := 1;
      Index_2 : Positive := 1;
      Left    : Point;
      Right   : Point;
      This    : Point;
   begin
      if Size_1 = 0 then
         return Possibility (B) = Confidence'First;
      elsif Size_2 = 0 then
         return Possibility (A) = Confidence'First;
      end if;
      Right := Get (A.Data, 1);
      Left  := (Number'First, others => Right.Left);
      This  := Get (B.Data, 1);
      loop
         if abs (This.Value - Right.Value) <= Eps then
            --
            -- The point is closer than Eps to the right interval bound.
            -- In this case it can be a non-singleton, but must  exactly
            -- match the bound.
            --
            if (  This.Left  /= Right.Left
               or else
                  This.Min   /= Right.Min
               or else
                  This.Max   /= Right.Max
               or else
                  This.Right /= Right.Right
               )
            then
               return False;
            end if;
            if Index_2 < Size_2 then
               Index_2 := Index_2 + 1;
               This := Get (B.Data, Index_2);
            else
               This := (Number'Last, others => This.Right);
            end if;
         elsif This.Value < Right.Value then
            --
            -- It  is  on  the interval. It has to be singular and match
            -- the interpolated value of the membership function on  the
            -- interval.
            --
            if not Is_Singular (This) then
               return False;
            end if;
            if Left.Right = Right.Left then
               -- Constant membership function
               if This.Left /= Left.Right then
                  return False;
               end if;
            else
               -- Linear membership function
               if (  This.Left
                  /= Interpolate
                     (  This.Value,
                        Left.Value,
                        Left.Right,
                        Right.Value,
                        Right.Left
                  )  )
               then
                  return False;
               end if;
            end if;
            if Index_2 < Size_2 then
               Index_2 := Index_2 + 1;
               This := Get (B.Data, Index_2);
            else
               This := (Number'Last, others => This.Right);
            end if;
         else
            --
            -- Look for the next interval
            --
            if Index_1 < Size_1 then
               Index_1 := Index_1 + 1;
               Left    := Right;
               Right   := Get (A.Data, Index_1);
            elsif Index_2 < Size_2 then
               Right := (Number'Last, others => Right.Right);
            else
               return True;
            end if;
         end if;
      end loop;
   end Equal;
--
-- Possibilities -- Implementation of
--
   package Possibilities is
      function P (A : Variable) return Confidence;
      function P (A : Number; B : Variable) return Confidence;
      function P (A : Interval;     B : Variable) return Confidence;
      function P (A : Fuzzy_Float; B : Variable) return Confidence;
      function P (A : Variable;     B : Variable) return Confidence;
   end Possibilities;
   package body Possibilities is separate;

   function Possibility (A : Variable) return Confidence
      renames Possibilities.P;
   function Possibility (A : Number; B : Variable)
      return Confidence renames Possibilities.P;
   function Possibility (A : Interval; B : Variable) return Confidence
      renames Possibilities.P;
   function Possibility (A : Fuzzy_Float; B : Variable)
      return Confidence renames Possibilities.P;
   function Possibility (A : Variable; B : Variable) return Confidence
      renames Possibilities.P;

   function Possibility (A : Variable; B : Number)
      return Confidence is
   begin
      return Possibilities.P (B, A);
   end Possibility;

   function Possibility (A : Variable; B : Interval) return Confidence is
   begin
      return Possibilities.P (B, A);
   end Possibility;

   function Possibility (A : Variable; B : Fuzzy_Float)
      return Confidence is
   begin
      return Possibilities.P (B, A);
   end Possibility;
--
-- Necessities -- Implementation of
--
   package Necessities is
      function N (A : Variable) return Confidence;
      function N (A : Number; B : Variable    ) return Confidence;
      function N (A : Interval;     B : Variable    ) return Confidence;
      function N (A : Fuzzy_Float; B : Variable    ) return Confidence;
      function N (A : Variable;     B : Variable    ) return Confidence;
      function N (A : Variable;     B : Number) return Confidence;
      function N (A : Variable;     B : Interval    ) return Confidence;
      function N (A : Variable;     B : Fuzzy_Float) return Confidence;
   end Necessities;
   package body Necessities is separate;

   function Necessity (A : Variable) return Confidence
      renames Necessities.N;
   function Necessity (A : Number; B : Variable) return Confidence
      renames Necessities.N;
   function Necessity (A : Interval; B : Variable) return Confidence
      renames Necessities.N;
   function Necessity (A : Fuzzy_Float; B : Variable) return Confidence
      renames Necessities.N;
   function Necessity (A : Variable; B : Variable) return Confidence
      renames Necessities.N;
   function Necessity (A : Variable; B : Number) return Confidence
      renames Necessities.N;
   function Necessity (A : Variable; B : Interval) return Confidence
      renames Necessities.N;
   function Necessity (A : Variable; B : Fuzzy_Float) return Confidence
      renames Necessities.N;

   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Insert_Point
      (  Var,
         Index,
         Value,
         Left,
         Confidence'Last,
         Confidence'First,
         Right,
         Purge
      );
   end Insert_Point;

   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Level : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Insert_Point
      (  Var,
         Index,
         Value,
         Level,
         Confidence'Last,
         Confidence'First,
         Level,
         Purge
      );
   end Insert_Point;

   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
      Size : constant Natural := Get_Size (Var.Data);
      This : Point;
   begin
      if (  Size + 1 < Index
         or else
            (  Index > 1
            and then
               Get (Var.Data, Index - 1).Value >= Value
            )
         or else
            (  Index <= Size
            and then
               Get (Var.Data, Index).Value <= Value
         )  )
      then
         raise Constraint_Error;
      end if;
      This.Value := Value;
      This.Left  := Left;
      This.Right := Right;
      if Left > Right then
         This.Min := Confidence'Min (Min, Right);
         This.Max := Confidence'Max (Max, Left);
      else
         This.Min := Confidence'Min (Min, Left);
         This.Max := Confidence'Max (Max, Right);
      end if;
      Insert (Var.Data, This);
      if Purge then
         Fuzzy.Linguistics.Purge (Var, Index + 1, 3, False);
      end if;
   end Insert_Point;

   function Is_In (A : Number; B : Variable)
      return Fuzzy_Boolean is
   begin
      return Is_In (A, B, Find (B.Data, A));
   end Is_In;

   function Is_In (A : Interval; B : Variable) return Fuzzy_Boolean is
   begin
      if A.From = A.To then
         --
         -- Only one point in the interval
         --
         return Is_In (A.From, B, Find (B.Data, A.From));
      end if;
      --
      -- Several points in the interval
      --
      declare
         From   : Integer := Find (B.Data, A.From);
         To     : Integer := Find (B.Data, A.To);
         Result : Fuzzy_Boolean :=
                     Is_In (A.From, B, From) + Is_In (A.To, B, To);
      begin
         --
         -- Result now contains data obtained from the interval ends. To
         -- complete  it,  we  have  to  go  through  all  points in the
         -- interval.
         --
         if From < 0 then
            From := -From;
         else
            From := From + 1;
         end if;
         if To <= 0 then
            To := - To - 1;
         else
            To := To - 1;
         end if;
         for Index in From..To loop
            declare
               This : Point renames Get (B.Data, Index);
            begin
               Result.Possibility := Result.Possibility or This.Max;
               Result.Necessity   := Result.Necessity  and This.Min;
            end;
         end loop;
         return Result;
      end;
   end Is_In;

   function Is_In (A : Fuzzy_Float; B : Variable)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In (A, B : Variable)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In (A : Variable; B : Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In (A : Variable; B : Interval)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   function Is_In (A : Variable; B : Fuzzy_Float)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (B, A),
         Necessity   => Necessity   (B, A)
      );
   end Is_In;

   procedure Move_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Purge : Boolean := True
             )  is
      Size : constant Natural := Get_Size (Var.Data);
      This : Point;
   begin
      if (  Index > Size
         or else
            (  Index > 1
            and then
               Get (Var.Data, Index - 1).Value >= Value
            )
         or else
            (  Index < Size
            and then
               Get (Var.Data, Index + 1).Value <= Value
         )  )
      then
         raise Constraint_Error;
      else
         This := Get (Var.Data, Index);
         if This.Value /= Value then
            Remove  (Var.Data, This);
            This.Value := Value;
            Replace (Var.Data, This);
            if Purge then
               Fuzzy.Linguistics.Purge (Var, Index + 1, 3, False);
            end if;
         end if;
      end if;
   end Move_Point;

   procedure Purge
             (  Var       : in out Variable;
                Keep_Ends : Boolean := False
             )  is
   begin
      Purge (Var, Positive'Last, Natural'Last, Keep_Ends);
   end Purge;

   procedure Purge
             (  Var       : in out Variable;
                Index     : Positive;
                Count     : Natural;
                Keep_Ends : Boolean
             )  is
      Size  : Natural := Get_Size (Var.Data);
      No    : Natural := Natural'Min (Index, Size);
      This  : Point;
      Left  : Point;
      Right : Point;
   begin
      if No <= 0 then
         return;
      end if;
      This := Get (Var.Data, No);
      if No < Size then
         Right := Get (Var.Data, No + 1);
      end if;
      for Candidate in 1..Count loop -- Maximal Count points to check
         if No = 1 then
            -- The first point
            if not Keep_Ends and then Is_Singular (This) then
               if Size = 1 then
                  if This.Left = Confidence'First then
                     Erase (Var.Data);
                  end if;
               else
                  if This.Right = Right.Left then
                     Remove (Var.Data, 1);
                  end if;
               end if;
            end if;
            return;
         end if;
         Left := Get (Var.Data, No - 1);
         if No = Size then
            -- The last point
            if (  not Keep_Ends
               and then
                  Is_Singular (This)
               and then
                  Left.Right = This.Left
               )
            then
               Remove (Var.Data, No);
               Size := Size - 1;
            else
               Right := This;
            end if;
         elsif Is_Singular (This) then
            -- A point in the middle
            if Left.Right = Right.Left then
               -- A constant membership function
               if Left.Right = This.Left then
                  -- Removing this point
                  Remove (Var.Data, No);
                  Size := Size - 1;
               else
                  Right := This;
               end if;
            else
               -- A linear membership function
               if (  This.Left
                  =  Interpolate
                     (  This.Value,
                        Left.Value,
                        Left.Right,
                        Right.Value,
                        Right.Left
                  )  )
               then
                  -- Removing this point
                  Remove (Var.Data, No);
                  Size := Size - 1;
               else
                  Right := This;
               end if;
            end if;
         else
            Right := This;
         end if;
         This := Left;
         No   := No - 1;
      end loop;
   end Purge;

   procedure Remove_Point
             (  Var   : in out Variable;
                Index : Positive;
                Purge : Boolean := True
             )  is
   begin
      Remove (Var.Data, Index);
      if Purge then
         Fuzzy.Linguistics.Purge (Var, Index, 2, False);
      end if;
   end Remove_Point;

   procedure Set_Point
             (  Var   : in out Variable;
                Index : Positive;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
            )  is
   begin
      Set_Point
      (  Var,
         Index,
         Left,
         Confidence'Last,
         Confidence'First,
         Right,
         Purge
      );
   end Set_Point;

   procedure Set_Point
             (  Var   : in out Variable;
                Index : Positive;
                Level : Confidence;
                Purge : Boolean := True
             )  is
   begin
      Set_Point
      (  Var,
         Index,
         Level,
         Confidence'Last,
         Confidence'First,
         Level,
         Purge
      );
   end Set_Point;

   procedure Set_Point
             (  Var   : in out Variable;
                Index : Positive;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             )  is
      This : Point := Get (Var.Data, Index);
   begin
      This.Left  := Left;
      This.Right := Right;
      if Left > Right then
         This.Max := Confidence'Max (Left,  Max);
         This.Min := Confidence'Min (Right, Min);
      else
         This.Max := Confidence'Max (Right, Max);
         This.Min := Confidence'Min (Left,  Min);
      end if;
      Replace (Var.Data, This);
      if Purge then
         Fuzzy.Linguistics.Purge (Var, Index + 1, 3, False);
      end if;
   end Set_Point;

   procedure Trim (Var : in out Variable) is
      Size  : constant Natural := Get_Size (Var.Data);
      First : Positive;
      Last  : Natural;
   begin
      Trim (Var, First, Last);
      if First > Last then
         Erase (Var.Data);
      else
         if Last < Size then
            Remove (Var.Data, Size);
         end if;
         if First > 1 then
            Remove (Var.Data, 1);
         end if;
      end if;
   end Trim;

   procedure Trim
             (  Var   : Variable;
                First : out Positive;
                Last  : out Natural
             )  is
      Size : constant Natural := Get_Size (Var.Data);
   begin
      First := 1;
      Last  := Size;
      case Size is
         when 0 =>
            null;
         when 1 =>
            declare
               This : constant Point := Get (Var.Data, 1);
            begin
               if (  Is_Singular (This)
                  and then
                     This.Left = Confidence'First
                  )
               then
                  Last := 0;
               end if;
            end;
         when 2 =>
            declare
               Left  : constant Point := Get (Var.Data, 1);
               Right : constant Point := Get (Var.Data, 2);
            begin
               if (  Is_Singular (Right)
                  and then
                     Left.Right = Right.Left
                  )
               then
                  if (  Is_Singular (Left)
                     and then
                        Left.Left = Confidence'First
                     )
                  then
                     Last := 0;
                  else
                     Last := 1;
                  end if;
               elsif (  Is_Singular (Left)
                     and then
                        Left.Right = Right.Left
                     )
               then
                  First := 2;
               end if;
            end;
         when others =>
            declare
               This : Point;
            begin
               This := Get (Var.Data, Size);
               if (  Is_Singular (This)
                  and then
                     This.Left = Get (Var.Data, Size - 1).Right
                  )
               then
                  Last := Size - 1;
               end if;
               This := Get (Var.Data, 1);
               if (  Is_Singular (This)
                  and then
                     This.Right = Get (Var.Data, 2).Left
                  )
               then
                  First := First + 1;
               end if;
            end;
      end case;
   end Trim;

   function "+" (Left : Variable; Right : Number)
      return Variable is
      Result : Variable;
      This   : Point;
   begin
      for Index in 1..Get_Size (Left.Data) loop
         This := Get (Left.Data, Index);
         This.Value := This.Value + Right;
         Add (Result.Data, This);
      end loop;
      return Result;
   end "+";

   function "-" (Left : Variable; Right : Number)
      return Variable is
      Result : Variable;
      This   : Point;
   begin
      for Index in 1..Get_Size (Left.Data) loop
         This := Get (Left.Data, Index);
         This.Value := This.Value - Right;
         Add (Result.Data, This);
      end loop;
      return Result;
   end "-";

   function "*" (Left : Variable; Right : Number)
      return Variable is
      Result : Variable;
      This   : Point;
   begin
      if Right > 0.0 then
         for Index in 1..Get_Size (Left.Data) loop
            This := Get (Left.Data, Index);
            This.Value := This.Value * Right;
            Add (Result.Data, This);
         end loop;
      else
         for Index in reverse 1..Get_Size (Left.Data) loop
            This := Get (Left.Data, Index);
            This.Value := This.Value * Right;
            Add (Result.Data, This);
         end loop;
      end if;
      return Result;
   end "*";

   function "/" (Left : Variable; Right : Number)
      return Variable is
      Result : Variable;
      This   : Point;
   begin
      if Right > 0.0 then
         for Index in 1..Get_Size (Left.Data) loop
            This := Get (Left.Data, Index);
            This.Value := This.Value / Right;
            Add (Result.Data, This);
         end loop;
      else
         for Index in reverse 1..Get_Size (Left.Data) loop
            This := Get (Left.Data, Index);
            This.Value := This.Value / Right;
            Add (Result.Data, This);
         end loop;
      end if;
      return Result;
   end "/";

   function "=" (A : Number; B : Variable)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Interval; B : Variable) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Fuzzy_Float; B : Variable) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Variable; B : Variable) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Variable; B : Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Variable; B : Interval) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "=" (A : Variable; B : Fuzzy_Float) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (A, B),
         Necessity   => Necessity (A, B) and Necessity (B, A)
      );
   end "=";

   function "/=" (A : Number; B : Variable)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Interval; B : Variable) return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Fuzzy_Float; B : Variable)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable; B : Variable) return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable; B : Number)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable; B : Interval) return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "/=" (A : Variable; B : Fuzzy_Float)
      return Fuzzy_Boolean is
   begin
      return not (A = B);
   end "/=";

   function "&" (Left : Pair; Right : Pair) return Variable is
      Result : Variable;
   begin
      Append (Result, Left.Value,  Left.Level);
      Append (Result, Right.Value, Right.Level);
      return Result;
   end "&";

   function "&" (Left : Variable; Right : Pair) return Variable is
      Result : Variable := Left;
   begin
      Append (Result, Right.Value, Right.Level);
      return Result;
   end "&";
--
-- Operations -- Implementation of
--
--    **            - The inducing dot operation
--    Append_Middle - To deal with an interval
--
   generic
      with function "**" (A, B : Confidence) return Confidence;
      with procedure Append_Middle
                     (  Result : in out Variable;
                        X1     : Number'Base;
                        A1, B1 : Confidence;
                        X2     : Number'Base;
                        A2, B2 : Confidence
                     )  is <>;
   package Operations is
      function Operation (A, B : Variable) return Variable;
      function Operation (A : Variable; B : Confidence) return Variable;
   end Operations;
   package body Operations is separate;

   package And_Operations is new Operations ("and");
   function "and" (A, B : Variable) return Variable
      renames And_Operations.Operation;
   function "and" (A : Variable; B : Confidence) return Variable
      renames And_Operations.Operation;
   function "and" (A : Confidence; B : Variable) return Variable is
   begin
      return B and A;
   end "and";

   package Or_Operations is new Operations ("or");
   function "or" (A, B : Variable) return Variable
      renames Or_Operations.Operation;
   function "or" (A : Variable; B : Confidence) return Variable
      renames Or_Operations.Operation;
   function "or" (A : Confidence; B : Variable) return Variable is
   begin
      return B or A;
   end "or";

   package Xor_Operations is new Operations ("xor", Append_Xor_Middle);
   function "xor" (A, B : Variable) return Variable
      renames Xor_Operations.Operation;
   function "xor" (A : Variable; B : Confidence) return Variable
      renames Xor_Operations.Operation;
   function "xor" (A : Confidence; B : Variable) return Variable is
   begin
      return B xor A;
   end "xor";

   function "not" (A : Variable) return Variable is
      First  : Positive;
      Last   : Natural;
      Result : Variable;
   begin
      Trim (A, First, Last);
      if First > Last then
         Append (Result, Number'First, Confidence'Last);
      else
         declare
            This : Point;
         begin
            for Index in First..Last loop
               This := Get (A.Data, Index);
               Append (Result, This.Value, not This.Left);
               if This.Left /= This.Min then
                  Append (Result, This.Value, not This.Min);
               end if;
               if This.Max /= This.Min then
                  Append (Result, This.Value, not This.Max);
               end if;
               if This.Right /= This.Max then
                  Append (Result, This.Value, not This.Right);
               end if;
            end loop;
         end;
      end if;
      return Result;
   end "not";

   function Empty return Variable is
      Result : Variable;
   begin
      return Result;
   end Empty;

   function To_Interval (Value : Variable) return Interval is
      First : Positive;
      Last  : Natural;
   begin
      Trim (Value, First, Last);
      if Is_Interval (Value, First, Last) then
         return
         (  Get (Value.Data, First).Value,
            Get (Value.Data, Last).Value
         );
      else
         raise Constraint_Error;
      end if;
   end To_Interval;

   function To_Number (Value : Variable) return Number is
      First : Positive;
      Last  : Natural;
   begin
      Trim (Value, First, Last);
      if Is_Singleton (Value, First, Last) then
         return Get (Value.Data, First).Value;
      else
         raise Constraint_Error;
      end if;
   end To_Number;

   function To_Variable (Value : Number) return Variable is
      Result : Variable;
   begin
      Append (Result, Value, Confidence'First);
      Append (Result, Value, Confidence'Last);
      Append (Result, Value, Confidence'First);
      return Result;
   end To_Variable;

   function To_Variable (Value : Interval) return Variable is
      Result : Variable;
   begin
      Append (Result, Value.From, Confidence'First);
      Append (Result, Value.From, Confidence'Last);
      Append (Result, Value.To,   Confidence'Last);
      Append (Result, Value.To,   Confidence'First);
      return Result;
   end To_Variable;

   function To_Variable (Value : Fuzzy_Float) return Variable is
      Result : Variable;
      X      : Number;
   begin
      X := Get_Interval (Value, Fuzzy_Floats.Interval_Index'First).From;
      Append (Result, X, Confidence'First);
      Append
      (  Result,
         X,
         Fuzzy_Floats.To_Confidence (Fuzzy_Floats.Interval_Index'First)
      );
      for Index in Fuzzy_Floats.Interval_Index'Succ
                      (Fuzzy_Floats.Interval_Index'First)
                .. Fuzzy_Floats.Interval_Index'Last
      loop
         X := Get_Interval (Value, Index).From;
         Append (Result, X, Fuzzy_Floats.To_Confidence (Index));
      end loop;
      for Index in reverse Fuzzy_Floats.Interval_Index'First
                        .. Fuzzy_Floats.Interval_Index'Last
      loop
         X := Get_Interval (Value, Index).To;
         Append (Result, X, Fuzzy_Floats.To_Confidence (Index));
      end loop;
      Append (Result, X, Confidence'First);
      return Result;
   end To_Variable;

end Fuzzy.Linguistics;
