--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistic.Operations                 Luebeck            --
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

separate (Fuzzy.Linguistics) package body Operations is
   type User_Data is limited record
      Result : Variable;
      Level  : Confidence;
   end record;

   procedure Do_Empty (Data : in out User_Data) is
      Y : constant Confidence := Confidence'First ** Data.Level;
   begin
      if Y /= Confidence'First then
         Append (Data.Result, Number'First, Y);
      end if;
   end Do_Empty;

   procedure Do_Point
             (  Data : in out User_Data;
                X    : Number'Base;
                Y    : Confidence
             )  is
   begin
      Append (Data.Result, X, Y ** Data.Level);
   end Do_Point;

   procedure Do_Interval
             (  Data : in out User_Data;
                X1   : Number'Base;
                Y1   : Confidence;
                X2   : Number'Base;
                Y2   : Confidence
             )  is
   begin
      Append_Middle
      (  Result => Data.Result,
         X1     => X1,
         A1     => Y1,
         B1     => Data.Level,
         X2     => X2,
         A2     => Y2,
         B2     => Data.Level
      );
   end Do_Interval;

   procedure Do_Unary is
      new Unary_Operation
          (  User_Data   => User_Data,
             Do_Empty    => Do_Empty,
             Do_Point    => Do_Point,
             Do_Interval => Do_Interval
          );

   function Operation
            (  A : Variable;
               B : Confidence
            )  return Variable is
      Data : User_Data;
   begin
      Data.Level := B;
      Do_Unary (Data, A);
      Trim (Data.Result);
      return Data.Result;
   end Operation;

   procedure Do_Empty
             (  Result : in out Variable;
                A      : Variable
             )  is
   begin
      Result := Operation (A, Confidence'First);
   end Do_Empty;

   procedure Do_Point
             (  Result : in out Variable;
                X      : Number'Base;
                A, B   : Confidence
             )  is
   begin
      Append (Result, X, A ** B);
   end Do_Point;

   procedure Do_Binary is
      new Binary_Operation
          (  User_Data   => Variable,
             Do_Empty    => Do_Empty,
             Do_Point    => Do_Point,
             Do_Interval => Append_Middle
          );

   function Operation (A, B : Variable) return Variable is
      Result : Variable;
   begin
      Do_Binary (Result, A, B);
      Trim (Result);
      return Result;
   end Operation;

end Operations;
