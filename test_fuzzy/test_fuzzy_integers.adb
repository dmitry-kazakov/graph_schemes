--                                                                    --
--  package Test_Fuzzy_Integers     Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  12:48 22 Jul 2017  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Confidence_Factors;       use Confidence_Factors;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Fuzzy;                    use Fuzzy;
with Fuzzy.Logic;              use Fuzzy.Logic;
with Fuzzy_Integers;           use Fuzzy_Integers;
with Fuzzy.Edit.Integers;      use Fuzzy.Edit.Integers;
with Integer_Intervals;        use Integer_Intervals;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

procedure Test_Fuzzy_Integers is
   function Image (X : Interval) return String is
   begin
      return Image (X.From) & ".." & Image (X.To);
   end Image;

   function Image (X : Integer) return String is
   begin
      return Image (X, 10);
   end Image;

   function Image (X : Fuzzy_Integer) return String is
   begin
      return Image (X, 10);
   end Image;
   
   function Image (X : Fuzzy_Boolean) return String is
   begin
      return
         "(" & Image (X.Possibility) & ", " & Image (X.Necessity) & ")";
   end Image;

   generic
      Name : String;
      type Left is private;
      type Right is private;
      type Result is private;
      with function Op (X : Left; Y : Right) return Result;
      with function Equal (X : Result; Y : Result) return Boolean;
      with function Image_Left (X : Left) return String is <>;
      with function Image_Right (X : Right) return String is <>;
      with function Image_Result (X : Result) return String is <>;
   procedure Check (X : Left; Y : Right; Z : Result);

   generic
      type Left is private;
      type Right is private;
      with function Image_Left (X : Left) return String is <>;
      with function Image_Right (X : Right) return String is <>;
      with function ">" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
      with function ">=" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
      with function "=" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
      with function "/=" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
      with function "<" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
      with function "<=" (X : Left; Y : Right)
         return Fuzzy_Boolean is <>;
   procedure Check_Fuzzy_Relations
             (  X : Left;
                Y : Right;
                LT, LE, EQ, GE, GT, NE : Fuzzy_Boolean
             );

   procedure Check (X : Left; Y : Right; Z : Result) is
   begin
      if not Equal (Op (X, Y), Z) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Name
            &  " (X, Y) [ X="
            &  Image_Left (X)
            &  ", Y="
            &  Image_Right (Y)
            &  " ] = "
            &  Image_Result (Op (X, Y))
            &  " /= "
            &  Image_Result (Z)
         )  );
      end if;
   end Check;

   procedure Check_Fuzzy_Relations
             (  X : Left;
                Y : Right;
                LT, LE, EQ, GE, GT, NE : Fuzzy_Boolean
             )  is
      procedure Compare
                (  Result   : Fuzzy_Boolean;
                   Expected : Fuzzy_Boolean;
                   Name     : String
                )  is
      begin
         if Result /= Expected then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Relation "
               &  Image_Left (X)
               &  " "
               &  Name
               &  " "
               &  Image_Right (Y)
               &  " = "
               &  Image (Result)
               &  " /= "
               &  Image (Expected)
            )  );
         end if;
      end Compare;
   begin
      Compare (X <  Y, LT, "<" );
      Compare (X >  Y, GT, ">" );
      Compare (X =  Y, EQ, "=" );
      Compare (X <= Y, LE, "<=");
      Compare (X >= Y, GE, ">=");
      Compare (X /= Y, NE, "/=");
   end Check_Fuzzy_Relations;

   procedure Check_Possibility is
      new Check
          (  "Possibility",
             Integer,
             Fuzzy_Integer,
             Confidence,
             Possibility,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Possibility is
      new Check
          (  "Possibility",
             Interval,
             Fuzzy_Integer,
             Confidence,
             Possibility,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Possibility is
      new Check
          (  "Possibility",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Confidence,
             Possibility,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Necessity is
      new Check
          (  "Necessity",
             Integer,
             Fuzzy_Integer,
             Confidence,
             Necessity,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Necessity is
      new Check
          (  "Necessity",
             Interval,
             Fuzzy_Integer,
             Confidence,
             Necessity,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Necessity is
      new Check
          (  "Necessity",
             Fuzzy_Integer,
             Integer,
             Confidence,
             Necessity,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Necessity is
      new Check
          (  "Necessity",
             Fuzzy_Integer,
             Interval,
             Confidence,
             Necessity,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Necessity is
      new Check
          (  "Necessity",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Confidence,
             Necessity,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Is_In is
      new Check
          (  "Is_In",
             Integer,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             Is_In,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Is_In is
      new Check
          (  "Is_In",
             Interval,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             Is_In,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Is_In is
      new Check
          (  "Is_In",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             Is_In,
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Equality is
      new Check
          (  "Equality",
             Integer,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             "=",
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Equality is
      new Check
          (  "Equality",
             Interval,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             "=",
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Equality is
      new Check
          (  "Equality",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Boolean,
             "=",
             "=",
             Image,
             Image,
             Image
          );
   procedure Check_Add is
      new Check
          (  "Addition",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Integer,
             "+",
             Equal,
             Image,
             Image,
             Image
          );
   procedure Check_Sub is
      new Check
          (  "Subtraction",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Integer,
             "-",
             Equal,
             Image,
             Image,
             Image
          );
   procedure Check_Mul is
      new Check
          (  "Multiplication",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Integer,
             "*",
             Equal,
             Image,
             Image,
             Image
          );
   procedure Check_Div is
      new Check
          (  "Division",
             Fuzzy_Integer,
             Fuzzy_Integer,
             Fuzzy_Integer,
             "/",
             Equal,
             Image,
             Image,
             Image
          );
   procedure Check_Relations is
      new Check_Fuzzy_Relations (Fuzzy_Integer, Integer, Image, Image);
   procedure Check_Relations is
      new Check_Fuzzy_Relations
          (  Fuzzy_Integer,
             Fuzzy_Integer,
             Image,
             Image
          );
   procedure Check_Relations is
      new Check_Fuzzy_Relations (Integer, Fuzzy_Integer, Image, Image);
   procedure Check_Relations is
      new Check_Fuzzy_Relations (Interval, Fuzzy_Integer, Image, Image);
begin
   Put_Line ("Testing fuzzy integers ...");
   --
   -- Checking relational operations in the singular domain points
   --
   for I in -5..15 loop
      for J in -2..10 loop
         if J < -2 then
            Check_Relations
            (  To_Fuzzy (I),
               J,
               LT => To_Fuzzy (I < J)  + (I <  (J - 1)),
               LE => To_Fuzzy (I <= J) + (I <= (J - 1)),
               EQ => To_Fuzzy (I = J)  + (I =  (J - 1)),
               GE => To_Fuzzy (I >= J) + (I >= (J - 1)),
               GT => To_Fuzzy (I > J)  + (I >  (J - 1)),
               NE => To_Fuzzy (I /= J) + (I /= (J - 1))
            );
            Check_Relations
            (  To_Fuzzy (I),
               To_Fuzzy (J),
               LT => To_Fuzzy (I < J)  + (I <  (J - 1)),
               LE => To_Fuzzy (I <= J) + (I <= (J - 1)),
               EQ => To_Fuzzy (I = J)  + (I =  (J - 1)),
               GE => To_Fuzzy (I >= J) + (I >= (J - 1)),
               GT => To_Fuzzy (I > J)  + (I >  (J - 1)),
               NE => To_Fuzzy (I /= J) + (I /= (J - 1))
            );
         elsif J > 10 then
            Check_Relations
            (  To_Fuzzy (I),
               J,
               LT => To_Fuzzy (I < J)  + (I <  (J + 1)),
               LE => To_Fuzzy (I <= J) + (I <= (J + 1)),
               EQ => To_Fuzzy (I = J)  + (I =  (J + 1)),
               GE => To_Fuzzy (I >= J) + (I >= (J + 1)),
               GT => To_Fuzzy (I > J)  + (I >  (J + 1)),
               NE => To_Fuzzy (I /= J) + (I /= (J + 1))
            );
            Check_Relations
            (  To_Fuzzy (I),
               To_Fuzzy (J),
               LT => To_Fuzzy (I < J)  + (I <  (J + 1)),
               LE => To_Fuzzy (I <= J) + (I <= (J + 1)),
               EQ => To_Fuzzy (I = J)  + (I =  (J + 1)),
               GE => To_Fuzzy (I >= J) + (I >= (J + 1)),
               GT => To_Fuzzy (I > J)  + (I >  (J + 1)),
               NE => To_Fuzzy (I /= J) + (I /= (J + 1))
            );
         else
            Check_Relations
            (  To_Fuzzy (I),
               J,
               LT => To_Fuzzy (I < J),
               LE => To_Fuzzy (I <= J),
               EQ => To_Fuzzy (I = J),
               GE => To_Fuzzy (I >= J),
               GT => To_Fuzzy (I > J),
               NE => To_Fuzzy (I /= J)
            );
            Check_Relations
            (  To_Fuzzy (I),
               To_Fuzzy (J),
               LT => To_Fuzzy (I < J),
               LE => To_Fuzzy (I <= J),
               EQ => To_Fuzzy (I = J),
               GE => To_Fuzzy (I >= J),
               GT => To_Fuzzy (I > J),
               NE => To_Fuzzy (I /= J)
            );
         end if;
      end loop;
   end loop;
   declare
      X    : constant Fuzzy_Integer := Value ("1..4:0.5, 2..3");
      Half : constant Confidence    := Possibility (1, X);
   begin
      if (  Half = Confidence'First
         or else
            Half = Confidence'Last
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in possibility/necessity"
         );
      end if;
      Check_Possibility (0, X, Confidence'First);
      Check_Possibility (1, X, Half);
      Check_Possibility (2, X, Confidence'Last);
      Check_Possibility (3, X, Confidence'Last);
      Check_Possibility (4, X, Half);
      Check_Possibility (5, X, Confidence'First);
      Check_Possibility ((2,3), X, Confidence'Last);
      Check_Possibility ((0,1), X, Half);
      Check_Possibility ((0,2), X, Confidence'Last);
      Check_Possibility (X, X, Confidence'Last);
      Check_Possibility (Fuzzy_Integer'(Value ("4..5")), X, Half);

      Check_Necessity (0, X, Confidence'First);
      Check_Necessity (1, X, Confidence'First);
      Check_Necessity (2, X, Confidence'First);
      Check_Necessity (3, X, Confidence'First);
      Check_Necessity (3, Fuzzy_Integer'(Value ("3")), Confidence'Last);
      Check_Necessity ((-1,1), X, Confidence'First);
      Check_Necessity ((1,2), X, Confidence'First);
      Check_Necessity ((2,3), X, not Half);
      Check_Necessity ((1,3), X, not Half);
      Check_Necessity ((1,4), X, Confidence'Last);
      Check_Necessity
      (  Fuzzy_Integer'(Value ("1..2")),
         X,
         Confidence'First
      );
      Check_Necessity (Fuzzy_Integer'(Value ("2..3")), X, not Half);
      Check_Necessity
      (  Fuzzy_Integer'(Value ("1..4")),
         X,
         Confidence'Last
      );
      Check_Necessity (X, 1, Half);
      Check_Necessity (X, 2, Confidence'Last);
      Check_Necessity (X, (1, 2), Half);
      Check_Necessity (X, (1, 3), Half);
      Check_Necessity (X, (0, 3), Confidence'First);
      Check_Necessity (X, (2, 3), Confidence'Last);
      Check_Necessity (X, X, Half);

      Check_Is_In (0, X, Certain_False);
      Check_Is_In (1, X, (Possibility => Half, Necessity => Half));
      Check_Is_In (3, X, Certain_True);
      Check_Is_In ((2,3), X, Certain_True);
      Check_Is_In
      (  (1,3),
         X,
         (Possibility => Confidence'Last, Necessity => Half)
      );
      Check_Is_In ((0,3), X, Uncertain);
      Check_Is_In
      (  X,
         X,
         (Possibility => Confidence'Last, Necessity => Half)
      );
      Check_Equality (0, X, Certain_False);
      Check_Equality
      (  1,
         X,
         (Possibility => Half, Necessity => Confidence'First)
      );
      Check_Equality (2, X, Uncertain);
      Check_Equality
      (  (1,2),
         Fuzzy_Integer'(Value ("1..2")),
         Certain_True
      );
      Check_Equality
      (  (2,3),
         X,
         (  Possibility => Confidence'Last,
            Necessity   => Half and not Half
      )  );
      Check_Equality
      (  (1,4),
         X,
         (  Possibility => Confidence'Last,
            Necessity   => Half
      )  );
      Check_Equality ((1,5), X, Uncertain);
      Check_Equality
      (  X,
         X,
         (  Possibility => Confidence'Last,
            Necessity   => Half
      )  );
      Check_Relations
      (  0,
         X,
         LT => Certain_True,
         LE => Certain_True,
         EQ => Certain_False,
         GE => Certain_False,
         GT => Certain_False,
         NE => Certain_True
      );
      Check_Relations
      (  1,
         X,
         LT => (Possibility => Confidence'Last, Necessity => not Half),
         LE => Certain_True,
         EQ => (Possibility => Half, Necessity => Confidence'First),
         GE => (Possibility => Half, Necessity => Confidence'First),
         GT => Certain_False,
         NE => (Possibility => Confidence'Last, Necessity => not Half)
      );
      Check_Relations
      (  2,
         X,
         LT => Uncertain,
         LE => (Possibility => Confidence'Last, Necessity => not Half),
         EQ => Uncertain,
         GE => Uncertain,
         GT => (Possibility => Half, Necessity => Confidence'First),
         NE => Uncertain
      );
      Check_Relations
      (  (5,6),
         X,
         LT => Certain_False,
         LE => Certain_False,
         EQ => Certain_False,
         GE => Certain_True,
         GT => Certain_True,
         NE => Certain_True
      );
      Check_Relations
      (  X,
         X,
         LT => Uncertain,
         LE => Uncertain,
         EQ => (Possibility => Confidence'Last, Necessity => Half),
         GE => Uncertain,
         GT => Uncertain,
         NE => (Possibility => not Half, Necessity => Confidence'First)
      );
      Check_Add (X, X, Fuzzy_Integer'(Value ("2..8:0.5, 4..6")));
      Check_Sub (X, X, Fuzzy_Integer'(Value ("-3..3:0.5, -1..1")));
      Check_Mul (X, X, Fuzzy_Integer'(Value ("1..16:0.5, 4..9")));
      Check_Div (X, X, Fuzzy_Integer'(Value ("0..4:0.5, 0..2")));
   end;

   begin
      declare
         X : constant Fuzzy_Integer := Value (",");
         Y : Fuzzy_Integer;
      begin
         Y := X;
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Value"
         );
      end;
   exception
      when End_Error =>
         null;
   end;

   declare
      Text    : constant String := "2, something";
      Pointer : Integer := Text'First;
      X       : Fuzzy_Integer;
   begin
      Get (Text, Pointer, X);
      if (  not Equal (X, To_Fuzzy (2))
         or else
            Pointer /= Text'First + 1
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;
   
   declare
      Text    : constant String := "2. something";
      Pointer : Integer := Text'First;
      X       : Fuzzy_Integer;
   begin
      Get (Text, Pointer, X);
      if (  not Equal (X, To_Fuzzy (2))
         or else
            Pointer /= Text'First + 1
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;
   declare
      Text    : constant String := "2 : something";
      Pointer : Integer := Text'First;
      X       : Fuzzy_Integer;
   begin
      Get (Text, Pointer, X);
      if (  not Equal (X, To_Fuzzy (2))
         or else
            Pointer /= Text'First + 1
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      raise Program_Error;
end Test_Fuzzy_Integers;
