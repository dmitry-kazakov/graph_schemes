--                                                                    --
--  procedure Test_Linguistic       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;
with Confidence_Factors;             use Confidence_Factors;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Fuzzy;                          use Fuzzy;
with Fuzzy.Logic;                    use Fuzzy.Logic;
with Fuzzy_Floats;                   use Fuzzy_Floats;
with Fuzzy.Edit;                     use Fuzzy.Edit;
with Fuzzy.Edit.Floats;              use Fuzzy.Edit.Floats;
with Fuzzy.Edit.Linguistics;         use Fuzzy.Edit.Linguistics;
with Fuzzy.Edit.Linguistic_Sets;     use Fuzzy.Edit.Linguistic_Sets;
with Fuzzy.Intuitionistic;           use Fuzzy.Intuitionistic;
with Fuzzy_Linguistic_Sets;          use Fuzzy_Linguistic_Sets;
with Fuzzy_Linguistics;              use Fuzzy_Linguistics;
with Float_Intervals;                use Float_Intervals;
with Strings_Edit.Floats;            use Strings_Edit.Floats;
with Strings_Edit.Integers;          use Strings_Edit.Integers;

with Fuzzy_Linguistic_Leftmost_Max;
with Fuzzy_Linguistic_Rightmost_Max;
with Fuzzy_Linguistic_Discrete_Center_Of_Gravity;
with Fuzzy_Linguistic_Center_Of_Area;
with Fuzzy_Linguistic_Center_Of_Gravity;
with Fuzzy_Measure_Linguistic_Sets;
with Units.Constants;

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

procedure Test_Linguistic is

   function Equal (X, Y : Confidence) return Boolean is
   begin
      return abs (Float (X) - Float (Y)) < 0.05;
   end Equal;

   function Equal (X, Y : Fuzzy_Boolean) return Boolean is
   begin
      return
      (  Equal (X.Possibility, Y.Possibility)
      and
         Equal (X.Necessity, Y.Necessity)
      );
   end Equal;

   function Equal (X, Y : Float) return Boolean is
   begin
      return abs (X - Y) < 0.0001;
   end Equal;

   function Img (Value : Interval) return String is
   begin
      return "[" & Image (Value.From) & ", " & Image (Value.To) & "]";
   end Img;

   function Img (Value : Float) return String is
   begin
      return Image (Value);
   end Img;

   function Img (Value : Fuzzy_Float) return String is
   begin
      return Image (Value);
   end Img;

   function Img (Value : Variable) return String is
   begin
      return Image (Value);
   end Img;

   generic
      type Number is private;
      with function Img (Value : Number) return String is <>;
      with function Is_In (Value : Number; Var : Variable)
         return Fuzzy_Boolean is <>;
      with function Possibility (Var : Variable; Value : Number)
         return Confidence is <>;
      with function Necessity (Var : Variable; Value : Number)
         return Confidence is <>;
   procedure Generic_Check
             (  Var      : Variable;
                Value    : Number;
                Expected : Fuzzy_Boolean
             );

   procedure Generic_Check
             (  Var      : Variable;
                Value    : Number;
                Expected : Fuzzy_Boolean
             )  is
      Result : constant Fuzzy_Boolean := Is_In (Value, Var);
   begin
      if not Equal (Result, Expected) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Is_In ("
            &  Img (Value)
            &  ", ("
            &  Image (Var)
            &  ")) = "
            &  "(Pos="
            &  Image (Result.Possibility)
            &  ", Nec="
            &  Image (Result.Necessity)
            &  ") while expected (Pos="
            &  Image (Expected.Possibility)
            &  ", Nec="
            &  Image (Expected.Necessity)
            &  ")"
         )  );
      end if;
      if Result.Possibility /= Possibility (Var, Value) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Is_In ("
            &  Img (Value)
            &  ", ("
            &  Image (Var)
            &  ").Possibility = "
            &  Image (Result.Possibility)
            &  ") while Possibility gives"
            &  Image (Possibility (Var, Value))
         )  );
      end if;
      if Result.Necessity /= Necessity (Var, Value) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Is_In ("
            &  Img (Value)
            &  ", ("
            &  Image (Var, 10)
            &  ").Necessity = "
            &  Image (Result.Necessity)
            &  ") while Necessity gives"
            &  Image (Necessity (Var, Value))
         )  );
      end if;
   end Generic_Check;

   procedure Check is new Generic_Check (Float);
   procedure Check is new Generic_Check (Interval);
   procedure Check is new Generic_Check (Fuzzy_Float);
   procedure Check is new Generic_Check (Variable);

   Shoulder  : Variable;
   Rectangle : Variable;
   Trapezoid : Variable;
   Singleton : Variable;
   Pulse     : Variable;
begin
   Put_Line ("Testing linguistic variables ...");

   Append (Shoulder, 0.0,  Confidence'Last);
   Append (Shoulder, 10.0, Confidence'First);

   Check (Shoulder, -100.0, Certain_True);
   Check (Shoulder,    0.0, Certain_True);
   Check (Shoulder,    5.0, (0.5, 0.5));
   Check (Shoulder,   10.0, Certain_False);
   Check (Shoulder,   20.0, Certain_False);

   Check (Shoulder, (-100.0,  0.0), Certain_True);
   Check (Shoulder, (-100.0,  5.0), (Confidence'Last, 0.5));
   Check (Shoulder, (   5.0, 10.0), (0.5, Confidence'First));
   Check (Shoulder, (  10.0, 20.0), Certain_False);

   Check
   (  Shoulder,
      Fuzzy_Float'(Value ("-10..5:0.5, -5..0")),
      (Confidence'Last, 0.5)
   );
   Check
   (  Shoulder,
      Fuzzy_Float'(Value ("-10..10:0.5, -5..0")),
      (Confidence'Last, 0.5)
   );

   Append (Rectangle, -10.0, Confidence'First);
   Append (Rectangle, -10.0, Confidence'Last);
   Append (Rectangle,  10.0, Confidence'Last);
   Append (Rectangle,  10.0, Confidence'First);

   Check (Rectangle, -100.0,  Certain_False);
   Check (Rectangle,  -10.0,  Uncertain);
   Check (Rectangle,    0.0,  Certain_True);
   Check (Rectangle,   10.0,  Uncertain);
   Check (Rectangle,   100.0, Certain_False);

   Check (Rectangle, (-40.0, -20.0), Certain_False);
   Check (Rectangle, (-40.0, -10.0), Uncertain);
   Check (Rectangle, ( -9.0,   9.0), Certain_True);
   Check (Rectangle, (-40.0,  40.0), Uncertain);

   Append (Trapezoid, -10.0, Confidence'First);
   Append (Trapezoid,   0.0, Confidence'Last);
   Append (Trapezoid,  10.0, Confidence'Last);
   Append (Trapezoid,  15.0, Confidence'First);

   Check (Trapezoid, -100.0, Certain_False);
   Check (Trapezoid, -10.0,  Certain_False);
   Check (Trapezoid,  -5.0,  (0.5, 0.5));
   Check (Trapezoid,   0.0,  Certain_True);
   Check (Trapezoid,  10.0,  Certain_True);
   Check (Trapezoid,  12.5,  (0.5, 0.5));
   Check (Trapezoid,  15.0,  Certain_False);
   Check (Trapezoid,  100.0, Certain_False);

   Check (Trapezoid, (-5.0, 5.0), (Confidence'Last, 0.5));

   Append (Singleton, 10.0, Confidence'First);
   Append (Singleton, 10.0, Confidence'Last);
   Append (Singleton, 10.0, Confidence'First);

   Check (Singleton, -10.0, Certain_False);
   Check (Singleton,  10.0, Uncertain);
   Check (Singleton,  20.0, Certain_False);

   Check (Singleton, ( 0.0, 10.0), Uncertain);
   Check (Singleton, (11.0, 12.0), Certain_False);
   Check (Singleton, (10.0, 10.0), Uncertain);

   Append (Pulse, 0.0, 0.75);
   Append (Pulse, 0.0, Confidence'Last);
   Append (Pulse, 0.0, 0.25);
   Append (Pulse, 0.0, 0.5);

   Check (Pulse, (-5.0, 5.0), (Confidence'Last, 0.25));
   Check (Pulse, (-5.0, 0.0), (Confidence'Last, 0.25));
   Check (Pulse, ( 0.0, 3.0), (Confidence'Last, 0.25));
   Check (Pulse, ( 0.0, 0.0), (Confidence'Last, 0.25));

   if Equal (Trapezoid, Singleton) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Equal"
      );
   end if;

   if (  Image (Variable'(Value (Image (Rectangle))))
      /= Image (Rectangle)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Value/Image"
      );
   end if;

   if (  Image (Variable'(Value (Image (Singleton))))
      /= Image (Singleton)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Value/Image"
      );
   end if;

   if (  Image (Variable'(Value (Image (Pulse))))
      /= Image (Pulse)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Value/Image"
      );
   end if;

   declare
      Text    : constant String := "10,";
      Pointer : Positive := Text'First;
      X       : Variable;
   begin
      Get (Text, Pointer, X);
      if Pointer /= Text'First + 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;

   declare
      Text    : constant String := "10 : something";
      Pointer : Positive := Text'First;
      X       : Variable;
   begin
      Get (Text, Pointer, X);
      if Pointer /= Text'First + 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;

   if not Equal
          (  Shoulder and 0.5,
             (5.0, 0.5) & (10.0, Confidence'First),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Shoulder and 0.5"
      );
   end if;

   declare
      S : constant Variable := Shoulder or 0.5;
      R : Variable;
   begin
      Append (R, 0.0, Confidence'Last);
      Append (R, 5.0, 0.5);
      if not Equal (S, R, 0.1)  then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Shoulder or 0.5 " & Image (S)
         );
      end if;
   end;

   if not Equal
          (  Singleton and 0.5,
             (  (10.0, Confidence'First)
             &  (10.0, 0.5)
             &  (10.0, Confidence'First)
          )  )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Singleton and 0.5"
      );
   end if;

   if not Is_Empty (Singleton and Confidence'First) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Singleton and False"
      );
   end if;

   if not Equal
          (  Singleton xor 0.3,
             (  (10.0, 0.3)
             &  (10.0, not 0.3)
             &  (10.0, 0.3)
          )  )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Singleton xor 0.3"
      );
   end if;

   if not Equal
          (  (  (  (-3.0, Confidence'First)
                &  (-1.0, Confidence'Last)
                &  ( 1.0, Confidence'First)
                )
             xor
                (  (-1.0, Confidence'First)
                &  ( 1.0, Confidence'Last)
                &  ( 3.0, Confidence'First)
             )  ),
             (  (-3.0, Confidence'First)
             &  (-1.0, Confidence'Last)
             &  ( 0.0, 0.5 xor 0.5)
             &  ( 1.0, Confidence'Last)
             &  ( 3.0, Confidence'First)
          )  )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Triangle xor Triangle"
      );
   end if;

   if not Equal
          (  Trapezoid and 0.5,
             (  (-10.0, 0.0)
             &  ( -5.0, 0.5)
             &  ( 12.5, 0.5)
             &  ( 15.0, 0.0)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid and 0.5"
      );
   end if;

   if not Equal
          (  Trapezoid or 0.5,
             (  (-10.0, 0.5)
             &  ( -5.0, 0.5)
             &  (  0.0, Confidence'Last)
             &  ( 10.0, Confidence'Last)
             &  ( 12.5, 0.5)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid or 0.5"
      );
   end if;

   if not Equal
          (  Trapezoid xor 0.3,
             (  (-7.0, 0.3)
             &  (-3.0, not 0.3)
             &  (11.5, not 0.3)
             &  (13.5, 0.3)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid xor 0.3 " & Image (Trapezoid xor 0.3)
      );
   end if;

   if not Equal
          (  not Trapezoid,
             (  (-10.0, Confidence'Last)
             &  (  0.0, Confidence'First)
             &  ( 10.0, Confidence'First)
             &  ( 15.0, Confidence'Last)
          )  )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in not Trapezoid"
      );
   end if;

   if not Equal
          (  not Pulse,
             (  (  0.0, not 0.75)
             &  (  0.0, Confidence'First)
             &  (  0.0, not 0.25)
             &  (  0.0, not 0.5)
          )  )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in not Pulse"
      );
   end if;

   if not Equal (Trapezoid, Trapezoid or Trapezoid) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid or Trapezoid"
      );
   end if;

   if not Equal
          (  Trapezoid and Shoulder,
             (  (-10.0, Confidence'First)
             &  (  0.0, Confidence'Last)
             &  ( 10.0, Confidence'First)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid and Shoulder"
      );
   end if;

   if not Equal
          (  Trapezoid and Pulse,
             (  (-10.0, Confidence'First)
             &  ( -2.5, 0.75)
             &  (  0.0, 0.75)
             &  (  0.0, Confidence'Last)
             &  (  0.0, 0.25)
             &  (  0.0, 0.5)
             &  ( 12.5, 0.5)
             &  ( 15.0, Confidence'First)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid and Pulse"
      );
   end if;

   declare
      S : constant Variable := (5.0, 0.5) & (10.0, Confidence'First);
   begin
      if not Equal
             (  S or Trapezoid,
                (  ( -5.0, 0.5)
                &  (  0.0, Confidence'Last)
                &  ( 10.0, Confidence'Last)
                &  ( 15.0, Confidence'First)
                ),
                0.1
             )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in S or Trapezoid"
         );
      end if;
   end;

   if not Equal
          (  (Shoulder and 0.5) or Trapezoid,
             (  ( -5.0, 0.5)
             &  (  0.0, Confidence'Last)
             &  ( 10.0, Confidence'Last)
             &  ( 15.0, Confidence'First)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in (Shoulder and 0.5) or Trapezoid"
      );
   end if;

   if not Equal
          (  Trapezoid or Pulse,
             (  ( -2.5, 0.75)
             &  (  0.0, Confidence'Last)
             &  ( 10.0, Confidence'Last)
             &  ( 12.5, 0.5)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid or Pulse"
      );
   end if;

   if not Equal
          (  Singleton or Pulse,
             (  (  0.0, 0.75)
             &  (  0.0, Confidence'Last)
             &  (  0.0, 0.25)
             &  (  0.0, 0.5)
             &  ( 10.0, 0.5)
             &  ( 10.0, Confidence'Last)
             &  ( 10.0, 0.5)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Singleton or Pulse"
      );
   end if;

   declare
      T : Variable := Shoulder;
   begin
      T := T or Shoulder;
      if not Equal (T, Shoulder, 0.1) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in T := T or Shoulder"
         );
      end if;
      T := T or Trapezoid;
      if not Equal
             (  T,
                (10.0, Confidence'Last) & (15.0, Confidence'First),
                0.1
             )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in T := T or Trapezoid"
         );
      end if;
   end;

   if not Equal
          (  (  (10.0, Confidence'Last) & (10.0, Confidence'First)
             and
                Rectangle
             ),
             Rectangle
          )
   then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Rectanges intersection"
         );
   end if;

   if not Equal
          (  Trapezoid xor Shoulder,
             (  (-10.0, Confidence'Last)
             &  (  0.0, Confidence'First)
             &  ( 10.0, Confidence'Last)
             &  ( 15.0, Confidence'First)
             ),
             0.1
          )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Trapezoid xor Shoulder"
      );
   end if;

   if Possibility (Pulse) /= Confidence'Last then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Possibility (Pulse)"
      );
   end if;

   if Necessity (Pulse) /= 0.25 then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Necessity (Pulse)"
      );
   end if;

   if Possibility (Trapezoid, Pulse) /= Confidence'Last then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Possibility (Trapezoid, Pulse)"
      );
   end if;

   if (  Necessity (Trapezoid, Pulse)
      /= not Possibility (not Trapezoid, Pulse)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Necessity (Trapezoid, Pulse)"
      );
   end if;

   if (  Necessity (Pulse, Trapezoid)
      /= not Possibility (not Pulse, Trapezoid)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Necessity (Pulse, Trapezoid)"
      );
   end if;

   declare
      Figures : Linguistic_Set;
   begin
      Add (Figures, "Shoulder",  Shoulder);
      Add (Figures, "Rectangle", Rectangle);
      Add (Figures, "Trapezoid", Trapezoid);
      Add (Figures, "Pulse",     Pulse);
      Add (Figures, "Singleton", Singleton);

      if not Equal (Value (Image (Figures)), Figures, 0.1) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Image (Figures)"
         );
      end if;
      begin
         declare
            X : constant Fuzzy.Intuitionistic.Set :=
                         Value (",", Figures);
            Y : Fuzzy.Intuitionistic.Set (X.Cardinality);
         begin
            Y := X;
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Value (Intuitionistic.Set)"
            );
         end;
      exception
         when End_Error =>
            null;
      end;

      declare
         Minus_Fifteen : constant Classification :=
            Classify (Figures, -15.0);
      begin
         if (  (  Minus_Fifteen.Possibility
               /= Minus_Fifteen.Necessity
               )
            or else
               (  Minus_Fifteen.Possibility
               /= (  Confidence'Last,
                     Confidence'First,
                     Confidence'First,
                     0.75,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, -15.0)"
            );
         end if;
      end;

      declare
         Minus_Five : constant Classification :=
            Classify (Figures, -5.0);
      begin
         if (  (  Minus_Five.Possibility
               /= Minus_Five.Necessity
               )
            or else
               (  Minus_Five.Possibility
               /= (  Confidence'Last,
                     Confidence'Last,
                     0.5,
                     0.75,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, -5.0)"
            );
         end if;
      end;

      declare
         Zero : constant Classification :=
            Classify (Figures, 0.0);
      begin
         if (  (  Zero.Possibility
               /= (  Confidence'Last,
                     Confidence'Last,
                     Confidence'Last,
                     Confidence'Last,
                     Confidence'First
               )  )
            or else
               (  Zero.Necessity
               /= (  Confidence'Last,
                     Confidence'Last,
                     Confidence'Last,
                     0.25,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, 0.0)"
            );
         end if;
      end;

      declare
         Five : constant Classification :=
            Classify (Figures, 5.0);
      begin
         if (  (  Five.Possibility
               /= Five.Necessity
               )
            or else
               (  Five.Possibility
               /= (  0.5,
                     Confidence'Last,
                     Confidence'Last,
                     0.5,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, 5.0)"
            );
         end if;
      end;

      declare
         Ten : constant Classification :=
            Classify (Figures, 10.0);
      begin
         if (  (  Ten.Possibility
               /= (  Confidence'First,
                     Confidence'Last,
                     Confidence'Last,
                     0.5,
                     Confidence'Last
               )  )
            or else
               (  Ten.Necessity
               /= (  Confidence'First,
                     Confidence'First,
                     Confidence'Last,
                     0.5,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, 10.0)"
            );
         end if;
      end;

      declare
         Twenty : constant Classification :=
            Classify (Figures, 20.0);
      begin
         if (  (  Twenty.Possibility
               /= Twenty.Necessity
               )
            or else
               (  Twenty.Necessity
               /= (  Confidence'First,
                     Confidence'First,
                     Confidence'First,
                     0.5,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, 20.0)"
            );
         end if;
      end;

      declare
         Minus_Plus_Five : constant Classification :=
            Classify (Figures, (-5.0, 5.0));
      begin
         if (  (  Minus_Plus_Five.Possibility
               /= (  Confidence'Last,
                     Confidence'Last,
                     Confidence'Last,
                     Confidence'Last,
                     Confidence'First
               )  )
            or else
               (  Minus_Plus_Five.Necessity
               /= (  0.5,
                     Confidence'Last,
                     0.5,
                     0.25,
                     Confidence'First
            )  )  )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Classify (Figures, (-5.0, 5.0)"
            );
         end if;
      end;

      declare
         S_T_P_S : constant Variable :=
                  Accumulate
                  (  Figures,
                     (  Confidence'Last,
                        Confidence'First,
                        Confidence'Last,
                        Confidence'Last,
                        0.5
                  )  );
      begin
         if not Equal
                (  S_T_P_S,
                   (10.0, Confidence'Last) & (12.5, 0.5),
                   0.1
                )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Accumulate 1,0,1,1"
            );
         end if;
      end;

      declare
         HS_T_P_S : constant Variable :=
                  Accumulate
                  (  Figures,
                     (  0.5,
                        Confidence'First,
                        Confidence'Last,
                        Confidence'Last,
                        0.5
                  )  );
      begin
         if not Equal
                (  HS_T_P_S,
                   (  (-2.5, 0.75)
                   &  ( 0.0, Confidence'Last)
                   &  (10.0, Confidence'Last)
                   &  (12.5, 0.5)
                   ),
                   0.1
                )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Accumulate 0.5,0,1,1"
            );
         end if;
      end;

      declare
         T : constant Fuzzy.Intuitionistic.Classification :=
                (  Cardinality => 5,
                   Possibility =>
                      (  Confidence'Last,
                         Confidence'Last,
                         0.75,
                         Confidence'Last,
                         Confidence'First
                      ),
                   Necessity =>
                      (  Confidence'Last,
                         Confidence'Last,
                         0.25,
                         Confidence'First,
                         Confidence'First
                )     );
         X : constant Defuzzification_Result := Defuzzify (Figures, T);
      begin
         if (  abs (X.Segment.From - (-7.5)) > 0.1
            or abs (X.Segment.To   - (-2.5)) > 0.1
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Defizzify"
            );
         end if;
      end;

      declare
         X : constant Fuzzy.Intuitionistic.Set :=
                Value
                (  "Trapezoid..Singleton, Shoulder:0.5:0.2",
                   Figures,
                   Certain_True
                );
      begin
         if (  (  X
               /= (  Cardinality => 5,
                     Possibility =>
                        (  1      => 0.5,
                           2      => Confidence'First,
                           others => Confidence'Last
                        ),
                     Necessity =>
                        (  1      => 0.2,
                           2      => Confidence'First,
                           others => Confidence'Last
               )  )     )
            or else
               Value (Image (Figures, X), Figures) /= X
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error in Value (Intuitionistic.Set)" & Image (Figures, X)
            );
         end if;
      end;
   end;

   if not Equal (Trapezoid, Value ("-10:0->0..10->15:0"), 0.1) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Value (Trapezoid)"
      );
   end if;

   if not Equal (Value (Image (Trapezoid)), Trapezoid, 0.1) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Image (Trapezoid)"
      );
   end if;

   if not Equal (Value (Image (Singleton)), Singleton, 0.1) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Image (Singleton)"
      );
   end if;

   if not Equal (Value (Image (Pulse)), Pulse, 0.1) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Image (Pulse)"
      );
   end if;

   declare
      Text    : constant String := "-> -100 -> 0 -> 10:false ,?";
      Pointer : Integer := Text'First;
      Value   : Variable := Pulse;
   begin
      Get (Text, Pointer, Value);
      if Text (Pointer..Text'Last) /= " ,?" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get (Shoulder)"
         );
      end if;
      if not Equal (Shoulder, Value, 0.1) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get (Shoulder)"
         );
      end if;
   end;

   if Image (Rectangle, AbsSmall => 0) /= "-10..10" then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Image (Rectangle)"
      );
   end if;

   if Image (Singleton, AbsSmall => 0) /= "10" then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Image (Singleton)"
      );
   end if;

   begin
      declare
         X : constant Variable := Value (",");
         Y : Variable;
      begin
         Y := X;
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Value (Variable)"
         );
      end;
   exception
      when End_Error =>
         null;
   end;

   begin
      declare
         X : constant Linguistic_Set := Value (",");
         Y : Linguistic_Set;
      begin
         Y := X;
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Value (Set)"
         );
      end;
   exception
      when End_Error =>
         null;
   end;

   declare
      X : constant Linguistic_Set :=
                   Value (" My Trapezoid (-10:0->0..10->15:0) ");
   begin
      if Get_Name (X, 1) /= "My Trapezoid" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Value (linguistic set)"
         );
      end if;
      if not Equal (Trapezoid, Get (X, 1), 0.1) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get from set"
         );
      end if;
   end;

   Check
   (  Value ("0:0->5..10->15:0"),
      (-10.0, -5.0),
      Certain_False
   );
   Check
   (  Value ("0:0->5..10->15:0"),
      (Float'First, -5.0),
      Certain_False
   );
   Check
   (  Value ("0:0->5..10->15:0"),
      (Float'First, 2.5),
      (0.5, 0.0)
   );
   Check
   (  Value ("0:0->5..10->"),
      (Float'First, 2.5),
      (0.5, 0.0)
   );
   Check
   (  Variable'(Value ("1:0->1->")),
      Variable'(Value ("0:0->2->")),
      (Confidence'Last, 0.5)
   );
   Check
   (  Variable'(Value ("0:0->0->")),
      Variable'(Value ("0:0->2->")),
      Certain_True
   );
   Check
   (  Variable'(Value ("-1:0->-1->")),
      Variable'(Value ("0:0->2->")),
      Certain_True
   );
   Check
   (  Variable'(Value ("2:0->2->")),
      Variable'(Value ("0:0->2->")),
      Uncertain
   );
   declare
      X : Variable;
      Y : Variable;
   begin
      Append (X, 10.0, Confidence'First);
      Append (X, Float'Succ (10.0), Confidence'Last);
      Append (X, Float'Pred (20.0), Confidence'Last);
      Append (X, 20.0, Confidence'First);

      Append (Y,  5.0, Confidence'Last);
      Append (Y, 20.0, Confidence'First);

      if (  Possibility (Y, Variable'(Value ("->10:0->10->20->20:0->")))
         /= Possibility (Y, X)
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Possibility"
         );
      end if;
   end;
   --
   -- Defuzzification methods
   --
   declare
      type Defuzzify_Method is
         access function (X : Variable) return Float;
      procedure Defuzzy
                (  X        : Variable;
                   Expected : Float;
                   Method   : Defuzzify_Method;
                   Name     : String
                )  is
         Result : constant Float := Method (X);
      begin
         if abs (Result - Expected) > 0.000001 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  Name
               &  " error: expected "
               &  Image (Expected)
               &  " got "
               &  Image (Result)
               &  " defuzzifying "
               &  Image (X)
            )  );
         end if;
      end Defuzzy;
      One  : constant Float := Float (Confidence'(0.1));
      Nine : constant Float := Float (Confidence'(0.9));
   begin
      declare
         X : constant Variable :=
             (  ( 0.0, 0.0)
             &  ( 1.0, 0.5)
             &  ( 2.0, 0.5)
             &  ( 3.0, 0.6)
             &  ( 5.0, 0.6)
             &  ( 7.0, Confidence'Last)
             &  ( 8.0, 0.7)
             &  ( 9.0, 0.0)
             &  (16.0, 0.1)
             &  (17.0, 0.3)
             &  (18.0, 0.0)
             );
      begin
         Defuzzy
         (  X,
            5.733789,
            Fuzzy_Linguistic_Center_Of_Area'Access,
            "CoA"
         );
         Defuzzy
         (  X,
            6.11707,
            Fuzzy_Linguistic_Center_Of_Gravity'Access,
            "CoG"
         );
         Defuzzy
         (  X,
            7.0,
            Fuzzy_Linguistic_Leftmost_Max'Access,
            "LM"
         );
         Defuzzy
         (  X,
            17.0,
            Fuzzy_Linguistic_Rightmost_Max'Access,
            "RM"
         );
      end;
      --
      -- Center of gravity
      --
      Defuzzy
      (  (-1.0, 0.0) & (0.0, 0.7) & (1.0, 0.0),
         0.0,
         Fuzzy_Linguistic_Center_Of_Gravity'Access,
         "CoG"
      );
      Defuzzy
      (  (0.0, 0.0) & (0.0, 0.7) & (1.0, 0.7) & (1.0, 0.0),
         0.5,
         Fuzzy_Linguistic_Center_Of_Gravity'Access,
         "CoG"
      );
      Defuzzy
      (  (0.0, 0.0) & (1.0, 0.7) & (2.0, 0.0),
         1.0,
         Fuzzy_Linguistic_Center_Of_Gravity'Access,
         "CoG"
      );
      Defuzzy
      (  (  (0.0,              0.0)
         &  (0.1,              0.0) & (0.1,              0.9)
         &  (0.1 + One,        0.9) & (0.1 + One,        0.1)
         &  (0.1 + One + Nine, 0.1) & (0.1 + One + Nine, 0.0)
         ),
         0.1 + (3.0 * One + Nine) / 4.0,
         Fuzzy_Linguistic_Center_Of_Gravity'Access,
         "CoG"
      );
      Defuzzy
      (  (  (0.0, 0.0)
         &  (1.0, 0.7)
         &  (2.0, 0.0)
         &  (2.0, 0.7)
         &  (3.0, 0.0)
         &  (4.0, 0.7)
         &  (4.0, 0.0)
         ),
         2.0,
         Fuzzy_Linguistic_Center_Of_Gravity'Access,
         "CoG"
      );
      --
      -- Center of area
      --
      Defuzzy
      (  (0.0, 0.0) & (1.0, 0.7) & (2.0, 0.0),
         1.0,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (  (0.0,              0.0)
         &  (0.1,              0.0) & (0.1,              0.9)
         &  (0.1 + One,        0.9) & (0.1 + One,        0.1)
         &  (0.1 + One + Nine, 0.1) & (0.1 + One + Nine, 0.0)
         ),
         0.1 + One,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (  (0.0, 0.0)
         &  (1.0, 0.7)
         &  (2.0, 0.0)
         &  (2.0, 0.7)
         &  (3.0, 0.0)
         &  (4.0, 0.7)
         &  (4.0, 0.0)
         ),
         2.0,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (  (0.0, 0.0)
         &  (1.0, 0.9)
         &  (2.0, 0.0)
         &  (3.0, 0.9)
         &  (4.0, 0.0)
         ),
         2.0,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (  (0.0, 0.0)
         &  (1.0, 0.9)
         &  (2.0, 0.0)
         &  (6.0, 0.0)
         &  (7.0, 0.9)
         &  (8.0, 0.0)
         ),
         2.0,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (0.0, 0.0) & (0.0, 0.9) & (10.0, 0.9) & (10.0, 0.0),
         5.0,
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      Defuzzy
      (  (0.0, 0.0) & (10.0, 0.9) & (10.0, 0.0),
         10.0 / sqrt (2.0),
         Fuzzy_Linguistic_Center_Of_Area'Access,
         "CoA"
      );
      --
      -- Rightmost maximum
      --
      Defuzzy
      (  (0.0, 0.0) & (1.0, 0.7) & (2.0, 0.0),
         1.0,
         Fuzzy_Linguistic_Rightmost_Max'Access,
         "RM"
      );
      Defuzzy
      (  (  ( 1.0, 0.0)
         &  ( 2.0, 0.1)
         &  ( 5.0, 0.1)
         &  ( 6.0, 0.2)
         &  ( 9.0, 0.2)
         &  (10.0, 0.7)
         &  (12.0, 0.7)
         &  (14.0, 0.5)
         &  (15.0, 0.6)
         &  (17.0, 0.6)
         &  (20.0, 0.0)
         ),
         17.0,
         Fuzzy_Linguistic_Rightmost_Max'Access,
         "RM"
      );
      Defuzzy
      (  (-1.0, 0.6) & (-1.0, 0.3) & (-1.0, 0.7) & (-1.0, 0.5),
         -1.0,
         Fuzzy_Linguistic_Rightmost_Max'Access,
         "RM"
      );
      --
      -- Leftmost maximum
      --
      Defuzzy
      (  (-1.0, 0.6) & (-1.0, 0.3) & (-1.0, 0.7) & (-1.0, 0.5),
         -1.0,
         Fuzzy_Linguistic_Leftmost_Max'Access,
         "LM"
      );
      Defuzzy
      (  (0.0, 0.0) & (1.0, 0.7) & (2.0, 0.0),
         1.0,
         Fuzzy_Linguistic_Leftmost_Max'Access,
         "LM"
      );
      Defuzzy
      (  (  ( 1.0, 0.0)
         &  ( 2.0, 0.1)
         &  ( 5.0, 0.1)
         &  ( 6.0, 0.2)
         &  ( 9.0, 0.2)
         &  (10.0, 0.7)
         &  (12.0, 0.7)
         &  (14.0, 0.5)
         &  (15.0, 0.6)
         &  (17.0, 0.6)
         &  (20.0, 0.0)
         ),
         10.0,
         Fuzzy_Linguistic_Leftmost_Max'Access,
         "LM"
      );
      --
      -- Discrete center of gravity
      --
      Defuzzy
      (  (  ( 1.0, 0.0)
         &  ( 1.0, 0.9)
         &  ( 1.0, 0.0)
         ),
         1.0,
         Fuzzy_Linguistic_Discrete_Center_Of_Gravity'Access,
         "DCoG"
      );
      Defuzzy
      (  (  ( 1.0, 0.0)
         &  ( 1.0, 0.9)
         &  ( 1.0, 0.0)
         &  ( 3.0, 0.0)
         &  ( 3.0, 0.9)
         &  ( 3.0, 0.0)
         ),
         2.0,
         Fuzzy_Linguistic_Discrete_Center_Of_Gravity'Access,
         "DCoG"
      );
   end;
   --
   -- Defuzzification test, contributed by Poul-Erik Andreasen
   --    (24 May 2006)
   --
   declare
      Set : constant Linguistic_Set :=
            Value ("low (1:0-> 2-> 4->10:0),high(4:0->10->12->13:0)");
      Low      : constant Classification := Classify (Set,  3.0);
      High     : constant Classification := Classify (Set, 11.0);
      Def_Low  : constant Defuzzification_Result :=
                          Defuzzify (Set, Low);
      Def_High : constant Defuzzification_Result :=
                          Defuzzify (Set, High);
   begin
      if (  Def_Low.Class /= Segment
         or else
            not Equal (Def_Low.Segment.From, 2.0)
         or else
            not Equal (Def_Low.Segment.To, 4.0)
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Defuzzify error: Def_Low /= [2,4]"
         );
      end if;
      if (  Def_High.Class /= Segment
         or else
            not Equal (Def_High.Segment.From, 10.0)
         or else
            not Equal (Def_High.Segment.To, 12.0)
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Defuzzify error: Def_Low /= [10,12]"
         );
      end if;
   end;

   declare
      use Fuzzy_Measure_Linguistic_Sets;
      function Create return Linguistic_Set is
         Result : Linguistic_Set;
      begin
         declare
            Triangle : Variable;
            From     : Float := -10.0;
            Step     : constant Float := 5.0;
         begin
            for Index in 1..4 loop
               Erase (Triangle);
               Append (Triangle, From,              Confidence'First);
               Append (Triangle, From + Step,       Confidence'Last );
               Append (Triangle, From + 2.0 * Step, Confidence'First);
               From := From + Step;
               Add
               (  Result,
                  "Triangle_" & Image (Index),
                  Triangle
               );
            end loop;
         end;
         return Result;
      end Create;
      Set_1 : Set_Measure;
      Set_2 : constant Set_Measure :=
                       (Units.Constants.Pressure, Create, 0.0);
   begin
      Set_1.Gain := Create;
      if Get_Use_Count (Set_1.Gain) /= 1 then
         Put_Line ("!!!A compiler bug in controlled types!!!");
         Put_Line ("Program_Error will propagate upon completion");
      end if;
      if Get_Use_Count (Set_2.Gain) /= 1 then
         Put_Line
         (  "!!!A compiler bug in initialization of controlled "
         &  "components of an aggregate!!!"
         );
         Put_Line ("Program_Error will propagate upon completion");
      end if;
   end;

   Put_Line ("... done");

end Test_Linguistic;
