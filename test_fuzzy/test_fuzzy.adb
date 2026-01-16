--                                                                    --
--  procedure Test_Fuzzy            Copyright (c)  Dmitry A. Kazakov  --
--  Test_Fuzzy                                     Luebeck            --
--                                                 Winter, 2000       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams;                use Ada.Streams;
with Ada.Text_IO;                use Ada.Text_IO;
with Confidence_Factors;         use Confidence_Factors;
with Confidence_Factors.Edit;    use Confidence_Factors.Edit;
with Fuzzy;                      use Fuzzy;
with Fuzzy.Abstract_Edit;        use Fuzzy.Abstract_Edit;
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Edit;                 use Fuzzy.Edit;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Fuzzy.Stream_IO;            use Fuzzy.Stream_IO;
with Strings_Edit;               use Strings_Edit;
with Strings_Edit.Integers;
with Test_Fuzzy_Floats;
with Test_Fuzzy_Integers;
with Test_Intuitionistic;
with Test_Linguistic;
with Test_Performance;

procedure Test_Fuzzy is
   subtype Fuzzy_Set is Set (1..10);
   Done : exception;
   type Logical is (F, T, U, C);

   function "+" (Value : Logical) return Fuzzy_Boolean is
   begin
      case Value is
         when F => return Certain_False;
         when T => return Certain_True;
         when U => return Uncertain;
         when C => return Contradictory;
      end case;
   end "+";

   procedure Test_Logic
             (  X       : Logical;
                Y       : Logical;
                X_and_Y : Logical;
                X_or_Y  : Logical;
                X_xor_Y : Logical;
                X_mul_Y : Logical;
                X_sum_Y : Logical;
                X_imp_Y : Logical;
                Not_X   : Logical
             )  is
   begin
      if ((+X) and (+Y)) /= (+X_and_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " and "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_and_Y)
         )  );
      end if;
      if ((+X) or (+Y)) /= (+X_or_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " or "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_or_Y)
         )  );
      end if;
      if ((+X) xor (+Y)) /= (+X_xor_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " xor "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_xor_Y)
         )  );
      end if;
      if ((+X) * (+Y)) /= (+X_mul_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " * "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_mul_Y)
         )  );
      end if;
      if ((+X) + (+Y)) /= (+X_sum_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " + "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_sum_Y)
         )  );
      end if;
      if ((+X) >= (+Y)) /= (+X_imp_Y) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  Logical'Image (X)
            &  " => "
            &  Logical'Image (Y)
            &  " /= "
            &  Logical'Image (X_imp_Y)
         )  );
      end if;
      if (not (+X)) /= (+Not_X) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "not "
            &  Logical'Image (X)
            &  " /= "
            &  Logical'Image (Not_X)
         )  );
      end if;
   end Test_Logic;

   procedure Get (A : in out Fuzzy_Set; Name : String) is
      Text : String (1..80);
      Last : Natural;
   begin
      Put ("Give a fuzzy set " & Name & " over ");
      Put (Trim (Integer'Image (Fuzzy_Set'First)));
      Put ("..");
      Put (Trim (Integer'Image (Fuzzy_Set'Last)));
      Put (": ");
      Get_Line (Text, Last);
      if Last = 0 then
         raise Done;
      end if;
      A := Value (Text (1..Last), Fuzzy_Set'First, Fuzzy_Set'Last);
   end Get;

   function Image (X : Fuzzy_Boolean) return String is
   begin
      if X = Certain_True then
         return "True";
      elsif X = Certain_False then
         return "False";
      elsif X = Contradictory then
         return "Contradictory";
      elsif X = Uncertain then
         return "Uncertain";
      else
         return Image (X.Possibility) & ": " & Image (X.Necessity);
      end if;
   end Image;

   A : Fuzzy_Set;
   B : Fuzzy_Set;
begin
   Test_Performance;
   Test_Linguistic;
   Test_Intuitionistic;
   Test_Fuzzy_Integers;
   Test_Fuzzy_Floats;
   --          X Y  and xor   =>
   --                 or  * +   not
   Test_Logic (F,F, F,F,F,F,F,T,T);
   Test_Logic (F,T, F,T,T,C,U,T,T);
   Test_Logic (F,U, F,U,U,F,U,T,T);
   Test_Logic (F,C, F,C,C,C,F,T,T);
   Test_Logic (T,F, F,T,T,C,U,F,F);
   Test_Logic (T,T, T,T,F,T,T,T,F);
   Test_Logic (T,U, U,T,U,T,U,U,F);
   Test_Logic (T,C, C,T,C,C,T,C,F);
   Test_Logic (U,F, F,U,U,F,U,C,U);
   Test_Logic (U,T, U,T,U,T,U,T,U);
   Test_Logic (U,U, U,U,U,U,U,T,U);
   Test_Logic (U,C, F,T,F,C,U,C,U);
   Test_Logic (C,F, F,C,C,C,F,U,C);
   Test_Logic (C,T, C,T,C,C,T,T,C);
   Test_Logic (C,U, F,T,F,C,U,U,C);
   Test_Logic (C,C, C,C,C,C,C,T,C);

   declare
      Color : Domain_Description;
      X     : constant Set (1..3) :=
                 (Confidence'First, Confidence'Last, Confidence'First);
   begin
      Add (Color, "Red",   -3);
      Add (Color, "Green", -2);
      Add (Color, "Blue",  -1);
      if Image (Color, Value ("Red..Blue", Color)) /= "Red..Blue" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Named_Edit"
         );
      end if;
      if Image (Color, X) /= "Green" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Named_Edit"
         );
      end if;
      if (  Value ("Green : 0.5", Color)
         /= Set'
            ( -3 => Confidence'First,
              -2 => 0.5,
              -1 => Confidence'First
         )  )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Named_Edit"
         );
      end if;
   end;

   Put_Line ("Confidence'Size: " & Integers.Image (Confidence'Size));

   begin
      declare
         X : constant Set := Value ("", 2, 10);
         Y : Set (2..10);
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
      X       : Set (1..10);
   begin
      Get (Text, Pointer, X);
      if Pointer /= Text'First + 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;

   declare
      Level : Confidence := Confidence'First;
   begin
      loop
         declare
            Data   : constant Set (1..10) := (others => Level);
            Data_1 : constant Stream_Element_Array := Image (Data);
            Data_2 : constant Set := Value (Data_1);
         begin
            if Data /= Data_2 then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Error in stream I/O for "
                  &  Image (Level)
                  &  "/="
                  &  Image (Data_2 (1))
                  &  Integer'Image (Stream_Element'Pos (Data_1 (1)))
               )  );
            end if;
         end;
         exit when Level = Confidence'Last;
         Level := Confidence'Succ (Level);
      end loop;
   end;

   declare
      Text    : constant String := "2 :, something";
      Pointer : Integer := Text'First;
      X       : Set (1..10);
   begin
      Get (Text, Pointer, X);
      if Pointer /= Text'First + 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Get"
         );
      end if;
   end;

   loop
      Get (A, "A");
      Put_Line ("A = " & Image (A));
      Get (B, "B");
      Put_Line ("B = " & Image (B));
      Put_Line ("not A = " & Image (not A));
      Put_Line ("A and B = " & Image (A and B));
      Put_Line ("A or B = " & Image (A or B));
      Put_Line ("A xor B = " & Image (A xor B));
      Put_Line ("A + B = " & Image (A + B));
      Put_Line ("A * B = " & Image (A * B));
      Put_Line ("A mod B = " & Image (A mod B));
      Put_Line ("A rem B = " & Image (A rem B));
      Put_Line ("P(A|B) = " & Image (Possibility (A, B)));
      Put_Line ("N(A|B) = " & Image (Necessity (A, B)));
      Put_Line ("A in B = " & Image (Is_In (A, B)));
      if (  Is_In (A, B)
         /= (  Possibility => Possibility (B, A),
               Necessity   => Necessity   (B, A)
         )  )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in Is_In"
         );
      end if;
   end loop;

exception
   when Done =>
      null;
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_Fuzzy;

