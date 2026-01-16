--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Binary              Luebeck            --
--  Separate body implementation                   Spring, 2002       --
--                                                                    --
--                                Last revision :  18:58 25 Jul 2018  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--
--
--  This is a small test procedure for the derived binary features.
--
with Ada.Exceptions;                 use Ada.Exceptions;
with Confidence_Factors;             use Confidence_Factors;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Fuzzy;                          use Fuzzy;
with Fuzzy.Edit;                     use Fuzzy.Edit;
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Feature.Context;          use Fuzzy.Feature.Context;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;      use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;   use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Intuitionistic;           use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

with Fuzzy.Lecture.General;

separate (Test_Graph_Schemes) procedure Test_Binary is
   Lesson  : Lecture_Handle := Fuzzy.Lecture.General.Create;
   Context : aliased Lecture_Context (Ptr (Lesson));
   X : constant Feature_Handle :=
          Create_Discrete
          (  "x",
             "Red, Green, Blue, Yellow, Pink, Cyan, Brown, Black, White"
          );
   Xi   : constant Bounded_Array := Create_Binary (X);
   Bits : constant      := 4;
   type Distribution is mod 2**Bits;

   procedure Check (Example : Positive; False, True : Distribution) is
      function Expected (Result : Distribution; Order : Integer)
         return Confidence is
      begin
         if 0 /= (Result and 2**(Bits - Order)) then
            return Confidence'Last;
         else
            return Confidence'First;
         end if;
      end Expected;

      procedure Check_Level (Order : Positive) is
         X : Confidence;
         D : Fuzzy.Set (1..2);
      begin
         D :=
            Get (Context'Access, Example, Get (Xi, Order).all, Has_In);
         for Index in D'Range loop
            declare
               Snap : Context_Snap;
            begin
               X :=
                  Get
                  (  Context'Access,
                     Example,
                     Get (Xi, Order).all,
                     Has_In,
                     Index
                  );
               if D (Index) /= X then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Inconsistency in"
                     &  Integer'Image (Example)
                     &  " "
                     &  Get_Name (Ref (Xi, Order))
                     &  " at"
                     &  Integer'Image (Index)
                     &  " got "
                     &  Image (X)
                     &  " expected: "
                     &  Image (D (Index))
                  )  );
               end if;
               if (  (Index = 1 and D (Index) /= Expected (False, Order))
                  or (Index = 2 and D (Index) /= Expected (True,  Order))
                  )
               then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Error in"
                     &  Integer'Image (Example)
                     &  " "
                     &  Get_Name (Ref (Xi, Order))
                     &  " at"
                     &  Integer'Image (Index)
                     &  " expected: ("
                     &  Boolean'Image
                           (Expected (False, Order) = Confidence'Last)
                     &  ","
                     &  Boolean'Image
                           (Expected (True, Order) = Confidence'Last)
                     &  ") got "
                     &  Image (D)
                  )  );
               end if;
               if Order < Bits and then D (Index) /= Confidence'First then
                  Create_Constraint (Ref (Xi, Order), Context);
                  Set_Constraint (Ref (Xi, Order), Context, 3 - Index);
                  Check_Level (Order + 1);
               end if;
            end;
         end loop;
      end Check_Level;
   begin
      Check_Level (1);
   end Check;
begin
   --
   --                X  X1 X2 X3 X4
   -- Example /      -  -----------
   --             1  1   0  0  0  0
   --             2  2   0  0  0  1
   --             3  3   0  0  1  0
   --             4  4   0  0  1  1
   --             5  5   0  1  0  0
   --             6  6   0  1  0  1
   --             7  7   0  1  1  0
   --             8  8   0  1  1  1
   --             9  9   1  0  0  0
   --
   for Index in 1..9 loop
      Put (Lesson, Index, X, Has_In, Index);
   end loop;
   Check (1, 2#1111#, 2#0000#);
   Check (2, 2#1110#, 2#0001#);
   Check (3, 2#1101#, 2#0010#);
   Check (4, 2#1100#, 2#0011#);
   Check (5, 2#1011#, 2#0100#);
   Check (6, 2#1010#, 2#0101#);
   Check (7, 2#1001#, 2#0110#);
   Check (8, 2#1000#, 2#0111#);
   Check (9, 2#0111#, 2#1000#);
   declare
      X    : constant Feature_Handle := Create_Integer ("X", 1, 7);
      Xi   : constant Bounded_Array  := Create_Binary (X);
      L    : Lecture_Handle := Fuzzy.Lecture.General.Create;
      C    : aliased Lecture_Context (Ptr (L));
      R    : Classification (2);
      Snap : Context_Snap;
   begin
      Put (L, 1, X, Has_In,  Value ("1,2,7", X));
      Put (L, 1, X, Has_Out, Value ("1,2,7", X));
      R.Possibility := Get (C'Access, 1, Get (Xi, 1).all, Has_In);
      R.Necessity   := not Get (C'Access, 1, Get (Xi, 1).all, Has_Out);
      if (  R.Possibility /= Fuzzy.Set'(1..2 => Confidence'Last)
         or else
            R.Necessity /= Fuzzy.Set'(1..2 => Confidence'First)
         )
      then
         Raise_Exception
         (  Failed'Identity,
            "Binary feature error in Get (classification, unconstrained)"
         );
      end if;
      Set_Range (Ref (Xi, 1), C, 1, 1);
      R.Possibility := Get (C'Access, 1, Get (Xi, 1).all, Has_In);
      R.Necessity   := not Get (C'Access, 1, Get (Xi, 1).all, Has_Out);
      if (  R.Possibility /= Fuzzy.Set'(1..2 => Confidence'Last)
         or else
            R.Necessity /= Fuzzy.Set'(1..2 => Confidence'First)
         )
      then
         Raise_Exception
         (  Failed'Identity,
            "Binary feature error in Get (classification, constrained)"
         );
      end if;
      R.Possibility := Get (C'Access, 1, Get (Xi, 2).all, Has_In);
      R.Necessity   := not Get (C'Access, 1, Get (Xi, 2).all, Has_Out);
      if (  (  R.Possibility
            /= Fuzzy.Set'(1..2 => Confidence'Last)
            )
         or else
            (  R.Necessity
            /= Fuzzy.Set'(1..2 => Confidence'First)
         )  )
      then
         Raise_Exception
         (  Failed'Identity,
            "Binary feature error in Get (classification, 2nd)"
         );
      end if;
      R.Possibility := Get (C'Access, 1, Get (Xi, 3).all, Has_In);
      R.Necessity   := not Get (C'Access, 1, Get (Xi, 3).all, Has_Out);
      if (  (  R.Possibility
            /= Fuzzy.Set'(1..2 => Confidence'Last)
            )
         or else
            (  R.Necessity
            /= Fuzzy.Set'(1..2 => Confidence'First)
         )  )
      then
         Raise_Exception
         (  Failed'Identity,
            "Binary feature error in Get (classification, 2nd)"
         );
      end if;
      Set_Range (Ref (Xi, 2), C, 1, 1);
      R.Possibility := Get (C'Access, 1, Get (Xi, 3).all, Has_In);
      R.Necessity   := not Get (C'Access, 1, Get (Xi, 3).all, Has_Out);
      if (  (  R.Possibility
            /= Fuzzy.Set'(1..2 => Confidence'Last)
            )
         or else
            (  R.Necessity
            /= Fuzzy.Set'(1..2 => Confidence'First)
         )  )
      then
         Raise_Exception
         (  Failed'Identity,
            "Binary feature error in Get (classification, 2nd)"
         );
      end if;
   end;
end Test_Binary;
