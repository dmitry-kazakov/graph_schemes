--                                                                    --
--  procedure Test_Performance      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  11:56 13 Oct 2012  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Real_Time;          use Ada.Real_Time;
with Ada.Text_IO;            use Ada.Text_IO;
with Fuzzy_Linguistics;      use Fuzzy_Linguistics;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Fuzzy_Linguistic_Leftmost_Max;
with Fuzzy_Linguistic_Rightmost_Max;
with Fuzzy_Linguistic_Center_Of_Area;
with Fuzzy_Linguistic_Center_Of_Gravity;

procedure Test_Performance is

   Rounds : constant := 100_000;

   type Defuzzify_Method is
        access function (X : Variable) return Float;

   procedure Check
             (  Var    : Variable;
                Method : Defuzzify_Method;
                Name   : String
             )  is
      T1, T2, T3 : Time;
      X, Y       : Float;
   begin
      T1 := Clock;
      for Index in 1..Rounds loop
         X := Method (Var);
      end loop;
      T2 := Clock;
      for Index in 1..Rounds loop
         X := Method (Var);
         Y := Method (Var);
      end loop;
      T3 := Clock;
      Put_Line
      (  "Performance of "
      &  Name
      &  " "
      &  Image
         (  Value => (  Float (To_Duration ((T3 - T2) - (T2 - T1)))
                     /  Float (Rounds)
                     *  1.0E9
                     ),
            AbsSmall => -3
         )
      &  "ns ("
      &  Image (Rounds)
      &  " rounds)"
      );
   end Check;

begin
   Put_Line ("Testing performance of various defuzzifying methods ...");
   Check
   (  (  (0.0, 0.0)
      &  (1.0, 0.7)
      &  (2.0, 0.0)
      &  (2.0, 0.7)
      &  (3.0, 0.0)
      &  (4.0, 0.7)
      &  (4.0, 0.0)
      ),
      Fuzzy_Linguistic_Center_Of_Gravity'Access,
      "CoG"
   );
   Check
   (  (  (0.0, 0.0)
      &  (1.0, 0.7)
      &  (2.0, 0.0)
      &  (2.0, 0.7)
      &  (3.0, 0.0)
      &  (4.0, 0.7)
      &  (4.0, 0.0)
      ),
      Fuzzy_Linguistic_Center_Of_Area'Access,
      "CoA"
   );
   Check
   (  (  (0.0, 0.0)
      &  (1.0, 0.7)
      &  (2.0, 0.0)
      &  (2.0, 0.7)
      &  (3.0, 0.0)
      &  (4.0, 0.7)
      &  (4.0, 0.0)
      ),
      Fuzzy_Linguistic_Rightmost_Max'Access,
      "RM"
   );
   Check
   (  (  (0.0, 0.0)
      &  (1.0, 0.7)
      &  (2.0, 0.0)
      &  (2.0, 0.7)
      &  (3.0, 0.0)
      &  (4.0, 0.7)
      &  (4.0, 0.0)
      ),
      Fuzzy_Linguistic_Leftmost_Max'Access,
      "LM"
   );
   Put_Line ("... done");

end Test_Performance;
