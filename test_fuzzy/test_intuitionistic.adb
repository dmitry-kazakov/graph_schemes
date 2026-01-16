--                                                                    --
--  package Test_Intuitionistic     Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Confidence_Factors;         use Confidence_Factors;
with Confidence_Factors.Edit;    use Confidence_Factors.Edit;
with Fuzzy;                      use Fuzzy;
with Fuzzy.Edit;                 use Fuzzy.Edit;
with Fuzzy.Edit.Intuitionistic;  use Fuzzy.Edit.Intuitionistic;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;

procedure Test_Intuitionistic is
   Half : constant Confidence := Value ("0.5");

   function Image (X : Fuzzy.Intuitionistic.Set) return String is
   begin
      return "(" & Image (X, Uncertain) & ")";
   end Image;

   function Image (X : Fuzzy_Boolean) return String is
   begin
      if X = Certain_False then
         return "false";
      elsif X = Certain_True then
         return "true";
      elsif X = Uncertain then
         return "uncertain";
      elsif X = Contradictory then
         return "contradictory";
      else
         return
         (  "("  & Image (X.Possibility)
         &  ", " & Image (X.Necessity)
         &  ")"
         );
      end if;
   end Image;
   
   generic
      type Left (<>) is private;
      type Right (<>) is private;
      with function "not" (A : Left)  return Left is <>;
      with function "not" (A : Right) return Right is <>;
      with function Is_In (A : Left; B : Right)
         return Fuzzy_Boolean is <>;
      with function "<=" (A : Left; B : Right)
         return Fuzzy_Boolean is <>;
      with function "<=" (A : Right; B : Left)
         return Fuzzy_Boolean is <>;
      with function ">=" (A : Left; B : Right)
         return Fuzzy_Boolean is <>;
      with function ">=" (A : Right; B : Left)
         return Fuzzy_Boolean is <>;
      with function "=" (A : Left; B : Right)
         return Fuzzy_Boolean is <>;
      with function "/=" (A : Left; B : Right)
         return Fuzzy_Boolean is <>;
      with function Image (A : Left) return String is <>;
      with function Image (A : Right) return String is <>;
   procedure Check_Comparison
             (  X               : Left;
                Y               : Right;
                Sub, LE, GE, EQ : Fuzzy_Boolean
             );

   procedure Check_Comparison
             (  X               : Left;
                Y               : Right;
                Sub, LE, GE, EQ : Fuzzy_Boolean
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
               (  "Comparison "
               &  Image (X)
               &  " "
               &  Name
               &  " "
               &  Image (Y)
               &  " is "
               &  Image (Result)
               &  ", expected "
               &  Image (Expected)
            )  );
         end if;
      end Compare;
   begin
      Compare (Is_In (X, Y), Sub, "in");
      Compare (Is_In (X, not Y), not Sub, "in not");
      Compare (X <= Y, LE, "<=");
      Compare ((not Y) <= (not X), LE, ">=");
      Compare (X >= Y, GE, ">=");
      Compare ((not Y) >= (not X), GE, "<=");
      Compare (X = Y, EQ, "=");
      Compare (X /= Y, not X = Y, "/=");
   end Check_Comparison;

   procedure Check is new
      Check_Comparison
      (  Fuzzy.Intuitionistic.Set,
         Fuzzy.Intuitionistic.Set
      );
begin
   Put_Line ("Testing intuitionistic sets...");
   begin
      declare
         X : constant Fuzzy.Intuitionistic.Set := Value ("", 2);
         Y : Fuzzy.Intuitionistic.Set (2);
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
   Check
   (  Value ("2..3", 4, Certain_True),
      Value ("2..3", 4, Certain_True),
      Sub => Certain_True,
      LE  => Certain_True,
      GE  => Certain_True,
      EQ  => Certain_True
   );
   Check
   (  Value ("2",    4, Certain_True),
      Value ("1..3", 4, Certain_True),
      Sub => Certain_True,
      LE  => Certain_True,
      GE  => Uncertain,
      EQ  => Uncertain
   );
   Check
   (  Value ("1..2", 4, Certain_True),
      Value ("3..4", 4, Certain_True),
      Sub => Certain_False,
      LE  => Certain_False,
      GE  => Certain_False,
      EQ  => Certain_False
   );
   Check
   (  Value ("1..2", 4, Certain_True),
      Value ("2..4", 4, Certain_True),
      Sub => Uncertain,
      LE  => Certain_False,
      GE  => Certain_False,
      EQ  => Certain_False
   );
   Check
   (  Value ("2:0.5:0,3,4:0.5:false", 4, Certain_True),
      Value ("2..4",                  4, Certain_True),
      Sub => Certain_True,
      LE  => Certain_True,
      GE  => Uncertain,
      EQ  => Uncertain
   );
   Check
   (  Value ("2:0.5:0,3,4:0.5:0",    4, Certain_True),
      Value ("3:0.5:0.0,4:true:0.5", 4, Certain_True),
      Sub => (Half, Confidence'First),
      LE  => (Half, Confidence'First),
      GE  => (Half, Confidence'First),
      EQ  => (Half, Confidence'First)
   );
   Check
   (  Value ("2:0.5:0, 3, 4:0.5:0", 4, Certain_True),
      Value ("3:0.5,      4",       4, Certain_True),
      Sub => (Half, not Half),
      LE  => (Half, not Half),
      GE  => (Half, Confidence'First),
      EQ  => (Half, Confidence'First)
   );
   Put_Line ("... Done");
exception
  when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      raise Program_Error;
end Test_Intuitionistic;
