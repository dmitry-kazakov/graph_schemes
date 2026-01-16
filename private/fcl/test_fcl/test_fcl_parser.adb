--                                                                    --
--  procedure Test_FCL_Parser       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2005       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;
with Fuzzy.Feature.Domain_Floats;   use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Parsers.FCL;                   use Parsers.FCL;
with Parsers.FCL.Code;              use Parsers.FCL.Code;
with Parsers.FCL.Code.Predicates;   use Parsers.FCL.Code.Predicates;
with Parsers.Multiline_Source;      use Parsers.Multiline_Source;

with Parsers.Multiline_Source.Text_IO;
with Units;

procedure Test_FCL_Parser is
   use Lexers;
   use Tokens;
   use Variables_Lists;

   function Equal (Left, Right : Term'Class) return Boolean is
   begin
      if Left in Logical_Form and then Right in Logical_Form then
         declare
            X : Logical_Form'Class renames Logical_Form'Class (Left);
            Y : Logical_Form'Class renames Logical_Form'Class (Right);
            Got : array (1..Get_Size (X)) of Boolean :=
                     (others => False);
         begin
            if Get_Size (X) /= Get_Size (Y) then
               return False;
            end if;
            for I in Got'Range loop
               for J in Got'Range loop
                  if (  not Got (J)
                     and then Get_Size (Y, J) = Get_Size (X, I)
                     )
                  then
                     Got (J) := True;
                     for K in 1..Get_Size (X, I) loop
                        if (  (  Get_Feature (X, I, K)
                              /= Get_Feature (Y, J, K)
                              )
                           or else
                              (  Get_Term (X, I, K)
                              /= Get_Term (Y, J, K)
                              )
                           or else
                              (  Get_Term (X, I, K)
                              /= Get_Term (Y, J, K)
                           )  )
                        then
                           Got (J) := False;
                           exit;
                        end if;
                     end loop;
                  end if;
               end loop;
            end loop;
            for I in Got'Range loop
               if not Got (I) then
                  return False;
               end if;
            end loop;
            return True;
         end;
      end if;
      return False;
   end Equal;

   File     : aliased File_Type;
   Parser   : aliased FCL_Expression;
   Result   : Argument_Token;
   Expected : Argument_Token;
   List     : Dictionary;
   Nowhere  : Parsers.Multiline_Source.Location := ((1,1),(1,1));
begin
   Add
   (  List,
      "temp",
      (  Discrete_Var,
         Create_Discrete ("Temp", "cold, warm, hot"),
         Nowhere
   )  );
   Add
   (  List,
      "x",
      (  Discrete_Var,
         Create_Discrete ("X", "a, b, c"),
         Nowhere
   )  );
   Add
   (  List,
      "y",
      (  Discrete_Var,
         Create_Discrete ("Y", "e, f, g"),
         Nowhere
   )  );
   Add
   (  List,
      "z",
      (  Discrete_Var,
         Create_Discrete ("Z", "h, i, j, k"),
         Nowhere
   )  );
   Add
   (  List,
      "i",
      (  Integer_Var,
         Create_Integer ("I", 1, 20),
         Nowhere
   )  );
   Add
   (  List,
      "T",
      (  Float_Var,
         Create_Float ("T", 20, -10.0, 10.0, "°C", Units.Latin1_Set),
         Nowhere
   )  );
   Add
   (  List,
      "pressure",
      (  Real_Var,
         Create_Linguistic
         (  "Pressure",
            (  "low    (->5->20:0->),"
            &  "middle (->10:0->15->30->35:0),"
            &  "high   (->20:0->40->)"
            ),
            "Pa"
         ),
         Nowhere,
         To_Unbounded_String ("Pa"),
         Nowhere
   )  );
   Open (File, In_File, "test_fcl_parser.fcl");
   declare
      Code : aliased Parsers.Multiline_Source.Text_IO.Source
                       (File'Access);
   begin
      loop
         declare
            Stub : Mark; -- Mark the tree stack
         begin
            Parse (Parser, Code, Result);
            declare
               Value : Term'Class :=
                          Get_Predicate (List, Result.Value.all);
            begin
               if Get (Code'Access, "equals") then
                  Parse (Parser, Code, Expected);
                  declare
                     Other : Term'Class :=
                        Get_Predicate (List, Expected.Value.all);
                  begin
                     if not Equal (Value, Other) then
                        Put_Line
                        (  Image (Result.Location)
                        &  ": "
                        &  Image (Result.Value.all, Units.Latin1_Set)
                        &  " ends at "
                        &  Image (Link (Code))
                        );
                        Put_Line
                        (  "Got:"
                        &  Image (Value, Units.Latin1_Set)
                        );
                        Put_Line
                        (  "Expected:"
                        &  Image (Other, Units.Latin1_Set)
                        );
                        Put_Line
                        (  Image (Result.Location)
                        &  ": "
                        &  Image (Result.Value.all, Units.Latin1_Set)
                        );
                        Put_Line
                        (  Image (Expected.Location)
                        &  ": "
                        &  Image (Expected.Value.all, Units.Latin1_Set)
                        );
                        return;
                     end if;
                  end;
               else
                  Put_Line
                  (  Image (Result.Location)
                  &  ": "
                  &  Image (Result.Value.all, Units.Latin1_Set)
                  &  " ends at "
                  &  Image (Link (Code))
                  );
                  Put_Line ("Got:" & Image (Value, Units.Latin1_Set));
               end if;
            end;
         exception
            when Error : Parsers.Syntax_Error =>
               if Get (Code'Access, "Then error") then
                  Put_Line
                  (  "   Rightly reported: "
                  &  Exception_Message (Error)
                  );
               else
                  if Result.Value /= null then
                     Put_Line
                     (  Image (Result.Location)
                     &  ": "
                     &  Image (Result.Value.all, Units.Latin1_Set)
                     &  " ends at "
                     &  Image (Link (Code))
                     );
                  else
                     Put_Line
                     (" Expression ends at " & Image (Link (Code))
                     );
                  end if;
                  Put ("Error: ");
                  Put_Line (Exception_Message (Error));
               end if;
         end;
         Next_Line (Code);
      end loop;
   exception
      when End_Error =>
         Put_Line ("----Normal completion----");
      when Error : others =>
         Put ("Error at " & Image (Link (Code)) & " ");
         Put_Line (Exception_Information (Error));
   end;
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_FCL_Parser;
