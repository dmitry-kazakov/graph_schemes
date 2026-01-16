--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--        Get_Rules                                Summer, 2005       --
--  Separate body implementation                                      --
--                                Last revision :  11:45 29 May 2020  --
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
--
--  This  procedure  recognizes  a  rule  block  definition.  It has the
--  syntax:
--
--     <name>
--        rule <integer> : if <rule-specification>;
--        if <rule-specification>;
--        and : min;
--        act : min;
--        acc : max;
--        or  : max;
--
with Intervals;                      use Intervals;
with Parsers.FCL.Code;               use Parsers.FCL.Code;
with Parsers.FCL.Code.Predicates;    use Parsers.FCL.Code.Predicates;
with Units;                          use Units;

with Parsers.FCL.Code.Orders.Numerics.Integers;
use  Parsers.FCL.Code.Orders.Numerics.Integers;

with Units.Base;

separate (Parsers.FCL.Compiler)
   procedure Get_Rules
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                In_Vars  : Variables_Lists.Dictionary;
                Out_Vars : Variables_Lists.Dictionary
             )  is

   This : Rules_Block;

   procedure Store
             (  Example : Positive;
                Feature : Feature_Handle;
                Term    : Logical_Term
             )  is
      use Logical_Set_Handles;
   begin
      for Image in Term'Range loop
         if Is_Valid (Term (Image)) then
            Put
            (  Lesson  => This.Lesson,
               Example => Example,
               Feature => Feature,
               Image   => Image,
               Value   => Ptr (Term (Image)).Value
            );
         end if;
      end loop;
   end Store;

   procedure Store
             (  Condition  : Logical_Form;
                Conclusion : Logical_Form;
                No         : Natural
             )  is
      Example : Positive := Get_Examples_Number (This.Lesson) + 1;
   begin
      if No = 0 then
         Example := Get_Examples_Number (This.Lesson) + 1;
      else
         Example := No;
      end if;
      for I in 1..Get_Size (Condition) loop
         for J in 1..Get_Size (Conclusion) loop
            for No in 1..Get_Size (Condition, I) loop
               Store
               (  Example,
                  Get_Feature (Condition, I, No),
                  Get_Term    (Condition, I, No)
               );
            end loop;
            for No in 1..Get_Size (Conclusion, J) loop
               Store
               (  Example,
                  Get_Feature (Conclusion, J, No),
                  Get_Term    (Conclusion, J, No)
               );
            end loop;
            Example := Example + 1;
         end loop;
      end loop;
   end Store;

   procedure Get_Rule (No : Natural) is
      Stub   : Mark; -- Mark the tree stack
      Result : Argument_Token;
      First  : Boolean := True;
   begin
      Lexers.Parse (Parser, Code, Result);
      declare
         Conclusion : Logical_Form;
         Condition  : Logical_Form :=
            Get_Predicate (In_Vars, Result.Value.all);
      begin
         if not Get (Code'Access, Then_Predicate_Text) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "'"
               &  Then_Predicate_Text
               &  "' is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         Condition := Normalize (Condition);
         loop
            Lexers.Parse (Parser, Code, Result);
            if First then
               Conclusion :=
                  Get_Predicate (Out_Vars, Result.Value.all);
               First := False;
            else
               Conclusion :=
                  Logical_And
                  (  Link (Code),
                     Conclusion,
                     Get_Predicate (Out_Vars, Result.Value.all)
                  );
            end if;
            exit when not Get (Code'Access, ",");
         end loop;
         Store (Condition, Normalize (Conclusion), No);
      end;
   end Get_Rule;

   use Rules_Maps;
   Got_It       : Boolean;
   Numbered     : Logical := Uncertain;
   Rules_List   : Rules_Maps.Map;
   Item         : Keyword;
   Method_Given : array (Methods) of Boolean := (others => False);
   Method_At    : array (Methods) of Parsers.Multiline_Source.Location;
begin
   Get_Blank (Parser, Code, Got_It);
   declare
      Name : constant String := Get (Code'Access, "variable");
   begin
      This.Location := Link (Code);
      if IsIn (Compiler.Rules, Name) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Rules block specification once appeared at "
            &  Image (Method_At (Item))
            &  " is repeated at "
            &  Image (Link (Code))
         )  );
      end if;
      This.Lesson := Create_Rules (Compiler, Name);
      loop
         Get_Blank (Parser, Code, Got_It);
         Item := Get (Code'Access, In_Rules);
         if Item in Methods then
            if Method_Given (Item) then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Method specification once appeared at "
                  &  Image (Method_At (Item))
                  &  " is repeated at "
                  &  Image (Link (Code))
               )  );
            else
               Method_Given (Item) := True;
               Method_At    (Item) := Link (Code);
            end if;
         end if;
         case Item is
            when And_Method | Activation_Method =>
               Get_Colon (Parser, Code);
               if Min_Method /= Get (Code'Access, In_Methods) then
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Infeasible and-method (not '"
                     &  Min_Method_Text
                     &  "') found at "
                     &  Image (Link (Code))
                  )  );
               end if;
               Method_At (Item) := Method_At (Item) & Link (Code);
            when Accumulation_Method | Or_Method =>
               Get_Colon (Parser, Code);
               if Max_Method /= Get (Code'Access, In_Methods) then
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Infeasible or-method (not '"
                     &  Max_Method_Text
                     &  "') found at "
                     &  Image (Link (Code))
                  )  );
               end if;
               Method_At (Item) := Method_At (Item) & Link (Code);
            when Rule =>
               case Numbered is
                  when False =>
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Non-numbered rules block has a rule "
                        &  "with a number at "
                        &  Image (Link (Code))
                     )  );
                  when True =>
                     null;
                  when Uncertain =>
                     Numbered := True;
               end case;
               declare
                  Stub   : Mark; -- Mark the tree stack
                  Result : Argument_Token;
               begin
                  Lexers.Parse (Parser, Code, Result);
                  declare
                     No : Int'Class renames
                             Get_Integer (Result.Value.all);
                  begin
                     Get_Colon (Parser, Code);
                     if not Get (Code'Access, If_Predicate_Text) then
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "'"
                           &  If_Predicate_Text
                           &  "' is expected at "
                           &  Image (Link (Code))
                        )  );
                     end if;
                     if No.Value < 1 then
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "The rule number is not positive at "
                           &  Image (No.Location)
                        )  );
                     end if;
                     if Is_In (Rules_List, Rule_Number (No.Value)) then
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "The rule"
                           &  Image (No, ASCII_Set)
                           &  " at "
                           &  Image
                              (  Get
                                 (  Rules_List,
                                    Rule_Number (No.Value)
                              )  )
                           &  " repeappears at "
                           &  Image (No.Location)
                        )  );
                     end if;
                     Add
                     (  Rules_List,
                        Rule_Number (No.Value),
                        No.Location
                     );
                     Get_Rule (Positive (No.Value));
                  end;
               end;
            when If_Predicate =>
               case Numbered is
                  when False =>
                     null;
                  when True =>
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Numbered rules block has a rule without "
                        &  "a number at "
                        &  Image (Link (Code))
                     )  );
                  when Uncertain =>
                     Numbered := False;
               end case;
               Get_Rule (0);
            when End_Ruleblock =>
               This.Location := This.Location & Link (Code);
               exit;
            when others =>
               null;
         end case;
         Get_Semicolon (Parser, Code);
      end loop;
      for Index in 1..Get_Size (Rules_List) loop
         if Index /= Integer (Get_Key (Rules_List, Index)) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Missing rule"
               &  Integer'Image (Index)
               &  " in the rule block at "
               &  Image (This.Location)
            )  );
         end if;
      end loop;
      Add (Compiler.Rules, Name, This);
   end;
end Get_Rules;
