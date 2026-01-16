--                                                                    --
--  package Parsers.FCL.Compiler    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2005       --
--                                                                    --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;

with Fuzzy.Lecture.General;
with Parsers.Generic_Source.Get_Token;

package body Parsers.FCL.Compiler is
   use Fuzzy.Feature.Domain_Floats.Measure_Edit;
   use Fuzzy.Feature.Handle.Container;
   use Rules_Lists;
   use Variables_Lists;
--
-- Keyword -- Ones of FCL
--
   type Keyword is
      (  And_Method,
         Accumulation_Method,
         Activation_Method,
         Or_Method,
         Asum_Method,
         Bdif_Method,
         Bsum_Method,
         COA_Method,
         COG_Method,
         COGS_Method,
         Default,
         End_Function_Block,
         End_Fuzzify,
         End_Ruleblock,
         End_Var,
         Defuzzify,
         Fuzzify,
         Function_Block,
         If_Predicate,
         LM_Method,
         Max_Method,
         Method,
         Min_Method,
         Prod_Method,
         RM_Method,
         Rule,
         Ruleblock,
         Fuzzify_Term,
         Then_Predicate,
         Var_Input,
         Var_Output
      );
   subtype Methods is Keyword range And_Method..Or_Method;
   package Keyword_Tables is new Tables (Keyword);
   package Keyword_Names is
      new Keyword_Tables.UTF8_Names (Blanks => Blank_Characters);
   use Keyword_Tables, Keyword_Names;

   procedure Get_Keyword is
      new Parsers.Multiline_Source.Code.Get_Token (Keyword_Tables);
--
-- The texts of FCL keywords
--
   And_Method_Text          : constant String := "and";
   Accumulation_Method_Text : constant String := "accu";
   Activation_Method_Text   : constant String := "act";
   Asum_Method_Text         : constant String := "asum";
   Bdif_Method_Text         : constant String := "bdif";
   Bsum_Method_Text         : constant String := "bsum";
   COA_Text                 : constant String := "coa";
   COG_Text                 : constant String := "cog";
   COGS_Text                : constant String := "cogs";
   Default_Text             : constant String := "default";
   Defuzzify_Text           : constant String := "defuzzify";
   End_Defuzzify_Text       : constant String := "end_defuzzify";
   End_Fuzzify_Text         : constant String := "end_fuzzify";
   End_Function_Block_Text  : constant String := "end_function_block";
   End_Ruleblock_Text       : constant String := "end_ruleblock";
   End_Var_Text             : constant String := "end_var";
   Function_Block_Text      : constant String := "function_block";
   Fuzzify_Term_Text        : constant String := "term";
   Fuzzify_Text             : constant String := "fuzzify";
   If_Predicate_Text        : constant String := "if";
   LM_Text                  : constant String := "lm";
   Max_Method_Text          : constant String := "max";
   Method_Text              : constant String := "method";
   Min_Method_Text          : constant String := "min";
   Or_Method_Text           : constant String := "or";
   Prod_Method_Text         : constant String := "prod";
   Range_Text               : constant String := "range";
   RM_Text                  : constant String := "rm";
   Rule_Text                : constant String := "rule";
   Ruleblock_Text           : constant String := "ruleblock";
   Then_Predicate_Text      : constant String := "then";
   Var_Input_Text           : constant String := "var_input";
   Var_Output_Text          : constant String := "var_output";

   package Types_Tables is new Tables (Variable_Type);
   package Types_Names is
      new Types_Tables.UTF8_Names (Blanks => Blank_Characters);
   use Types_Tables, Types_Names;

   procedure Get_Type is
      new Parsers.Multiline_Source.Code.Get_Token (Types_Tables);

   Types_Table : Types_Names.Dictionary;
--
-- Identifiers_Tables -- Tables of identifiers
--
   package Identifiers_Tables is
      new Tables (Parsers.Multiline_Source.Location);
   package Identifiers_Names is
      new Identifiers_Tables.UTF8_Names (Blanks => Blank_Characters);
   use Identifiers_Tables, Identifiers_Names;

   In_Function_Block   : Keyword_Names.Dictionary;
   In_Methods          : Keyword_Names.Dictionary;
   In_Rules            : Keyword_Names.Dictionary;
   In_Defuzzify_Method : Keyword_Names.Dictionary;

   function Same (Left, Right : String) return Boolean is
      J : Integer := Right'First;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if To_Lower (Left (I)) /= To_Lower (Right (J)) then
            return False;
         end if;
         J := J + 1;
      end loop;
      return True;
   end Same;

   generic
      with package Folder_Tables is new Tables (<>);
   function Table_Image (Folder : Folder_Tables.Table'Class)
      return String;

   function Table_Image (Folder : Folder_Tables.Table'Class)
      return String is
      use Folder_Tables;
      Size : constant Natural := GetSize (Folder);
      function Get_List (Index : Positive) return String is
      begin
         if Index > Size then
            return "";
         elsif Index = 1 then
            return
            (  "'"
            &  GetName (Folder, Index)
            &  "'"
            &  Get_List (2)
            );
         else
            return
            (  "|'"
            &  GetName (Folder, Index)
            &  "'"
            &  Get_List (Index + 1)
            );
         end if;
      end Get_List;
   begin
      return "{" &  Get_List (1) & "}";
   end Table_Image;

   function Image is new Table_Image (Keyword_Tables);
   function Image is new Table_Image (Types_Tables);

   procedure Created_Feature
             (  Compiler : in out Program;
                Feature  : in out Feature_Handle
             )  is
   begin
      null;
   end Created_Feature;

   function Create_Rules
            (  Compiler : Program;
               Name     : String
            )  return Lecture_Handle is
   begin
      return Fuzzy.Lecture.General.Create;
   end Create_Rules;

   function Get
            (  Code  : access Source'Class;
               Table : Keyword_Names.Dictionary
            )  return Keyword is
      Result : Keyword;
      Got_It : Boolean;
   begin
      Set_Pointer (Code.all, Get_Pointer (Code.all));
      Get_Keyword (Code.all, Table, Result, Got_It);
      if not Got_It then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  Image (Table)
            &  " is expected at "
            &  Image (Link (Code.all))
         )  );
      end if;
      return Result;
   end Get;

   procedure Get_Colon
             (  Parser : in out FCL_Expression;
                Code   : in out Parsers.Multiline_Source.Source'Class
             )  is
      Got_It : Boolean;
   begin
      Get_Blank (Parser, Code, Got_It);
      if not Get (Code'Access, ":") then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Colon ':' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Parser, Code, Got_It);
   end Get_Colon;

   procedure Get_Semicolon
             (  Parser : in out FCL_Expression;
                Code   : in out Parsers.Multiline_Source.Source'Class
             )  is
      Got_It : Boolean;
   begin
      Get_Blank (Parser, Code, Got_It);
      if not Get (Code'Access, ";") then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Semicolon ';' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Parser, Code, Got_It);
   end Get_Semicolon;

   procedure Get_Scale
             (  Parser : in out FCL_Expression;
                Code   : in out Parsers.Multiline_Source.Source'Class;
                Scale  : out Measure;
                Text   : out Unbounded_String
             )  is
      Stub    : Mark; -- Mark the tree stack
      Ptr     : Line_Ptr;
      Pointer : Integer;
      Last    : Integer;
      Got_It  : Boolean;
   begin
      if Get (Code'Access, "[") then
         Get_Blank (Parser, Code, Got_It);
         Get_Line (Code, Ptr, Pointer, Last);
         declare
            Line  : String renames Ptr (1..Last);
            Start : constant Integer := Pointer;
         begin
            Get (Line, Pointer, Scale, Parsing_Mode);
            Set_Pointer (Code, Pointer);
            Text := To_Unbounded_String (Line (Start..Pointer - 1));
            Get_Blank (Parser, Code, Got_It);
            if not Get (Code'Access, "]") then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Right bracket ']' is expected at "
                  &  Image (Link (Code))
               )  );
            end if;
         exception
            when End_Error =>
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Unit specification is expected at "
                  &  Image (Link (Code))
               )  );
            when Data_Error =>
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Illegal unit specification at "
                  &  Image (Link (Code))
               )  );
            when Constraint_Error =>
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Numeric error in unit specification at "
                  &  Image (Link (Code))
               )  );
         end;
      else
         Scale := Np;
         Text  := Null_Unbounded_String;
      end if;
   end Get_Scale;

   procedure Check
             (  Code : in out Parsers.Multiline_Source.Source'Class;
                Vars : Variables_Lists.Dictionary;
                Text : String
             )  is
   begin
      for Index in 1..GetSize (Vars) loop
         if not Is_Valid (GetTag (Vars, Index).Feature) then
            Reset_Pointer (Code);
            Raise_Exception
            (  Syntax_Error'Identity,
               (  Text
               &  " block for the variable '"
               &  GetName (Vars, Index)
               &  "' declared at "
               &  Image (GetTag (Vars, Index).Location)
               &  " must appear no later than at "
               &  Image (Link (Code))
            )  );
         end if;
      end loop;
   end Check;

   procedure Get_Variables
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                In_Vars  : in out Variables_Lists.Dictionary;
                Out_Vars : Variables_Lists.Dictionary
             )  is separate;

   procedure Get_Fuzzify
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                Vars     : in out Variables_Lists.Dictionary;
                Words    : Keyword_Names.Dictionary;
                Text     : String
             )  is separate;

   procedure Get_Rules
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                In_Vars  : Variables_Lists.Dictionary;
                Out_Vars : Variables_Lists.Dictionary
             )  is separate;

   In_Fuzzify   : Keyword_Names.Dictionary;
   In_Defuzzify : Keyword_Names.Dictionary;

   procedure Compile
             (  Compiler : in out Program'Class;
                Code     : in out Parsers.Multiline_Source.Source'Class
             )  is
      Parser : aliased FCL_Expression;
      Got_It : Boolean;
   begin
      Erase (Compiler.Input);
      Erase (Compiler.Output);
      Erase (Compiler.Rules);
      Get_Blank (Parser, Code, Got_It);
      if not Get (Code'Access, Function_Block_Text) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  Function_Block_Text
            &  " is expected at "
            &  Image (Link (Code))
         )  );
      end if;
      Get_Blank (Parser, Code, Got_It);
      Compiler.Name :=
         To_Unbounded_String (Get (Code'Access, "function block"));
      declare
         In_Vars  : Variables_Lists.Dictionary;
         Out_Vars : Variables_Lists.Dictionary;
      begin
         loop
            Get_Blank (Parser, Code, Got_It);
            case Get (Code'Access, In_Function_Block) is
               when Var_Input  =>
                  Get_Variables
                  (  Compiler,
                     Parser,
                     Code,
                     In_Vars,
                     Out_Vars
                  );
               when Var_Output =>
                  Get_Variables
                  (  Compiler,
                     Parser,
                     Code,
                     Out_Vars,
                     In_Vars
                  );
               when Fuzzify =>
                  Get_Fuzzify
                  (  Compiler,
                     Parser,
                     Code,
                     In_Vars,
                     In_Fuzzify,
                     "fuzzification"
                  );
               when Defuzzify =>
                  Get_Fuzzify
                  (  Compiler,
                     Parser,
                     Code,
                     Out_Vars,
                     In_Defuzzify,
                     "defuzzification"
                  );
               when Ruleblock =>
                  Check (Code, In_Vars,  "Fuzzification");
                  Check (Code, Out_Vars, "Defuzzification");
                  Get_Rules
                  (  Compiler,
                     Parser,
                     Code,
                     In_Vars,
                     Out_Vars
                  );
               when End_Function_Block =>
                  exit;
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     "Internal error at " & Image (Link (Code))
                  );
            end case;
         end loop;
         for Index in 1..GetSize (In_Vars) loop
            Add (Compiler.Input, GetTag (In_Vars, Index).Feature);
         end loop;
         for Index in 1..GetSize (Out_Vars) loop
            Add (Compiler.Output, GetTag (Out_Vars, Index).Feature);
         end loop;
      end;
   end Compile;

begin
   Add (In_Function_Block, End_Function_Block_Text, End_Function_Block);
   Add (In_Function_Block, Var_Input_Text,          Var_Input);
   Add (In_Function_Block, Var_Output_Text,         Var_Output);
   Add (In_Function_Block, Ruleblock_Text,          Ruleblock);
   Add (In_Function_Block, Fuzzify_Text,            Fuzzify);
   Add (In_Function_Block, Defuzzify_Text,          Defuzzify);

   Add (In_Fuzzify,   Fuzzify_Term_Text,    Fuzzify_Term);
   Add (In_Fuzzify,   End_Fuzzify_Text,     End_Fuzzify);

   Add (In_Defuzzify, Accumulation_Method_Text, Accumulation_Method);
   Add (In_Defuzzify, Default_Text,             Default);
   Add (In_Defuzzify, End_Defuzzify_Text,       End_Fuzzify);
   Add (In_Defuzzify, Fuzzify_Term_Text,        Fuzzify_Term);
   Add (In_Defuzzify, Method_Text,              Method);

   Add (In_Defuzzify_Method, COA_Text,  COA_Method);
   Add (In_Defuzzify_Method, COG_Text,  COG_Method);
   Add (In_Defuzzify_Method, COGS_Text, COGS_Method);
   Add (In_Defuzzify_Method, LM_Text,   LM_Method);
   Add (In_Defuzzify_Method, RM_Text,   RM_Method);

   Add (In_Rules, And_Method_Text,          And_Method);
   Add (In_Rules, Accumulation_Method_Text, Accumulation_Method);
   Add (In_Rules, Activation_Method_Text,   Activation_Method);
   Add (In_Rules, End_Ruleblock_Text,       End_Ruleblock);
   Add (In_Rules, If_Predicate_Text,        If_Predicate);
   Add (In_Rules, Or_Method_Text,           Or_Method);
   Add (In_Rules, Rule_Text,                Rule);

   Add (In_Methods, Asum_Method_Text, Asum_Method);
   Add (In_Methods, Bdif_Method_Text, Bdif_Method);
   Add (In_Methods, Bsum_Method_Text, Bsum_Method);
   Add (In_Methods, Max_Method_Text,  Max_Method);
   Add (In_Methods, Min_Method_Text,  Min_Method);
   Add (In_Methods, Prod_Method_Text, Prod_Method);

   Add (Types_Table, "nominal",  Discrete_Var);
   Add (Types_Table, "float",    Float_Var);
   Add (Types_Table, "integer",  Integer_Var);
   Add (Types_Table, "real",     Real_Var);

end Parsers.FCL.Compiler;
