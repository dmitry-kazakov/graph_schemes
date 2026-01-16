--                                                                    --
--  package Parsers.FCL             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2005       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;
with Units;                    use Units;

with Strings_Edit.UTF8.Categorization;
with Parsers.Generic_Source.Get_UTF8_Text;
with Strings_Edit.UTF8.Maps.Constants;

package body Parsers.FCL is
   use Fuzzy.Feature.Domain_Floats.Measure_Edit;
   use Strings_Edit.UTF8.Categorization;

   procedure Get_UTF8_Text is
      new Parsers.Multiline_Source.Code.Get_UTF8_Text;

   function To_UTF8 (Value : UTF8_Code_Point)
      return String renames Strings_Edit.UTF8.Image;

   function "and" (Left, Right : Operations) return Boolean is
   begin
      case Right is
         when Logical_And | Lattice_And =>
            case Left is
               when Logical_Or | Lattice_Xor | Lattice_Or =>
                  return False;
               when others =>
                  return True;
            end case;
         when Logical_Or | Lattice_Or =>
            case Left is
               when Logical_And | Lattice_Xor | Lattice_And =>
                  return False;
               when others =>
                  return True;
            end case;
         when Lattice_Xor =>
            case Left is
               when Logical_And | Logical_Or |
                    Lattice_And | Lattice_Or =>
                  return False;
               when others =>
                  return True;
            end case;
         when Unary =>
            case Left is
               when Additive  | Unary | Multiplying | Pow |
                    Abs_Value | Postfix_Pow =>
                  return False;
               when others =>
                  return True;
            end case;
         when Abs_Value =>
            case Left is
               when Pow | Abs_Value =>
                  return False;
               when others =>
                  return True;
            end case;
         when Pow =>
            case Left is
               when Pow =>
                  return False;
               when others =>
                  return True;
            end case;
         when Postfix_Pow =>
            case Left is
               when Postfix_Pow =>
                  return False;
               when others =>
                  return True;
            end case;
         when Postfix =>
            case Left is
               when Postfix =>
                  return False;
               when others =>
                  return True;
            end case;
         when Right_Bracket =>
            case Left is
               when Left_Interval | Left_Dimension =>
                  return False;
               when others =>
                  return True;
            end case;
         when Right_Interval =>
            case Left is
               when Left_Bracket | Left_Index =>
                  return False;
               when others =>
                  return True;
            end case;
         when Colon =>
            case Left is
               when Left_Interval | Left_Dimension =>
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
            return True;
      end case;
   end "and";

   function Get
            (  Code : access Source'Class;
               Text : String
            )  return Boolean is
      Got_It : Boolean;
   begin
      Get_UTF8_Text
      (  Code.all,
         Text,
         Got_It,
         Strings_Edit.UTF8.Maps.Constants.Lower_Case_Map
      );
      return Got_It;
   end Get;

   procedure Get_Identifier_Body
             (  Code      : in out Source'Class;
                Line      : String;
                Pointer   : in out Integer;
                Malformed : out Boolean
             )  is
      Underline : Boolean := False;
      Index     : Integer;
      Symbol    : Code_Point;
   begin
      Malformed := False;
      while Pointer <= Line'Last loop
         Index := Pointer;
         Get (Line, Index, Symbol);
         exit when
            not
            (  Is_Identifier_Body (Symbol)
            or else
               Is_Unit_Symbol (Symbol)
            );
         if Category (Symbol) = Pc then
            Malformed := Malformed or Underline;
            Underline := True;
         else
            Underline := False;
         end if;
         Pointer := Index;
      end loop;
      Malformed := Malformed or Underline;
   exception
      when Data_Error =>
         Set_Pointer (Code, Pointer);
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Invalid UTF-8 code at " & Image (Link (Code))
         );
   end Get_Identifier_Body;

   function Get
            (  Code   : access Source'Class;
               Object : String
            )  return String is
      Line      : String renames Get_Line (Code.all);
      Start     : constant Integer := Get_Pointer (Code.all);
      Pointer   : Integer := Start;
      Malformed : Boolean;
      Symbol    : Code_Point := 0;
   begin
      Set_Pointer (Code.all, Pointer);
      if Pointer <= Line'Last then
         begin
            Get (Line, Pointer, Symbol);
         exception
            when Data_Error =>
               Set_Pointer (Code.all, Pointer);
               Raise_Exception
               (  Parsers.Syntax_Error'Identity,
                  "Invalid UTF-8 code at " & Image (Link (Code.all))
               );
         end;
      end if;
      if not Is_Letter (Symbol) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Name of "
            &  Object
            &  " is expected at "
            &  Image (Link (Code.all))
         )  );
      end if;
      Get_Identifier_Body (Code.all, Line, Pointer, Malformed);
      Set_Pointer (Code.all, Pointer);
      if Malformed then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Malformed name of "
            &  Object
            &  " at "
            &  Image (Link (Code.all))
         )  );
      end if;
      return Line (Start..Pointer - 1);
   end Get;

   procedure Get_Blank
             (  Context : in out FCL_Expression;
                Code    : in out Source'Class;
                Got_It  : out Boolean
             )  is
      Buffer     : Line_Ptr;
      Pointer    : Integer;
      Last       : Integer;
      Start_At   : Location;
      In_Comment : Boolean := False;
   begin
      loop
         Get_Line (Code, Buffer, Pointer, Last);
         declare
            Line : String renames Buffer (Buffer'First..Last);
         begin
            while Pointer <= Line'Last loop
               if In_Comment then
                  -- Inside a (*..*) comment
                  if (  Pointer < Line'Last
                     and then
                        Line (Pointer..Pointer + 1) = "*)"
                     )
                  then
                     Pointer    := Pointer + 2;
                     In_Comment := False;
                  else
                     begin
                        Skip (Line, Pointer);
                     exception
                        when Data_Error =>
                           Set_Pointer (Code, Pointer);
                           Set_Pointer (Code, Pointer);
                           Raise_Exception
                           (  Syntax_Error'Identity,
                              (  "Illegal UTF-8 encoding at "
                              &  Image (Link (Code))
                           )  );
                     end;
                  end if;
               else
                  -- Outside comments
                  Get (Line, Pointer, Blank_Characters);
                  exit when Pointer > Line'Last;
                  if Pointer < Line'Last then
                     exit when Line (Pointer..Pointer + 1) = "--";
                     if Line (Pointer..Pointer + 1) = "(*" then
                        Set_Pointer (Code, Pointer);
                        Pointer := Pointer + 2;
                        Set_Pointer (Code, Pointer);
                        Start_At   := Link (Code);
                        In_Comment := True;
                     else
                        Got_It := True;
                        Set_Pointer (Code, Pointer);
                        return;
                     end if;
                  else
                     Got_It := True;
                     Set_Pointer (Code, Pointer);
                     return;
                  end if;
               end if;
            end loop;
            Set_Pointer (Code, Line'Last + 1);
         end;
         Next_Line (Code);
      end loop;
   exception
      when End_Error =>
         if In_Comment then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unclosed comment beginning at "
               &  Image (Start_At)
            )  );
         end if;
         Got_It := False;
   end Get_Blank;

   function Get_Location (Item : Node'Class)
      return Parsers.Multiline_Source.Location is
   begin
      if Item in Expression'Class then
         declare
            Tree   : Expression'Class renames Expression'Class (Item);
            Result : Parsers.Multiline_Source.Location := Tree.Location;
         begin
            for Index in Tree.Operands'Range loop
               Result :=
                  Result & Get_Location (Tree.Operands (Index).all);
            end loop;
            return Result;
         end;
      else
         return Term (Item).Location;
      end if;
   end Get_Location;

   function Is_Blank (Value : UTF8_Code_Point) return Boolean is
   begin
      case Value is
         when 16#09# | 16#0A# | 16#0B# | 16#0C# | 16#0D# | 16#85# =>
            return True; -- HT, LF, VY, FF, CR, LF
         when others =>
            return Category (Value) in Separator;
      end case;
   end Is_Blank;

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      case Left is
         when Component =>
            return Right = Component;
         when others =>
            return False;
      end case;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      case Operation is
         when Sub | Div =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      case Operation is
         when Add | Sub =>
            return Add_Inv;
         when Mul | Div =>
            return Mul_Inv;
         when others =>
            raise Program_Error;
      end case;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
      Symbol : Code_Point;
      Index  : Integer := Pointer;
   begin
      Get (Source, Index, Symbol);
      if not Is_Identifier_Body (Symbol) then
         return True;
      end if;
      Index := Pointer;
      Get_Backwards (Source, Index, Symbol);
      return
         not (  Is_Identifier_Body (Symbol)
             or else
                Is_Unit_Symbol (Symbol)
             );
   exception
      when Data_Error =>
         return False;
   end Check_Matched;

   function Call
            (  Context   : access FCL_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : constant Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Operation.Operation;
         This.Location  := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Operation.Location & Link (List));
   end Call;

   function Enclose
            (  Context : access FCL_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : constant Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Left.Operation;
         This.Location  := Left.Location & Right.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Left.Location & Right.Location & Link (List));
   end Enclose;

   procedure Get_Character_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Identifier
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Numeric_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Power
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_String_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is separate;

   procedure Get_Operand
             (  Context  : in out FCL_Expression;
                Code     : in out Source'Class;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      use Fuzzy.Logic;
   begin
      declare
         Line    : String renames Get_Line (Code);
         Pointer : constant Integer := Get_Pointer (Code);
      begin
         case Line (Pointer) is
            when '"' =>
               Get_String_Literal (Code, Line, Pointer, Argument);
               Got_It := True;
               return;
            when ''' =>
               Get_Character_Literal (Code, Line, Pointer, Argument);
               Got_It := True;
               return;
            when '0'..'9' =>
               Get_Numeric_Literal (Code, Line, Pointer, Argument);
            when others =>
               declare
                  Symbol : Code_Point;
                  Index  : Integer := Pointer;
               begin
                  Get (Line, Index, Symbol);
                  if Symbol = 16#22A4# then -- Contradiction
                     Set_Pointer (Code, Index);
                     Argument.Value :=
                        new Truth_Literal'
                            (  Node
                            with
                               Link (Code), Contradictory
                            );
                     Argument.Location := Link (Code);
                     Got_It := True;
                     return;
                  elsif Symbol = 16#22A5# then -- Uncertainty
                     Set_Pointer (Code, Index);
                     Argument.Value :=
                        new Truth_Literal'
                            (  Node
                            with
                               Link (Code), Uncertain
                            );
                     Argument.Location := Link (Code);
                     Got_It := True;
                     return;
                  elsif (  Is_Identifier_Body (Symbol)
                        or else
                           Is_Unit_Symbol (Symbol)
                        )
                  then
                     Get_Identifier (Code, Line, Pointer, Argument);
                     Got_It := True;
                     return;
                  end if;
                  Got_It := False;
                  return;
               exception
                  when Data_Error =>
                     Set_Pointer (Code, Index);
                     Set_Pointer (Code, Index);
                     Raise_Exception
                     (  Parsers.Syntax_Error'Identity,
                        "Invalid UTF-8 code at " & Image (Link (Code))
                     );
               end;
         end case;
      end;
      --
      -- A numeric literal has been recognized,  now checking if a  unit
      -- specification follows.
      --
      declare
         Number : Numeric_Literal'Class renames
                      Numeric_Literal'Class (Argument.Value.all);
      begin
         Get_Blank (Context, Code, Got_It);
         declare
            Line    : String renames Get_Line (Code);
            Pointer : Integer := Get_Pointer (Code);
         begin
            Get_Unit
            (  Line,
               Pointer,
               Number.Dimension,
               Parsing_Mode
            );
            Set_Pointer (Code, Pointer);
            Number.Location   := Number.Location & Link (Code);
            Argument.Location := Number.Location;
         exception
            when others =>
               null;
         end;
      end;
      Got_It := True;
   exception
      when End_Error =>
         Got_It := False;
   end Get_Operand;

   function Image (Action : Operations) return String is
   begin
      case Action is
         when Abs_Value        => return "'abs'";
         when Add              => return "'+'";
         when Add_Inv          => return "0-x";
         when Colon            => return "':'";
         when Comma            => return "','";
         when Component        => return "'.'";
         when Concatenate      => return "'&'";
         when Div              => return "'/'";
         when EQ         => return "'='";
         when Ellipsis   => return "'..'";
         when GE         => return "'>=', '"& To_UTF8 (16#2265#) &''';
         when GT               => return "'>'";
         when Has_Element      => return '''& To_UTF8 (16#220B#) &''';
         when Has_Not_Element  => return '''& To_UTF8 (16#220C#) &''';
         when Has_Not_Superset => return '''& To_UTF8 (16#2288#) &''';
         when Has_Superset     => return '''& To_UTF8 (16#2286#) &''';
         when Intersection     => return '''& To_UTF8 (16#2229#) &''';
         when Is_Element       => return '''& To_UTF8 (16#2208#) &''';
         when Is_In_Subset     => return "'in'";
         when Is_Not_Element   => return '''& To_UTF8 (16#2209#) &''';
         when Is_Not_Superset  => return '''& To_UTF8 (16#2289#) &''';
         when Is_Subset        => return "'is'";
         when Is_Superset      => return '''& To_UTF8 (16#2287#) &''';
         when LE         => return "'<=', '"& To_UTF8 (16#2264#) &''';
         when LT         => return "'<'";
         when Lattice_And   => return "'and'";
         when Lattice_Not      => return "'not'";
         when Lattice_Or       => return "'or'";
         when Lattice_Xor      => return "'xor'";
         when Left_Bracket     => return "'('";
         when Left_Dimension   => return "'['";
         when Left_Index       => return "'('";
         when Left_Interval => return "'['";
         when Logical_And   => return '''& To_UTF8 (16#22C0#) &''';
         when Logical_Not   => return '''& To_UTF8 (16#00AC#) &''';
         when Logical_Or    => return '''& To_UTF8 (16#22C1#) &''';
         when Member        => return "'in'";
         when Membership    => return "'^'";
         when Minus         => return "'-'";
         when Mul           => return "'*'";
         when Mul_Inv       => return "1/x";
         when NE         => return "'/=', '" & To_UTF8 (16#2260#) &''';
         when Not_In     => return "'not in'";
         when Not_Subset => return "'not'";
         when Plus          => return "'+'";
         when Postfix_Pow   => return "postfix pow";
         when Pow           => return "'**'";
         when Pow_0     => return '''& To_UTF8 (Superscript_0) &''';
         when Pow_1     => return '''& To_UTF8 (Superscript_1) &''';
         when Pow_2     => return '''& To_UTF8 (Superscript_2) &''';
         when Pow_3     => return '''& To_UTF8 (Superscript_3) &''';
         when Pow_4     => return '''& To_UTF8 (Superscript_4) &''';
         when Pow_5     => return '''& To_UTF8 (Superscript_5) &''';
         when Pow_6     => return '''& To_UTF8 (Superscript_6) &''';
         when Pow_7     => return '''& To_UTF8 (Superscript_7) &''';
         when Pow_8     => return '''& To_UTF8 (Superscript_8) &''';
         when Pow_9     => return '''& To_UTF8 (Superscript_9) &''';
         when Pow_Minus => return '''& To_UTF8 (Superscript_Minus) &''';
         when Pow_Plus  => return '''& To_UTF8 (Superscript_Plus)  &''';
         when Reserved         => return "'then', 'else'";
         when Right_Bracket    => return "')'";
         when Right_Interval   => return "']'";
         when Sub              => return "'-'";
         when Union            => return '''& To_UTF8 (16#222A#) &''';
      end case;
   end Image;

   function Image (Item : Node'Class; Mode : Code_Set) return String is
   begin
      return Image (No_Feature, Item, Mode);
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Mark;
               Mode    : Code_Set
            )  return String is
   begin
      return "";
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Numeric_Literal;
               Mode    : Code_Set
            )  return String is
      function Value_Image return String is
      begin
         if Item.Malformed then
            return "<malformed>";
         elsif Item.Exponent = Integer'First then
            return "<underflown>";
         elsif Item.Exponent = Integer'Last then
            return "<overflown>";
         elsif Item.Base = 10 then
            if Item.Exponent = 0 then
               return Item.Value;
            else
               return Item.Value & "E" & Image (Item.Exponent);
            end if;
         else
            if Item.Exponent = 0 then
               return Image (Item.Base) & '#' & Item.Value & '#';
            else
               return
               (  Image (Item.Base)
               & '#' & Item.Value & '#'
               & "E" & Image (Item.Exponent)
               );
            end if;
         end if;
      end Value_Image;
   begin
      if Item.Dimension = Np then
         return Value_Image;
      else
         return
         (  Value_Image
         &  ' '
         &  Image (Item.Dimension, Units.Latin1_Set)
         );
      end if;
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : String_Literal;
               Mode    : Code_Set
            )  return String is
   begin
      return Quote (Item.Value);
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Character_Literal;
               Mode    : Code_Set
            )  return String is
   begin
      return Quote (Image (Item.Value), ''');
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Power_Literal;
               Mode    : Code_Set
            )  return String is
   begin
      return Image (Item.Value);
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Truth_Literal;
               Mode    : Code_Set
            )  return String is
   begin
      return
      (  '['
      &  Image (Item.Value.Possibility)
      &  ','
      &  Image (Item.Value.Necessity)
      &  ']'
      );
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Identifier;
               Mode    : Code_Set
            )  return String is
   begin
      if Item.Malformed then
         return "<malformed>";
      else
         return Item.Value;
      end if;
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Missing_Operand;
               Mode    : Code_Set
            )  return String is
   begin
      return "<missing>";
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Expression;
               Mode    : Units.Code_Set := Units.UTF8_Set
            )  return String is
      function Image (List : Argument_List) return String is
      begin
         if List'Length = 0 then
            return ")";
         else
            return
            (  ", "
            &  Image (Feature, List (List'First).all, Mode)
            &  Image (List (List'First + 1..List'Last))
            );
         end if;
      end Image;
   begin
      return
      (  Operations'Image (Item.Operation)
      &  "("
      &  Image (Feature, Item.Operands (1).all, Mode)
      &  Image (Item.Operands (2..Item.Count))
      );
   end Image;

   function Is_Identifier_Body (Value : UTF8_Code_Point)
      return Boolean is
   begin
      case Category (Value) is
         when Lu..Lo | Mn | Mc | Nd | Nl | Pc | Cf =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Identifier_Body;

   function Is_Unit_Symbol (Value : UTF8_Code_Point) return Boolean is
   begin
      case Value is
         when 16#00B0# |  -- Degree sign
              16#00B5# |  -- Micro
              16#2103# |  -- Celsius
              16#2109# |  -- Fahrenheit
              16#2125# |  -- Ounce
              16#212A# |  -- Kelvin
              16#212B# |  -- Ångström
              16#2126# => -- Ohm
            return True;
         when others =>
            return False;
      end case;
   end Is_Unit_Symbol;

   procedure On_Missing_Operation
             (  Context  : in out FCL_Expression;
                Code     : in out Source'Class;
                Modifier : Tokens.Operation_Token;
                Token    : out Lexers.Token_Lexer.Implementation.
                               Lexical_Token;
                Got_It   : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Lattice_Not =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "'in' is expected at "
               &  Image (Link (Code))
               &  " after 'not' at "
               &  Image (Modifier.Location)
            )  );
         when others =>
            Raise_Exception
            (  Parsers.Syntax_Error'Identity,
               (  "Unknown error after a modifier at "
               &  Image (Modifier.Location)
            )  );
      end case;
   end On_Missing_Operation;

   procedure On_Postmodifier
             (  Context   : in out FCL_Expression;
                Code      : in out Source'Class;
                Operation : in out Tokens.Operation_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Member =>
            case Operation.Operation is
               when Is_Subset =>
                  Operation.Operation := Is_In_Subset;
                  Operation.Location :=
                     Operation.Location & Modifier.Location;
                  Got_It := True;
               when Lattice_Not | Not_Subset =>
                  Operation.Operation := Not_In;
                  Operation.Location :=
                     Operation.Location & Modifier.Location;
                  Got_It := True;
               when others =>
                  Reset_Pointer (Code);
                  Got_It := False;
            end case;
         when others =>
            Reset_Pointer (Code);
            Got_It := False;
      end case;
   end On_Postmodifier;

   procedure On_Postmodifier
             (  Context   : in out FCL_Expression;
                Code      : in out Source'Class;
                Argument  : in out Tokens.Argument_Token;
                Modifier  : Tokens.Operation_Token;
                Got_It    : out Boolean
             )  is
   begin
      case Modifier.Operation is
         when Pow_0 | Pow_1 | Pow_2 | Pow_3 | Pow_4 |
              Pow_5 | Pow_6 | Pow_7 | Pow_8 | Pow_9 |
              Pow_Minus | Pow_Plus =>
            --
            -- Step  back  and  then  emulate infix power. This switches
            -- parser  into  operand input and the superscript number is
            -- recognized as a literal.
            --
            Reset_Pointer (Code);
            declare
               Line    : Line_Ptr;
               Pointer : Integer;
               Last    : Integer;
               Power   : Tokens.Argument_Token;
            begin
               Get_Line (Code, Line, Pointer, Last);
               Get_Power
               (  Code,
                  Line (Line'First..Last),
                  Pointer,
                  Power
               );
               Lexers.Token_Lexer.Implementation.Do_Operand
               (  Context  => Context,
                  Argument => Argument
               );
               declare
                  Power : Tokens.Operation_Token :=
                             (Postfix_Pow, Link (Code));
               begin
                  Lexers.Token_Lexer.Implementation.Do_Binary
                  (  Context  => Context,
                     Code     => Code,
                     Operator => Power,
                     Left     => 12,
                     Right    => 12,
                     Got_It   => Got_It
                  );
                  if not Got_It then
                     return;
                  end if;
               end;
               Argument := Power;
            end;
            Got_It := True;
         when others =>
            Reset_Pointer (Code);
            Got_It := False;
      end case;
   end On_Postmodifier;

begin
   Add_Operator (Infixes,  "or",               Lattice_Or,  0, 0);
   Add_Operator (Infixes,  "xor",              Lattice_Xor, 0, 0);
   Add_Operator (Infixes,  "and",              Lattice_And, 0, 0);

   Add_Operator (Infixes,  To_UTF8 (16#22C0#), Logical_And, 0, 0);
   Add_Operator (Infixes,  To_UTF8 (16#22C1#), Logical_Or,  0, 0);

   Add_Operator (Infixes,  "is",               Is_Subset,        1, 1);
   Add_Operator (Infixes,  "in",               Is_In_Subset,     1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2286#), Has_Superset,     1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2287#), Is_Superset,      1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2288#), Has_Not_Superset, 1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2289#), Is_Not_Superset,  1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2208#), Is_Element,       1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2209#), Is_Not_Element,   1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#220B#), Has_Element,      1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#220C#), Has_Not_Element,  1, 1);
   Add_Operator (Infixes,  "not",              Not_Subset,       1, 1);
   Add_Operator (Infixes,  "=",                EQ,               1, 1);
   Add_Operator (Infixes,  "/=",               NE,               1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2260#), NE,               1, 1);
   Add_Operator (Infixes,  "<",                LT,               1, 1);
   Add_Operator (Infixes,  "<=",               LE,               1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2264#), LE,               1, 1);
   Add_Operator (Infixes,  ">",                GT,               1, 1);
   Add_Operator (Infixes,  ">=",               GE,               1, 1);
   Add_Operator (Infixes,  To_UTF8 (16#2265#), GE,               1, 1);

   Add_Operator (Infixes,  To_UTF8 (16#2229#), Intersection,     2, 2);
   Add_Operator (Infixes,  To_UTF8 (16#222A#), Union,            2, 2);

   Add_Operator (Infixes,  "..",               Ellipsis,         3, 3);
   Add_Index    (Infixes,  "[",                Left_Dimension,      3);
   Add_Operator (Infixes,  "^",                Membership,       3, 3);

   Add_Operator (Infixes,  "+",                Add,          4, 4);
   Add_Operator (Infixes,  "-",                Sub,          4, 4);
   Add_Operator (Infixes,  "&",                Concatenate,  4, 4);

   Add_Operator (Prefixes, "+",                Plus,         6, 5);
   Add_Operator (Prefixes, "-",                Minus,        6, 5);
   Add_Operator (Prefixes, "abs",              Abs_Value,    6, 8);
   Add_Operator (Prefixes, "not",              Lattice_Not,  6, 1);
   Add_Operator (Prefixes, To_UTF8 (16#00AC#), Logical_Not,  6, 1);

   Add_Operator (Infixes,  "*",                Mul,          7, 7);
   Add_Operator (Infixes,  "/",                Div,          7, 7);

   Add_Operator (Infixes,  "**",               Pow,          8, 8);

   Add_Operator (Infixes,  ".",                Component,  11, 11);
   Add_Index    (Infixes,  "(",                Left_Index,     10);

   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_Plus),  Pow_Plus);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_Minus), Pow_Minus);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_0),     Pow_0);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_1),     Pow_1);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_2),     Pow_2);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_3),     Pow_3);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_4),     Pow_4);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_5),     Pow_5);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_6),     Pow_6);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_7),     Pow_7);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_8),     Pow_8);
   Add_Postmodifier (Postfixes, To_UTF8 (Superscript_9),     Pow_9);

   Add_Bracket (Prefixes,  "(", Left_Bracket);
   Add_Bracket (Prefixes,  "[", Left_Interval);
   Add_Bracket (Postfixes, ")", Right_Bracket);
   Add_Bracket (Postfixes, "]", Right_Interval);

   Add_Comma    (Infixes, ",", Comma);
   Add_Ligature (Infixes, ":", Colon);

   Add_Postmodifier (Prefixes, "in",   Member);
   Add_Postmodifier (Infixes,  "then", Reserved);
   Add_Postmodifier (Infixes,  "else", Reserved);

end Parsers.FCL;
