--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Get_Power                       Luebeck            --
--  Separate body implementation                   Winter, 2009       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Strings_Edit.UTF8.Superscript.Integer_Edit;

separate (Parsers.FCL)
   procedure Get_Power
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   package Power_Edit is
      new Strings_Edit.UTF8.Superscript.Integer_Edit (Domain_Integer);

   procedure Skip (Index : in out Integer) is
      Symbol : UTF8_Code_Point;
   begin
      while Index <= Line'Last loop
         Get (Line, Index, Symbol);
         case Symbol is
            when Superscript_0 | Superscript_1 | Superscript_2 |
                 Superscript_3 | Superscript_4 | Superscript_5 |
                 Superscript_6 | Superscript_7 | Superscript_8 |
                 Superscript_9 | Superscript_Plus | Superscript_Minus =>
               null;
            when others =>
               exit;
         end case;
      end loop;
      Set_Pointer (Code, Index);
   exception
      when Data_Error =>
         Set_Pointer (Code, Index);
   end Skip;

   Index : Integer := Pointer;
   Value : Integer;
begin
   Power_Edit.Get (Line, Index, Value);
   Set_Pointer (Code, Index);
   Argument.Location := Link (Code);
   Argument.Value    := new Power_Literal;
   Power_Literal (Argument.Value.all).Value := Value;
exception
   when Constraint_Error =>
      Skip (Index);
      Raise_Exception
      (  Parsers.Syntax_Error'Identity,
         "Superscript power is too large at " & Image (Link (Code))
      );
   when Data_Error =>
      Skip (Index);
      Raise_Exception
      (  Syntax_Error'Identity,
         "Malformed superscript power at " & Image (Link (Code))
      );
end Get_Power;
