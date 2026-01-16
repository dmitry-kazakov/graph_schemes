--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Get_Character_Literal           Luebeck            --
--  Separate body implementation                   Winter, 2005       --
--                                                                    --
--                                Last revision :  12:48 16 Oct 2010  --
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

separate (Parsers.FCL)
   procedure Get_Character_Literal
             (  Code     : in out Source'Class;
                Line     : String;
                Pointer  : Integer;
                Argument : out Tokens.Argument_Token
             )  is
   Index : Integer := Pointer + 1;
   Value : UTF8_Code_Point;
begin
   Get (Line, Index, Value);
   if Index <= Line'Last and then ''' = Line (Index) then
      Set_Pointer (Code, Index + 1);
      Argument.Location := Link (Code);
      Argument.Value    := new Character_Literal;
      Character_Literal (Argument.Value.all).Value := Value;
      return;
   end if;
   Set_Pointer (Code, Index);
   Raise_Exception
   (  Parsers.Syntax_Error'Identity,
      "Missing ' in the character literal at " & Image (Link (Code))
   );
exception
   when Data_Error =>
      Set_Pointer (Code, Index);
      Set_Pointer (Code, Index);
      Raise_Exception
      (  Syntax_Error'Identity,
         "Illegal UTF-8 encoding at " & Image (Link (Code))
      );
end Get_Character_Literal;
