--                                                                    --
--  package HTML                    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body HTML is
   function To_String (Value : Color) return String is
      function Put (Value : Color_Component) return String is
         Result  : String (1..2);
         Pointer : Integer := Result'First;
      begin
         Put
         (  Result,
            Pointer,
            Integer (Value),
            Base    => 16,
            Field   => 2,
            Justify => Strings_Edit.Right,
            Fill    => '0'
         );
         return Result;
      end Put;
   begin
      return
      (  "#"
      &  Put (Value.Red)
      &  Put (Value.Green)
      &  Put (Value.Blue)
      );
   end To_String;

   function Color_Beg (Value : Color) return String is
   begin
      return "<font color=""" & To_String (Value) & """>";
   end Color_Beg;

   procedure Put_Latin1 (File : File_Type; Value : String) is
      Symbol : Character;
   begin
      for Index in Value'Range loop
         Symbol := Value (Index);
         case Symbol is
            when '&' =>
               Put (File, "&amp;");
            when '<' =>
               Put (File, "&lt;");
            when '>' =>
               Put (File, "&gt;");
            when '"' =>
               Put (File, "&quot;");
            when ' ' | '!' | '#'..'%' | '''..';' | '=' | '?'..'~' =>
               Put (File, Symbol);
            when others =>
               Put (File, "&#");
               Put (File, Image (Integer (Character'Pos (Symbol))));
               Put (File, ";");
         end case;
      end loop;
   end Put_Latin1;

   procedure Put_UTF8
             (  File  : File_Type;
                Value : String;
                Error : UTF8_Code_Point 
             )  is
      Pointer : Integer := Value'First;
      Symbol  : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         begin
            Get (Value, Pointer, Symbol);
         exception
            when Data_Error =>
               Symbol  := Error;
               Pointer := Pointer + 1;
         end;
         case Symbol is
            when 16#26# =>   -- &
               Put (File, "&amp;");
            when 16#3C# =>   -- <
               Put (File, "&lt;");
            when 16#3E# =>   -- >
               Put (File, "&gt;");
            when 16#22# =>   -- "
               Put (File, "&quot;");
            when 16#20# | 16#21# | 16#23#..16#25# | 16#27#..16#3B# |
                 16#3D# | 16#3F#..16#7E# =>
               Put (File, Character'Val (Symbol));
            when others =>
               Put (File, "&#");
               Put (File, Image (Integer (Symbol)));
               Put (File, ";");
         end case;
      end loop;
   end Put_UTF8;
 
end HTML;
