--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Confidence_Factors.Edit                     Luebeck            --
--  Implementation                                 Winter, 2000       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Strings_Edit.UTF8;          use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Mapping;  use Strings_Edit.UTF8.Mapping;
with Strings_Edit.UTF8.Maps;     use Strings_Edit.UTF8.Maps;
with Strings_Edit.Floats;        use Strings_Edit.Floats;

with Ada.IO_Exceptions;
with Name_Tables;

package body Confidence_Factors.Edit is
   Max : constant Float := Float (Confidence'Last);

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Confidence
             )  is
      Result : Confidence;
      Index  : Integer := Pointer;
      function Compare (Pattern : String) return Boolean is
         SrcIndex : Integer := Index;
      begin
         if Pattern'Length > Source'Last + 1 - SrcIndex then
            return False;
         end if;
         for PatIndex in Pattern'Range loop
            if (  Character'Pos (Pattern (PatIndex))
               /= To_Lowercase (Character'Pos (Source (SrcIndex)))
               )  then
               return False;
            end if;
            SrcIndex := SrcIndex + 1;
         end loop;
         Index := SrcIndex;
         return True;
      end Compare;
   begin
      if Compare ("true") then
         Result := Confidence'Last;
      elsif Compare ("false") then
         Result := Confidence'First;
      else
         declare
            Level : Float;
         begin
            Get (Source, Index, Level, First => 0.0, Last => 1.0);
            begin
               Result := Confidence (Level * Max);
            exception
               when Constraint_Error =>
                  if Level < 0.5 then
                     Result := Confidence'First;
                  else
                     Result := Confidence'Last;
                  end if;
            end;
         end;
      end if;
      Value := Result;
      Pointer := Index;
   end Get;

   function Value (Source : in String) return Confidence is
      Result  : Confidence;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Ada.IO_Exceptions.Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Confidence;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
   begin
      if Value = Confidence'Last then
         Put (Destination, Pointer, '1');
      elsif Value = Confidence'First then
         Put (Destination, Pointer, '0');
      else
         Put
         (  Destination,
            Pointer,
            Float (Value) / Max,
            AbsSmall => -Confidence'Aft,
            Field    => Field,
            Justify  => Justify,
            Fill     => Fill
         );
      end if;
   end Put;

   function Image (Value : in Confidence) return String is
      Text    : String (1..80);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Value);
      return Text (Text'First..Pointer - 1);
   end Image;

end Confidence_Factors.Edit;
