--                                                                    --
--  package Fuzzy.Basic_Edit        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2003       --
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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.UTF8.Maps;   use Strings_Edit.UTF8.Maps;

with Name_Tables;

package body Fuzzy.Basic_Edit is

   procedure Get_Delimiter
             (  Source    : String;
                Pointer   : in out Integer;
                Success   : out Boolean;
                Delimiter : Delimiter_Type
             )  is
      Index : Integer := Pointer;
   begin
      Success := False;
      Get (Source, Index, Name_Tables.Blanks);
      case Delimiter is
         when Arrow =>
            if Index > Source'Last or else Source (Index) /= '-' then
               return;
            end if;
            Index := Index + 1;
            if Index > Source'Last or else Source (Index) /= '>' then
               return;
            end if;
            Index := Index + 1;
         when Ellipsis =>
            if Index > Source'Last or else Source (Index) /= '.' then
               return;
            end if;
            Index := Index + 1;
            if Index > Source'Last or else Source (Index) /= '.' then
               return;
            end if;
            Index := Index + 1;
            if Index <= Source'Last and then Source (Index) = '.' then
               Index := Index + 1;
            end if;
         when Comma =>
            if Index > Source'Last or else Source (Index) /= ',' then
               return;
            end if;
            Index := Index + 1;
         when Colon =>
            if Index > Source'Last or else Source (Index) /= ':' then
               return;
            end if;
            Index := Index + 1;
         when Semicolon =>
            if Index > Source'Last or else Source (Index) /= ';' then
               return;
            end if;
            Index := Index + 1;
      end case;
      Get (Source, Index, Name_Tables.Blanks);
      Pointer := Index;
      Success := True;
   end Get_Delimiter;

   procedure Get_Weight
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Confidence;
                Default : Confidence := Confidence'Last
             )  is
      Index : Integer := Pointer;
   begin
      Get (Source, Index, Name_Tables.Blanks);
      if Index > Source'Last or else Source (Index) /= ':' then
         Value := Default;
      else
         Index := Index + 1;
         Get (Source, Index, Name_Tables.Blanks);
         Get (Source, Index, Value);
         Pointer := Index;
      end if;
   exception
      when Constraint_Error =>
         raise Data_Error;
      when End_Error =>
         Value := Default;
   end Get_Weight;

end Fuzzy.Basic_Edit;
