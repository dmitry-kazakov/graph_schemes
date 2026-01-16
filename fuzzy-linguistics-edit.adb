--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Edit                      Luebeck            --
--  Implementation                                 Winter, 2003       --
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
with Fuzzy.Basic_Edit;         use Fuzzy.Basic_Edit;
with Strings_Edit.Fields;      use Strings_Edit.Fields;
with Strings_Edit.UTF8.Maps;   use Strings_Edit.UTF8.Maps;

with Name_Tables;

package body Fuzzy.Linguistics.Edit is
   use Float_Edit;

   Size_Inc : constant := 80;

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Variable;
                Base    : NumberBase := 10
             )  is
   --
   -- Get_Item -- Get one domain value
   --
   --    Pointer - The current string position
   --
      procedure Get_Item (Pointer : in out Integer);
      Result    : Variable;
      Got_Arrow : Boolean   := False;
      Got_Item  : Boolean   := False;
      Left      : Number_Of := Number_Of'First;

      procedure Get_Item (Pointer : in out Integer) is
         Got_Ellipsis : Boolean;
         Index        : Integer := Pointer;
         Right        : Number_Of;
         Level        : Confidence;
      begin
         if Got_Item and then not Got_Arrow then
            Get_Delimiter (Source, Index, Got_Item, Comma);
            if not Got_Item then
               return;
            end if;
         end if;
         begin
            Get (Source, Index, Right, Base);
         exception
            when End_Error =>
               Got_Item := False;
               return;
         end;
         if Right < Left then
            raise Data_Error;
         elsif not Got_Arrow and then Left /= Right then
            Append (Result, Right, Confidence'First);
         end if;
         Left := Right;
         Get_Delimiter (Source, Index, Got_Ellipsis, Ellipsis);
         if Got_Ellipsis then
            begin
               Get (Source, Index, Right, Base);
            exception
               when End_Error =>
                  raise Data_Error;
            end;
            if Right < Left then
               raise Data_Error;
            end if;
         else
            Right := Left;
         end if;
         Get_Weight (Source, Index, Level);
         Append (Result, Left, Level);
         if Got_Ellipsis then
            Append (Result, Right, Level);
            Left := Right;
         end if;
         Get_Delimiter (Source, Index, Got_Arrow, Arrow);
         if not Got_Arrow then
            Append (Result, Right, Confidence'First);
         end if;
         Got_Item := True;
         Pointer  := Index;
      end Get_Item;

      Index : Integer := Pointer;
   begin
      Get_Delimiter (Source, Index, Got_Arrow, Arrow);
      Get_Item (Index);
      if not Got_Item then
         if Got_Arrow then
            raise Data_Error;
         else
            raise End_Error;
         end if;
      end if;
      while Got_Item loop
         Get_Item (Index);
      end loop;
      Value   := Result;
      Pointer := Index;
   end Get;

   function Image
            (  Value    : Variable;
               Base     : NumberBase := 10;
               RelSmall : Positive   := MaxSmall;
               AbsSmall : Integer    := -MaxSmall
            )  return String is
      Size : Natural := Size_Inc;
   begin
      begin
         loop
            declare
               Text    : String (1..Size);
               Pointer : Positive := 1;
            begin
               Put
               (  Text,
                  Pointer,
                  Value,
                  Base,
                  RelSmall,
                  AbsSmall
               );
               return Text (Text'First..Pointer - 1);
            exception
               when Layout_Error =>
                  null;
            end;
            Size := Size + Size_Inc;
         end loop;
      end;
   end Image;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Variable is
      Pointer : Integer := Source'First;
      Result  : Variable;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Variable;
                Base        : NumberBase := 10;
                RelSmall    : Positive   := MaxSmall;
                AbsSmall    : Integer    := -MaxSmall;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
      Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;

      procedure Put_Point (X : Number_Of; Y : Confidence) is
      begin
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => X,
            Base        => Base,
            RelSmall    => RelSmall,
            AbsSmall    => AbsSmall
         );
         if Y /= Confidence'Last then
            Put (Text, Index, ':');
            Put (Text, Index, Y);
         end if;
      end Put_Point;

      procedure Put_Range (From, To : Number_Of; Y : Confidence) is
      begin
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => From,
            Base        => Base,
            RelSmall    => RelSmall,
            AbsSmall    => AbsSmall
         );
         Put (Text, Index, "..");
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => To,
            Base        => Base,
            RelSmall    => RelSmall,
            AbsSmall    => AbsSmall
         );
         if Y /= Confidence'Last then
            Put (Text, Index, ':');
            Put (Text, Index, Y);
         end if;
      end Put_Range;

      procedure Do_Point (Dot : Point) is
      begin
         Put_Point (Dot.Value, Dot.Left);
         if Dot.Left /= Dot.Min then
            Put (Text, Index, "->");
            Put_Point (Dot.Value, Dot.Min);
         end if;
         if Dot.Min /= Dot.Max then
            Put (Text, Index, "->");
            Put_Point (Dot.Value, Dot.Max);
         end if;
         if Dot.Max /= Dot.Right then
            Put (Text, Index, "->");
            Put_Point (Dot.Value, Dot.Right);
         end if;
      end Do_Point;

      Size : constant Natural := Get_Points_Number (Value);
   begin
      case Size is
         when 0 =>
            Put (Text, Index, "0:0");
         when 1 =>
            declare
               Left : Point renames Get (Value.Data, 1);
            begin
               if (  Left.Left = Confidence'First
                  and then
                     Left.Right = Confidence'First
                  )
               then
                  Put_Point (Left.Value, Left.Max);
               else
                  if Left.Left /= Confidence'First then
                     Put (Text, Index, "->");
                  end if;
                  Do_Point (Left);
                  if Left.Right /= Confidence'First then
                     Put (Text, Index, "->");
                  end if;
               end if;
            end;
         when 2 =>
            declare
               Left  : Point renames Get (Value.Data, 1);
               Right : Point renames Get (Value.Data, 2);
            begin
               if (  Left.Left = Confidence'First
                  and then
                     Left.Min = Confidence'First
                  and then
                     Left.Max = Left.Right
                  and then
                     Right.Left = Left.Right
                  and then
                     Right.Max = Right.Left
                  and then
                     Right.Min = Confidence'First
                  and then
                     Right.Right = Confidence'First
                  )
               then
                  Put_Range (Left.Value, Right.Value, Left.Max);
               else
                  if Left.Left /= Confidence'First then
                     Put (Text, Index, "->");
                  end if;
                  Do_Point (Left);
                  Put (Text, Index, "->");
                  Do_Point (Right);
                  if Right.Right /= Confidence'First then
                     Put (Text, Index, "->");
                  end if;
               end if;
            end;
         when others =>
            for Element in 1..Size loop
               declare
                  Dot : Point renames Get (Value.Data, Element);
               begin
                  if (  Element /= 1
                     or else
                        Dot.Left /= Confidence'First
                     )
                  then
                     Put (Text, Index, "->");
                  end if;
                  Do_Point (Dot);
                  if (  Element = Size
                     and then
                        Dot.Right /= Confidence'First
                     )
                  then
                     Put (Text, Index, "->");
                  end if;
               end;
            end loop;
      end case;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

end Fuzzy.Linguistics.Edit;
