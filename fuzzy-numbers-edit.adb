--                                                                    --
--  package Fuzzy.Numbers.Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
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
with Strings_Edit.Fields;      use Strings_Edit.Fields;
with Strings_Edit.UTF8.Maps;   use Strings_Edit.UTF8.Maps;

with Name_Tables;

package body Fuzzy.Numbers.Edit is
--
-- To_Level -- Confidence factor to level conversion
--
--      Factor - A confidence level
--
-- Returns :
--
--      The corresponding index
--
   function To_Level (Factor : Confidence) return Interval_Index is
   begin
      for Index in Interval_Index'Range loop
         if Factor <= To_Confidence (Index) then
            return Index;
         end if;
      end loop;
      return Interval_Index'Last;
   end To_Level;
--
-- Get -- An item from the string
--
--      Source  - The string to be processed
--      Pointer - The current position in the string
--      Value   - The interval
--      Base    - The base of the interval bounds
--      Level   - The confidence factor
--
-- Exceptions:
--
--      Data_Error    - Syntax error
--      End_Error     - Nothing is here
--      Layout_Error  - Pointer not in Source'First .. Source'Last + 1
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Interval;
                Base    : in NumberBase;
                Level   : out Confidence
             )  is
      From  : Number;
      To    : Number;
   --
   -- Get_To -- Get the right margin of an interval
   --
   --    Pointer - The current string position
   --
   -- This procedure reads ellipsis followed by a value  of  To.  If  no
   -- ellipsis or value present it does not change Pointer and  sets  To
   -- equal to From.
   --
      procedure Get_To (Pointer : in out Integer) is
         Index : Integer := Pointer;
      begin
         Get (Source, Index, Name_Tables.Blanks);
         if Index <= Source'Last and then Source (Index) = '.' then
            Index := Index + 1;
            if Index <= Source'Last and then Source (Index) = '.' then
               Index := Index + 1;
               if Index <= Source'Last and then Source (Index) = '.' then
                  Index := Index + 1;
               end if;
               Get (Source, Index, Name_Tables.Blanks);
               Get (Source, Index, To, Base);
               if not (To >= From) then
                  raise Constraint_Error;
               end if;
               Pointer := Index;
               return;
            end if;
         end if;
         To := From;
      exception
         when End_Error =>
            To := From;
      end Get_To;
   --
   -- Get_Weight -- Get the confidence factor
   --
   --    Pointer - The current string position
   --
   -- This  procedure  reads  colon  and following it Level. It does not
   -- change Pointer if no colon or confidence factor present.
   --
      procedure Get_Weight (Pointer : in out Integer) is
         Index : Integer := Pointer;
      begin
         Get (Source, Index, Name_Tables.Blanks);
         if Index > Source'Last or else Source (Index) /= ':' then
            Level := Confidence'Last;
         else
            Index := Index + 1;
            Get (Source, Index, Name_Tables.Blanks);
            Get (Source, Index, Level);
            Pointer := Index;
         end if;
      exception
         when End_Error =>
            Level := Confidence'Last;
      end Get_Weight;

      Index : Integer := Pointer;
   begin
      Get (Source, Index, From, Base);
      Get_To (Index);
      Get_Weight (Index);
      Value   := To_Interval (From, To);
      Pointer := Index;
   end Get;

   type Interval_Flags is array (Interval_Index) of Boolean;

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Number;
                Base    : in NumberBase := 10
             )  is
      Set    : Interval_Flags := (others => False);
      Index  : Integer        := Pointer;
      Saved  : Integer        := Pointer;
      Result : Fuzzy_Number;
      Span   : Interval;
      Factor : Confidence;
      Level  : Interval_Index;
   begin
      if Index not in Source'First..Source'Last + 1 then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         raise End_Error;
      end if;
      loop
         begin
            Get (Source, Index, Span, Base, Factor);
            Saved := Index;
         exception
            when End_Error =>
               if Saved = Pointer then
                  raise;
               end if;
               Index := Saved;
               exit;
         end;
         if Factor /= Confidence'First then
            Level := To_Level (Factor);
            if Set (Level) then
               --
               -- This interval was already  set.  The  new  one  should
               -- contain the old one.  Otherwise,  Constraint_Error  is
               -- raised.
               --
               if not Is_In (Result.Set (Level), Span) then
                  raise Constraint_Error;
               end if;
            else
               Set (Level) := True;
            end if;
            if Level < Interval_Index'Last then
               --
               -- Check that higher confidence  intervals  are  in  this
               -- one.
               --
               for Index in
                  Interval_Index'Succ (Level)..Interval_Index'Last
               loop
                  if Set (Index) then
                     exit when Is_In (Result.Set (Index), Span);
                     raise Constraint_Error;
                  end if;
               end loop;
            end if;
            Result.Set (Level) := Span;
            if Level > Interval_Index'First then
               --
               -- Ensure  that all intervals of lesser confidence levels
               -- be set as well.
               --
               for Index in reverse
                  Interval_Index'First..Interval_Index'Pred (Level)
               loop
                  if Set (Index) then
                     exit when Is_In (Span, Result.Set (Index));
                     raise Constraint_Error;
                  end if;
                  Result.Set (Index) := Span;
               end loop;
            end if;
         end if;
         Get (Source, Index, Name_Tables.Blanks);
         if Index >= Source'Last or else Source (Index) /= ',' then
            Index := Saved;
            exit;
         end if;
         Index := Index + 1;
         Get (Source, Index, Name_Tables.Blanks);
      end loop;
      if not Set (Interval_Index'Last) then
         raise Constraint_Error;
      end if;
      Value   := Result;
      Pointer := Index;
   end Get;

   function Value
            (  Source : in String;
               Base   : in NumberBase := 10
            )  return Fuzzy_Number is
      Pointer : Integer := Source'First;
      Result  : Fuzzy_Number;
   begin
      Get (Source, Pointer);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer);
      if Pointer <= Source'Last then
         raise Data_Error;
      else
         return Result;
      end if;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Fuzzy_Number;
                Base        : in NumberBase := 10;
                RelSmall    : in Positive   := MaxSmall;
                AbsSmall    : in Integer    := -MaxSmall;
                Field       : in Natural    := 0;
                Justify     : in Alignment  := Left;
                Fill        : in Character  := ' '
             )  is
      Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
      First : Boolean := True;
   begin
      for Level in reverse Interval_Index'Range loop
         if (  Level = Interval_Index'Last
            or else
               (  From (Value.Set (Level))
               /= From (Value.Set (Interval_Index'Succ (Level)))
               )
            or else
               (  To (Value.Set (Level))
               /= To (Value.Set (Interval_Index'Succ (Level)))
            )  )
         then
            if First then
               First := False;
            else
               Put (Text, Index, ',');
            end if;
            Put
            (  Text,
               Index,
               From (Value.Set (Level)),
               Base,
               RelSmall,
               AbsSmall
            );
            if From (Value.Set (Level)) /= To (Value.Set (Level)) then
               Put (Text, Index, "..");
               Put
               (  Text,
                  Index,
                  To (Value.Set (Level)),
                  Base,
                  RelSmall,
                  AbsSmall
               );
            end if;
            if Confidence'Last /= To_Confidence (Level) then
               Put (Text, Index, ":");
               Put (Text, Index, To_Confidence (Level));
            end if;
         end if;
      end loop;
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

   function Image
            (  Value    : in Fuzzy_Number;
               Base     : in NumberBase := 10;
               RelSmall : in Positive   := MaxSmall;
               AbsSmall : in Integer    := -MaxSmall
            )  return String is
      Size_Inc : constant := 80;
      Size     : Natural := Size_Inc;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Positive := 1;
         begin
            Put (Text, Pointer, Value, Base, RelSmall, AbsSmall);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               null;
         end;
         Size := Size + Size_Inc;
      end loop;
   end Image;

end Fuzzy.Numbers.Edit;
