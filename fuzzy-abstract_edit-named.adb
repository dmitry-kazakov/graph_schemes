--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Abstract_Edit.Named                   Luebeck            --
--  Implementation                                 Summer, 2003       --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;     use Fuzzy.Basic_Edit;
with Strings_Edit.Fields;  use Strings_Edit.Fields;

package body Fuzzy.Abstract_Edit.Named is

   procedure Add
             (  Domain    : in out Domain_Description;
                Name      : String;
                Index     : Integer;
                Unchecked : Boolean := False
             )  is
      Offset : Positive;
   begin
      if Index in Domain.First..Domain.Last then
         -- Inserted
         Put (Domain.Index_Map, Domain.Last + 1, 1); -- Allocate index
         begin
            --
            -- Increasing by one the indices of the moved  elements  and
            -- correcting them in the table
            --
            for Old_Index in reverse Index..Domain.Last loop
               Replace
               (  Domain.Name_Map,
                  Get (Domain.Index_Map, Old_Index),
                  Old_Index + 1
               );
            end loop;
            if Unchecked then
               Name_Tables.String_Maps.Add
               (  Name_Tables.String_Maps.Table (Domain.Name_Map),
                  Name,
                  Index,
                  Offset
               );
            else
               Add (Domain.Name_Map, Name, Index, Offset);
            end if;
         exception
            when others =>
               -- Rolling back the changes in the indices
               for Old_Index in reverse Index..Domain.Last loop
                  Replace
                  (  Domain.Name_Map,
                     Get (Domain.Index_Map, Old_Index),
                     Old_Index
                  );
               end loop;
               raise;
         end;
         Domain.Last := Domain.Last + 1;
      else
         -- Added
         if Domain.First <= Domain.Last then
            if Index < Domain.First then
               if Index + 1 /= Domain.First then
                  raise Constraint_Error;
               end if;
            else
               if Index - 1 /= Domain.Last then
                  raise Constraint_Error;
               end if;
            end if;
         end if;
         Put (Domain.Index_Map, Index, 1); -- Allocate index
         if Unchecked then
            Name_Tables.String_Maps.Add
            (  Name_Tables.String_Maps.Table (Domain.Name_Map),
               Name,
               Index,
               Offset
            );
         else
            Add (Domain.Name_Map, Name, Index, Offset);
         end if;
         if Domain.First <= Domain.Last then
            if Domain.First > Index then
               Domain.First := Index;
            else
               Domain.Last  := Index;
            end if;
         else
            Domain.First := Index;
            Domain.Last  := Index;
         end if;
      end if;
      --
      -- Remapping all names starting from the inserted one to the  last
      -- name in the table. These names have new offsets.
      --
      for Item in Offset..GetSize (Domain.Name_Map) loop
         Put (Domain.Index_Map, GetTag (Domain.Name_Map, Item), Item);
      end loop;
   end Add;

   procedure Erase (Domain : in out Domain_Description) is
   begin
      Domain.First := 1;
      Domain.Last  := 0;
      Erase (Domain.Name_Map);
   end Erase;

   procedure Copy
             (  Source : Domain_Description;
                Target : in out Domain_Description'Class
             )  is
   begin
      Target.First    := Source.First;
      Target.Last     := Source.Last;
      Target.Name_Map := Source.Name_Map;
      for Index in Target.First..Target.Last loop
         Put (Target.Index_Map, Index, Get (Source.Index_Map, Index));
      end loop;
   end Copy;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Domain  : Domain_Description;
                Index   : out Integer
             )  is
  begin
      Get (Source, Pointer, Domain.Name_Map, Index);
   end Get;

   function Get_Index
            (  Domain : Domain_Description;
               Name   : String
            )  return Integer is
   begin
      return Find (Domain.Name_Map, Name);
   end Get_Index;

   function Get_Name
            (  Domain : Domain_Description;
               Index  : Integer
            )  return String is
   begin
      return GetName (Domain.Name_Map, Get (Domain.Index_Map, Index));
   exception
      when End_Error =>
         raise Constraint_Error;
   end Get_Name;

   procedure Get_Max_Range
             (  Domain  : Domain_Description;
                First   : out Integer;
                Last    : out Integer
             )  is
   begin
      First := Domain.First;
      Last  := Domain.Last;
   end Get_Max_Range;

   function Get_Cardinality (Domain : Domain_Description)
      return Natural is
   begin
      return Domain.Last - Domain.First + 1;
   end Get_Cardinality;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Domain  : Domain_Description;
                From    : out Integer;
                To      : out Integer
             )  is
      Got_It : Boolean;
      First  : Integer;
   begin
      Get (Source, Pointer, Domain.Name_Map, First);
      Get_Delimiter (Source, Pointer, Got_It, Ellipsis);
      if Got_It then
         declare
            Last : Integer;
         begin
            Get (Source, Pointer, Domain.Name_Map, Last);
            if Last < First then
               raise Data_Error;
            end if;
            To := Last;
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         To := First;
      end if;
      From := First;
   end Get;

   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Domain    : Domain_Description;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             )  is
      First : Integer;
      Last  : Integer;
   begin
      Get (Source, Pointer, Domain, First, Last);
      From := Value_Index (First - Domain.First + 1);
      To   := Value_Index (Last  - Domain.First + 1);
      Exclusive := True;
   end Get;

   function Is_Empty (Domain : Domain_Description) return Boolean is
   begin
      return Domain.Last < Domain.First;
   end Is_Empty;

   procedure Move
             (  Domain : in out Domain_Description;
                From   : Integer;
                To     : Integer
             )  is
   begin
      if (  From not in Domain.First..Domain.Last
         or else
            To not in Domain.First..Domain.Last
         )
      then
         raise Constraint_Error;
      end if;
      if From /= To then
         declare
            Moved  :constant  Positive := Get (Domain.Index_Map, From);
            Offset : Positive;
         begin
            if From < To then
               --
               -- Decreasing the indices of the elements From + 1..To in
               -- the  table  and then correcting offsets to their names
               -- in the index map.
               --
               for Old_Index in From + 1..To loop
                  Offset := Get (Domain.Index_Map, Old_Index);
                  Replace (Domain.Name_Map, Offset, Old_Index - 1);
                  Domain.Index_Map.Vector (Old_Index - 1) := Offset;
               end loop;
            else
               --
               -- Increasing the indices of the elements To..From - 1 in
               -- the  table  and then correcting offsets to their names
               -- in the index map.
               --
               for Old_Index in reverse To..From - 1 loop
                  Offset := Get (Domain.Index_Map, Old_Index);
                  Replace (Domain.Name_Map, Offset, Old_Index + 1);
                  Domain.Index_Map.Vector (Old_Index + 1) := Offset;
               end loop;
            end if;
            Replace (Domain.Name_Map, Moved, To);
            Domain.Index_Map.Vector (To) := Moved;
         end;
      end if;
   end Move;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Domain      : Domain_Description;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      if (  From > To
         or else
            From < Domain.First
         or else
            To > Domain.Last
         )
      then
         raise Constraint_Error;
      end if;
      Put
      (  Text,
         Index,
         GetName (Domain.Name_Map, Domain.Index_Map.Vector (From))
      );
      if From /= To then
         Put (Text, Index, "..");
         Put
         (  Text,
            Index,
            GetName (Domain.Name_Map, Domain.Index_Map.Vector (To))
         );
      end if;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   exception
      when End_Error =>
         raise Constraint_Error;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Domain      : Domain_Description;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Domain      => Domain_Description'Class (Domain),
         From        => Integer (From) + Domain.First - 1,
         To          => Integer (To)   + Domain.First - 1,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Remove
             (  Domain : in out Domain_Description;
                Index  : Integer
             )  is
   begin
      if Index not in Domain.First..Domain.Last then
         raise Constraint_Error;
      end if;
      if Domain.Last = Domain.First then
         Erase (Domain);
      else
         Delete (Domain.Name_Map, Get (Domain.Index_Map, Index));
         declare
            Item : Integer;
         begin
            --
            -- All table items are remapped, those  which  indices  were
            -- greater than ones of  the  removed  element  receive  the
            -- index decreased by one.
            --
            for Offset in 1..GetSize (Domain.Name_Map) loop
               Item := GetTag (Domain.Name_Map, Offset);
               if Item > Index then
                  Item := Item - 1;
                  Replace (Domain.Name_Map, Offset, Item);
               end if;
               Domain.Index_Map.Vector (Item) := Offset;
            end loop;
         end;
         Domain.Last := Domain.Last - 1;
      end if;
   end Remove;

   procedure Remove
             (  Domain : in out Domain_Description;
                Name   : String
             )  is
   begin
      Remove (Domain, Find (Domain.Name_Map, Name));
   exception
      when End_Error =>
         null;
   end Remove;

   procedure Rename
             (  Domain    : in out Domain_Description;
                Index     : Integer;
                Name      : String;
                Unchecked : Boolean := False
             )  is
   begin
      if Index not in Domain.First..Domain.Last then
         raise Constraint_Error;
      end if;
      Name_Tables.Check_Name (Name);
      declare
         From : constant Positive := Get (Domain.Index_Map, Index);
         To   : Positive;
      begin
         if GetName (Domain.Name_Map, From) /= Name then
            Delete (Domain.Name_Map, From);
            if Unchecked then
               Name_Tables.String_Maps.Add
               (  Name_Tables.String_Maps.Table (Domain.Name_Map),
                  Name,
                  Index,
                  To
               );
            else
               Add (Domain.Name_Map, Name, Index, To);
            end if;
            if From /= To then
               --
               -- Remapping all table items  between  the  old  and  new
               -- offsets
               --
               if From < To then
                  for Offset in From..To loop
                     Put
                     (  Domain.Index_Map,
                        GetTag (Domain.Name_Map, Offset),
                        Offset
                     );
                  end loop;
               else
                  for Offset in To..From loop
                     Put
                     (  Domain.Index_Map,
                        GetTag (Domain.Name_Map, Offset),
                        Offset
                     );
                  end loop;
               end if;
            end if;
         end if;
      end;
   end Rename;

   procedure Rename
             (  Domain    : in out Domain_Description;
                Old_Name  : String;
                New_Name  : String;
                Unchecked : Boolean := False
             )  is
   begin
      Rename
      (  Domain,
         Find (Domain.Name_Map, Old_Name),
         New_Name,
         Unchecked
      );
   end Rename;

   procedure Swap
             (  Domain  : in out Domain_Description;
                Index_1 : Integer;
                Index_2 : Integer
             )  is
   begin
      if (  Index_1 not in Domain.First..Domain.Last
         or else
            Index_2 not in Domain.First..Domain.Last
         )
      then
         raise Constraint_Error;
      end if;
      declare
         Item_1 : constant Positive := Get (Domain.Index_Map, Index_1);
         Item_2 : constant Positive := Get (Domain.Index_Map, Index_2);
      begin
         Domain.Index_Map.Vector (Index_1) := Item_2;
         Domain.Index_Map.Vector (Index_2) := Item_1;
         Replace (Domain.Name_Map, Item_1, Index_2);
         Replace (Domain.Name_Map, Item_2, Index_1);
      end;
   end Swap;

end Fuzzy.Abstract_Edit.Named;
