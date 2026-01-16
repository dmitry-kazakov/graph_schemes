--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Samples_Lists                         Luebeck            --
--  Implementation                                 Autumn, 2006       --
--                                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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

package body Fuzzy.Samples_Lists is

   use Examples_Number_List.Segmented_Stack;
   use Examples_Ranges_List.Segmented_Stack;

   procedure Add (List : in out Examples_List; Example : Positive) is
   begin
      if not Is_Empty (List.Ranges) then
         declare
            Last : Examples_Range := Top (List.Ranges);
         begin
            if Last.To >= Example then
               return;
            elsif Last.To + 1 = Example then
               --
               -- Adding Example to the last range
               --
               Last.To := Example;
               Put (List.Ranges, Mark (List.Ranges), Last);
               return;
            end if;
         end;
      end if;
      if not Is_Empty (List.Singular) then
         declare
            Last : Positive := Top (List.Singular);
         begin
            if Last >= Example then
               return;
            elsif Last + 1 = Example then
               --
               -- Converting the last example to a range with Example
               --
               Push (List.Ranges, (Example - 1, Example));
               Pop (List.Singular);
               return;
            end if;
         end;
      end if;
      Push (List.Singular, Example);
   end Add;

   function Get (List : Examples_List; Index : Examples_List_Position)
      return Examples_Range is
   begin
      if Index.Singular <= Mark (List.Singular) then
         if Index.Ranges <= Mark (List.Ranges) then
            declare
               Interval : Examples_Range;
               Example  : Positive;
            begin
               Interval := Get (List.Ranges,   Index.Ranges);
               Example  := Get (List.Singular, Index.Singular);
               if Example < Interval.From then
                  return (Example, Example);
               else
                  return Interval;
               end if;
            end;
         else
            declare
               Example : Positive :=
                            Get (List.Singular, Index.Singular);
            begin
               return (Example, Example);
            end;
         end if;
      else
         if Index.Ranges <= Mark (List.Ranges) then
            return Get (List.Ranges, Index.Ranges);
         else
            raise Constraint_Error;
         end if;
      end if;
   end Get;

   function Is_Empty (List : Examples_List) return Boolean is
   begin
      return Is_Empty (List.Singular) and then Is_Empty (List.Ranges);
   end Is_Empty;

   function Is_In (List : Examples_List; Index : Examples_List_Position)
      return Boolean is
   begin
      return
      (  Index.Singular <= Mark (List.Singular)
      or else
         Index.Ranges <= Mark (List.Ranges)
      );
   end Is_In;

   function Next (List : Examples_List; Index : Examples_List_Position)
      return Examples_List_Position is
   begin
      if Index.Singular <= Mark (List.Singular) then
         if Index.Ranges <= Mark (List.Ranges) then
            if (  Get (List.Singular, Index.Singular)
               <  Get (List.Ranges,   Index.Ranges).From
               )
            then
               return (Index.Singular + 1, Index.Ranges);
            else
               return (Index.Singular, Index.Ranges + 1);
            end if;
         else
            return (Index.Singular + 1, Index.Ranges);
         end if;
      else
         if Index.Ranges <= Mark (List.Ranges) then
            return (Index.Singular, Index.Ranges + 1);
         else
            raise Constraint_Error;
         end if;
      end if;
   end Next;

end Fuzzy.Samples_Lists;
