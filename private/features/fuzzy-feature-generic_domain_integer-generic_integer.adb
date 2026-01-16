--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Integer.       Luebeck            --
--        Generic_Integer                          Summer, 2002       --
--  Implementation                                                    --
--                                Last revision :  07:55 21 Jul 2016  --
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
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

package body Fuzzy.Feature.Generic_Domain_Integer.Generic_Integer is
   use Integer_Edit;

   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Classification is
      Point  : constant Domain_Integer'Base := Value - Feature.From + 1;
      Result : Fuzzy.Intuitionistic.Classification :=
               (  Cardinality => Feature.Cardinality,
                  Possibility => (others => Confidence'First),
                  Necessity   => (others => Confidence'First)
               );
   begin
      if Point in 1..Domain_Integer'Base (Feature.Cardinality) then
         Result.Possibility (Integer (Point)) := Confidence'Last;
         Result.Necessity   (Integer (Point)) := Confidence'Last;
      end if;
      return Result;
   end Classify;

   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      if Value.From = Value.To then
         return Classify (Feature, Value.From);
      else
         declare
            Cardinality : constant Domain_Integer'Base :=
                             Domain_Integer'Base (Feature.Cardinality);
            Points : constant Interval :=
               Value - Integer_Intervals.Number_Of'(Feature.From - 1);
            Result : Fuzzy.Intuitionistic.Classification :=
                     (  Cardinality => Feature.Cardinality,
                        Possibility => (others => Confidence'First),
                        Necessity   => (others => Confidence'First)
                     );
         begin
            if (  Points.From <= Cardinality
               and then
                  Points.To >= 1
               )
            then
               for Point in Integer
                            (  Domain_Integer'Base'Max
                               (  1,
                                  Points.From
                            )  )
                         .. Integer
                            (  Domain_Integer'Base'Min
                               (  Points.To,
                                  Cardinality
                            )  )
               loop
                  Result.Possibility (Point) := Confidence'Last;
                  Result.Necessity   (Point) := Confidence'Last;
               end loop;
            end if;
            return Result;
         end;
      end if;
   end Classify;

   function Classify
            (  Feature : Integer_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Classification is
      Point  : Domain_Integer := Feature.From;
      Result : Fuzzy.Intuitionistic.Classification (Feature.Cardinality);
   begin
      for Index in Result.Possibility'Range loop
         Result.Possibility (Index) := Possibility (Point, Value);
         Result.Necessity   (Index) := Necessity   (Point, Value);
         Point := Point + 1;
      end loop;
      return Result;
   end Classify;

   function Get_Class (Feature : Integer_Feature_Object)
      return String is
   begin
      return Integer_Class;
   end Get_Class;

   function Get_From (Feature : Feature_Object'Class)
      return Domain_Integer is
   begin
      return Integer_Feature_Object'Class (Feature).From;
   end Get_From;

   function Get_From (Feature : Feature_Handle)
      return Domain_Integer is
   begin
      return Integer_Feature_Object'Class (Ptr (Feature).all).From;
   end Get_From;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Integer_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
      Got_It : Boolean;
      First  : constant Domain_Integer'Base := Feature.From;
      Last   : constant Domain_Integer'Base :=
                  (  Feature.From
                  +  (  Domain_Integer'Base (Feature.Cardinality)
                     -  1
                  )  );
      Lower  : Domain_Integer'Base;
      Upper  : Domain_Integer'Base;
   begin
      Get
      (  Source  => Source,
         Pointer => Pointer,
         Value   => Lower,
         Base    => Parameters.Base,
         First   => First,
         Last    => Last
      );
      Get_Delimiter (Source, Pointer, Got_It, Ellipsis);
      if Got_It then
         begin
            Get
            (  Source  => Source,
               Pointer => Pointer,
               Value   => Upper,
               Base    => Parameters.Base,
               First   => First,
               Last    => Last
            );
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         Upper := Lower;
      end if;
      if Lower > Upper then
         raise Data_Error;
      end if;
      From      := Positive ((Lower - First) + 1);
      To        := Positive ((Upper - First) + 1);
      Exclusive := True;
   end Get_Range;

   function Get_To (Feature : Feature_Object'Class)
      return Domain_Integer is
      Object : Integer_Feature_Object'Class renames
                  Integer_Feature_Object'Class (Feature);
   begin
      return
      (  Object.From
      +  (Domain_Integer'Base (Object.Cardinality) - 1)
      );
   end Get_To;

   function Get_To (Feature : Feature_Handle)
      return Domain_Integer is
      Object : Integer_Feature_Object'Class renames
                  Integer_Feature_Object'Class (Ptr (Feature).all);
   begin
      return
      (  Object.From
      +  (Domain_Integer'Base (Object.Cardinality) - 1)
      );
   end Get_To;

   function Is_Integer (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Integer_Feature_Object'Class;
   end Is_Integer;

   function Is_Integer (Feature : Feature_Handle) return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
         This /= null and then This.all in Integer_Feature_Object'Class;
   end Is_Integer;

   function Constructor
            (  Name : String;
               From : Domain_Integer;
               To   : Domain_Integer
            )  return Feature_Object_Ptr is
      Result : Feature_Object_Ptr;
   begin
      if From > To then
         raise End_Error;
      end if;
      Result := new Integer_Feature_Object (Natural (To - From) + 1);
      Result.Self := Result;
      Result.Name := new String'(Name);
      Integer_Feature_Object (Result.all).From := From;
      return Result;
   end Constructor;

   function Create
            (  Name : String;
               From : Domain_Integer;
               To   : Domain_Integer
            )  return Feature_Handle is
   begin
      return Ref (Constructor (Name, From, To));
   end Create;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Integer_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
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
      if To > Feature.Cardinality or else From > To then
         raise Constraint_Error;
      end if;
      Put
      (  Destination => Text,
         Pointer     => Index,
         Value       => Feature.From + (Domain_Integer'Base (From) - 1),
         Base        => Parameters.Base,
         PutPlus     => Parameters.Put_Plus
      );
      if From /= To then
         Put (Text, Index, "..");
         Put
         (  Destination => Text,
            Pointer     => Index,
            Value       => Feature.From + (Domain_Integer'Base (To) - 1),
            Base        => Parameters.Base,
            PutPlus     => Parameters.Put_Plus
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
   end Put_Range;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      From, To : Domain_Integer;
      Got_It   : Boolean         := False;
      Position : aliased Integer := Pointer;
      Name     : constant String :=
                    Get_Quoted (Source, Position'Access);
   begin
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      Get (Source, Position, From);
      Get_Delimiter (Source, Position, Got_It, Ellipsis);
      if not Got_It then
         raise Data_Error;
      end if;
      Get (Source, Position, To);
      if From > To then
         raise Data_Error;
      end if;
      Feature := To_Deposit_Ptr (Constructor (Name, From, To));
      Pointer := Position;
   exception
      when Constraint_Error =>
         raise Data_Error;
      when End_Error =>
         if Got_It then
            raise Data_Error;
         else
            raise;
         end if;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Integer_Feature_Object
             )  is
      Position : aliased Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Put (Destination, Position, Feature.From);
      Put (Destination, Position, "..");
      Put (Destination, Position, Get_To (Feature));
      Pointer := Position;
   end Store;

   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Domain_Integer
            )  return Fuzzy.Intuitionistic.Set is
      Point  : constant Domain_Integer'Base := Value - Feature.From + 1;
      Result : Fuzzy.Intuitionistic.Set :=
               (  Cardinality => Feature.Cardinality,
                  Possibility => (others => Confidence'First),
                  Necessity   => (others => Confidence'First)
               );
   begin
      if Point in 1..Domain_Integer'Base (Feature.Cardinality) then
         Result.Possibility (Integer (Point)) := Confidence'Last;
         Result.Necessity   (Integer (Point)) := Confidence'Last;
      end if;
      return Result;
   end To_Set;

   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Interval
            )  return Fuzzy.Intuitionistic.Set is
      Cardinality : constant Domain_Integer'Base :=
                       Domain_Integer'Base (Feature.Cardinality);
      Points : constant Interval :=
               Value - Integer_Intervals.Number_Of'(Feature.From - 1);
      Result : Fuzzy.Intuitionistic.Set :=
               (  Cardinality => Feature.Cardinality,
                  Possibility => (others => Confidence'First),
                  Necessity   => (others => Confidence'First)
               );
   begin
      if (  Points.From <= Cardinality
         and then
            Points.To >= 1
         )
      then
         for Point in Integer
                      (  Domain_Integer'Base'Max
                         (  1,
                            Points.From
                      )  )
                   .. Integer
                      (  Domain_Integer'Base'Min
                         (  Points.To,
                            Cardinality
                      )  )
         loop
            Result.Possibility (Point) := Confidence'Last;
            Result.Necessity   (Point) := Confidence'Last;
         end loop;
      end if;
      return Result;
   end To_Set;

   function To_Set
            (  Feature : Integer_Feature_Object;
               Value   : Fuzzy_Integer
            )  return Fuzzy.Intuitionistic.Set is
      Point  : Domain_Integer := Feature.From;
      Result : Fuzzy.Intuitionistic.Set (Feature.Cardinality);
   begin
      for Index in Result.Possibility'Range loop
         Result.Possibility (Index) := Possibility (Value, Point);
         Result.Necessity   (Index) := Necessity   (Value, Point);
         Point := Point + 1;
      end loop;
      return Result;
   end To_Set;

begin
   if not Is_Registered (Integer_Class) then
      Register_Class (Integer_Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Integer.Generic_Integer;
