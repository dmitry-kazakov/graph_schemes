--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures.Linguistics.                 Luebeck            --
--        Sets                                     Autumn, 2006       --
--  Iimplementation                                                   --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;

package body Fuzzy.Measures.Linguistics.Sets is

   function Accumulate
            (  Set          : Set_Measure;
               Distribution : Fuzzy.Set
            )  return Variable_Measure is
      Result : Variable_Measure (Set.SI);
   begin
      Result.Gain   := Accumulate (Set.Gain, Distribution);
      Result.Offset := Set.Offset;
      return Result;
   end Accumulate;

   procedure Add
             (  Set   : in out Set_Measure;
                Name  : String;
                Value : Variable_Measure
             )  is
   begin
      if Value.SI /= Set.SI then
         raise Unit_Error;
      elsif Value.Offset = Set.Offset then
         Add (Set.Gain, Name, Value.Gain);
      else
         Add (Set.Gain, Name, Value.Gain + Value.Offset - Set.Offset);
      end if;
   end Add;

   function Classify_Empty (Set : Set_Measure) return Classification is
      pragma Inline (Classify_Empty);
      Result : Classification (Get_Cardinality (Set));
   begin
      for Index in 1..Result.Cardinality loop
         Result.Possibility (Index) := Confidence'First;
         Result.Necessity (Index)   := not Possibility (Get (Set, Index));
      end loop;
      return Result;
   end Classify_Empty;

   function Classify
            (  Set   : Set_Measure;
               Value : Measure
            )  return Classification is
   begin
      if Value.SI /= Set.SI then
         return Classify_Empty (Set);
      elsif Value.Offset = Set.Offset then
         return Classify (Set.Gain, Value.Gain);
      else
         return
            Classify
            (  Set.Gain,
               Value.Gain + Value.Offset - Set.Offset
            );
      end if;
   end Classify;

   function Classify
            (  Set   : Set_Measure;
               Value : Interval_Measure
            )  return Classification is
   begin
      if Value.SI /= Set.SI then
         return Classify_Empty (Set);
      elsif Value.Offset = Set.Offset then
         return Classify (Set.Gain, Interval'(Value.From, Value.To));
      else
         return
            Classify
            (  Set.Gain,
               (  (  Interval'(Value.From, Value.To)
                  +  Value.Offset
                  )
               -  Set.Offset
            )  );
      end if;
   end Classify;

   function Classify
            (  Set   : Set_Measure;
               Value : Fuzzy_Measure
            )  return Classification is
   begin
      if Value.SI /= Set.SI then
         return Classify_Empty (Set);
      elsif Value.Offset = Set.Offset then
         return Classify (Set.Gain, Value.Gain);
      else
         return
            Classify
            (  Set.Gain,
               Value.Gain + Value.Offset - Set.Offset
            );
      end if;
   end Classify;

   function Classify
            (  Set   : Set_Measure;
               Value : Variable_Measure
            )  return Classification is
   begin
      if Value.SI /= Set.SI then
         return Classify_Empty (Set);
      elsif Value.Offset = Set.Offset then
         return Classify (Set.Gain, Value.Gain);
      else
         return
            Classify
            (  Set.Gain,
               Value.Gain + Value.Offset - Set.Offset
            );
      end if;
   end Classify;

   function Defuzzify
            (  Set   : Set_Measure;
               Value : Classification
            )  return Defuzzification_Result is
      Result : constant Fuzzy_Linguistic_Sets.Defuzzification_Result :=
                  Defuzzify (Set.Gain, Value);
   begin
      case Result.Class is
         when Empty =>
            return (Class => Empty);
         when Singleton =>
            return
            (  Class     => Singleton,
               Singleton =>
               (  SI     => Set.SI,
                  Gain   => Result.Singleton,
                  Offset => Set.Offset
            )  );
         when Segment =>
            return
            (  Class   => Segment,
               Segment =>
               (  SI     => Set.SI,
                  From   => Result.Segment.From,
                  To     => Result.Segment.To,
                  Offset => Set.Offset
            )  );
         when Subset =>
            return
            (  Class  => Subset,
               Subset =>
               (  SI     => Set.SI,
                  Gain   => Result.Subset,
                  Offset => Set.Offset
            )  );
      end case;
   end Defuzzify;

   procedure Erase (Set : in out Set_Measure) is
   begin
      Erase (Set.Gain);
   end;

   function Empty (Scale : Measure) return Set_Measure is
   begin
      return
      (  SI     => Scale.SI,
         Gain   => Empty,
         Offset => Scale.Offset
      );
   end Empty;

   function Empty (SI : Unit; Offset : Number'Base := 0.0)
      return Set_Measure is
   begin
      return
      (  SI     => SI,
         Gain   => Empty,
         Offset => Offset
      );
   end Empty;

   function Equal
            (  Left, Right : Set_Measure;
               Eps         : Number'Base := 0.0
            )  return Boolean is
   begin
      if Left.SI /= Right.SI then
         return False;
      elsif Left.Offset = Right.Offset then
         return Equal (Left.Gain, Right.Gain);
      end if;
      declare
         Size : constant Natural := Get_Cardinality (Left);
      begin
         if Get_Cardinality (Right) /= Size then
            return False;
         elsif Size = 0 then
            return True;
         end if;
         declare
            function Same (Index : Positive) return Boolean is
               First  : String renames Get_Name (Left.Gain,  Index);
               Second : String renames Get_Name (Right.Gain, Index);
            begin
               if First'Length /= Second'Length then
                  return False;
               else
                  for Index in First'Range loop
                     if (  To_Lower (First  (Index))
                        /= To_Lower (Second (Index))
                        )
                     then
                        return False;
                     end if;
                  end loop;
               end if;
               return True;
            end Same;
         begin
            for Index in 1..Size loop
               if not (  Same (Index)
                      and then
                         Equal
                         (  Get (Left,  Index),
                            Get (Right, Index),
                            Eps
                      )  )
               then
                  return False;
               end if;
            end loop;
         end;
         return True;
      end;
   end Equal;

   function Get
            (  Set   : Set_Measure;
               Index : Positive
            )  return Variable_Measure is
   begin
      return
      (  SI     => Set.SI,
         Gain   => Get (Set.Gain, Index),
         Offset => Set.Offset
      );
   end Get;

   function Get
            (  Set  : Set_Measure;
               Name : String
            )  return Positive is
   begin
      return Get (Set.Gain, Name);
   end Get;

   function Get (Set : Set_Measure; Name : String)
      return Variable_Measure is
   begin
      return
      (  SI     => Set.SI,
         Gain   => Get (Set.Gain, Name),
         Offset => Set.Offset
      );
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Set_Measure;
                Index   : out Positive
             )  is
   begin
      Get (Source, Pointer, Set.Gain, Index);
   end Get;

   function Get_Cardinality (Set : Set_Measure) return Natural is
   begin
      return Get_Cardinality (Set.Gain);
   end Get_Cardinality;

   function Get_Domain (Set : Set_Measure)
      return Domain_Description_Ptr is
   begin
      return Get_Domain (Set.Gain);
   end Get_Domain;

   function Get_Name (Set : Set_Measure; Index : Positive)
      return String is
   begin
      return Get_Name (Set.Gain, Index);
   end Get_Name;

   function Get_Unit (Set : Set_Measure) return Unit is
   begin
      return Set.SI;
   end Get_Unit;

   function Get_Value (Set : Set_Measure) return Linguistic_Set is
      Result : Linguistic_Set;
   begin
      for Index in 1..Get_Cardinality (Set) loop
         Add
         (  Result,
            Get_Name (Set, Index),
            Get (Set.Gain, Index) + Set.Offset
         );
      end loop;
      return Result;
   end Get_Value;

   function Get_Value_As (Set : Set_Measure; Scale : Measure)
      return Linguistic_Set is
      Result : Linguistic_Set;
   begin
      if Set.SI /= Scale.SI then
         raise Unit_Error;
      end if;
      for Index in 1..Get_Cardinality (Set) loop
         Add
         (  Result,
            Get_Name (Set.Gain, Index),
            (  (Get (Set.Gain, Index) + (Set.Offset - Scale.Offset))
            /  Scale.Gain
         )  );
      end loop;
      return Result;
   end Get_Value_As;

   procedure Insert
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String;
                Value : Variable_Measure
             )  is
   begin
      if Value.SI /= Set.SI then
         raise Unit_Error;
      elsif Value.Offset = Set.Offset then
         Insert (Set.Gain, Index, Name, Value.Gain);
      else
         Insert
         (  Set.Gain,
            Index,
            Name,
            Value.Gain + Value.Offset - Set.Offset
         );
      end if;
   end Insert;

   function Is_Empty (Set : Set_Measure) return Boolean is
   begin
      return Is_Empty (Set.Gain);
   end Is_Empty;

   procedure Move
             (  Set  : in out Set_Measure;
                From : Positive;
                To   : Positive
             )  is
   begin
      Move (Set.Gain, From, To);
   end Move;

   procedure Remove (Set : in out Set_Measure; Index : Positive) is
   begin
      Remove (Set.Gain, Index);
   end Remove;

   procedure Remove (Set : in out Set_Measure; Name : String) is
   begin
      Remove (Set.Gain, Name);
   end Remove;

   procedure Rename
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String
             )  is
   begin
      Rename (Set.Gain, Index, Name);
   end Rename;

   procedure Replace
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String;
                Value : Variable_Measure
             )  is
   begin
      if Value.SI /= Set.SI then
         raise Unit_Error;
      elsif Value.Offset = Set.Offset then
         Replace (Set.Gain, Index, Name, Value.Gain);
      else
         Replace
         (  Set.Gain,
            Index,
            Name,
            Value.Gain + Value.Offset - Set.Offset
         );
      end if;
   end Replace;

   procedure Replace
             (  Set   : in out Set_Measure;
                Index : Positive;
                Value : Variable_Measure
             )  is
   begin
      if Value.SI /= Set.SI then
         raise Unit_Error;
      elsif Value.Offset = Set.Offset then
         Replace (Set.Gain, Index, Value.Gain);
      else
         Replace
         (  Set.Gain,
            Index,
            Value.Gain + Value.Offset - Set.Offset
         );
      end if;
   end Replace;

   procedure Swap
             (  Set     : in out Set_Measure;
                Index_1 : Positive;
                Index_2 : Positive
             )  is
   begin
      Swap (Set.Gain, Index_1, Index_2);
   end Swap;

   function To_Set
            (  Set   : Set_Measure;
               Value : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Set.SI /= Value.SI then
         return
         (  Cardinality => Get_Cardinality (Set),
            Possibility | Necessity => (others => Confidence'First)
         );
      elsif Set.Offset = Value.Offset then
         return To_Set (Set.Gain, Value.Gain);
      else
         return
            To_Set (Set.Gain, Value.Gain + Value.Offset - Set.Offset);
      end if;
   end To_Set;

   function To_Set
            (  Set   : Set_Measure;
               Value : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Set.SI /= Value.SI then
         return
         (  Cardinality => Get_Cardinality (Set),
            Possibility | Necessity => (others => Confidence'First)
         );
      elsif Set.Offset = Value.Offset then
         return To_Set (Set.Gain, Interval'(Value.From, Value.To));
      else
         return
            To_Set
            (  Set.Gain,
               (  (  Interval'(Value.From, Value.To)
                  +  Value.Offset
                  )
               -  Set.Offset
            )  );
      end if;
   end To_Set;

   function To_Set
            (  Set   : Set_Measure;
               Value : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Set.SI /= Value.SI then
         return
         (  Cardinality => Get_Cardinality (Set),
            Possibility | Necessity => (others => Confidence'First)
         );
      elsif Set.Offset = Value.Offset then
         return To_Set (Set.Gain, Value.Gain);
      else
         return
            To_Set
            (  Set.Gain,
               Value.Gain +
               (  Interval'(Value.Offset, Value.Offset)
               -  Set.Offset
            )  );
      end if;
   end To_Set;

   function To_Set
            (  Set   : Set_Measure;
               Value : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Set.SI /= Value.SI then
         return
         (  Cardinality => Get_Cardinality (Set),
            Possibility => (others => Confidence'First),
            Necessity   => (others => not Possibility (Value))
         );
      elsif Set.Offset = Value.Offset then
         return To_Set (Set.Gain, Value.Gain);
      else
         return
            To_Set
            (  Set.Gain,
               Value.Gain + (Value.Offset - Set.Offset)
            );
      end if;
   end To_Set;

end Fuzzy.Measures.Linguistics.Sets;
