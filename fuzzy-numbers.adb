--                                                                    --
--  package Fuzzy.Numbers           Copyright (c)  Dmitry A. Kazakov  --
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

package body Fuzzy.Numbers is
   function Equal (Left : Number; Right : Interval) return Boolean;
   pragma Inline (Equal);

   generic
      with function Operation (Left, Right : Interval)
         return Interval;
      with function Operation (Left : Interval; Right : Number)
         return Interval;
      with function Operation (Left : Number; Right : Interval)
         return Interval;
   package Arithmetic is
      function Fuzzy_x_Fuzzy (Left, Right : Fuzzy_Number)
         return Fuzzy_Number;
      function Fuzzy_x_Number (Left : Fuzzy_Number; Right : Number)
         return Fuzzy_Number;
      function Number_x_Fuzzy (Left : Number; Right : Fuzzy_Number)
         return Fuzzy_Number;
      function Fuzzy_x_Interval (Left : Fuzzy_Number; Right : Interval)
         return Fuzzy_Number;
      function Interval_x_Fuzzy (Left : Interval; Right : Fuzzy_Number)
         return Fuzzy_Number;
   end Arithmetic;

   generic
      with function Compare (Left, Right : Interval)
         return Logical;
      with function Compare (Left : Interval; Right : Number)
         return Logical;
      with function Compare (Left : Number; Right : Interval)
         return Logical;
   package Relational is
      function Fuzzy_vs_Fuzzy (Left, Right : Fuzzy_Number)
         return Fuzzy_Boolean;
      function Fuzzy_vs_Number (Left : Fuzzy_Number; Right : Number)
         return Fuzzy_Boolean;
      function Number_vs_Fuzzy (Left : Number; Right : Fuzzy_Number)
         return Fuzzy_Boolean;
      function Fuzzy_vs_Interval (Left : Fuzzy_Number; Right : Interval)
         return Fuzzy_Boolean;
      function Interval_vs_Fuzzy (Left : Interval; Right : Fuzzy_Number)
         return Fuzzy_Boolean;
   end Relational;

   function Is_In (Left, Right : Fuzzy_Number) return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (Left, Right),
         Necessity   => Necessity   (Right, Left)
      );
   end Is_In;

   function Is_In (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (Left, Right),
         Necessity   => Necessity   (Right, Left)
      );
   end Is_In;

   function Is_In (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (Left, Right),
         Necessity   => Necessity   (Right, Left)
      );
   end Is_In;

   function Is_In (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (Left, Right),
         Necessity   => Necessity   (Right, Left)
      );
   end Is_In;

   function Is_In (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility => Possibility (Left, Right),
         Necessity   => Necessity   (Right, Left)
      );
   end Is_In;

   function Equal (Left : Number; Right : Interval) return Boolean is
   begin
      return Left = From (Right) and then Left = To (Right);
   end Equal;

   function Necessity (Left : Number; Right : Fuzzy_Number)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if not Equal (Left, Right.Set (Index)) then
            return not To_Confidence (Index);
         end if;
      end loop;
      return Confidence'Last;
   end Necessity;

   function Necessity (Left : Fuzzy_Number; Right : Number)
      return Confidence renames Possibility;

   function Necessity (Left : Interval; Right : Fuzzy_Number)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if not Is_In (Right.Set (Index), Left) then
            return not To_Confidence (Index);
         end if;
      end loop;
      return Confidence'Last;
   end Necessity;

   function Necessity (Left : Fuzzy_Number; Right : Interval)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if Is_In (Right, Left.Set (Index)) then
            return To_Confidence (Index);
         end if;
      end loop;
      return Confidence'First;
   end Necessity;

   function Necessity (Left, Right : Fuzzy_Number)
      return Confidence is
      Result : Confidence := Confidence'Last;
   begin
      for Index in Interval_Index'Range loop
         Result :=
            (  Result
            and
               (  Necessity (Left, Right.Set (Index))
               or not To_Confidence (Index)
            )  );
      end loop;
      return Result;
   end Necessity;

   function Possibility (Left : Number; Right : Fuzzy_Number)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if Is_In (Left, Right.Set (Index)) then
            return To_Confidence (Index);
         end if;
      end loop;
      return Confidence'First;
   end Possibility;

   function Possibility (Left : Fuzzy_Number; Right : Number)
      return Confidence is
   begin
      return Possibility (Right, Left);
   end Possibility;

   function Possibility (Left : Interval; Right : Fuzzy_Number)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if Left & Right.Set (Index) then
            return To_Confidence (Index);
         end if;
      end loop;
      return Confidence'First;
   end Possibility;

   function Possibility (Left : Fuzzy_Number; Right : Interval)
      return Confidence is
   begin
      return Possibility (Right, Left);
   end Possibility;

   function Possibility (Left, Right : Fuzzy_Number)
      return Confidence is
   begin
      for Index in reverse Interval_Index'Range loop
         if Left.Set (Index) & Right.Set (Index) then
            return To_Confidence (Index);
         end if;
      end loop;
      return Confidence'First;
   end Possibility;

   function To_Fuzzy (Left : Number) return Fuzzy_Number is
      Point : constant Interval := To_Interval (Left);
   begin
      return (Set => (others => Point));
   end To_Fuzzy;

   function To_Fuzzy (Left : Interval) return Fuzzy_Number is
   begin
      return (Set => (others => Left));
   end To_Fuzzy;

   function "abs" (Left : Fuzzy_Number) return Fuzzy_Number is
      Result : Fuzzy_Number;
   begin
      for Level in Interval_Array'Range loop
         Result.Set (Level) := abs Left.Set (Level);
      end loop;
      return Result;
   end "abs";

   function "+" (Left : Fuzzy_Number) return Fuzzy_Number is
   begin
      return Left;
   end "+";

   function "-" (Left : Fuzzy_Number) return Fuzzy_Number is
      Result : Fuzzy_Number;
   begin
      for Level in Interval_Array'Range loop
         Result.Set (Level) := -Left.Set (Level);
      end loop;
      return Result;
   end "-";

   package body Arithmetic is
      function Fuzzy_x_Fuzzy (Left, Right : Fuzzy_Number)
         return Fuzzy_Number is
         Result : Fuzzy_Number;
      begin
         for Level in Interval_Array'Range loop
            Result.Set (Level) :=
               Operation (Left.Set (Level), Right.Set (Level));
         end loop;
         return Result;
      end Fuzzy_x_Fuzzy;

      function Fuzzy_x_Number (Left : Fuzzy_Number; Right : Number)
         return Fuzzy_Number is
         Result : Fuzzy_Number;
      begin
         for Level in Interval_Array'Range loop
            Result.Set (Level) := Operation (Left.Set (Level), Right);
         end loop;
         return Result;
      end Fuzzy_x_Number;

      function Number_x_Fuzzy (Left : Number; Right : Fuzzy_Number)
         return Fuzzy_Number is
         Result : Fuzzy_Number;
      begin
         for Level in Interval_Array'Range loop
            Result.Set (Level) :=
               Operation (Left, Right.Set (Level));
         end loop;
         return Result;
      end Number_x_Fuzzy;

      function Fuzzy_x_Interval (Left : Fuzzy_Number; Right : Interval)
         return Fuzzy_Number is
         Result : Fuzzy_Number;
      begin
         for Level in Interval_Array'Range loop
            Result.Set (Level) := Operation (Left.Set (Level), Right);
         end loop;
         return Result;
      end Fuzzy_x_Interval;

      function Interval_x_Fuzzy (Left : Interval; Right : Fuzzy_Number)
         return Fuzzy_Number is
         Result : Fuzzy_Number;
      begin
         for Level in Interval_Array'Range loop
            Result.Set (Level) :=
               Operation (Left, Right.Set (Level));
         end loop;
         return Result;
      end Interval_x_Fuzzy;
   end Arithmetic;

   package Addition is new Arithmetic ("+", "+", "+");
   function "+" (Left, Right : Fuzzy_Number)
      return Fuzzy_Number renames Addition.Fuzzy_x_Fuzzy;
   function "+" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number renames Addition.Fuzzy_x_Number;
   function "+" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number renames Addition.Fuzzy_x_Interval;
   function "+" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number renames Addition.Number_x_Fuzzy;
   function "+" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number renames Addition.Interval_x_Fuzzy;

   package Subtraction is new Arithmetic ("-", "-", "-");
   function "-" (Left, Right : Fuzzy_Number)
      return Fuzzy_Number renames Subtraction.Fuzzy_x_Fuzzy;
   function "-" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number renames Subtraction.Fuzzy_x_Number;
   function "-" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number renames Subtraction.Fuzzy_x_Interval;
   function "-" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number renames Subtraction.Number_x_Fuzzy;
   function "-" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number renames Subtraction.Interval_x_Fuzzy;

   package Multiplication is new Arithmetic ("*", "*", "*");
   function "*" (Left, Right : Fuzzy_Number)
      return Fuzzy_Number renames Multiplication.Fuzzy_x_Fuzzy;
   function "*" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number renames Multiplication.Fuzzy_x_Number;
   function "*" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number renames Multiplication.Fuzzy_x_Interval;
   function "*" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number renames Multiplication.Number_x_Fuzzy;
   function "*" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number renames Multiplication.Interval_x_Fuzzy;

   package Division is new Arithmetic ("/", "/", "/");
   function "/" (Left, Right : Fuzzy_Number)
      return Fuzzy_Number renames Division.Fuzzy_x_Fuzzy;
   function "/" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number renames Division.Fuzzy_x_Number;
   function "/" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number renames Division.Fuzzy_x_Interval;
   function "/" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number renames Division.Number_x_Fuzzy;
   function "/" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number renames Division.Interval_x_Fuzzy;

   function "**" (Left : Fuzzy_Number; Right : Natural)
      return Fuzzy_Number is
      Result : Fuzzy_Number;
   begin
      for Level in Interval_Array'Range loop
         Result.Set (Level) := Left.Set (Level) ** Right;
      end loop;
      return Result;
   end "**";

   package body Relational is
      function Fuzzy_vs_Fuzzy (Left, Right : Fuzzy_Number)
         return Fuzzy_Boolean is
         Pro    : Confidence := Confidence'First;
         Contra : Confidence := Confidence'First;
      begin
         for Level in Interval_Array'Range loop         
            case Compare (Left.Set (Level), Right.Set (Level)) is
               when True =>
                  Pro := Pro or To_Confidence (Level);
               when False =>
                  Contra := Contra or To_Confidence (Level);
               when others =>
                  Pro := Pro or To_Confidence (Level);
                  Contra := Contra or To_Confidence (Level);
            end case;
         end loop;
         return (Possibility => Pro, Necessity => not Contra);
      end Fuzzy_vs_Fuzzy;

      function Fuzzy_vs_Number (Left : Fuzzy_Number; Right : Number)
         return Fuzzy_Boolean is
         Pro    : Confidence := Confidence'First;
         Contra : Confidence := Confidence'First;
      begin
         for Level in Interval_Array'Range loop         
            case Compare (Left.Set (Level), Right) is
               when True =>
                  Pro := Pro or To_Confidence (Level);
               when False =>
                  Contra := Contra or To_Confidence (Level);
               when others =>
                  Pro := Pro or To_Confidence (Level);
                  Contra := Contra or To_Confidence (Level);
            end case;
         end loop;
         return (Possibility => Pro, Necessity => not Contra);
      end Fuzzy_vs_Number;

      function Fuzzy_vs_Interval (Left : Fuzzy_Number; Right : Interval)
         return Fuzzy_Boolean is
         Pro    : Confidence := Confidence'First;
         Contra : Confidence := Confidence'First;
      begin
         for Level in Interval_Array'Range loop         
            case Compare (Left.Set (Level), Right) is
               when True =>
                  Pro := Pro or To_Confidence (Level);
               when False =>
                  Contra := Contra or To_Confidence (Level);
               when others =>
                  Pro := Pro or To_Confidence (Level);
                  Contra := Contra or To_Confidence (Level);
            end case;
         end loop;
         return (Possibility => Pro, Necessity => not Contra);
      end Fuzzy_vs_Interval;

      function Number_vs_Fuzzy (Left : Number; Right : Fuzzy_Number)
         return Fuzzy_Boolean is
         Pro    : Confidence := Confidence'First;
         Contra : Confidence := Confidence'First;
      begin
         for Level in Interval_Array'Range loop         
            case Compare (Left, Right.Set (Level)) is
               when True =>
                  Pro := Pro or To_Confidence (Level);
               when False =>
                  Contra := Contra or To_Confidence (Level);
               when others =>
                  Pro := Pro or To_Confidence (Level);
                  Contra := Contra or To_Confidence (Level);
            end case;
         end loop;
         return (Possibility => Pro, Necessity => not Contra);
      end Number_vs_Fuzzy;

      function Interval_vs_Fuzzy (Left : Interval; Right : Fuzzy_Number)
         return Fuzzy_Boolean is
         Pro    : Confidence := Confidence'First;
         Contra : Confidence := Confidence'First;
      begin
         for Level in Interval_Array'Range loop         
            case Compare (Left, Right.Set (Level)) is
               when True =>
                  Pro := Pro or To_Confidence (Level);
               when False =>
                  Contra := Contra or To_Confidence (Level);
               when others =>
                  Pro := Pro or To_Confidence (Level);
                  Contra := Contra or To_Confidence (Level);
            end case;
         end loop;
         return (Possibility => Pro, Necessity => not Contra);
      end Interval_vs_Fuzzy;
   end Relational;

   package GT is new Relational (">", ">", ">");
   function ">" (Left, Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GT.Fuzzy_vs_Fuzzy;
   function ">" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean renames GT.Fuzzy_vs_Number;
   function ">" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GT.Number_vs_Fuzzy;
   function ">" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean renames GT.Fuzzy_vs_Interval;
   function ">" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GT.Interval_vs_Fuzzy;

   package GE is new Relational (">=", ">=", ">=");
   function ">=" (Left, Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GE.Fuzzy_vs_Fuzzy;
   function ">=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean renames GE.Fuzzy_vs_Number;
   function ">=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GE.Number_vs_Fuzzy;
   function ">=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean renames GT.Fuzzy_vs_Interval;
   function ">=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean renames GT.Interval_vs_Fuzzy;

   function "<" (Left, Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left >= Right);
   end "<";

   function "<" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean is
   begin
      return not (Left >= Right);
   end "<";

   function "<" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left >= Right);
   end "<";

   function "<" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean is
   begin
      return not (Left >= Right);
   end "<";

   function "<" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left >= Right);
   end "<";

   function "<=" (Left, Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function "<=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function "<=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function "<=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function "<=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left > Right);
   end "<=";
   
   function "=" (Left, Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility =>
            Possibility (Left, Right),
         Necessity =>
            Necessity (Left, Right) and Necessity (Right, Left)
      );
   end "=";

   function "=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility =>
            Possibility (Left, Right),
         Necessity =>
            Necessity (Left, Right) and Necessity (Right, Left)
      );
   end "=";

   function "=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility =>
            Possibility (Left, Right),
         Necessity =>
            Necessity (Left, Right) and Necessity (Right, Left)
      );
   end "=";

   function "=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility =>
            Possibility (Left, Right),
         Necessity =>
            Necessity (Left, Right) and Necessity (Right, Left)
      );
   end "=";

   function "=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return
      (  Possibility =>
            Possibility (Left, Right),
         Necessity =>
            Necessity (Left, Right) and Necessity (Right, Left)
      );
   end "=";

   function "/="
            (  Left  : Fuzzy_Number;
               Right : Fuzzy_Number
            )  return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function "/=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean is
   begin
      return not (Left = Right);
   end "/=";

   function Get_Interval
            (  Value : Fuzzy_Number;
               Index : Interval_Index
            )  return Interval is
   begin
      return Value.Set (Index);
   end Get_Interval;

end Fuzzy.Numbers;
