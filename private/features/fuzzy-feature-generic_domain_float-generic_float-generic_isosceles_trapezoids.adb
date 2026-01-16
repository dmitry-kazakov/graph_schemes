--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Float.                           Autumn, 2005       --
--           Generic_Isosceles_Trapezoids                             --
--                                                                    --
--  Implementation                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Basic_Edit;           use Fuzzy.Basic_Edit;
with Fuzzy.Feature.Edit;         use Fuzzy.Feature.Edit;
with Integer_Intervals;          use Integer_Intervals;
with Strings_Edit.Fields;        use Strings_Edit.Fields;
with Strings_Edit.Quoted;        use Strings_Edit.Quoted;
with Strings_Edit.UTF8.Maps;     use Strings_Edit.UTF8.Maps;
with Units.Base;                 use Units.Base;

with Strings_Edit.Integers;
with Name_Tables;

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Float.
             Generic_Isosceles_Trapezoids is

   function Accumulate
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measure is
   begin
      return
      (  Accumulate (Feature.Domain, Value)
      *  (Feature.SI, Feature.Gain, Feature.Offset)
      );
   end Accumulate;

   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Constructor
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Shoulder    : Domain_Float;
               Dimension   : String;
               Default     : Measure;
               Mode        : Code_Set
            )  return Feature_Object_Ptr is
      use Strings_Edit.Integers;
      Step  : constant Domain_Float'Base :=
                 (To - From) / Domain_Float'Base (Cardinality - 2);
      Scale : Measure := Default;

      function Create (Unit_Text : String) return Feature_Object_Ptr is
         Result : constant Feature_Object_Ptr :=
                     new Isosceles_Trapezoid_Feature_Object
                         (  Cardinality,
                            Scale.SI,
                            Unit_Text'Length
                         );
         Object : Isosceles_Trapezoid_Feature_Object renames
                     Isosceles_Trapezoid_Feature_Object (Result.all);
         Left   : Domain_Float'Base := From;
         Right  : Domain_Float'Base;
      begin
         Object.Self := Result;
         Add
         (  Object.Domain,
            "L",
            (From, Confidence'Last) & (From + Shoulder, Confidence'First)
         );
         for Index in 1..Cardinality - 2 loop
            Right := Left + Step;
            Add
            (  Object.Domain,
               "T" & Image (Index),
               (  (Left - Shoulder,  Confidence'First)
               &  (Left,             Confidence'Last)
               &  (Right,            Confidence'Last)
               &  (Right + Shoulder, Confidence'First)
            )  );
            Left := Right;
         end loop;
         Add
         (  Object.Domain,
            "R",
            (To - Shoulder, Confidence'First) & (To, Confidence'Last)
         );
         Object.Name      := new String'(Name);
         Object.Dimension := Unit_Text;
         Object.Gain      := Scale.Gain;
         Object.Offset    := Scale.Offset;
         Object.From      := From;
         Object.To        := To;
         Object.Shoulder  := Shoulder;
         return Result;
      end Create;
   begin
      if From > To then
         raise End_Error;
      elsif From = To then
         if Cardinality /= 2 then
            raise Constraint_Error;
         end if;
      else
         if Cardinality < 3 then
            raise Constraint_Error;
         end if;
      end if;
      if Shoulder < 0.0 then
         raise Constraint_Error;
      end if;
      if Dimension'Length > 0 then
         begin
            Scale := Value (Dimension, Mode);
         exception
            when others =>
               raise Unit_Error;
         end;
      end if;
      if Scale.Gain <= 0.0 then
         raise Unit_Error;
      end if;
      if Step = 0.0 and then Cardinality /= 1 then
         raise Constraint_Error;
      end if;
      return Create (Trim (Dimension));
   end Constructor;

   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Shoulder    : Domain_Float;
               Scale       : String;
               Mode        : Code_Set := UTF8_Set
            )  return Feature_Handle is
   begin
      return
         Ref
         (  Constructor
            (  Name,
               Cardinality,
               From,
               To,
               Shoulder,
               Scale,
               Float_Measures.Np,
               Mode
         )  );
   end Create;

   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Shoulder    : Domain_Float;
               Scale       : Measure := Float_Measures.Np
            )  return Feature_Handle is
   begin
      return
         Ref
         (  Constructor
            (  Name,
               Cardinality,
               From,
               To,
               Shoulder,
               "",
               Scale,
               UTF8_Set
         )  );
   end Create;

   function Get_Class (Feature : Isosceles_Trapezoid_Feature_Object)
      return String is
   begin
      return Isosceles_Trapezoid_Class;
   end Get_Class;

   function Get_Shoulder (Feature : Feature_Object'Class)
      return Measure is
      This : Isosceles_Trapezoid_Feature_Object'Class renames
                Isosceles_Trapezoid_Feature_Object'Class (Feature);
   begin
      return This.Shoulder * (This.SI, This.Gain, This.Offset);
   end Get_Shoulder;

   function Get_Shoulder (Feature : Feature_Handle) return Measure is
   begin
      return Get_Shoulder (Ptr (Feature).all);
   end Get_Shoulder;

   function Get_Variable
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Positive
            )  return Variable_Measure is
   begin
      return
      (  Get (Feature.Domain, Value)
      *  (Feature.SI, Feature.Gain, Feature.Offset)
      );
   end Get_Variable;
--
-- Get_Range -- Convert an interval to range
--
--    Feature - The object
--    Bounds  - An interval to convert
--    From    - The lower bound (output)
--    To      - The upper bound
--
-- Exceptions :
--
--    Constraint_Error - Bounds is not an interval
--    Data_Error       - Empty result
--
   procedure Get_Range
             (  Feature : Isosceles_Trapezoid_Feature_Object;
                Bounds  : Float_Intervals.Interval;
                From    : out Positive;
                To      : out Positive
             )  is
      Threshold : constant Confidence := Confidence'Last / 2.0;
   begin
      if Bounds.From > Bounds.To then
         raise Constraint_Error;
      end if;
      --
      -- Bounds is now an interval of the domain values in the units  of
      -- the feature. All variables that are possible on  this  interval
      -- more than 0.5, are in the range.
      --
      for Left in 1..Get_Cardinality (Feature.Domain) loop
         --
         -- Searching for the left bound of the range
         --
         if Possibility (Get (Feature.Domain, Left), Bounds) > Threshold
         then
            From := Left;
            To   := Left;
            for Right in Left + 1
                      .. Get_Cardinality (Feature.Domain)
            loop
               exit when
                    (  Possibility (Get (Feature.Domain, Right), Bounds)
                    <  Threshold
                    );
               To := Right;
            end loop;
            return;
         end if;
      end loop;
      raise Data_Error; -- This cannot happen
   end Get_Range;
--
-- Get_Trapezoid -- Trapezoid notation /x,y\ or /x[
--
-- Trapezoid  notation /x,y\ or /x[. Both or neither bound should have a
-- dimension  unit  specified. The dimension can also be given after the
-- closing bracket. Then this dimension is applied to both bounds.
--
   procedure Get_Trapezoid
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Isosceles_Trapezoid_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Parameters : Input_Parameters'Class
             )  is
      pragma Inline (Get_Trapezoid);
      Scale  : constant Measure := Get_Scale (Feature);
      Index  : Integer := Pointer + 1;
      Lower  : Scaled;
      Upper  : Scaled;
      Got_It : Boolean;
   begin
      Get (Source, Index, Name_Tables.Blanks);
      Get (Source, Index, Lower, Parameters);
      Get_Delimiter (Source, Index, Got_It, Comma);
      if Got_It then
         --
         -- The format is /x,y\
         --
         Get (Source, Index, Upper, Parameters);
         if (  Lower.Format in Float_Measures.Canonic
                            .. Float_Measures.Jumbled
            xor
               Upper.Format in Float_Measures.Canonic
                            .. Float_Measures.Jumbled
            )
         then
            raise Unit_Error;
         end if;
         Get (Source, Index, Name_Tables.Blanks);
         if Index > Source'Last or else Source (Index) /= '\' then
            raise Data_Error;
         end if;
         Index := Index + 1;
         if Lower.Format in Float_Measures.Canonic
                         .. Float_Measures.Jumbled
         then -- Both units specified
            Lower.Numeral :=
               Get_Value_As (Lower.Numeral * Lower.Scale, Scale);
            Upper.Numeral :=
               Get_Value_As (Upper.Numeral * Upper.Scale, Scale);
         else
            -- No units given
            Lower.Numeral := Lower.Numeral * Lower.Scale.Gain;
            Upper.Numeral := Upper.Numeral * Upper.Scale.Gain;
            Get_Unit
            (  Source,
               Index,
               Upper.Scale,
               Got_It,
               Parameters
            );
            if Got_It then
               Lower :=
                  (  Float_Measures.Canonic, -- A unit was given
                     Get_Value_As (Lower.Numeral * Upper.Scale, Scale),
                     Float_Measures.Np
                  );
               Upper.Numeral :=
                  Get_Value_As (Upper.Numeral * Upper.Scale, Scale);
            end if;
         end if;
      elsif Index > Source'Last or else Source (Index) /= '[' then
         raise Data_Error;
      else
         --
         -- The format is /x[
         --
         Index := Index + 1;
         if Lower.Format in Float_Measures.Canonic
                         .. Float_Measures.Jumbled
         then -- Units specified for the lower bound
            Lower.Numeral :=
               Get_Value_As (Lower.Numeral * Lower.Scale, Scale);
         else
            -- No units given
            Lower.Numeral := Lower.Numeral * Lower.Scale.Gain;
            Get_Unit (Source, Index, Upper.Scale, Got_It, Parameters);
            if Got_It then
               Lower :=
                  (  Float_Measures.Canonic,	-- A unit was given
                     Get_Value_As (Lower.Numeral * Upper.Scale, Scale),
                     Float_Measures.Np
                  );
            end if;
         end if;
         Upper.Numeral := Domain_Float'Last;
      end if;
      if (  Parameters.Get_Units
         and then
            Scale /= Float_Measures.Np
         and then
            Lower.Format not in Float_Measures.Canonic
                             .. Float_Measures.Jumbled
         )
      then
         raise Unit_Error;
      end if;
      Get_Range (Feature, (lower.Numeral, Upper.Numeral), From, To);
      Pointer := Index;
   exception
      when End_Error =>
         raise Data_Error;
   end Get_Trapezoid;
--
-- Get_Shoulder -- Trapezoid notation ]y\ or ][
--
   procedure Get_Shoulder
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Isosceles_Trapezoid_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Parameters : Input_Parameters'Class
             )  is
      pragma Inline (Get_Shoulder);
      Scale  : constant Measure := Get_Scale (Feature);
      Index  : Integer := Pointer + 1;
      Upper  : Scaled;
      Got_It : Boolean;
   begin
      Get (Source, Index, Name_Tables.Blanks);
      if Index <= Source'Last and then Source (Index) = '[' then
         --
         -- The format is ][
         --
         Index := Index + 1;
         From  := 1;
         To    := Feature.Cardinality;
      else
         --
         -- The format is ]y\
         --
         Get (Source, Index, Upper, Parameters);
         Get (Source, Index, Name_Tables.Blanks);
         if Index > Source'Last or else Source (Index) /= '\' then
            raise Data_Error;
         end if;
         Index := Index + 1;
         if Upper.Format in Float_Measures.Canonic
                         .. Float_Measures.Jumbled
         then -- Units specified
            Upper.Numeral :=
               Get_Value_As (Upper.Numeral * Upper.Scale, Scale);
         else
            -- No units given
            Upper.Numeral := Upper.Numeral * Upper.Scale.Gain;
            Get_Unit
            (  Source,
               Index,
               Upper.Scale,
               Got_It,
               Parameters
            );
            if Got_It then
               Upper.Numeral :=
                  Get_Value_As (Upper.Numeral * Upper.Scale, Scale);
            end if;
         end if;
         if (  Parameters.Get_Units
            and then
               Scale /= Float_Measures.Np
            and then
               Upper.Format not in Float_Measures.Canonic
                                .. Float_Measures.Jumbled
            )
         then
            raise Unit_Error;
         end if;
         Get_Range
         (  Feature,
            (Domain_Float'First, Upper.Numeral),
            From,
            To
         );
      end if;
      Pointer := Index;
   exception
      when End_Error =>
         raise Data_Error;
   end Get_Shoulder;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Isosceles_Trapezoid_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
   begin
      if Pointer < Source'First then
         raise Layout_Error;
      end if;
      if Pointer > Source'Last then
         if Pointer - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      case Source (Pointer) is
         when '/' =>
            Get_Trapezoid
            (  Source,
               Pointer,
               Feature,
               From,
               To,
               Parameters
            );
            Exclusive := False;
         when ']' =>
            Get_Shoulder
            (  Source,
               Pointer,
               Feature,
               From,
               To,
               Parameters
            );
            Exclusive := False;
         when others =>
            Get
            (  Source,
               Pointer,
               Get_Domain (Feature.Domain).all,
               From,
               To
            );
            Exclusive := True;
      end case;
   end Get_Range;

   procedure Get_Trapezoid_Parameters
             (  Feature  : Feature_Handle;
                From     : out Measure;
                To       : out Measure;
                Shoulder : out Measure
             )  is
   begin
      Get_Trapezoid_Parameters
      (  Ptr (Feature).all,
         From,
         To,
         Shoulder
      );
   end Get_Trapezoid_Parameters;

   procedure Get_Trapezoid_Parameters
             (  Feature : Feature_Object'Class;
                From     : out Measure;
                To       : out Measure;
                Shoulder : out Measure
             )  is
      This  : Isosceles_Trapezoid_Feature_Object'Class renames
                 Isosceles_Trapezoid_Feature_Object'Class (Feature);
      Scale : constant Measure := Get_Scale (This);
   begin
      From     := This.From     * Scale;
      To       := This.To       * Scale;
      Shoulder := This.Shoulder * Scale;
   end Get_Trapezoid_Parameters;

   function Is_Isosceles_Trapezoid (Feature : Feature_Handle)
      return Boolean is
   begin
      return
      (  Feature.Is_Valid
      and then
         Is_Isosceles_Trapezoid (Ptr (Feature).all)
      );
   end Is_Isosceles_Trapezoid;

   function Is_Isosceles_Trapezoid (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Isosceles_Trapezoid_Feature_Object'Class;
   end Is_Isosceles_Trapezoid;

   function Is_Domain_Linguistic
            (  Feature : Isosceles_Trapezoid_Feature_Object
            )  return Boolean is
   begin
      return True;
   end Is_Domain_Linguistic;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Isosceles_Trapezoid_Feature_Object;
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

      function Get_Bound (Value : Integer; Offset : Integer)
         return Domain_Float'Base is
         This  : Variable renames Get (Feature.Domain, Value);
         From  : Domain_Float'Base;
         To    : Domain_Float'Base;
         Left  : Confidence;
         Min   : Confidence;
         Max   : Confidence;
         Right : Confidence;
      begin
         Get_Point
         (  This,
            Offset,
            From,
            Left,
            Min,
            Max,
            Right
         );
         Get_Point
         (  This,
            Offset + 1,
            To,
            Left,
            Min,
            Max,
            Right
         );
         return (From + To) / 2.0;
      end Get_Bound;

      procedure Put (Value : Domain_Float'Base) is
      begin
         if Parameters.Use_SI or else Feature.Unit_Length = 0 then
            Put
            (  Destination => Text,
               Pointer     => Index,
               Value       => Get_Value (Value * Get_Scale (Feature)),
               PutPlus     => Parameters.Put_Plus,
               AbsSmall    => Parameters.Abs_Small,
               RelSmall    => Parameters.Rel_Small
            );
         else
            Put
            (  Destination => Text,
               Pointer     => Index,
               Value       => Value,
               PutPlus     => Parameters.Put_Plus,
               AbsSmall    => Parameters.Abs_Small,
               RelSmall    => Parameters.Rel_Small
            );
         end if;
      end Put;
   begin
      if From > To or else From < 1 or else To > Feature.Cardinality
      then
         raise Constraint_Error;
      end if;
      if From = 1 then
         Put (Text, Index, "]");
         if To = Feature.Cardinality then
            Put (Text, Index, "[");
            Adjust_Output_Field
            (  Destination,
               Pointer,
               Index,
               Out_Field,
               Field,
               Justify,
               Fill
            );
            return;
         end if;
         if To = 1 then
            Put (Get_Bound (To, 1)); -- Shoulder HHH1\2
         else
            Put (Get_Bound (To, 3)); -- Trapezoid /HHH3\4
         end if;
         Put (Text, Index, "\");
      else
         Put (Text, Index, "/");
         Put (Get_Bound (From, 1));  -- Either 1/2HHH or HHH1\2
         if To = Feature.Cardinality then
            Put (Text, Index, "[");
         else
            Put (Text, Index, ", "); -- Trapezoid /HHH3\4
            Put (Get_Bound (To, 3));
            Put (Text, Index, "\");
         end if;
      end if;
      if Parameters.Put_Units then
         declare
            Dimension : String renames
               Get_Scale_Text (Feature, Parameters);
         begin
            if Dimension'Length /= 0 then
               Put (Text, Index, ' ');
               Put (Text, Index, Dimension);
            end if;
         end;
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
      Position    : aliased Integer := Pointer;
      Cardinality : Positive;
      From, To    : Domain_Float'Base;
      Shoulder    : Domain_Float'Base;
      Name        : constant String :=
                       Get_Quoted (Source, Position'Access);
      Got_It      : Boolean := False;
   begin
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      Integers.Get (Source, Position, Cardinality);
      Get_Delimiter (Source, Position, Got_It, Colon);
      if not Got_It then
         raise Data_Error;
      end if;
      Get (Source, Position, From);
      Get_Delimiter (Source, Position, Got_It, Ellipsis);
      if not Got_It then
         raise Data_Error;
      end if;
      Get (Source, Position, To);
      if From > To then
         raise Data_Error;
      end if;
      Get (Source, Position);
      Get_Delimiter (Source, Position, Got_It, Colon);
      if not Got_It then
         raise Data_Error;
      end if;
      Get (Source, Position, Shoulder);
      Get (Source, Position);
      if Position > Source'Last then
         raise Data_Error;
      end if;
      if Source (Position) = '"' then
         Feature :=
            To_Deposit_Ptr
            (  Constructor
               (  Name        => Name,
                  Cardinality => Cardinality,
                  From        => From,
                  To          => To,
                  Shoulder    => Shoulder,
                  Default     => Float_Measures.Np,
                  Mode        => UTF8_Set,
                  Dimension   =>
                     Get_Quoted (Source, Position'Access)
             )  );
      else
         declare
            Scale : Measure;
         begin
            Get (Source, Position, Scale, ASCII_Set);
            Feature :=
               To_Deposit_Ptr
               (  Constructor
                  (  Name        => Name,
                     Cardinality => Cardinality,
                     From        => From,
                     To          => To,
                     Shoulder    => Shoulder,
                     Dimension   => "",
                     Default     => Scale,
                     Mode        => UTF8_Set
                )  );
         end;
      end if;
      Pointer := Position;
   exception
      when Constraint_Error | Unit_Error | End_Error =>
         raise Data_Error;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Isosceles_Trapezoid_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ':');
      Integers.Put (Destination, Position, Feature.Cardinality);
      Put (Destination, Position, ':');
      Put (Destination, Position, Feature.From);
      Put (Destination, Position, "..");
      Put (Destination, Position, Feature.To);
      Put (Destination, Position, ':');
      Put (Destination, Position, Feature.Shoulder);
      Put (Destination, Position, ' ');
      Put_Dimension (Destination, Position, Feature.Dimension, Feature);
      Pointer := Position;
   end Store;

   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

   function To_Set
            (  Feature : Isosceles_Trapezoid_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

begin
   if not Is_Registered (Isosceles_Trapezoid_Class) then
      Register_Class (Isosceles_Trapezoid_Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Float.Generic_Float.
    Generic_Isosceles_Trapezoids;
