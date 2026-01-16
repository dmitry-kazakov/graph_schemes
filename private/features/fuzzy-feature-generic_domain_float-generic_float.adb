--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Float                            Summer, 2002       --
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

with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;        use Fuzzy.Basic_Edit;
with Fuzzy.Feature.Edit;      use Fuzzy.Feature.Edit;
with Strings_Edit.Fields;     use Strings_Edit.Fields;
with Strings_Edit.Quoted;     use Strings_Edit.Quoted;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;
with Units.Base;              use Units.Base;

with Strings_Edit.Integers;
with Name_Tables;

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Float is
-- use Float_Intervals;

   function Get_Step (Feature : Float_Feature_Object'Class)
      return Interval is
      pragma Inline (Get_Step);
   begin
      return
      (  ((Feature.To, Feature.To) - Feature.From)
      /  Domain_Float'Base (Feature.Cardinality)
      );
   end Get_Step;

   function Convert_To
            (  Value      : Interval;
               From_Scale : Measure;
               To_Offset  : Domain_Float'Base
            )  return Interval is
      pragma Inline (Convert_To);
   begin
      return
         (  Value * From_Scale.Gain
         +  ((To_Offset, To_Offset) - From_Scale.Offset)
         );
   end Convert_To;

   function Accumulate
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measure is
      Step   : constant Interval := Get_Step (Feature);
      Item   : Interval := Step + Feature.From;
      Result : Variable;
   begin
      if Value'Length /= Feature.Cardinality then
         raise Constraint_Error;
      else
         Item.From := Feature.From;
         for Index in Value'Range loop
            Result :=
               Result or (To_Variable (Item) and Value (Index));
            Item := Item + Step;
         end loop;
         return Result * (Feature.SI, Feature.Gain, Feature.Offset);
      end if;
   end Accumulate;
--
-- Classify_* -- Value classifications
--
--    Feature - The feature
--    Value   - It should be an upper estimation of
--
-- Returns :
--
--    The classification
--
   function Classify_Interval
            (  Feature : Float_Feature_Object;
               Value   : Interval
            )  return Classification is
      Result : Classification (Feature.Cardinality);
      Step   : constant Interval := Get_Step (Feature);
      Lower  : Interval := (Feature.From, Feature.From);
      Upper  : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         if (Lower.From, Upper.To) & Value then
            Result.Possibility (Index) := Confidence'Last;
            if (Lower.To, Upper.From) & Value then
               Result.Necessity (Index) := Confidence'Last;
            else
               Result.Necessity (Index) := Confidence'First;
            end if;
         else
            Result.Possibility (Index) := Confidence'First;
            Result.Necessity   (Index) := Confidence'First;
         end if;
         Lower := Upper;
      end loop;
      return Result;
   end Classify_Interval;

   function Classify_Fuzzy_Number
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy_Float
            )  return Classification is
      Result : Classification (Feature.Cardinality);
      Step   : constant Interval := Get_Step (Feature);
      Lower  : Interval := (Feature.From, Feature.From);
      Upper  : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         Result.Possibility (Index) :=
            Possibility ((Lower.From, Upper.To), Value);
         Result.Necessity (Index) :=
            Necessity ((Lower.To, Upper.From), Value);
         Lower := Upper;
      end loop;
      return Result;
   end Classify_Fuzzy_Number;

   function Classify_Variable
            (  Feature : Float_Feature_Object;
               Value   : Variable
            )  return Classification is
      Result : Classification (Feature.Cardinality);
      Step   : constant Interval := Get_Step (Feature);
      Lower  : Interval := (Feature.From, Feature.From);
      Upper  : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         Result.Possibility (Index) :=
            Possibility ((Lower.From, Upper.To), Value);
         Result.Necessity (Index) :=
            Necessity ((Lower.To, Upper.From), Value);
         Lower := Upper;
      end loop;
      return Result;
   end Classify_Variable;

   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Measure
            )  return Classification is
   begin
      return
         Classify_Interval
         (  Feature,
            Get_Value_As
            (  To_Interval_Measure (Value),
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Interval_Measure
            )  return Classification is
   begin
      return
         Classify_Interval
         (  Feature,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Classification is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return Classify_Fuzzy_Number (Feature, Value.Gain);
      else
         return
            Classify_Fuzzy_Number
            (  Feature,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end Classify;

   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return Classify_Variable (Feature, Value.Gain);
      else
         return
            Classify_Variable
            (  Feature,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end Classify;

   function Constructor
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Dimension   : String;
               Default     : Measure;
               Mode        : Code_Set
            )  return Feature_Object_Ptr is
      Step  : constant Domain_Float'Base :=
                 (To - From) / Domain_Float'Base (Cardinality);
      Scale : Measure := Default;

      function Create (Unit_Text : String) return Feature_Object_Ptr is
         Result : constant Feature_Object_Ptr :=
                     new Float_Feature_Object
                         (  Cardinality,
                            Scale.SI,
                            Unit_Text'Length
                         );
         Object : Float_Feature_Object renames
                     Float_Feature_Object (Result.all);
      begin
         Object.Self      := Result;
         Object.Gain      := Scale.Gain;
         Object.Offset    := Scale.Offset;
         Object.Name      := new String'(Name);
         Object.Dimension := Unit_Text;
         Object.From      := From;
         Object.To        := To;
         return Result;
      end Create;
   begin
      if From > To then
         raise End_Error;
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
               "",
               Scale,
               UTF8_Set
         )  );
   end Create;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Float_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
      Step   : constant Interval := Get_Step (Feature);
      Lower  : Scaled;
      Upper  : Scaled;
      Got_It : Boolean;
      Left   : Character;
      Index  : Integer := Pointer;

   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      Left := Source (Index);
      if Left = '[' or else Left = ']' then
         --
         -- Interval notation [x,y]. Both or neither bound should have a
         -- dimension unit specified. The dimension can  also  be  given
         -- after the closing bracket. Then this dimension is applied to
         -- both bounds.
         --
         begin
            Index := Index + 1;
            Get (Source, Index, Name_Tables.Blanks);
            Get (Source, Index, Lower, Parameters);
            if Left = ']' then
               Lower.Numeral := Domain_Float'Succ (Lower.Numeral);
            end if;
            Get_Delimiter (Source, Index, Got_It, Comma);
            if not Got_It then
               raise Data_Error;
            end if;
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
            if Index > Source'Last then
               raise Data_Error;
            end if;
            case Source (Index) is
               when '[' =>
                  Upper.Numeral := Domain_Float'Pred (Upper.Numeral);
               when ']' =>
                  null;
               when others =>
                  raise Data_Error;
            end case;
            Index := Index + 1;
            if Lower.Format in Float_Measures.Canonic
                            .. Float_Measures.Jumbled then
               -- Both units specified
               Lower.Numeral :=
                  Get_Value_As
                  (  Lower.Numeral * Lower.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
               Upper.Numeral :=
                  Get_Value_As
                  (  Upper.Numeral * Upper.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
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
                        Get_Value_As
                        (  Lower.Numeral * Upper.Scale,
                           (Feature.SI, Feature.Gain, Feature.Offset)
                        ),
                        Float_Measures.Np
                     );
                  Upper.Numeral :=
                     Get_Value_As
                     (  Upper.Numeral * Upper.Scale,
                        (Feature.SI, Feature.Gain, Feature.Offset)
                     );
               end if;
            end if;
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         --
         -- Slice notation x..y or  x.  When  the  lower  bound  has  no
         -- dimension specified then one of the upper bound  is  applied
         -- to both.
         --
         Get (Source, Index, Lower, Parameters);
         Get_Delimiter (Source, Index, Got_It, Ellipsis);
         if Got_It then
            -- Slice x..y
            Get (Source, Index, Upper, Parameters);
            if Lower.Format in Float_Measures.Canonic
                            .. Float_Measures.Jumbled then
               if Upper.Format not in Float_Measures.Canonic
                                   .. Float_Measures.Jumbled
               then
                  raise Unit_Error;
               end if;
               -- Both units specified
               Lower.Numeral :=
                  Get_Value_As
                  (  Lower.Numeral * Lower.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
               Upper.Numeral :=
                  Get_Value_As
                  (  Upper.Numeral * Upper.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
            elsif Upper.Format in Float_Measures.Canonic
                               .. Float_Measures.Jumbled then
               -- Only the upper bound unit given
               Lower :=
                  (  Float_Measures.Canonic,	-- A unit was given
                     Get_Value_As
                     (  Lower.Numeral * Lower.Scale * Upper.Scale,
                        (Feature.SI, Feature.Gain, Feature.Offset)
                     ),
                     Float_Measures.Np
                  );
               Upper.Numeral :=
                  Get_Value_As
                  (  Upper.Numeral * Upper.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
            else
               -- No units given
               Lower.Numeral := Lower.Numeral * Lower.Scale.Gain;
               Upper.Numeral := Upper.Numeral * Upper.Scale.Gain;
            end if;
         else
            -- Single value
            if Lower.Format in Float_Measures.Canonic
                            .. Float_Measures.Jumbled
            then
               Lower.Numeral :=
                  Get_Value_As
                  (  Lower.Numeral * Lower.Scale,
                     (Feature.SI, Feature.Gain, Feature.Offset)
                  );
            else
               Lower.Numeral := Lower.Numeral * Lower.Scale.Gain;
            end if;
            Upper.Numeral := Lower.Numeral;
         end if;
      end if;
      if (  Parameters.Get_Units
         and then
            Feature.SI /= Unitless
         and then
            Lower.Format not in Float_Measures.Canonic
                             .. Float_Measures.Jumbled
         )
      then
         -- No enforced units specified explicitly
         raise Unit_Error;
      end if;
      if (  Lower.Numeral > Upper.Numeral
         or else
            Lower.Numeral > Feature.To
         or else
            Upper.Numeral < Feature.From
         )
      then
         raise Constraint_Error;
      end if;
      declare
         L_Index : constant Integer :=
            Integer
            (  Domain_Float'Ceiling
               (  (Lower.Numeral - Feature.From)
               /  Step.To
            )  );
      begin
         if L_Index < 1 then
            From := 1;
         elsif L_Index > Feature.Cardinality then
            From := Feature.Cardinality;
         else
            From := L_Index;
         end if;
      end;
      declare
         U_Index : constant Integer :=
            (  Integer
               (  Domain_Float'Floor
                  ((Upper.Numeral - Feature.From) / Step.From)
               )
            +  1
            );
      begin
         if U_Index < 1 then
            To := 1;
         elsif U_Index > Feature.Cardinality then
            To := Feature.Cardinality;
         else
            To := U_Index;
         end if;
      end;
      Pointer   := Index;
      Exclusive := False;
   end Get_Range;

   function Get_Class (Feature : Float_Feature_Object) return String is
   begin
      return Float_Class;
   end Get_Class;

   function Get_From (Feature : Feature_Object'Class) return Measure is
      This : Float_Feature_Object'Class renames
                Float_Feature_Object'Class (Feature);
   begin
      return This.From * (This.SI, This.Gain, This.Offset);
   end Get_From;

   function Get_From (Feature : Feature_Handle) return Measure is
   begin
      return Get_From (Ptr (Feature).all);
   end Get_From;

   function Get_Interval
            (  Feature : Feature_Handle;
               Value   : Positive
            )  return Interval_Measure is
   begin
      return Get_Interval (Ptr (Feature).all, Value);
   end Get_Interval;

   function Get_Interval
            (  Feature : Feature_Object'Class;
               Value   : Positive
            )  return Interval_Measure is
      This : Float_Feature_Object'Class renames
                Float_Feature_Object'Class (Feature);
   begin
      if This.Cardinality < Value then
         raise Constraint_Error;
      else
         declare
            Step : constant Interval := Get_Step (This);
            From : constant Interval :=
                   (  This.From
                   +  Step * Domain_Float'Base (Value - 1)
                   );
            To   : constant Interval := From + Step;
         begin
            return
            (  Interval'(From.From, To.To)
            *  Measure'(This.SI, This.Gain, This.Offset)
            );
         end;
      end if;
   end Get_Interval;

   function Get_Scale (Feature : Float_Feature_Object) return Measure is
   begin
      return (Feature.SI, Feature.Gain, Feature.Offset);
   end Get_Scale;

   function Get_Scale_Text
            (  Feature    : Float_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String is
   begin
      if Parameters.Use_SI or else Feature.Unit_Length = 0 then
         if Feature.SI = Unitless then
            return "";
         else
            if Parameters.Quote_Units then
               return
               (  '['
               &  Image
                  (  (Feature.SI, 1.0, 0.0),
                     Parameters.Mode,
                     Parameters.Use_Derived
                  )
               &  ']'
               );
            else
               return
                  Image
                  (  (Feature.SI, 1.0, 0.0),
                     Parameters.Mode,
                     Parameters.Use_Derived
                  );
            end if;
         end if;
      else
         if Parameters.Quote_Units then
            return '[' & Feature.Dimension & ']';
         else
            return Feature.Dimension;
         end if;
      end if;
   end Get_Scale_Text;

   function Get_Span (Feature : Feature_Handle)
      return Interval_Measure is
   begin
      return Get_Span (Ptr (Feature).all);
   end Get_Span;

   function Get_Span (Feature : Feature_Object'Class)
      return Interval_Measure is
      This : Float_Feature_Object'Class renames
                Float_Feature_Object'Class (Feature);
   begin
      return
      (  To_Interval_Measure (This.From, This.To)
      *  Measure'(This.SI, This.Gain, This.Offset)
      );
   end Get_Span;

   function Get_To (Feature : Feature_Object'Class)
      return Measure is
      This : Float_Feature_Object'Class renames
                Float_Feature_Object'Class (Feature);
   begin
      return This.To * (This.SI, This.Gain, This.Offset);
   end Get_To;

   function Get_To (Feature : Feature_Handle) return Measure is
   begin
      return Get_To (Ptr (Feature).all);
   end Get_To;

   function Get_Unit (Feature : Float_Feature_Object) return Unit is
   begin
      return Feature.SI;
   end Get_Unit;

   function Get_Variable
            (  Feature : Float_Feature_Object;
               Value   : Positive
            )  return Variable_Measure is
   begin
      return To_Variable_Measure (Get_Interval (Feature, Value));
   end Get_Variable;

   function Is_Float (Feature : Feature_Object'Class) return Boolean is
   begin
      return Feature in Float_Feature_Object'Class;
   end Is_Float;

   function Is_Float (Feature : Feature_Handle) return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
         This /= null and then This.all in Float_Feature_Object'Class;
   end Is_Float;

   function Is_Domain_Linguistic (Feature : Float_Feature_Object)
      return Boolean is
   begin
      return False;
   end Is_Domain_Linguistic;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Float_Feature_Object;
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
      Step  : constant Interval := Get_Step (Feature);

      function To_Domain_Lower (Value : Integer)
         return Domain_Float'Base is
         pragma Inline (To_Domain_Lower);
         Result : constant Interval :=
                  Feature.From + Domain_Float'Base (Value - 1) * Step;
      begin
         return Result.From;
      end To_Domain_Lower;

      function To_Domain_Upper (Value : Integer)
         return Domain_Float'Base is
         pragma Inline (To_Domain_Upper);
         Result : constant Interval :=
                  Feature.From + Domain_Float'Base (Value - 1) * Step;
      begin
         return Result.To;
      end To_Domain_Upper;

      procedure Put (Value : Domain_Float'Base) is
      begin
         if Parameters.Use_SI or else Feature.Unit_Length = 0 then
            Put
            (  Destination => Text,
               Pointer     => Index,
               PutPlus     => Parameters.Put_Plus,
               AbsSmall    => Parameters.Abs_Small,
               RelSmall    => Parameters.Rel_Small,
               Value =>
                  Get_Value
                  (  Value
                  *  (Feature.SI, Feature.Gain, Feature.Offset)
            )     );
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
      if To > Feature.Cardinality or else From > To
      then
         raise Constraint_Error;
      end if;
      Put (Text, Index, '[');
      Put (To_Domain_Lower (From));
      Put (Text, Index, ", ");
      Put (To_Domain_Upper (To + 1));
      Put (Text, Index, ']');
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
      Cardinality : Positive;
      From, To    : Domain_Float'Base;
      Got_It      : Boolean         := False;
      Position    : aliased Integer := Pointer;
      Name        : constant String :=
                       Get_Quoted (Source, Position'Access);
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
      if Position > Source'Last then
         raise Data_Error;
      end if;
      if Source (Position) = '"' then
         Feature :=
            To_Deposit_Ptr
            (  Constructor
               (  Name,
                  Cardinality,
                  From,
                  To,
                  Get_Quoted (Source, Position'Access),
                  Float_Measures.Np,
                  UTF8_Set
             )  );
      else
         declare
            Scale : Measure;
         begin
            Get (Source, Position, Scale, ASCII_Set);
            Feature :=
               To_Deposit_Ptr
               (  Constructor
                  (  Name,
                     Cardinality,
                     From,
                     To,
                     "",
                     Scale,
                     UTF8_Set
                )  );
         end;
      end if;
      Pointer := Position;
   exception
      when Constraint_Error | Unit_Error =>
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
                Feature     : Float_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Integers.Put (Destination, Position, Feature.Cardinality);
      Put (Destination, Position, ':');
      Put (Destination, Position, Feature.From);
      Put (Destination, Position, "..");
      Put (Destination, Position, Feature.To);
      Put (Destination, Position, " ");
      Put_Dimension (Destination, Position, Feature.Dimension, Feature);
      Pointer := Position;
   end Store;
--
-- *_To_Set -- Value to set conversion
--
--    Cardinality - The number of intervals
--    Step        - Estimation of (interval)
--    From        - Estimation of (interval)
--    Value       - The exact value
--
-- The parameters Step and From are obtained from the feature by conversion
-- to the measurement units of Value. They are estimated by intervals.
--
-- Returns :
--
--    The set
--
   function Interval_To_Set
            (  Cardinality : Positive;
               Step        : Interval;
               From        : Interval;
               Value       : Interval
            )  return Fuzzy.Intuitionistic.Set is
      Result : Fuzzy.Intuitionistic.Set (Cardinality);
      Lower  : Interval := From;
      Upper  : Interval;
      Item   : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         Item  := (Lower.From, Upper.To);
         if Item & Value then
            Result.Possibility (Index) := Confidence'Last;
            if Item & Value then
               Result.Necessity (Index) := Confidence'Last;
            else
               Result.Necessity (Index) := Confidence'First;
            end if;
         else
            Result.Possibility (Index) := Confidence'First;
            Result.Necessity   (Index) := Confidence'First;
         end if;
         Lower := Upper;
      end loop;
      return Result;
   end Interval_To_Set;

   function Fuzzy_Number_To_Set
            (  Cardinality : Positive;
               Step        : Interval;
               From        : Interval;
               Value       : Fuzzy_Float
            )  return Fuzzy.Intuitionistic.Set is
      Result : Fuzzy.Intuitionistic.Set (Cardinality);
      Lower  : Interval := From;
      Upper  : Interval;
      Item   : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         Item  := (Lower.From, Upper.To);
         Result.Possibility (Index) := Possibility (Value, Item);
         Result.Possibility (Index) := Necessity   (Value, Item);
         Lower := Upper;
      end loop;
      return Result;
   end Fuzzy_Number_To_Set;

   function Variable_To_Set
            (  Cardinality : Positive;
               Step        : Interval;
               From        : Interval;
               Value       : Variable
            )  return Fuzzy.Intuitionistic.Set is
      Result : Fuzzy.Intuitionistic.Set (Cardinality);
      Lower  : Interval := From;
      Upper  : Interval;
      Item   : Interval;
   begin
      for Index in Result.Possibility'Range loop
         Upper := Lower + Step;
         Item  := (Lower.From, Upper.To);
         Result.Possibility (Index) := Possibility (Value, Item);
         Result.Possibility (Index) := Necessity   (Value, Item);
         Lower := Upper;
      end loop;
      return Result;
   end Variable_To_Set;

   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return
            Interval_To_Set
            (  Feature.Cardinality,
               Get_Step (Feature),
               (Feature.From, Feature.From),
               (Value.Gain, Value.Gain)
            );
      else
         return
            Interval_To_Set
            (  Feature.Cardinality,
               Convert_To
               (  Get_Step (Feature),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Convert_To
               (  (Feature.From, Feature.From),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               (Value.Gain, Value.Gain)
            );
      end if;
   end To_Set;

   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return
            Interval_To_Set
            (  Feature.Cardinality,
               Get_Step (Feature),
               (Feature.From, Feature.From),
               (Value.From, Value.To)
            );
      else
         return
            Interval_To_Set
            (  Feature.Cardinality,
               Convert_To
               (  Get_Step (Feature),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Convert_To
               (  (Feature.From, Feature.From),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               (Value.From, Value.To)
            );
      end if;
   end To_Set;

   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return
            Fuzzy_Number_To_Set
            (  Feature.Cardinality,
               Get_Step (Feature),
               (Feature.From, Feature.From),
               Value.Gain
            );
      else
         return
            Fuzzy_Number_To_Set
            (  Feature.Cardinality,
               Convert_To
               (  Get_Step (Feature),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Convert_To
               (  (Feature.From, Feature.From),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Value.Gain
            );
      end if;
   end To_Set;

   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return
            Variable_To_Set
            (  Feature.Cardinality,
               Get_Step (Feature),
               (Feature.From, Feature.From),
               Value.Gain
            );
      else
         return
            Variable_To_Set
            (  Feature.Cardinality,
               Convert_To
               (  Get_Step (Feature),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Convert_To
               (  (Feature.From, Feature.From),
                  (Feature.SI, Feature.Gain, Feature.Offset),
                  Value.Offset
               ),
               Value.Gain
            );
      end if;
   end To_Set;

begin
   if not Is_Registered (Float_Class) then
      Register_Class (Float_Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Float.Generic_Float;
