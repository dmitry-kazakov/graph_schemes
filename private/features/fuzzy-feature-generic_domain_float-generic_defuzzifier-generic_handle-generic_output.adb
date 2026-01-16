--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier.Generic_Handle.      Autumn, 2005       --
--           Generic_Output                                           --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with Units;                use Units;

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
             Generic_Handle.Generic_Output is
   use Float_Handles;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Feature_Object'Class,
             Feature_Object_Ptr
          );

   function Accumulate
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measures.Variable_Measure is
   begin
      return Accumulate (Ptr (Feature.Source).all, Value);
   end Accumulate;

   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return Classify (Ptr (Feature.Source).all, Value);
   end Classify;

   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return Classify (Ptr (Feature.Source).all, Value);
   end Classify;

   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return Classify (Ptr (Feature.Source).all, Value);
   end Classify;

   function Classify
            (  Feature : Output_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return Classify (Ptr (Feature.Source).all, Value);
   end Classify;

   function Create
            (  Name    : String;
               Feature : Feature_Handle;
               Method  : Defuzzifier_Handle;
               Default : Measure
            )  return Feature_Handle is
      Source : constant Domain_Feature_Object_Ptr :=
                  To_Domain_Feature_Object_Ptr (Ptr (Feature));
   begin
      if Get_Unit (Source.all) /= Default.SI then
         raise Unit_Error;
      end if;
      declare
         Result : Feature_Object_Ptr :=
                  new Output_Feature_Object (Get_Cardinality (Feature));
         Object : Output_Feature_Object renames
                     Output_Feature_Object (Result.all);
      begin
         Object.Self    := Result;
         Object.Source  := Ref (Source);
         Object.Method  := Method;
         Object.Name    := new String'(Name);
         Object.Default := Default;
         return Ref (Result);
      exception
         when others =>
            Free (Result);
            raise;
      end;
   end Create;

   procedure Create_Constraint
             (  Feature : Output_Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             )  is
   begin
      Create_Constraint (Ptr (Feature.Source).all, Context, Allowed);
   end Create_Constraint;

   function Create_Data (Feature : Output_Feature_Object)
      return Feature_Data_Ptr is
   begin
      return null;
   end Create_Data;

   function Defuzzify
            (  Feature : Feature_Object'Class;
               Value   : Fuzzy.Set
            )  return Measure is
      Object : Output_Feature_Object'Class renames
                  Output_Feature_Object'Class (Feature);
   begin
      return
         Defuzzify
         (  Object.Method,
            Accumulate (Ptr (Object.Source).all, Value)
         );
   exception
      when Constraint_Error =>
         return Object.Default;
   end Defuzzify;

   function Defuzzify
            (  Feature : Feature_Object'Class;
               Value   : Fuzzy.Set;
               Default : Measure
            )  return Measure is
      Object : Output_Feature_Object'Class renames
                  Output_Feature_Object'Class (Feature);
   begin
      if Default.SI /= Object.Default.SI then
         raise Constraint_Error;
      else
         begin
            return
               Defuzzify
               (  Object.Method,
                  Accumulate (Ptr (Object.Source).all, Value)
               );
         exception
            when Constraint_Error =>
               return Default;
         end;
      end if;
   end Defuzzify;

   function Defuzzify
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set
            )  return Measure is
   begin
      return Defuzzify (Ptr (Feature).all, Value);
   end Defuzzify;

   function Defuzzify
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set;
               Default : Measure
            )  return Measure is
   begin
      return Defuzzify (Ptr (Feature).all, Value, Default);
   end Defuzzify;

   function Get
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set is
   begin
      return Get (Ptr (Feature.Source).all, Context, Image);
   end Get;

   function Get
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence is
   begin
      return Get (Ptr (Feature.Source).all, Context, Image, Value);
   end Get;

   function Get_Class (Feature : Output_Feature_Object)
      return String is
   begin
      return Output_Class;
   end Get_Class;

   function Get_Constraint
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset is
   begin
      return Get_Constraint (Ptr (Feature.Source).all, Context);
   end Get_Constraint;

   function Get_Default
            (  Feature : Feature_Object'Class
            )  return Measure is
   begin
      return Output_Feature_Object'Class (Feature).Default;
   end Get_Default;

   function Get_Default
            (  Feature : Feature_Handle
            )  return Measure is
   begin
      return Output_Feature_Object'Class (Ptr (Feature).all).Default;
   end Get_Default;

   function Get_Defuzzifier
            (  Feature : Feature_Object'Class
            )  return Defuzzifier_Handle is
   begin
      return Output_Feature_Object'Class (Feature).Method;
   end Get_Defuzzifier;

   function Get_Defuzzifier
            (  Feature : Feature_Handle
            )  return Defuzzifier_Handle is
   begin
      return Output_Feature_Object'Class (Ptr (Feature).all).Method;
   end Get_Defuzzifier;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Output_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
   begin
      Get_Range
      (  Source     => Source,
         Pointer    => Pointer,
         Feature    => Ptr (Feature.Source).all,
         From       => From,
         To         => To,
         Exclusive  => Exclusive,
         Parameters => Parameters
      );
   end Get_Range;

   procedure Get_Referents
             (  Feature : Output_Feature_Object;
                List    : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Feature.Source)), False);
      Add (List, To_Deposit_Ptr (Ptr (Feature.Method)), False);
   end Get_Referents;

   function Get_Scale (Feature : Output_Feature_Object)
      return Measure is
   begin
      return Get_Scale (Ptr (Feature.Source).all);
   end Get_Scale;

   function Get_Scale_Text
            (  Feature    : Output_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String is
   begin
      return Get_Scale_Text (Ptr (Feature.Source).all, Parameters);
   end Get_Scale_Text;

   function Get_Source
            (  Feature : Feature_Object'Class
            )  return Float_Handles.Handle is
   begin
      return Output_Feature_Object'Class (Feature).Source;
   end Get_Source;

   function Get_Source
            (  Feature : Feature_Handle
            )  return Float_Handles.Handle is
   begin
      return Get_Source (Ptr (Feature).all);
   end Get_Source;

   function Get_Unit (Feature : Output_Feature_Object)
      return Unit is
   begin
      return Get_Unit (Ptr (Feature.Source).all);
   end Get_Unit;

   function Get_Variable
            (  Feature : Output_Feature_Object;
               Value   : Positive
            )  return Variable_Measure is
   begin
      return Get_Variable (Ptr (Feature.Source).all, Value);
   end Get_Variable;

   function Is_Computed
            (  Feature : Output_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is
   begin
      return Is_Computed (Ptr (Feature.Source).all, Source);
   end Is_Computed;

   function Is_Defined
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Defined (Ptr (Feature.Source).all, Context, Image);
   end Is_Defined;

   function Is_Known
            (  Feature : Output_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Known (Ptr (Feature.Source).all, Context, Image);
   end Is_Known;

   function Is_Domain_Linguistic (Feature : Output_Feature_Object)
      return Boolean is
   begin
      return Is_Domain_Linguistic (Ptr (Feature.Source).all);
   end Is_Domain_Linguistic;

   function Is_Output (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Output_Feature_Object'Class;
   end Is_Output;

   function Is_Output (Feature : Feature_Handle)
      return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
      (  This /= null
      and then
         This.all in Output_Feature_Object'Class
      );
   end Is_Output;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Output_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put_Range
      (  Destination => Destination,
         Pointer     => Pointer,
         Feature     => Ptr (Feature.Source).all,
         From        => From,
         To          => To,
         Parameters  => Parameters,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put_Range;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      Position : aliased Integer := Pointer;
      Method   : Defuzzifier_Handle;
      Parent   : Float_Handles.Handle;
      Default  : Domain_Float;
      Name     : constant String :=
                    Get_Quoted (Source, Position'Access);
   begin
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      begin
         Get (Source, Position, Default);
      exception
         when Constraint_Error | End_Error =>
            raise Data_Error;
      end;
      Parent :=
         Ref
         (  To_Domain_Feature_Object_Ptr
            (  To_Feature_Object_Ptr
               (  Get (List, 1)
         )  )  );
      Method := Ref (To_Defuzzifier_Object_Ptr (Get (List, 2)));
      declare
         Result : Feature_Object_Ptr :=
                  new Output_Feature_Object (Ptr (Parent).Cardinality);
         Object : Output_Feature_Object renames
                     Output_Feature_Object (Result.all);
      begin
         Object.Self    := Result;
         Object.Source  := Parent;
         Object.Method  := Method;
         Object.Name    := new String'(Name);
         Object.Default := Default * Get_Scale (Ptr (Parent).all);
         Feature        := To_Deposit_Ptr (Result);
      exception
         when others =>
            Free (Result);
            raise;
      end;
      Pointer := Position;
   exception
      when Constraint_Error | Unit_Error | End_Error =>
         raise Data_Error;
   end Restore;

   procedure Set_Default
             (  Feature : in out Feature_Object'Class;
                Default : Measure
             )  is
      Object : Output_Feature_Object'Class renames
                  Output_Feature_Object'Class (Feature);
   begin
      if Object.Default.SI = Default.SI then
         Object.Default := Default;
      else
         raise Constraint_Error;
      end if;
   end Set_Default;

   procedure Set_Default
             (  Feature : in out Feature_Handle;
                Default : Measure
             )  is
   begin
      Set_Default (Ptr (Feature).all, Default);
   end Set_Default;

   procedure Set_Defuzzifier
             (  Feature : in out Feature_Object'Class;
                Method  : Defuzzifier_Handle
             )  is
   begin
      if Is_Valid (Method) then
         Output_Feature_Object'Class (Feature).Method := Method;
      else
         raise Constraint_Error;
      end if;
   end Set_Defuzzifier;

   procedure Set_Defuzzifier
             (  Feature : in out Feature_Handle;
                Method  : Defuzzifier_Handle
             )  is
   begin
      if Is_Valid (Method) then
         Output_Feature_Object'Class (Ptr (Feature).all).Method :=
            Method;
      else
         raise Constraint_Error;
      end if;
   end Set_Defuzzifier;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Output_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Put
      (  Destination,
         Position,
         Get_Value_As
         (  Feature.Default,
            Get_Scale (Ptr (Feature.Source).all)
      )  );
      Pointer := Position;
   exception
      when Constraint_Error =>
         raise Use_Error;
   end Store;

   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Ptr (Feature.Source).all, Value);
   end To_Set;

   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Ptr (Feature.Source).all, Value);
   end To_Set;

   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Ptr (Feature.Source).all, Value);
   end To_Set;

   function To_Set
            (  Feature : Output_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Ptr (Feature.Source).all, Value);
   end To_Set;

begin
   if not Is_Registered (Output_Class) then
      Register_Class (Output_Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle.Generic_Output;
