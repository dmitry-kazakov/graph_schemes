--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--        Get_Fuzzify                              Summer, 2005       --
--  Separate body implementation                                      --
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
--
--  This  procedure  recognizes  a [de-]fuzzification definition. It has
--  the syntax:
--
--     <name>
--        term <term-name> := <real-membership-function>;
--        ...
--        term <term-name> := <real-membership-function>;
--      [ method : {coa|cog|cogs|lm|rm}; ]
--      [ accu : {asum|bdif|bsum|max|min|prod}; ]
--      [ default := <number>; ]
--
with Fuzzy.Feature.Domain_Floats;   use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Units;                         use Units;

with Fuzzy.Feature.Handle.Defuzzifiers;
with Fuzzy.Feature.Output_Handle.Center_Of_Area;
with Fuzzy.Feature.Output_Handle.Center_Of_Gravity;
with Fuzzy.Feature.Output_Handle.Discrete_Center_Of_Gravity;
with Fuzzy.Feature.Output_Handle.Leftmost_Max;
with Fuzzy.Feature.Output_Handle.Rightmost_Max;

with Parsers.FCL.Code.Subsets.Reals;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Units.Base;

separate (Parsers.FCL.Compiler)
   procedure Get_Fuzzify
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                Vars     : in out Variables_Lists.Dictionary;
                Words    : Keyword_Names.Dictionary;
                Text     : String
             )  is
   use Variables;
   use Variable_Measures;
   use Variable_Sets;
   use Fuzzy.Feature.Handle.Defuzzifiers;

   package Reals renames Parsers.FCL.Code.Subsets.Reals;
   Got_It           : Boolean;
   Default_Given    : Boolean := False;
   Method_Given     : Boolean := False;
   Method_At        : Parsers.Multiline_Source.Location;
   Default_At       : Parsers.Multiline_Source.Location;
   Defuzzify_At     : Parsers.Multiline_Source.Location;
   Default_Value    : Measure;
   Defuzzify_Method : Keyword := Default;

   procedure Get_Default (Name : String) is
      Stub   : Mark; -- Mark the tree stack
      Result : Argument_Token;
   begin
      Get_Blank (Parser, Code, Got_It);
      if not Get (Code'Access, ":=") then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Assignment ':=' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Parser, Code, Got_It);
      if Default_Given then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Variable '"
            &  Name
            &  "' already has defuzzification default specified (at "
            &  Image (Defuzzify_At)
            &  ") as found at "
            &  Image (Link (Code))
         )  );
      end if;
      Lexers.Parse (Parser, Code, Result);
      declare
         use Parsers.FCL.Code.Orders.Numerics;
         use Parsers.FCL.Code.Orders.Numerics.Reals;
         Default : Parsers.FCL.Code.Orders.Numerics.Numeric'Class
                      renames Get_Numeric (Result.Value.all);
      begin
         Default_Value := To_Measure (Default);
         Default_At    := Default.Location;
         Default_Given := True;
      end;
   end Get_Default;

   procedure Get_Method (Name : String) is
   begin
      Get_Colon (Parser, Code);
      if Defuzzify_Method /= Default then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Variable '"
            &  Name
            &  "' already has defuzzification method specified (at "
            &  Image (Defuzzify_At)
            &  ") as found at "
            &  Image (Link (Code))
         )  );
      end if;
      Defuzzify_Method := Get (Code'Access, In_Defuzzify_Method);
      Defuzzify_At := Link (Code);
   end Get_Method;

   procedure Get_Term
             (  Domain : in out Linguistic_Set;
                Scale  : Measure;
                Map    : in out Identifiers_Names.Dictionary
             )  is
      Term_Name : constant String := Get (Code'Access, "term");
      Stub      : Mark; -- Mark the tree stack
      Result    : Argument_Token;
      Term_At   : Parsers.Multiline_Source.Location;
   begin
      Term_At := Link (Code);
      Get_Blank (Parser, Code, Got_It);
      if not Get (Code'Access, ":=") then
         Raise_Exception
         (  Syntax_Error'Identity,
            "Assignment ':=' is expected at " & Image (Link (Code))
         );
      end if;
      Get_Blank (Parser, Code, Got_It);
      Lexers.Parse (Parser, Code, Result);
      declare
         Membership : Reals.Set'Class renames
            Reals.Get_Value_Or_Set (Result.Value.all);
      begin
         Add (Map, Term_Name, Term_At & Link (Code));
         if (  Membership.Value.SI = Units.Base.Unitless
            and then
               Membership.Value.Offset = 0.0
            )
         then
            Add
            (  Domain,
               Term_Name,
               Get_Value (Membership.Value)
            );
         else
            Add
            (  Domain,
               Term_Name,
               Get_Value_As (Membership.Value, Scale)
            );
         end if;
      exception
         when Unit_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Term '"
               &  Term_Name
               &  "' has dimension ["
               &  Image
                  (  Measure'(Get_Unit (Membership.Value), 1.0, 0.0),
                     Messaging_Parameters.Mode
                  )
               &  "] incompatible with expected ["
               &  Image
                  (  Measure'(Scale.SI, 1.0, 0.0),
                     Messaging_Parameters.Mode
                  )
               &  "] as found at "
               &  Image (Term_At & Link (Code))
            )  );
      end;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Malformed term name '"
            &  Term_Name
            &  "' at "
            &  Image (Term_At)
         )  );
      when Name_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Term '"
            &  Term_Name
            &  "' once appeared at "
            &  Image (Find (Map, Term_Name))
            &  " is repeated at "
            &  Image (Term_At)
         )  );
   end Get_Term;

begin
   Get_Blank (Parser, Code, Got_It);
   declare
      Name     : constant String := Get (Code'Access, "variable");
      Domain   : Linguistic_Set;
      Scale    : Measure;
      Location : Parsers.Multiline_Source.Location;
   begin
      Location := Link (Code);
      declare
         Variable : Variable_Descriptor := Find (Vars, Name);
         Map      : Identifiers_Names.Dictionary;
      begin
         if Variable.Kind_Of /= Real_Var then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Variable '"
               &  Name
               &  "' declared at "
               &  Image (Variable.Location)
               &  " is not of real type as expected at "
               &  Image (Location)
            )  );
         elsif Is_Valid (Variable.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Variable '"
               &  Name
               &  "' already has one fuzzification block (at "
               &  Image (Variable.Fuzzify_At)
               &  ") as found at "
               &  Image (Location)
            )  );
         end if;
         if 0 /= Length (Variable.Scale) then
            Scale := Value (To_String (Variable.Scale), Parsing_Mode);
         end if;
         Get_Blank (Parser, Code, Got_It);
         loop
            case Get (Code'Access, Words) is
               when Fuzzify_Term =>
                  Get_Blank (Parser, Code, Got_It);
                  Get_Term (Domain, Scale, Map);
               when Method =>
                  Get_Method (Name);
               when Accumulation_Method =>
                  if Method_Given then
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Method specification once appeared at "
                        &  Image (Method_At)
                        &  " is repeated at "
                        &  Image (Link (Code))
                     )  );
                  end if;
                  Get_Blank (Parser, Code, Got_It);
                  Get_Colon (Parser, Code);
                  if Max_Method /= Get (Code'Access, In_Methods) then
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Infeasible accumulation method (not '"
                        &  Max_Method_Text
                        &  "') found at "
                        &  Image (Link (Code))
                     )  );
                  end if;
                  Method_Given := True;
                  Method_At    := Link (Code);
               when Default =>
                  Get_Default (Name);
               when others =>
                  exit;
            end case;
            Get_Semicolon (Parser, Code);
         end loop;
         if Get_Cardinality (Domain) = 0 then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Empty "
               &  Text
               &  " block at "
               &  Image (Location & Link (Code))
            )  );
         end if;
         if Text = "defuzzification" then
            --
            -- An output feature
            --
            if not Default_Given then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Variable '"
                  &  Name
                  &  "' should have a default value specified "
                  &  Image (Location & Link (Code))
               )  );
            end if;
            if Defuzzify_Method = Default then
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Variable '"
                  &  Name
                  &  "' should have a defuzzification method specified "
                  &  Image (Location & Link (Code))
               )  );
            end if;
            declare
               Defuzzifier : Defuzzifier_Handle;
            begin
               case Defuzzify_Method is
                  when COA_Method =>
                     Defuzzifier := Fuzzy.Feature.Output_Handle.
                                    Center_Of_Area.Create;
                  when COG_Method =>
                     Defuzzifier := Fuzzy.Feature.Output_Handle.
                                    Center_Of_Gravity.Create;
                  when COGS_Method =>
                     Defuzzifier := Fuzzy.Feature.Output_Handle.
                                    Discrete_Center_Of_Gravity.Create;
                  when LM_Method =>
                     Defuzzifier := Fuzzy.Feature.Output_Handle.
                                    Leftmost_Max.Create;
                  when RM_Method =>
                     Defuzzifier := Fuzzy.Feature.Output_Handle.
                                    Rightmost_Max.Create;
                  when others =>
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        "Internal error " & Image (Link (Code))
                     );
               end case;
               if Length (Variable.Scale) /= 0 then
                  Variable.Feature :=
                     Create_Linguistic
                     (  Name & ".Source",
                        Domain,
                        To_String (Variable.Scale),
                        Parsing_Mode
                     );
                  Created_Feature (Compiler, Variable.Feature);
               else
                  Variable.Feature :=
                     Create_Linguistic (Name & ".Source", Domain);
                  Created_Feature (Compiler, Variable.Feature);
               end if;
               if (  Default_Value.SI = Units.Base.Unitless
                  and then
                     Default_Value.Offset = 0.0
                  )
               then
                  Default_Value := Default_Value * Scale;
               end if;
               Variable.Feature :=
                  Create_Output
                  (  Name,
                     Variable.Feature,
                     Defuzzifier,
                     Default_Value
                  );
               Created_Feature (Compiler, Variable.Feature);
            exception
               when Unit_Error =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Default value dimension ["
                     &  Image
                        (  Measure'(Default_Value.SI, 1.0, 0.0),
                           Messaging_Parameters.Mode
                        )
                     &  "] is incompatible with expected ["
                     &  Image
                        (  Measure'(Scale.SI, 1.0, 0.0),
                           Messaging_Parameters.Mode
                        )
                     &  "] as found at "
                     &  Image (Default_At)
                  )  );
            end;
         else
            --
            -- An input feature
            --
            if Length (Variable.Scale) /= 0 then
               Variable.Feature :=
                  Create_Linguistic
                  (  Name,
                     Domain,
                     To_String (Variable.Scale),
                     Parsing_Mode
                  );
               Created_Feature (Compiler, Variable.Feature);
            else
               Variable.Feature := Create_Linguistic (Name, Domain);
               Created_Feature (Compiler, Variable.Feature);
            end if;
         end if;
         Variable.Fuzzify_At := Location & Link (Code);
         Replace (Vars, Name, Variable);
      end;
   exception
      when Name_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Undeclared input variable '"
            &  Name
            &  "' at "
            &  Image (Location)
         )  );
   end;
end Get_Fuzzify;
