--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--         Get_Variables.Get_Float                 Summer, 2005       --
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
--  This procedure recognizes a floating-point variable  definition.  It
--  has the syntax:
--
--     <name> : float <cardinality> range <values-range>
--     <name> : float [<scale>] <cardinality> range <values-range>
--
--  The values in the list have the syntax of an  identifier.  They  may
--  not repeat.
--
with Fuzzy.Feature.Domain_Floats;   use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Units;                         use Units;

with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Subsets.Ranges.Reals;
with Units.Base;

separate (Parsers.FCL.Compiler.Get_Variables)
   procedure Get_Float
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is
   use Fuzzy.Feature.Domain_Floats.Float_Intervals;
   use Fuzzy.Feature.Domain_Floats.Interval_Measures;
   use Integer_Edit;
   use Parsers.FCL.Code.Orders.Numerics.Integers;
   use Parsers.FCL.Code.Subsets.Ranges.Reals;

   Scale  : Measure;
   Text   : Unbounded_String;
   Stub   : Mark; -- Mark the tree stack
   Result : Argument_Token;
   Got_It : Boolean;
begin
   Get_Scale (Parser, Code, Scale, Text);
   Lexers.Parse (Parser, Code, Result);
   declare
      Cardinality : Int'Class renames Get_Integer (Result.Value.all);
   begin
      if not Get (Code'Access, Range_Text) then
         Raise_Exception
         (  Syntax_Error'Identity,
            "'range' is expected at " & Image (Link (Code))
         );
      end if;
      if Cardinality.Value < 1 then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Non-positive cardinality found at "
            &  Image (Cardinality.Location)
         )  );
      elsif Cardinality.Value > Max_Cardinality then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Too large cardinality (exceeds"
            &  Image (Domain_Integer (Max_Cardinality))
            &  ") found at "
            &  Image (Cardinality.Location)
         )  );
      end if;
      Get_Blank (Parser, Code, Got_It);
      Lexers.Parse (Parser, Code, Result);
      declare
         Span  : Real_Range'Class renames Get_Range (Result.Value.all);
         Value : Interval;
      begin
         if (  Span.Value.SI /= Units.Base.Unitless
            or else
               Span.Value.Offset /= 0.0
            )
         then
            begin
               Value := Get_Value_As (Span.Value, Scale);
            exception
               when Unit_Error =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Values range has dimension ["
                     &  Image
                        (  Measure'
                           (  Get_Unit (Span.Value),
                              1.0,
                              0.0
                           ),
                           Messaging_Parameters.Mode
                        )
                     &  "] incompatible with expected ["
                     &  Image
                        (  Measure'(Scale.SI, 1.0, 0.0),
                           Messaging_Parameters.Mode
                        )
                     &  "] as found at "
                     &  Image (Span.Location)
                  )  );
            end;
         else
            Value := Get_Value (Span.Value);
         end if;
         declare
            Feature : Feature_Handle :=
                         Create_Float
                         (  Name,
                            Integer (Cardinality.Value),
                            Value.From,
                            Value.To,
                            To_String (Text),
                            Parsing_Mode
                         );
         begin
            Created_Feature (Compiler, Feature);
            Add
            (  In_Vars,
               Name,
               (  Kind_Of  => Float_Var,
                  Feature  => Feature,
                  Location => Location & Link (Code)
            )  );
         end;
      end;
   end;
end Get_Float;
