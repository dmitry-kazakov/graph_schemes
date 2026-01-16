--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--        Get_Variables.Get_Integer                Summer, 2005       --
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
--  This procedure recognizes an integer variable definition. It has the
--  syntax:
--
--     <name> : Integer <range-value>
--
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;

with Parsers.FCL.Code.Subsets.Ranges.Integers;

separate (Parsers.FCL.Compiler.Get_Variables)
   procedure Get_Integer
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is
   use Integer_Edit;
   use Parsers.FCL.Code.Subsets.Ranges.Integers;

   Stub   : Mark; -- Mark the tree stack
   Result : Argument_Token;
begin
   Lexers.Parse (Parser, Code, Result);
   declare
      Span : Integer_Range'Class renames Get_Range (Result.Value.all);
   begin
      if Span.Value.To - Span.Value.From >= Max_Cardinality then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Too large cardinality (exceeds"
            &  Image (Domain_Integer (Max_Cardinality))
            &  ") found at "
            &  Image (Span.Location)
         )  );
      end if;
      declare
         Feature : Feature_Handle :=
            Create_Integer (Name, Span.Value.From, Span.Value.To);
      begin
         Created_Feature (Compiler, Feature);
         Add
         (  In_Vars,
            Name,
            (  Kind_Of  => Integer_Var,
               Feature  => Feature,
               Location => Location & Link (Code)
         )  );
      end;
   end;
end Get_Integer;
