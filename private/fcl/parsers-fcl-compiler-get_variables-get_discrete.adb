--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--         Get_Variables.Get_Discrete              Summer, 2005       --
--  Separate body implementation                                      --
--                                Last revision :  21:30 10 Nov 2009  --
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
--  This procedure recognizes a discrete variable definition. It has the
--  syntax:
--
--     <name> : discrete (<value>, <value>, ...)
--
--  The values in the list have the syntax of an  identifier.  They  may
--  not repeat.
--
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;

separate (Parsers.FCL.Compiler.Get_Variables)
   procedure Get_Discrete
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is
   Map      : Identifiers_Names.Dictionary;
   Domain   : Unbounded_String;
   Value_At : Parsers.Multiline_Source.Location;
   Got_It   : Boolean;
begin
   if not Get (Code'Access, "(") then
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Left bracket opening a list of discrete values "
         &  "is expected at "
         &  Image (Link (Code))
      )  );
   end if;
   Get_Blank (Parser, Code, Got_It);
   loop
      declare
         Value : String renames Get (Code'Access, "discrete value");
      begin
         Value_At := Link (Code);
         Add (Map, Value, Value_At);
         Append (Domain, Value);
      exception
         when Constraint_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Malformed discrete value '"
               &  Value
               &  "' at "
               &  Image (Value_At)
            )  );
         when Name_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Discrete value '"
               &  Value
               &  "' once appeared at "
               &  Image (Find (Map, Value))
               &  " is repeated at "
               &  Image (Value_At)
            )  );
      end;
      Get_Blank (Parser, Code, Got_It);
      exit when not Get (Code'Access, ",");
      Append (Domain, ",");
      Get_Blank (Parser, Code, Got_It);
   end loop;
   if not Get (Code'Access, ")") then
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Right bracket closing the list of discrete values "
         &  "is expected at "
         &  Image (Link (Code))
      )  );
   end if;
   declare
      Feature : Feature_Handle :=
                   Create_Discrete (Name, To_String (Domain));
   begin
      Created_Feature (Compiler, Feature);
      Add
      (  In_Vars,
         Name,
         (  Kind_Of  => Discrete_Var,
            Feature  => Feature,
            Location => Location & Link (Code)
      )  );
   end;
end Get_Discrete;
