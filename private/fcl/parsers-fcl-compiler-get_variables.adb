--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--        Get_Variables                            Summer, 2005       --
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

separate (Parsers.FCL.Compiler)
   procedure Get_Variables
             (  Compiler : in out Program'Class;
                Parser   : in out FCL_Expression;
                Code     : in out Parsers.Multiline_Source.Source'Class;
                In_Vars  : in out Variables_Lists.Dictionary;
                Out_Vars : Variables_Lists.Dictionary
             )  is
   procedure Get_Discrete
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is separate;
   procedure Get_Float
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is separate;
   procedure Get_Integer
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is separate;
   procedure Get_Real
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is separate;
   Got_It : Boolean;
begin
   loop
      Get_Blank (Parser, Code, Got_It);
      declare
         Name     : constant String := Get (Code'Access, "variable");
         Type_Of  : Variable_Type;
         Location : Parsers.Multiline_Source.Location;
      begin
         Location := Link (Code);
         Get_Blank (Parser, Code, Got_It);
         if not Get (Code'Access, ":") then
            exit when Same (Name, End_Var_Text);
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Colon ':' is expected at "
               &  Image (Link (Code))
            )  );
         end if;
         if IsIn (In_Vars, Name) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Duplicate declaration of '"
               &  Name
               &  "' (first declaration at "
               &  Image (Find (In_Vars, Name).Location)
               &  ") at "
               &  Image (Location)
            )  );
         end if;
         if IsIn (Out_Vars, Name) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Duplicate declaration of '"
               &  Name
               &  "' (first declaration at "
               &  Image (Find (Out_Vars, Name).Location)
               &  ") at "
               &  Image (Location)
            )  );
         end if;
         Get_Blank (Parser, Code, Got_It);
         Set_Pointer (Code, Get_Pointer (Code));
         Get_Type (Code, Types_Table, Type_Of, Got_It);
         if not Got_It then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Variable type "
               &  Image (Types_Table)
               &  " is expected at " & Image (Link (Code))
            )  );
         end if;
         Get_Blank (Parser, Code, Got_It);
         case Type_Of is
            when Discrete_Var =>
               Get_Discrete (Name, Location);
            when Float_Var =>
               Get_Float (Name, Location);
            when Integer_Var =>
               Get_Integer (Name, Location);
            when Real_Var =>
               Get_Real (Name, Location);
         end case;
         Get_Semicolon (Parser, Code);
      end;
   end loop;
end Get_Variables;
