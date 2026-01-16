--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Name_Tables                         Luebeck            --
--  Implementation                                 Winter, 2003       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package body Name_Tables is

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
      Code  : UTF8_Code_Point;
      Index : Integer := Pointer;
   begin
      Get (Source, Index, Code);
      return not Is_In (Code, Non_Breaks);
   exception
      when Data_Error =>
         return False;
   end Check_Matched;

   procedure Check_Name (Name : String) is
      Code    : UTF8_Code_Point;
      Pointer : Integer := Name'First;
   begin
      if Name'Length = 0 then
         Raise_Exception (Constraint_Error'Identity, "Empty name");
      end if;
      Get (Name, Pointer, Code);
      if not Is_In (Code, Capitals) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Name starts with a wrong character"
         );
      end if;
      while Pointer <= Name'Last loop
         Get (Name, Pointer, Code);
         if Is_In (Code, Singletons) then
            loop -- Skip ignored characters
               if Pointer > Name'Last then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Name ends with a wrong character"
                  );
               end if;
               Get (Name, Pointer, Code);
               exit when not Is_In (Code, Ignored);
            end loop;
            if Is_In (Code, Singletons) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Name contains consequent singleton character"
               );
            end if;
         end if;
         if not Is_In (Code, Name_Body) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Name contains a wrong character"
            );
         end if;
      end loop;
   exception
      when Data_Error =>
         Raise_Exception
         (  Constraint_Error'Identity,
            "Name is not an UTF-8 string"
         );
   end Check_Name;

   function Get_Name (Source : String; Pointer : access Integer)
      return String is
      Index : Integer := Pointer.all;
      To    : Integer;
      Code  : UTF8_Code_Point;
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
      --
      -- Get the first code point of the name
      --
      begin
         Get (Source, Index, Code);
      exception
         when Data_Error =>
            raise End_Error;
      end;
      if not Is_In (Code, Capitals) then
         raise End_Error;
      end if;
      To := Index;
      --
      -- Searching for  the  name  end,  first  we  skip  anything  from
      -- Name_Body.
      --
      declare
         Singletons_Chain : Boolean := False;
      begin
         while Index <= Source'Last loop
            Get (Source, Index, Code);
            exit when not Is_In (Code, Name_Body);
            if not Is_In (Code, Ignored) then
               if Is_In (Code, Singletons) then
                  if Singletons_Chain then
                     --
                     -- A chain of singletons  is  broken  as  early  as
                     -- possible. When a singeton cannot be broken at it
                     -- is consumed by the name.
                     --
                     exit when not Is_In (Code, Non_Breaks);
                     To := Index;
                  else
                     --
                     -- A new  chain  of  singletons  begins  here.  The
                     -- singleton is consumed when cannot be broken at.
                     --
                     Singletons_Chain := True;
                     if Is_In (Code, Non_Breaks) then
                        To := Index;
                     end if;
                  end if;
               else
                  Singletons_Chain := False;
                  To := Index;
               end if;
            end if;
         end loop;
      exception
         when Data_Error =>
            null;
      end;
      Index := Pointer.all;
      Check_Name (Source (Index..To - 1));
      Pointer.all := To;
      return Name_Maps.Canonize (Source (Index..To - 1));
   end Get_Name;

   function Is_Body (Value : UTF8_Code_Point) return Boolean is
   begin
      case Category (Value) is
         when Letter | Nl | Nd | Pc | Zs | Mn | Mc | Cf => return True;
         when others => return Value = 16#09#;
      end case;
   end Is_Body;

   function Is_Blank (Value : UTF8_Code_Point) return Boolean is
   begin
      return Value = 16#09# or else Category (Value) = Zs;
   end Is_Blank;

   function Is_Non_Break (Value : UTF8_Code_Point) return Boolean is
   begin
      case Category (Value) is
         when Letter | Nl | Nd | Pc | Mn | Mc | Cf => return True;
         when others => return False;
      end case;
   end Is_Non_Break;

   function Is_Singleton (Value : UTF8_Code_Point) return Boolean is
   begin
      case Category (Value) is
         when Pc | Zs => return True;
         when others  => return Value = 16#09#;
      end case;
   end Is_Singleton;

end Name_Tables;
