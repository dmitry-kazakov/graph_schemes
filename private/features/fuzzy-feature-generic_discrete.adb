--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Discrete              Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
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

with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;  use Ada.Strings.Maps.Constants;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;            use Fuzzy.Basic_Edit;
with Strings_Edit.Quoted;         use Strings_Edit.Quoted;

with Fuzzy.Feature.Discrete;

pragma Elaborate_All (Fuzzy.Feature.Discrete);

package body Fuzzy.Feature.Generic_Discrete is

   function Create (Name : String) return Feature_Handle is
      Ptr : constant Feature_Object_Ptr :=
               new Discrete_Feature_Object
                   (  Domain_Type'Pos (Domain_Type'Last)
                   -  Domain_Type'Pos (Domain_Type'First)
                   +  1
                   );
   begin
      Ptr.Self := Ptr;
      Ptr.Name := new String'(Name);
      return Ref (Ptr);
   end Create;

   procedure Get_Value
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Positive
             )  is
      Index : Integer := Pointer;
   begin
      while (  Index <= Source'Last
            and then
               Is_In (Source (Index), Alphanumeric_Set)
            )
      loop
         Index := Index + 1;
      end loop;
      Value :=
         (  Domain_Type'Pos
            (  Domain_Type'Value
               (  Source
                  (  Pointer
                  .. Index - 1
            )  )  )
         -  Domain_Type'Pos (Domain_Type'First)
         +  1
         );
      Pointer := Index;
   exception
      when Constraint_Error =>
         raise End_Error;
   end Get_Value;

   function Get_Class (Feature : Discrete_Feature_Object)
      return String is
   begin
      return Fuzzy.Feature.Discrete.Discrete_Class;
   end Get_Class;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Discrete_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
      Got_It : Boolean;
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
      Get_Value (Source, Pointer, From);
      Get_Delimiter (Source, Pointer, Got_It, Ellipsis);
      if Got_It then
         begin
             Get_Value (Source, Pointer, To);
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         To := From;
      end if;
      Exclusive := True;
   end Get_Range;

   function Image (Index : Positive) return String is
   begin
      return
         Domain_Type'Image
         (  Domain_Type'Val
            (  Index
            -  1
            +  Domain_Type'Pos (Domain_Type'First)
         )  );
   end Image;

   function Is_Discrete (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Discrete_Feature_Object'Class;
   end Is_Discrete;

   function Is_Discrete (Feature : Feature_Handle) return Boolean is
   begin
      return
      (  Feature.Is_Valid
      and then
         Ptr (Feature).all in Discrete_Feature_Object'Class
      );
   end Is_Discrete;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if To > Feature.Cardinality or else From > To then
         raise Constraint_Error;
      end if;
      if From = To then
         Put
         (  Destination,
            Pointer,
            Image (From),
            Field,
            Justify,
            Fill
         );
      else
         Put
         (  Destination,
            Pointer,
            Image (From) & ".." & Image (To),
            Field,
            Justify,
            Fill
         );
      end if;
   end Put_Range;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Feature.Name.all);
      Put (Destination, Position, ": ");
      for Element in 1..Feature.Cardinality loop
         if Element /= 1 then
            Put (Destination, Position, ", ");
         end if;
         Put (Destination, Position, Image (Element));
      end loop;
      Pointer := Position;
   end Store;

end Fuzzy.Feature.Generic_Discrete;
