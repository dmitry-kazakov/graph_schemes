--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Logic.                     Luebeck            --
--        Intuitionistic                           Summer, 2005       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Confidence_Factors;       use Confidence_Factors;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;

with Parsers.FCL.Code.Logic.Plain;

package body Parsers.FCL.Code.Logic.Intuitionistic is

   function To_Truth_Value (Literal : Truth_Literal'Class)
      return Truth_Value'Class is
      Result : Truth_Value;
   begin
      Result.Location := Literal.Location;
      Result.Value    := Literal.Value;
      return Result;
   end To_Truth_Value;


   function To_Truth (Left : Constant_Value'Class)
      return Fuzzy_Boolean is
   begin
      if Left in Truth_Value'Class then
         return Truth_Value'Class (Left).Value;
      elsif Left in Plain.Truth_Value'Class then
         declare
            Level : constant Confidence :=
                    Plain.Truth_Value'Class (Left).Value;
         begin
            return (Level, Level);
         end;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            "Truth value [0,1] is expected at " & Image (Left.Location)
         );
      end if;
   end To_Truth;

   function Get_Logical
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Truth_Value'Class is
      Result : constant Lattice'Class := Get_Lattice (Context, Tree);
   begin
      return
         Truth_Value'
         (  Result.Location,
            To_Truth (Result)
         );
   end Get_Logical;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Truth_Value'
         (  Left.Location & Right.Location & Location,
            not (Left.Value xor To_Truth (Right))
         );
   end EQ;

   function Get_Preference (Left : Truth_Value) return Preference is
   begin
      return Intuitionistic_Logical_Preference;
   end Get_Preference;

   function Image
            (  Feature : Feature_Handle;
               Item    : Truth_Value;
               Mode    : Code_Set
            )  return String is
   begin
      if Item.Value.Possibility = Item.Value.Necessity then
         return Image (Item.Value.Possibility);
      else
         return
         (  Image (Item.Value.Necessity)
         &  ".."
         &  Image (Item.Value.Possibility)
         );
      end if;
   end Image;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Truth_Value'
         (  Left.Location & Right.Location & Location,
            Left.Value and To_Truth (Right)
         );
   end Logical_And;

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Truth_Value
            )  return Lattice'Class is
   begin
      return Truth_Value'(Left.Location & Location, not Left.Value);
   end Logical_Not;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Truth_Value'
         (  Left.Location & Right.Location & Location,
            Left.Value or To_Truth (Right)
         );
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Truth_Value;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Truth_Value'
         (  Left.Location & Right.Location & Location,
            Left.Value xor To_Truth (Right)
         );
   end Logical_Xor;

   function "not" (Left : Truth_Value) return Truth_Value is
   begin
      return (Left.Location, not Left.Value);
   end "not";

end Parsers.FCL.Code.Logic.Intuitionistic;
