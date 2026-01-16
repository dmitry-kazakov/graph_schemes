--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Logic                      Luebeck            --
--  Implementation                                 Summer, 2005       --
--                                                                    --
--                                Last revision :  12:48 16 Oct 2010  --
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
with Units.Base;         use Units.Base;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Logic.Plain;

package body Parsers.FCL.Code.Logic is

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is
   begin
      return Cut (Location, Context, Left, Confidence'Last);
   end Cut;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Confidence
            )  return Lattice'Class is
   begin
      if not Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Subset is expected at "
            &  Image (Left.Location)
            &  " in the cut-operation at "
            &  Image (Location)
         )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Subset of '"
            &  Get_Name (Context.Expected.Feature)
            &  "' (declared at "
            &  Image (Context.Expected.Location)
            &  ") is expected at "
            &  Image (Left.Location)
            &  " in the cut-operation at "
            &  Image (Location)
         )  );
      end if;
      return Logic.Plain.Truth_Value'(Logical with 0.0);
   end Cut;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term renames Subset_Logical_Term;

   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term renames Subset_Logical_Term;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term renames Subset_Logical_Term;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Logical;
               Right    : Measure
            )  return Constant_Value'Class is
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Logical value at "
         &  Image (Left.Location)
         &  " cannot have dimension as found at "
         &  Image (Location)
      )  );
      return Logic.Plain.Truth_Value'(Logical with 0.0);
   end Set_Dimension;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Logical
            )  return Logical_Term is
      Result : Logical_Term;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Logical operand at "
         &  Image (Value.Location)
         &  " is not allowed for '"
         &  Get_Name (Context.Expected.Feature)
         &  "' in the operation at "
         &  Image (Location)
      )  );
      return Result;
   end Subset_Logical_Term;

   function To_Logical (Left : Constant_Value'Class)
      return Logical'Class is
   begin
      if Left in Intuitionistic.Truth_Value'Class then
         return Intuitionistic.Truth_Value'Class (Left);
      elsif Left in Plain.Truth_Value'Class then
         return Plain.Truth_Value'Class (Left);
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         "Truth value [0,1] is expected at " & Image (Left.Location)
      );
   end To_Logical;

   function To_Confidence (Value : Domain_Integer) return Confidence is
   begin
      if Value = 0 then
         return Confidence'First;
      elsif Value = 1 then
         return Confidence'Last;
      end if;
      raise Data_Error;
   end To_Confidence;

   function To_Confidence (Value : Domain_Float) return Confidence is
   begin
      if Value in 0.0..1.0 then
         begin
            return Confidence (Value);
         exception
            when Constraint_Error =>
               if Value < 0.5 then
                  return Confidence'First;
               else
                  return Confidence'Last;
               end if;
         end;
      end if;
      raise Data_Error;
   end To_Confidence;

   function To_Confidence (Value : Measure) return Confidence is
   begin
      if Value.SI /= Unitless then
         raise Unit_Error;
      else
         return To_Confidence (Get_Value (Value));
      end if;
   end To_Confidence;

end Parsers.FCL.Code.Logic;
