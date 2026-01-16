--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets                    Luebeck            --
--  Implementation                                 Summer, 2005       --
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

with Ada.Exceptions;  use Ada.Exceptions;

with Parsers.FCL.Code.Orders.Numerics;

package body Parsers.FCL.Code.Subsets is

   function Classify_To_Set (Set : Fuzzy.Intuitionistic.Classification)
      return Fuzzy.Intuitionistic.Set is
   begin
      return
      (  Cardinality => Set.Cardinality,
         Possibility => Set.Possibility,
         Necessity   => Set.Necessity
      );
   end Classify_To_Set;

   function Get_Set
            (  Context : Resolution_Context;
               Left    : Constant_Value'Class
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Left in Subset'Class then
         return To_Set (Context, Subset'Class (Left));
      end if;
      if Is_Valid (Context.Expected.Feature) then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Subset of '"
            &  Get_Name (Context.Expected.Feature)
            &  "' (declared at "
            &  Image (Context.Expected.Location)
            &  ") is expected at "
            &  Image (Left.Location)
         )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            "Subset is expected at " & Image (Left.Location)
         );
      end if;
   end Get_Set;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Subset
            )  return Logical_Term is
      Result : Logical_Term;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Subset operand at "
         &  Image (Value.Location)
         &  " is not allowed for '"
         &  Get_Name (Context.Expected.Feature)
         &  "' in the operation at "
         &  Image (Location)
      )  );
      return Result;
   end Greater_Logical_Term;

   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Subset
            )  return Logical_Term renames Greater_Logical_Term;

   function Subset_Test
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Node'Class;
               Right    : Node'Class;
               Mode     : Subsetting_Mode
            )  return Logic.Logical'Class is
      First  : constant Constant_Value'Class :=
                  Get_Comparable (Context, Left);
      Second : constant Lattice'Class :=
                  Get_Lattice (Context, Right);
   begin
      if Second not in Subset'Class then
         if Is_Valid (Context.Expected.Feature) then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Subset of '"
               &  Get_Name (Context.Expected.Feature)
               &  "' (declared at "
               &  Image (Context.Expected.Location)
               &  ") is expected at "
               &  Image (Second.Location)
               &  " in operation at "
               &  Image (Location)
           )  );
         else
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Subset is expected at "
               &  Image (Second.Location)
               &  " in operation at "
               &  Image (Location)
           )  );
         end if;
      end if;
      case Mode is
         when Subset_Mode =>
            if First not in Subset'Class then
               if Is_Valid (Context.Expected.Feature) then
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Subset of '"
                     &  Get_Name (Context.Expected.Feature)
                     &  "' (declared at "
                     &  Image (Context.Expected.Location)
                     &  ") is expected at "
                     &  Image (First.Location)
                     &  " in operation at "
                     &  Image (Location)
                  )  );
               else
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Subset is expected at "
                     &  Image (First.Location)
                     &  " in operation at "
                     &  Image (Location)
                  )  );
               end if;
            end if;
         when Element_Mode =>
            if First in Subset'Class then
               if Is_Valid (Context.Expected.Feature) then
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Domain value (instead of a subset) of '"
                     &  Get_Name (Context.Expected.Feature)
                     &  "' (declared at "
                     &  Image (Context.Expected.Location)
                     &  ") is expected at "
                     &  Image (First.Location)
                     &  " in operation at "
                     &  Image (Location)
                  )  );
               else
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Domain value (instead of a subset) is "
                     &  "expected at "
                     &  Image (First.Location)
                     &  " in operation at "
                     &  Image (Location)
                  )  );
               end if;
            end if;
         when Universal_Mode =>
            null;
      end case;
      if First in Orders.Numerics.Numeric'Class then
         return
            Is_Subset
            (  Location,
               Context,
               First,
               Subset'Class (Second),
               False
            );
      elsif Get_Preference (Second) < Get_Preference (First) then
         return
            Is_Subset
            (  Location,
               Context,
               Subset'Class (Second),
               Subset'Class (First),
               True
            );
      else
         return
            Is_Subset
            (  Location,
               Context,
               First,
               Subset'Class (Second),
               False
            );
      end if;
   end Subset_Test;

end Parsers.FCL.Code.Subsets;
