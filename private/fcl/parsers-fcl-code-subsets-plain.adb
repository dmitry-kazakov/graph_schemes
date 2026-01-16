--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Plain                                    Spring, 2005       --
--  Implementation                                                    --
--                                Last revision :  11:45 29 May 2020  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Fuzzy;                      use Fuzzy;
with Fuzzy.Edit;                 use Fuzzy.Edit;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Orders.Numerics;
with Parsers.FCL.Code.Subsets.Singletons.Reals;

package body Parsers.FCL.Code.Subsets.Plain is

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term is
   begin
      return Equal_Logical_Term (Get_Set (Context, Value));
   end Equal_Logical_Term;

   function Get_Preference (Left : Set) return Preference is
   begin
      return Fuzzy_Set_Preference;
   end Get_Preference;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Set (1);
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Subset of '"
         &  Get_Name (Context.Expected.Feature)
         &  "' (declared at "
         &  Image (Context.Expected.Location)
         &  ") as found at "
         &  Image (Left.Location)
         &  " cannot have dimension as found at "
         &  Image (Location)
      )  );
      return Result;
   end Set_Dimension;

   function To_Set
            (  Context : Resolution_Context;
               Left    : Constant_Value'Class
            )  return Fuzzy.Set is
   begin
      if Left in Set'Class then
         return Set'Class (Left).Value;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Subset of '"
            &  Get_Name (Context.Expected.Feature)
            &  "' (declared at "
            &  Image (Context.Expected.Location)
            &  ") is expected at "
            &  Image (Left.Location)
         )  );
      end if;
   end To_Set;

   function To_Set
            (  Context : Resolution_Context;
               Left    : Set
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return (Left.Cardinality, Left.Value, Left.Value);
   end To_Set;

   function Equal (Left, Right : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return Is_In (Left, Right) and Is_In (Right, Left);
   end Equal;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is
      Result : Set := Left;
   begin
      Result.Location := Location;
      And_At (Result.Value, Right.Possibility);
      return Result;
   end Cut;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Confidence
            )  return Lattice'Class is
      Result : Set := Left;
   begin
      Result.Location := Location;
      And_At (Result.Value, Right);
      return Result;
   end Cut;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Logic.Intuitionistic.Truth_Value'
         (  Left.Location & Right.Location & Location,
            Equal (Left.Value, To_Set (Context, Right))
         );
   end EQ;

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Set
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Cardinality => Left.Cardinality,
            Location    => Left.Location & Location,
            Value       => not Left.Value
         );
   end Logical_Not;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Cardinality => Left.Cardinality,
            Location    => Left.Location & Right.Location & Location,
            Value       => Left.Value and To_Set (Context, Right)
         );
   end Logical_And;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Cardinality => Left.Cardinality,
            Location    => Left.Location & Right.Location & Location,
            Value       => Left.Value or To_Set (Context, Right)
         );
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Set;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Set'
         (  Cardinality => Left.Cardinality,
            Location    => Left.Location & Right.Location & Location,
            Value       => Left.Value xor To_Set (Context, Right)
         );
   end Logical_Xor;

   function Image
            (  Feature : Feature_Handle;
               Item    : Set;
               Mode    : Code_Set
            )  return String is
   begin
      if Feature.Is_Valid then
         return Image (Feature, Item.Value);
      else
         return Image (Item.Value);
      end if;
   end Image;

   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Set;
               Inversed : Boolean
            )  return Logic.Logical'Class is
      use Singletons;
      use Singletons.Reals;
   begin
      if Inversed then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Location & Right.Location,
               Is_In (Right.Value, Get_Set (Context, Left))
            );
      else
         if (  Left in Orders.Numerics.Numeric'Class
            or else
               Left in Singleton'Class
           )
         then
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In
                  (  Get_Set (Context, To_Set (To_Singleton (Left))),
                     Right.Value
               )  );
         else
            return
               Logic.Intuitionistic.Truth_Value'
               (  Left.Location & Location & Right.Location,
                  Is_In (Get_Set (Context, Left), Right.Value)
               );
         end if;
      end if;
   end Is_Subset;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Set
            )  return Logical_Term is
   begin
      return Subset_Logical_Term (Get_Set (Context, Value));
   end Subset_Logical_Term;

end Parsers.FCL.Code.Subsets.Plain;
