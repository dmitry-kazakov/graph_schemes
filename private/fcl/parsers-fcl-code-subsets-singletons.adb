--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Singletons                               Spring, 2005       --
--  Implementation                                                    --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Parsers.FCL.Code.Orders;  use Parsers.FCL.Code.Orders;

with Parsers.FCL.Code.Logic.Plain;
with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Orders.Numerics.Reals;
with Parsers.FCL.Code.Subsets.Singletons.Integers;
with Parsers.FCL.Code.Subsets.Singletons.Reals;

package body Parsers.FCL.Code.Subsets.Singletons is

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is
   begin
      return
         Cut
         (  Location,
            Context,
            To_Set (Singleton'Class (Left)),
            Right
         );
   end Cut;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context : Resolution_Context;
               Value   : Singleton
            )  return Logical_Term is
   begin
      return
         Equal_Logical_Term
         (  Location,
            Context,
            To_Set (Singleton'Class (Value))
         );
   end Equal_Logical_Term;

   function Get_Singleton
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Node'Class;
               Right    : Node'Class
            )  return Singleton'Class is
      Value : Numerics.Numeric'Class renames
                 Numerics.Get_Numeric (Context, Left);
      Level : Lattice'Class renames Get_Lattice (Context, Right);
   begin
      if Level in Logic.Plain.Truth_Value'Class then
         if Value in Numerics.Integers.Int'Class then
            return
               Integers.Integer_Singleton'
               (  Location & Value.Location & Level.Location,
                  Numerics.Integers.Int'Class (Value).Value,
                  Logic.Plain.Truth_Value'Class (Level).Value
               );
         else
            return
               Reals.Real_Singleton'
               (  Location & Value.Location & Level.Location,
                  (  Numerics.Reals.Real'Class (Value).Value,
                     Logic.Plain.Truth_Value'Class (Level).Value
               )  );
         end if;
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            "Membership value is expected at " & Image (Level.Location)
         );
      end if;
   end Get_Singleton;

   function Get_Singleton
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Tree     : Node'Class
            )  return Singleton'Class is
      Value : Numerics.Numeric'Class renames
                 Numerics.Get_Numeric (Context, Tree);
   begin
      if Value in Numerics.Integers.Int'Class then
         return
            Integers.Integer_Singleton'
            (  Location & Value.Location,
               Numerics.Integers.Int'Class (Value).Value,
               Confidence'First
            );
      else
         return
            Reals.Real_Singleton'
            (  Location & Value.Location,
               (  Numerics.Reals.Real'Class (Value).Value,
                  Confidence'First
            )  );
      end if;
   end Get_Singleton;

   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Singleton;
               Inversed : Boolean
            )  return Logic.Logical'Class is
   begin
      return
         Is_Subset
         (  Location,
            Context,
            Left,
            To_Set (Singleton'Class (Right)),
            Inversed
         );
   end Is_Subset;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Logical_And
         (  Location,
            Context,
            To_Set (Singleton'Class (Left)),
            Right
         );
   end Logical_And;

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Singleton
            )  return Lattice'Class is
   begin
      return Logical_Not (Location, To_Set (Singleton'Class (Left)));
   end Logical_Not;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Logical_Or
         (  Location,
            Context,
            To_Set (Singleton'Class (Left)),
            Right
         );
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Singleton;
               Right    : Lattice'Class
            )  return Lattice'Class is
   begin
      return
         Logical_Xor
         (  Location,
            Context,
            To_Set (Singleton'Class (Left)),
            Right
         );
   end Logical_Xor;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context : Resolution_Context;
               Value   : Singleton
            )  return Logical_Term is
   begin
      return
         Subset_Logical_Term
         (  Location,
            Context,
            To_Set (Singleton'Class (Value))
         );
   end Subset_Logical_Term;

   function To_Set
            (  Context  : Resolution_Context;
               Left     : Singleton
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return To_Set (Context, To_Set (Singleton'Class (Left)));
   end To_Set;

end Parsers.FCL.Code.Subsets.Singletons;
