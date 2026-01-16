--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                    Luebeck            --
--        Singletons.Integers                      Summer, 2005       --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Fuzzy.Feature.Domain_Floats;    use Fuzzy.Feature.Domain_Floats;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Subsets.Integers;

package body Parsers.FCL.Code.Subsets.Singletons.Integers is
   use Integer_Edit;
   use Variables;

   function To_Singleton (Left : Constant_Value'Class)
      return Integer_Singleton is
   begin
      if Left in Integer_Singleton'Class then
         return Integer_Singleton (Left);
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Integer singleton is expected at "
         &  Image (Left.Location)
      )  );
   end To_Singleton;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Confidence
            )  return Lattice'Class is
   begin
      return
         Integer_Singleton'
         (  Left.Location,
            Left.Value,
            Left.Level and Right
         );
   end Cut;

   function Get_Preference (Left : Integer_Singleton)
      return Preference is
   begin
      return Integer_Singleton_Preference;
   end Get_Preference;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Measure;
   begin
      begin
         Result := Domain_Float (Left.Value) * Right;
         if Result.Gain'Valid then
            return
               Reals.Real_Singleton'
               (  Left.Location & Location,
                  (Result, Left.Level)
               );
         end if;
      exception
         when Unit_Error =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Unit error in scaling at "
               &  Image (Left.Location)
               &  " by the scale at "
               &  Image (Location)
            )  );
         when others =>
            null;
      end;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric error in scaling at "
         &  Image (Left.Location)
         &  " by the scale at "
         &  Image (Location)
      )  );
   end Set_Dimension;

   function To_Real_Singleton (Left : Integer_Singleton)
      return Reals.Real_Singleton is
   begin
      return
         Reals.Real_Singleton'
         (  Left.Location,
            (To_Measure (Domain_Float (Left.Value)), Left.Level)
         );
   end To_Real_Singleton;

   function Equal
            (  Left  : Integer_Singleton;
               Right : Integer_Singleton
            )  return Fuzzy_Boolean is
   begin
      if Left.Value = Right.Value then
         return
         (  Possibility => Left.Level and Right.Level,
            Necessity   => not (Left.Level xor Right.Level)
         );
      else
         return
         (  Possibility => Confidence'First,
            Necessity   => not (Left.Level or Right.Level)
         );
      end if;
   end Equal;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Logic.Intuitionistic.Truth_Value'         (  Location & Left.Location & Right.Location,
            Equal (Left, To_Singleton (Right))
         );
   end EQ;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Integer_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Reals.Join
         (  Location,
            Context,
            Reals.To_Singleton (Left),
            Right
         );
   end Join;

   function Image
            (  Feature : Feature_Handle;
               Item    : Integer_Singleton;
               Mode    : Code_Set
            )  return String is
   begin
      if Item.Level = Confidence'Last then
         return Image (Item.Value);
      else
         return Image (Item.Value) & ":" & Image (Item.Level);
      end if;
   end Image;

   function To_Set (Left : Integer_Singleton) return Subset'Class is
      Point : constant Domain_Float := Domain_Float (Left.Value);
   begin
      return
         Subsets.Integers.Set'
         (  Left.Location,
            (  Pair'(Domain_Float'Pred (Point), Confidence'First)
            &  Pair'(Point,                     Left.Level      )
            &  Pair'(Domain_Float'Succ (Point), Confidence'First)
         )  );
   end To_Set;

end Parsers.FCL.Code.Subsets.Singletons.Integers;
