--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Singletons.Reals                         Spring, 2005       --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Confidence_Factors;           use Confidence_Factors;
with Confidence_Factors.Edit;      use Confidence_Factors.Edit;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Units;                        use Units;

with Parsers.FCL.Code.Logic.Intuitionistic;
with Parsers.FCL.Code.Subsets.Singletons.Integers;
with Parsers.FCL.Code.Subsets.Reals;
with Units.Base;

package body Parsers.FCL.Code.Subsets.Singletons.Reals is
   use Float_Edit;
   use Measure_Edit;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Confidence
            )  return Lattice'Class is
   begin
      return
         Real_Singleton'
         (  Left.Location,
            (Left.Value.Value, Left.Value.Level and Right)
         );
   end Cut;


   function Get_Dimension (Left : Real_Singleton'Class) return String is
   begin
      return
      (  "["
      &  Image
         (  Measure'(Left.Value.Value.SI, 1.0, 0.0),
            Messaging_Parameters.Mode
         )
      &  "]"
      );
   end Get_Dimension;

   function Get_Preference (Left : Real_Singleton) return Preference is
   begin
      return Real_Singleton_Preference;
   end Get_Preference;

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Measure;
   begin
      begin
         Result := Left.Value.Value * Right;
         if Result.Gain'Valid then
            return
               Real_Singleton'
               (  Left.Location & Location,
                  (Result, Left.Value.Level)
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

   function To_Singleton (Left : Constant_Value'Class)
      return Real_Singleton is
   begin
      if Left in Real_Singleton'Class then
         return Real_Singleton (Left);
      elsif Left in Integers.Integer_Singleton'Class then
         return
            Integers.To_Real_Singleton
            (  Integers.Integer_Singleton'Class (Left)
            );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Singleton is expected at "
         &  Image (Left.Location)
      )  );
   end To_Singleton;

   function Equal
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Real_Singleton;
               Right    : Real_Singleton
            )  return Logic.Logical'Class is
   begin
      if Left.Value.Value = Right.Value.Value then
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Right.Location & Location,
               (  Possibility =>
                     Left.Value.Level and Right.Value.Level,
                  Necessity =>
                     not (Left.Value.Level xor Right.Value.Level)
            )  );
      else
         return
            Logic.Intuitionistic.Truth_Value'
            (  Left.Location & Right.Location & Location,
               (  Possibility =>
                     Confidence'First,
                  Necessity =>
                     not (Left.Value.Level or Right.Value.Level)
            )  );
      end if;
   exception
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible units "
            &  Get_Dimension (Left)
            &  " and "
            &  Get_Dimension (Right)
            &  " of values at "
            &  Image (Left.Location)
            &  " and at "
            &  Image (Right.Location)
         )  );
      when Constraint_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Out of range values at "
            &  Image (Left.Location)
            &  " and at "
            &  Image (Right.Location)
         )  );
   end Equal;

   --  function Greater
   --           (  Location : Parsers.Multiline_Source.Location;
   --              Left     : Real_Singleton;
   --              Right    : Real_Singleton
   --           )  return Logic.Logical'Class is
   --  begin
   --     if Left.Value.Value > Right.Value.Value then
   --        return
   --           Logic.Intuitionistic.Truth_Value'
   --           (  Left.Location & Right.Location & Location,
   --              (  Possibility => Left.Value.Level and Right.Value.Level,
   --                 Necessity   => Confidence'Last
   --           )  );
   --     else
   --        return
   --           Logic.Intuitionistic.Truth_Value'
   --           (  Left.Location & Right.Location & Location,
   --              (  Possibility =>
   --                    Confidence'First,
   --                 Necessity =>
   --                    not (Left.Value.Level and Right.Value.Level)
   --           )  );
   --     end if;
   --  exception
   --     when Unit_Error =>
   --        Raise_Exception
   --        (  Syntax_Error'Identity,
   --           (  "Incompatible units "
   --           &  Get_Dimension (Left)
   --           &  " and "
   --           &  Get_Dimension (Right)
   --           &  " of values at "
   --           &  Image (Left.Location)
   --           &  " and at "
   --           &  Image (Right.Location)
   --        )  );
   --     when Constraint_Error =>
   --        Raise_Exception
   --        (  Syntax_Error'Identity,
   --           (  "Out of range values at "
   --           &  Image (Left.Location)
   --           &  " and at "
   --           &  Image (Right.Location)
   --        )  );
   --  end Greater;
   --
   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return Equal (Location, Left, To_Singleton (Right));
   end EQ;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Real_Singleton;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
      Other : constant Real_Singleton := To_Singleton (Right);
   begin
      return
      (  Subsets.Reals.Set'
         (  Left.Location & Right.Location,
            Left.Value & Other.Value
      )  );
   exception
      when Data_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Singleton value at "
            &  Image (Left.Location)
            &  " must precede one at "
            &  Image (Right.Location)
         )  );
      when Unit_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Incompatible units "
            &  Get_Dimension (Left)
            &  " and "
            &  Get_Dimension (Other)
            &  " of values at "
            &  Image (Left.Location)
            &  " and at "
            &  Image (Right.Location)
         )  );
      when Constraint_Error =>
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Out of range values at "
            &  Image (Left.Location)
            &  " and at "
            &  Image (Right.Location)
         )  );
   end Join;

   function Get_Unit_Text (Item : Measure; Mode : Code_Set)
       return String is
   begin
      if Item.SI = Units.Base.Unitless and then Item.Offset = 0.0 then
         return "";
      else
         return
         (  " ["
         &  Image (Measure'(Item.SI, 1.0, Item.Offset), Mode)
         &  "]"
         );
      end if;
   end Get_Unit_Text;

   function Image
            (  Feature : Feature_Handle;
               Item    : Real_Singleton;
               Mode    : Code_Set
            )  return String is
   begin
      if Item.Value.Level = Confidence'Last then
         return
         (  Image (Item.Value.Value.Gain)
         &  Get_Unit_Text (Item.Value.Value, Mode)
         );
      else
         return
         (  Image (Item.Value.Value.Gain)
         &  Get_Unit_Text (Item.Value.Value, Mode)
         &  ":"
         &  Image (Item.Value.Level)
         );
      end if;
   end Image;

   function To_Set (Left : Real_Singleton) return Subset'Class is
      Point : constant Measure := Left.Value.Value;
   begin
      return
         Subsets.Reals.Set'
         (  Left.Location,
            (  Pair_Measure'
               (  (  Point.SI,
                     Domain_Float'Pred (Point.Gain),
                     Point.Offset
                  ),
                  Confidence'First
               )
            &  Pair_Measure'(Point, Left.Value.Level)
            &  Pair_Measure'
               (  (  Point.SI,
                     Domain_Float'Succ (Point.Gain),
                     Point.Offset
                  ),
                  Confidence'First
         )  )  );
   end To_Set;

end Parsers.FCL.Code.Subsets.Singletons.Reals;
