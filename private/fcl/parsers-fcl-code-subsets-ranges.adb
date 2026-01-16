--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Subsets.                   Luebeck            --
--        Ranges                                   Summer, 2005       --
--  Implementation                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

package body Parsers.FCL.Code.Subsets.Ranges is

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Numeric_Range
            )  return Logical_Term is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Value));
   begin
      return Equal_Logical_Term (Location, Context, Set); 
   end Equal_Logical_Term;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Fuzzy_Boolean
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Cut (Location, Context, Set, Right);
   end Cut;

   function Cut
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Confidence
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Cut (Location, Context, Set, Right);
   end Cut;

   function Is_Subset
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Constant_Value'Class;
               Right    : Numeric_Range;
               Inversed : Boolean
            )  return Logic.Logical'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Right));
   begin
      return Is_Subset (Location, Context, Left, Set, Inversed);
   end Is_Subset;

   function Join
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Join
         (  Location,
            Context,
            To_Set (Numeric_Range'Class (Left)),
            Right
         );
   end Join;

   function Logical_Not
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Numeric_Range
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Logical_Not (Location, Set); 
    end Logical_Not;

   function Logical_And
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Lattice'Class 
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Logical_And (Location, Context, Set, Right);
   end Logical_And;

   function Logical_Or
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Lattice'Class 
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Logical_Or (Location, Context, Set, Right);
   end Logical_Or;

   function Logical_Xor
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Numeric_Range;
               Right    : Lattice'Class 
            )  return Lattice'Class is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Left));
   begin
      return Logical_Xor (Location, Context, Set, Right);
   end Logical_Xor;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Numeric_Range
            )  return Logical_Term is
      Set : Subset'Class renames To_Set (Numeric_Range'Class (Value));
   begin
      return Subset_Logical_Term (Location, Context, Set);
   end Subset_Logical_Term;

end Parsers.FCL.Code.Subsets.Ranges;
