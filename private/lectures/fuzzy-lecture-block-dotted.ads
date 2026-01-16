--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Block.Dotted                  Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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
--
--  This package defines the type Dot_Column_Block. The column blocks of
--  this type can contain only delta values. I.e.  ones  having  exactly
--  one domain point in any value.
--
package Fuzzy.Lecture.Block.Dotted is

   Uncertain_Index : constant :=  0;
   Undefined_Index : constant := -1;

   type Dot_Array is array (Subindex) of Integer;
   type Dot_Column_Block is new Column_Block with record
      Data : Dot_Array := (others => Uncertain_Index);
   end record;
--
-- Get - Overrides Fuzzy.Lecture.Block...
--
   overriding
   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Set;
--
-- Get - Overrides Fuzzy.Lecture.Block...
--
   overriding
   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex;
               Value : Positive
            )  return Confidence;
--
-- Get - Overrides Fuzzy.Lecture.Block...
--
   overriding
   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex;
               Value : Set
            )  return Confidence;
--
-- Is_Defined - Overrides Fuzzy.Lecture.Block...
--
   overriding
   function Is_Defined
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Value_Status;
--
-- Is_Known - Overrides Fuzzy.Lecture.Block...
--
   overriding
   function Is_Known
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Boolean;
--
-- Put - Overrides Fuzzy.Lecture.Block...
--
-- This  procedure  always raise Constraint_Error if the parameter Value
-- is not a singleton.
--
   overriding
   procedure Put
             (  Block : in out Dot_Column_Block;
                Index : Subindex;
                Value : Set
             );
--
-- Put - Overrides Fuzzy.Lecture.Block...
--
-- This procedure raises Constraint_Error  if  Value  differs  from  one
-- already set.
--
   overriding
   procedure Put
             (  Block : in out Dot_Column_Block;
                Index : Subindex;
                Value : Positive
             );
--
-- Set_Undefined - Overrides Fuzzy.Lecture.Block...
--
   overriding
   procedure Set_Undefined
             (  Block  : in out Dot_Column_Block;
                Index  : Subindex;
                Status : Undefined_Status
             );

private
   pragma Inline (Is_Defined);
   pragma Inline (Put);
   pragma Inline (Set_Undefined);

end Fuzzy.Lecture.Block.Dotted;
