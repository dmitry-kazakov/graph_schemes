--                                                                    --
--  package Fuzzy.Lecture.Block     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  12:48 30 Aug 2010  --
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
--  This package defines the type Column_Block. The type is abstract and
--  servers  as  the  base for different kinds of column blocks of fuzzy
--  training set. Each column is broken into a set of blocks. Blocks are
--  allocated  as  necessary.  The  block  size determines the amount of
--  examples by which a teaching set is enlarged.
--
package Fuzzy.Lecture.Block is
   Block_Size : constant := 64;
   subtype Subindex is Positive range 1..Block_Size;
--
-- Value_Status -- The status of a value in the block
--
--     Uncertain - The value is not defined yet
--     Undefined - The value is set to be explicity undefined
--     Defined   - The value is defined
--
   type Value_Status is (Uncertain, Undefined, Defined);
   subtype Undefined_Status is Value_Status range Uncertain..Undefined;
   type Column_Block (Cardinality : Positive) is
      abstract tagged null record;
   type Column_Block_Ptr is access Column_Block'Class;
   type Column_Block_Ptr_Array is
      array (Positive range <>) of Column_Block_Ptr;
--
-- Get - Get value from the block
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--
-- This function extracts a complete value (a fuzzy set) from the column
-- block Block. If the value is unset the result is all 1.
--
-- Returns :
--
--    The value (fuzzy set)
--
   function Get
            (  Block : Column_Block;
               Index : Subindex
            )  return Set is abstract;
--
-- Get - Get one value point from the block
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--    Value - The domain set point 1..Cardinality
--
-- This function extracts the confidence that the point Value belongs to
-- the referenced value from the Block.
--
-- Returns :
--
--    The confidence the Value belongs to
--
-- Exceptions :
--
--    Constraint_Error - Value is not in 1..Cardinality
--
   function Get
            (  Block : Column_Block;
               Index : Subindex;
               Value : Positive
            )  return Confidence is abstract;
--
-- Get - Get conditional possibility
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--    Value - The set
--
-- This function calculates the conditional possibility of the parameter
-- Value. The condition is the value in Block referenced by Index.
--
-- Returns :
--
--    The confidence
--
-- Exceptions :
--
--    Constraint_Error - Value has cardinality other than Cardinality
--
   function Get
            (  Block : Column_Block;
               Index : Subindex;
               Value : Set
            )  return Confidence is abstract;
--
-- Is_Defined - Check if a value is in the block
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--
-- Returns :
--
--    Status of the value
--
   function Is_Defined
            (  Block : Column_Block;
               Index : Subindex
            )  return Value_Status is abstract;
--
-- Is_Known - Check if a value is not all 1
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--
-- Returns :
--
--    True if the value is defined and not all 1
--
   function Is_Known
            (  Block : Column_Block;
               Index : Subindex
            )  return Boolean is abstract;
--
-- Put - Set value to the block
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--    Value - The value set
--
-- Exceptions :
--
--    Constraint_Error - Value has cardinality other than Cardinality
--
   procedure Put
             (  Block : in out Column_Block;
                Index : Subindex;
                Value : Set
             )  is abstract;
--
-- Put - Set a singular point in the block value
--
--    Block - The block of a column
--    Index - The row number within the block 1..Block_Size
--    Value - The domain set point 1..Cardinality
--
-- Exceptions :
--
--    Constraint_Error - Value is not in 1..Cardinality
--
   procedure Put
             (  Block : in out Column_Block;
                Index : Subindex;
                Value : Positive
             )  is abstract;
--
-- Set_Undefined - Delete a value from the block
--
--    Block  - The block of a column
--    Index  - The row number within the block 1..Block_Size
--    Status - The value status to set
--
   procedure Set_Undefined
             (  Block  : in out Column_Block;
                Index  : Subindex;
                Status : Undefined_Status
             )  is abstract;

end Fuzzy.Lecture.Block;
