--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Samples_Lists                         Luebeck            --
--  Interface                                      Autumn, 2006       --
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

with Generic_Segmented_Stack;

package Fuzzy.Samples_Lists is
   pragma Elaborate_Body (Fuzzy.Samples_Lists);
--
-- Examples_List -- List of examples
--
   type Examples_List is limited private;
--
-- Examples_Range -- Range of examples
--
   type Examples_Range is record
      From : Positive;
      To   : Positive;
   end record;
--
-- Examples_List_Position -- Position in a list
--
   type Examples_List_Position is private;
--
-- First -- The position of the first item in the list
--
   First : constant Examples_List_Position;
--
-- Add -- An example to the list
--
--    List    - A list of examples
--    Example - To add to
--
-- This procedure adds an example to the list  of.  Examples  should  be
-- added in sequence.
--
   procedure Add (List : in out Examples_List; Example : Positive);
--
-- Get -- Examples from the list
--
--    List  - A list of examples
--    Index - A position in the list
--
-- Returns :
--
--    The range of examples corresponding to Index
--
-- Exceptions :
--
--    Constraint_Error - Index is beyond the list end
--
   function Get
            (  List  : Examples_List;
               Index : Examples_List_Position
            )  return Examples_Range;
--
-- Is_Empty -- Check if the list is empty
--
--    List - A list of examples
--
-- Returns :
--
--    True if the list is empty
--
   function Is_Empty (List : Examples_List) return Boolean;
--
-- Is_In -- Test a position in the list
--
--    List  - A list of examples
--    Index - A position in the list
--
-- Returns :
--
--    True if Index in List
--
   function Is_In
            (  List  : Examples_List;
               Index : Examples_List_Position
            )  return Boolean;
--
-- Next -- Examples from the list
--
--    List  - A list of examples
--    Index - A position in the list
--
-- The result can be beyond the list end  if  Index  is  the  last  list
-- position.
--
-- Returns :
--
--    The position following Index
--
-- Exceptions :
--
--    Constraint_Error - Index is beyond the list end
--
   function Next
            (  List  : Examples_List;
               Index : Examples_List_Position
            )  return Examples_List_Position;

private
--
-- Examples_Number_List -- Stacks of example numbers
--
   type Singular_Index is new Positive;
   package Examples_Number_List is
      new Generic_Segmented_Stack
          (  Index_Type   => Singular_Index,
             Object_Type  => Positive,
             Null_Element => 1
          );
--
-- Examples_Ranges_List -- Stacks of examples ranges
--
   type Ranges_Index is new Positive;
   package Examples_Ranges_List is
      new Generic_Segmented_Stack
          (  Index_Type   => Ranges_Index,
             Object_Type  => Examples_Range,
             Null_Element => (1, 1)
          );
--
-- Examples_List -- List of examples
--
--    Singular - The list of example numbers
--    Ranges   - The list of intervals of examples numbers
--
   type Examples_List is limited record
      Singular : Examples_Number_List.Segmented_Stack.Stack;
      Ranges   : Examples_Ranges_List.Segmented_Stack.Stack;
   end record;

   type Examples_List_Position is record
      Singular : Singular_Index;
      Ranges   : Ranges_Index;
   end record;
   First : constant Examples_List_Position := (1, 1);

   pragma Inline (Is_Empty);
   pragma Inline (Is_In);

end Fuzzy.Samples_Lists;
