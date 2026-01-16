--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Abstract_Edit.Named                   Luebeck            --
--  Interface                                      Summer, 2003       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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
--  This package provides string I/O for fuzzy sets over discrete domain
--  sets,  with  named  elements.  The  type  Domain_Description  is   a
--  descendant  of  Fuzzy.Abstract_Edit.User_Data  which  provides   the
--  functionality.
--
with Generic_Unbounded_Array;
with Name_Tables;

package Fuzzy.Abstract_Edit.Named is
   pragma Elaborate_Body (Fuzzy.Abstract_Edit.Named);
--
-- Domain_Description -- Discrete domain
--
   type Domain_Description is new User_Data with private;
--
-- Add -- Add one domain set element
--
--    Domain    - To add an element
--    Name      - Of the element
--    Index     - Of the element (in the set)
--    Unchecked - Skip conformity check
--
-- This procedure adds Name under Index to Domain.  Elements  should  be
-- added in an order keeping the set of  indices  contiguos.  Otherwise,
-- Constraint_Error is propagated. When the element is inserted  in  the
-- middle of the set, the elements right of it are moved to  the  right.
-- When Unchecked is True, Name is not checked for being conform.
--
-- Exceptions :
--
--    Constraint_Error - Illegal name or index
--    Name_Error       - The name is already used
--
   procedure Add
             (  Domain    : in out Domain_Description;
                Name      : String;
                Index     : Integer;
                Unchecked : Boolean := False
             );
--
-- Copy -- A domain set
--
--    Source - To be copied
--    Target - The result
--
   procedure Copy
             (  Source : Domain_Description;
                Target : in out Domain_Description'Class
             );
--
-- Erase -- Makes domain empty
--
--    Domain - To add an element
--
   procedure Erase (Domain : in out Domain_Description);
--
-- Get -- Replaces Fuzzy.Abstract_Edit...
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Domain  : Domain_Description;
                From    : out Integer;
                To      : out Integer
             );
--
-- Get -- Get an element
--
--    Source  - String to parse
--    Pointer - Starting string position
--    Domain  - The domain
--    Index   - Of the element
--
-- This procedure parses the string Source. If a name of an element from
-- Domain  is  successfully  matched  starting  from  Source  (Pointer),
-- Pointer is advanced to the position following the  matched  name  and
-- Index is set to respond to  the  domain  value  associated  with  the
-- variable.
--
-- Exceptions :
--
--    End_Error    - Nothing matched
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Domain  : Domain_Description;
                Index   : out Integer
             );
--
-- Get_Cardinality -- Get number of elements in a domain
--
--    Domain - The domain
--
-- Returns :
--
--    The number of elements
--
   function Get_Cardinality (Domain : Domain_Description)
      return Natural;
--
-- Get_Index -- Of a domain set element
--
--    Domain - The domain description
--    Name   - Of the element
--
-- Returns :
--
--    The index of the element named by Name
--
-- Exceptions :
--
--    End_Error - No such element
--
   function Get_Index
            (  Domain : Domain_Description;
               Name   : String
            )  return Integer;
--
-- Get_Name -- Of a domain set element
--
--    Domain - The domain description
--    Index  - Of the element (in the set)
--
-- Returns :
--
--    The name associated with Index
--
-- Exceptions :
--
--    Constraint_Error - No such element (illegal index)
--
   function Get_Name
            (  Domain : Domain_Description;
               Index  : Integer
            )  return String;
--
-- Get_Max_Range -- Overrides Fuzzy.Abstract_Edit...
--
   procedure Get_Max_Range
             (  Domain : Domain_Description;
                First  : out Integer;
                Last   : out Integer
             );
--
-- Is_Empty -- Empty set test
--
--    Domain - The domain description
--
-- Returns :
--
--    True if Domain is empty
--
   function Is_Empty (Domain : Domain_Description) return Boolean;
--
-- Move -- A domain set element
--
--    Domain - To swap elements
--    From   - The old index of the element (in the set)
--    To     - The new index of
--
-- This procedure moves the element in the Domain from the position From
-- to  the  position  To. Note that the indices of other elements within
-- the range From..To will change.
--
-- Exceptions :
--
--    Constraint_Error - Illegal indices
--
   procedure Move
             (  Domain : in out Domain_Description;
                From   : Integer;
                To     : Integer
             );
--
-- Put -- Replaces Fuzzy.Abstract_Edit...
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Domain      : Domain_Description;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Remove-- A domain set element
--
--    Domain - To rename an element
--    Index  - Of the element (in the set)
--
-- This procedure removes an element under Index from Domain. Note  that
-- the indices of the elements following the removed one will decrease.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Remove
             (  Domain : in out Domain_Description;
                Index  : Integer
             );
--
-- Remove -- A domain set element by name
--
--    Domain - To rename an element
--    Name   - Of the element
--
-- This  procedure  removes  an  element with the name Name from Domain.
-- Note  that the indices of the elements following the removed one will
-- decrease. Nothing happens if there is no element named by Name.
--
   procedure Remove
             (  Domain : in out Domain_Description;
                Name   : String
             );
--
-- Rename -- A domain set element
--
--    Domain    - To rename an element
--    Index     - Of the element (in the set)
--    Name      - The new name of the element
--    Unchecked - Skip conformity check
--
-- This procedure renames an element under  Index  in  the  Domain  into
-- Name. When Unchecked is True, Name is not checked for being conform.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name or illegal index
--    Name_Error       - The new name is already used
--
   procedure Rename
             (  Domain    : in out Domain_Description;
                Index     : Integer;
                Name      : String;
                Unchecked : Boolean := False
             );
--
-- Rename -- A domain set element
--
--    Domain    - To rename an element
--    Old_Name  - Of the element
--    New_Name  - Of the element
--    Unchecked - Skip conformity check
--
-- This procedure renames an element under  Index  in  the  Domain  into
-- Name.  When  Unchecked  is  True,  New_Name  is not checked for being
-- conform.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name
--    End_Error        - No element with the name Old_Name exits
--    Name_Error       - The new name is already used
--
   procedure Rename
             (  Domain    : in out Domain_Description;
                Old_Name  : String;
                New_Name  : String;
                Unchecked : Boolean := False
             );
--
-- Swap -- A domain set element
--
--    Domain  - To swap elements
--    Index_1 - Of the element (in the set)
--    Index_2 - Of the element (in the set)
--
-- This  procedure  swaps  two  elements in the Domain. The elements are
-- specified by their indices of the domain values. Nothing happens when
-- Index_1 = Index_2.
--
-- Exceptions :
--
--    Constraint_Error - Illegal indices
--
   procedure Swap
             (  Domain  : in out Domain_Description;
                Index_1 : Integer;
                Index_2 : Integer
             );
private
   pragma Inline (Get);
   pragma Inline (Get_Cardinality);
   pragma Inline (Get_Index);
   pragma Inline (Is_Empty);

   use Name_Tables.Name_Maps;

   type Positive_Array is array (Integer range <>) of Positive;
   package Set_To_Name_Maps is
      new Generic_Unbounded_Array
          (  Index_Type        => Integer,
             Object_Type       => Positive,
             Object_Array_Type => Positive_Array,
             Null_Element      => 1
          );
   use Set_To_Name_Maps;
--
-- Domain_Description -- Discrete domain
--
-- The  fields of the record: Name_Map is a table of names of the domain
-- set elements. Each element is associated with an  unique  index  from
-- First..Last.  The  sets  over  the  domain  are   declared   as   Set
-- (First..Last). Index_Map is an  array  which  for  every  index  from
-- First..Last contains  the  position  of  the  corresponding  name  in
-- Name_Map.   This   position   is   a   positive   number,  names  are
-- alphabetically ordered.
--
   type Domain_Description is new User_Data with record
      First     : Integer := 1;
      Last      : Integer := 0;
      Name_Map  : Dictionary;
      Index_Map : Unbounded_Array;
   end record;
--
-- Get -- Overrides Fuzzy.Abstract_Edit...
--
   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Domain    : Domain_Description;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             );
--
-- Put -- Overrides Fuzzy.Abstract_Edit...
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Domain      : Domain_Description;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );

end Fuzzy.Abstract_Edit.Named;
