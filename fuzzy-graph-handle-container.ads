--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Container                Luebeck            --
--   Interface                                     Winter, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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
--  This package provides sets of graph node handles. It is  implemented
--  through an instantiation of the package Object.Handle.Generic_Set.
--
with Object.Handle.Generic_Set;

package Fuzzy.Graph.Handle.Container is
   type Set is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the set
--
--    Container - The set to be modified
--    Item      - To be added (a pointer or handle to object)
--
-- This procedure adds the object specified by Item to the  set.  It  is
-- added only if it is not already in the set.
--
   procedure Add (Container : in out Set; Item : Graph_Node_Ptr);
   procedure Add (Container : in out Set; Item : Node_Handle);
--
-- Add -- Add items from one set to another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This procedure adds the  objects  from  the  set  Items  to  the  set
-- Container. An object is added only if it is not already in the set.
--
   procedure Add (Container : in out Set; Items : Set);
--
-- Create -- Create a new empty set
--
-- Returns :
--
--    The new set
--
   function Create return Set;
--
-- Erase -- Remove all elements from the set
--
--    Container - The set
--
   procedure Erase (Container : in out Set);
--
-- Find -- Find an item in the set
--
--    Container - The set
--    Item      - To searched for (a pointer or handle to the object)
--
-- When search is successful the result is positive.  Otherwise,  it  is
-- negative and indicates the position the  item  should  be  placed  if
-- inserted into the list.
--
-- Returns :
--
--    [+]  The index of the item
--    [-]  The negated index of the position the item should be
--
   function Find (Container : Set; Item : Graph_Node'Class)
      return Integer;
   function Find (Container : Set; Item : Graph_Node_Ptr)
      return Integer;
   function Find (Container : Set; Item : Node_Handle)
      return Integer;
--
-- Get -- Get an object by its index
--
--    Container - The set
--    Index     - The number of the object to get
--
-- Returns :
--
--    A pointer to the object (see also Ref)
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get (Container : Set; Index : Positive)
      return Graph_Node_Ptr;
--
-- Get_Size -- Get the number of elements in the set
--
--    Container - The set
--
-- Returns :
--
--    The number of objects in the set
--
   function Get_Size (Container : Set) return Natural;
--
-- Is_Empty -- Test if the set is empty
--
--    Container - The set
--
-- Returns :
--
--    True if the set contains no objects
--
   function Is_Empty (Container : Set) return Boolean;
--
-- Is_In -- Test if an item in the set
--
--    Container - The set
--    Item      - To search for (pointer, handle, object)
--
-- Returns :
--
--    True if the object is in the set
--
   function Is_In (Container : Set; Item : Graph_Node'Class)
      return Boolean;
   function Is_In (Container : Set; Item : Graph_Node_Ptr)
      return Boolean;
   function Is_In (Container : Set; Item : Node_Handle)
      return Boolean;
--
-- Ref -- Get an object by its index
--
--    Container - The set
--    Index     - The number of the object to get
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Ref (Container : Set; Index : Positive)
      return Node_Handle;
--
-- Remove -- Remove element from the set
--
--    Container - The set
--    Index     - Of the item to be removed
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove (Container : in out Set; Index : Positive);
--
-- Remove -- Remove element from the set
--
--    Container - The set
--    Item      - To be removed (pointer, handle, object)
--
-- If the object is not in the set, nothing happens.
--
   procedure Remove (Container : in out Set; Item : Graph_Node'Class);
   procedure Remove (Container : in out Set; Item : Graph_Node_Ptr);
   procedure Remove (Container : in out Set; Item : Node_Handle);
--
-- Remove -- Remove elements of one set from another set
--
--    Container - The set
--    Items     - The set of items to be removed
--
-- If  an object from the set Items is not in the set Container, nothing
-- happens. Otherwise it is removed from Container.
--
   procedure Remove (Container : in out Set; Items : Set);
--
-- and -- Intersection
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    Intersection of two sets
--
   function "and" (Left, Right : Set) return Set;
--
-- or -- Union
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    Union of two sets
--
   function "or" (Left, Right : Set) return Set;
--
-- xor -- Difference
--
--    Left  - A set
--    Right - A set
--
-- The result is the set of elements present in only one of two sets.
--
-- Returns :
--
--    Difference of two sets
--
   function "xor" (Left, Right : Set) return Set;
--
-- = -- Comparison
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    True if both sets contain same items
--
   function "=" (Left, Right : Set) return Boolean;

private
   pragma Inline (Add);
   pragma Inline (Create);
   pragma Inline (Find);
   pragma Inline (Get);
   pragma Inline (Get_Size);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Ref);
   pragma Inline (Remove);
   pragma Inline ("and");
   pragma Inline ("or");
   pragma Inline ("xor");
   pragma Inline ("=");

   package Handles_Set is new Handles.Generic_Set;

   type Set is new Handles_Set.Set with null record;

end Fuzzy.Graph.Handle.Container;
