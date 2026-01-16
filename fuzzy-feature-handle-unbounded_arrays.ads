--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Unbounded_Arrays       Luebeck            --
--  Interface                                      Autumn, 2008       --
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

with Object.Handle.Generic_Unbounded_Array;

package Fuzzy.Feature.Handle.Unbounded_Arrays is
   type Unbounded_Array is new Ada.Finalization.Controlled with private;
--
-- Adjust -- Assignment (part of)
--
--    Container - The array
--
-- The  assignment  does  not copy the array. It only increments the use
-- count  of  the array body. Only destructive operations like Put cause
-- array replication.
--
   procedure Adjust (Container : in out Unbounded_Array);
--
-- Erase -- Delete all array items
--
--    Container - The array
--
-- This procedure makes Container empty.
--
   procedure Erase (Container : in out Unbounded_Array);
--
-- Finalize -- Destructor
--
--    Container - The array
--
   procedure Finalize (Container : in out Unbounded_Array);
--
-- First -- The lower array bound
--
--    Container - The array
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - The array is presently empty
--
   function First (Container : Unbounded_Array) return Integer;
--
-- Get -- Get an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- Returns :
--
--    The element or null if none
--
   function Get
            (  Container : Unbounded_Array;
               Index     : Integer
            )  return Feature_Object_Ptr;
--
-- Last -- The upper array bound
--
--    Container - The array
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - The array is presently empty
--
   function Last (Container : Unbounded_Array) return Integer;
--
-- Put -- Replace an array element by its index
--
--    Container - The array
--    Index     - Of the element
--    Element   - The new element (a pointer/handle to)
--
-- The array is expanded as necessary.
--
   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Integer;
                Element   : Feature_Object_Ptr
             );
   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Integer;
                Element   : Feature_Handle
             );
--
-- Ref -- Get a handle to an array element by its index
--
--    Container - The array
--    Index     - Of the element
--
-- Returns :
--
--    A handle to the element
--
-- Exceptions :
--
--    Constraint_Error - No such element
--
   function Ref
            (  Container : Unbounded_Array;
               Index     : Integer
            )  return Feature_Handle;
--
-- To_Unbounded_Array -- Conversion
--
--    Container - The array
--
-- Returns :
--
--    A unbounded array
--
   function To_Unbounded_Array (Container : Feature_Array)
      return Unbounded_Array;
--
-- To_Feature_Array -- Conversion
--
--    Container - The array
--
-- Returns :
--
--    A feature array
--
   function To_Feature_Array (Container : Unbounded_Array)
      return Feature_Array;

private
   pragma Inline (First);
   pragma Inline (Get);
   pragma Inline (Last);
   pragma Inline (Put);
   pragma Inline (Ref);

   package Unbounded_Arrays is
      new Fuzzy.Feature.Handles.Generic_Unbounded_Array
          (  Index_Type  => Integer,
             Handle_Type => Feature_Handle
          );
   type Unbounded_Array is
      new Unbounded_Arrays.Unbounded_Array with null record;

end Fuzzy.Feature.Handle.Unbounded_Arrays;
