--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Bounded_Arrays         Luebeck            --
--  Interface                                      Summer, 2003       --
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

with Object.Handle.Generic_Bounded_Array;

package Fuzzy.Feature.Handle.Bounded_Arrays is
   type Bounded_Array (First, Last : Integer) is
      new Ada.Finalization.Controlled with private;
--
-- Adjust -- Assignment (part of)
--
--    Container - The array
--
   procedure Adjust (Container : in out Bounded_Array);
--
-- Append -- Elements to an array
--
--    Container - The array
--    Element   - The element to append (a pointer/handle to)
--    Count     - The number of times
--
-- When  the  upper  bound  of  the  result  cannot  be  represented  as
-- Index_Type, Constraint_Error is propagated.
--
-- Returns :
--
--    Container with Count Elements added
--
-- Exceptions :
--
--    Constraint_Error - Bound error
--
   function Append
            (  Container : Bounded_Array;
               Element   : Feature_Object_Ptr := null;
               Count     : Natural            := 1
            )  return Bounded_Array;
   function Append
            (  Container : Bounded_Array;
               Element   : Feature_Handle;
               Count     : Natural := 1
            )  return Bounded_Array;
--
-- Delete -- A slice of the array
--
--    Container - The array
--    From      - The first element of the slice
--    Count     - The number of elements to delete
--
-- When Count exceeds the number of elements in the array, the available
-- elements are removed.
--
-- Returns :
--
--    Container with the slice removed
--
-- Exceptions :
--
--    Constraint_Error - From is not in Container
--
   function Delete
            (  Container : Bounded_Array;
               From      : Integer;
               Count     : Natural := 1
            )  return Bounded_Array;
--
-- Finalize -- Destructor
--
--    Container - The array
--
   overriding
   procedure Finalize (Container : in out Bounded_Array);
--
-- Fill -- Replace an array slice
--
--    Container - The array
--    From      - Index of the element in the slice
--    To        - Index of the last element
--    Element   - The replacement (a pointer/handle to)
--
-- Nothing  happens if From > To. Otherwise both have to be in the array
-- index range.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Element   : Feature_Object_Ptr
             );
   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Element   : Feature_Handle
             );
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
            (  Container : Bounded_Array;
               Index     : Integer
            )  return Feature_Object_Ptr;
--
-- Get -- A slice of the array
--
--    Container - The array
--    From      - The first element of the slice
--    To        - The last element of the slice
--
-- The  first index of the result is From. The result is empty when From
-- > To. Otherwise it is From..To slice of Container.
--
-- Returns :
--
--    The slice From..To of Container
--
-- Exceptions :
--
--    Constraint_Error - From..To  is  not  empty and not in Container's
--                       First..Last.
--
   function Get
            (  Container : Bounded_Array;
               From      : Integer;
               To        : Integer
            )  return Bounded_Array;
--
-- Prepend -- Elements to an array
--
--    Container - The array
--    Element   - The element to prepend (a pointer/handle to)
--    Count     - The number times
--
-- When  the  lower  bound  of  the  result  cannot  be  represented  as
-- Index_Type, Constraint_Error is propagated.
--
-- Returns :
--
--    Container with Count Elements added in front of
--
-- Exceptions :
--
--    Constraint_Error - Bound error
--
   function Prepend
            (  Container : Bounded_Array;
               Element   : Feature_Object_Ptr := null;
               Count     : Natural            := 1
            )  return Bounded_Array;
   function Prepend
            (  Container : Bounded_Array;
               Element   : Feature_Handle;
               Count     : Natural := 1
            )  return Bounded_Array;
--
-- Put -- Replace an array element by its index
--
--    Container - The array
--    Index     - Of the element
--    Element   - The new element (a pointer/handle to)
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Integer;
                Element   : Feature_Object_Ptr
             );
   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Integer;
                Element   : Feature_Handle
             );
--
-- Put -- Replace a slice of array elements by
--
--    Container - The array
--    From      - Of the first element in the slice
--    To        - Of the last element in the slice
--    Elements  - New elements
--
-- The  elements  From..To  of  Container  are  replaced  with ones from
-- Elements. If Elements is longer than the slice its rightmost elements
-- are ignored. When Elements is shorter, then the rest of the slice  is
-- filled with invalid handles. When From > To, the operation  is  void,
-- otherwise From..To must be in First..Last of Container. If it is not,
-- Contstraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--
   procedure Put
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Elements  : Bounded_Array
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
            (  Container : Bounded_Array;
               Index     : Integer
            )  return Feature_Handle;
--
-- To_Bounded_Array -- Conversion
--
--    Container - The array
--
-- Returns :
--
--    A bounded array
--
   function To_Bounded_Array (Container : Feature_Array)
      return Bounded_Array;
--
-- To_Feature_Array -- Conversion
--
--    Container - The array
--
-- Returns :
--
--    A feature array
--
   function To_Feature_Array (Container : Bounded_Array)
      return Feature_Array;
--
-- & -- Concatentation of arrays
--
--    Left  - The first array (prefix)
--    Right - The second array (suffix)
--
-- When Right is an empty array, the result is Left. When Left is empty,
-- the  result  is  Right.  Otherwise, the result is a concatentation of
-- Left  and  Right  elements  and the index of the first element of the
-- result is Index_Type'First.
--
-- Returns :
--
--    An array of elements from Left followed by the elements of Right
--
   function "&" (Left, Right : Bounded_Array) return Bounded_Array;
--
-- Empty -- An empty array
--
   Empty : constant Bounded_Array;

private
   pragma Inline (Append);
   pragma Inline (Get);
   pragma Inline (Prepend);
   pragma Inline (Put);
   pragma Inline (Ref);
   pragma Inline (To_Bounded_Array);
   pragma Inline (To_Feature_Array);

   package Bounded_Arrays is
      new Fuzzy.Feature.Handles.Generic_Bounded_Array
          (  Index_Type  => Integer,
             Handle_Type => Feature_Handle
          );
   type Bounded_Array (First, Last : Integer) is
      new Bounded_Arrays.Bounded_Array (First, Last) with null record;

   Empty : constant Bounded_Array :=
              (  Bounded_Arrays.Empty
              with
                 First => Bounded_Arrays.Empty.First,
                 Last  => Bounded_Arrays.Empty.Last
              );

end Fuzzy.Feature.Handle.Bounded_Arrays;
