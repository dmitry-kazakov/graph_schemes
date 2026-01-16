--                                                                    --
--  package Indicator.Handle        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2003       --
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
--  This package defines handles to the objects indicating  progress  of
--  some potentially lengthy operations.
--
package Indicator.Handle is
   pragma Elaborate_Body (Indicator.Handle);

   type Indicator_Handle is tagged private;
--
-- Cancel -- Operation cancellation request
--
--    Viewer - A handle to the indicator
--
-- This  procedure  requests  operation  cancellation.   It   does   not
-- necessarily abort it immediately, instead it postpones it to the time
-- the indicator gets notified through Check, Done or  Reset  call.  For
-- example,  if  the  visual  appearance has the cancel button, then the
-- callback  of  the  button  should  call  to  Cancel. Viewer can be an
-- invalid handle.
--
   procedure Cancel (Viewer : Indicator_Handle);
--
-- Check -- Called at a check point
--
--    Viewer - The indicator
--
-- Viewer can be an invalid handle.
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Check (Viewer : Indicator_Handle);
--
-- Done -- Called when the operation ends
--
--    Viewer - The indicator
--
-- If  an  suboperation  pending,  Done  ends  the  suboperation,  which
-- corresponds to one check point of the parent operation. Viewer can be
-- an invalid handle.
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Done (Viewer : Indicator_Handle);
--
-- Invalidate -- Detach handle from the object
--
--    Viewer - A handle to the indicator
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the later is destroyed.
--
   procedure Invalidate (Viewer : in out Indicator_Handle);
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Viewer - A handle to the indicator
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Viewer : Indicator_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by the handle
--
--    Viewer - A handle to the indicator
--
-- Returns :
--
--    The referenced object or null if Reference is invalid
--
   function Ptr (Viewer : Indicator_Handle) return Indicator_Object_Ptr;
--
-- Ref -- Get handle to an object
--
--    Viewer - A pointer to an indicator object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Viewer : Indicator_Object_Ptr)
      return Indicator_Handle;
--
-- Ref -- Set a handle to an object
--
--    Handle - The handle to set
--    Viewer - The object
--
   procedure Ref
             (  Handle : in out Indicator_Handle;
                Viewer : Indicator_Object_Ptr
             );
--
-- Reset -- Called when the operation starts
--
--    Viewer - The indicator
--    Total  - The number of check points to expect
--
-- This procedure is called to indicate  the  start  of  a  suboperation
-- which  later  ended  by  Done. As a whole it corresponds to one check
-- point of the current operation. There can be an unlimited  number  of
-- nested  operation levels. The parameter Total specifies the number of
-- check points or steps of the suboperation. Zero is specified when the
-- total number of steps is unknown, like when a source file is read and
-- the number of lines to expect is unknown. Usually it has no sense for
-- a  suboperation  to  specify  Total  as  0.  Viewer can be an invalid
-- handle.
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Reset
             (  Viewer : Indicator_Handle;
                Total  : Natural := 0
             );
--
-- No_Indicator -- No indication object
--
   No_Indicator : constant Indicator_Handle;

private
   pragma Inline (Cancel);
   pragma Inline (Check);
   pragma Inline (Done);
   pragma Inline (Invalidate);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Reset);

   type Indicator_Handle is new Handles.Handle with null record;

   No_Indicator : constant Indicator_Handle :=
                     (Handles.Handle with null record);

end Indicator.Handle;
