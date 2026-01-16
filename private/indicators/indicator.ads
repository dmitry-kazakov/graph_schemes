--                                                                    --
--  package Indicator               Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
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
--
--  This package defines the base type for the  user  ones  intended  to
--  indicate progress of some potentially lengthy operations. A  derived
--  type  should  override  the  procedure  Check  which is periodically
--  called during an operation performance. 
--
with Ada.Finalization;
with Object.Handle;

package Indicator is
   pragma Elaborate_Body (Indicator);
--
-- Indicator -- Of the operation processing
--
-- This  type  is  used  as  an indicator when the operation progress is
-- measured in the number of completed items. 
--
   type Indicator_Object is new Object.Entity with null record;
   type Indicator_Object_Ptr is access Indicator_Object'Class;
   type Indicator_Object_Ref is access constant Indicator_Object'Class;
--
-- Indicator_Data -- The abstract  base  for  subprogram-specific  para-
--                   meters. Some subprograms using indicators may  call
-- Set_Data to pass additional parameters to the indicator at hand.  The
-- parameters to pass should be packed into a  type  derived  from  this
-- base. 
--
   type Indicator_Data is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Cancel -- Operation cancellation request
--
--    Viewer - The indicator
--
-- This  procedure  requests  operation  cancellation.   It   does   not
-- necessarily abort it immediately, instead it postpones it to the time
-- the indicator gets notified through Check, Done or  Reset  call.  For
-- example,  if  the  visual  appearance has the cancel button, then the
-- callback  of  the  button  should  call  to   Cancel.   The   default
-- implementation does nothing. 
--
   procedure Cancel (Viewer : in out Indicator_Object);
--
-- Check -- Called at a check point
--
--    Viewer - The indicator
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Check (Viewer : in out Indicator_Object);
--
-- Done -- Called when the operation ends
--
--    Viewer - The indicator
--
-- If  an  suboperation  pending,  Done  ends  the  suboperation,  which
-- corresponds to one check point of the parent operation. 
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Done (Viewer : in out Indicator_Object);
--
-- Reset -- Called when the operation starts
--
--    Viewer - The indicator
--    Total  - The number of check points to expect
--
-- A consequent call to Reset indicates a suboperation which to be ended
-- by  Done. As a whole it corresponds to one check point of the current
-- operation. There can be  an  unlimited  number  of  nested  operation
-- levels.  The  parameter Total specifies the number of check points or
-- steps of the suboperation. Zero is specified when the total number of
-- steps is unknown, like when a source file is read and the  number  of
-- lines  to  expect  is  unknown.  Usually  it  has  no  sense  for   a
-- suboperation to specify Total as 0. 
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Reset
             (  Viewer : in out Indicator_Object;
                Total  : Natural := 0
             );
--
-- Set_Data -- Pass additional data to the indicator
--
--    Viewer - The indicator
--  [ Data ] - The data to pass to the indicator
--
-- This  procedure  can  be  used  to  make  the indicator aware of some
-- additional  parameters.  A  program  dealing  with  indicator may use
-- Set_Data  to communicate with the indicator. The indicator can always
-- ignore this  call.  The  default  implementation  does  nothing.  The
-- variant without the second parameter reverses the effect  of  setting
-- any data. 
--
-- Exceptions :
--
--    End_Error - To abort the operation
--
   procedure Set_Data
             (  Viewer : in out Indicator_Object;
                Data   : in out Indicator_Data'Class
             );
   procedure Set_Data (Viewer : in out Indicator_Object);
   
   Negleter : aliased Indicator_Object;

private
--
-- Handles -- To Indicator_Objects
--
   package Handles is
      new Object.Handle (Indicator_Object, Indicator_Object_Ptr);

end Indicator;
