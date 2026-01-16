--                                                                    --
--  package Indicator.Advance       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2006       --
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
--  This package  provides  abstract  progress  indicators.  A  progress
--  indicator is used to show advance of an  operation.  The  base  type
--  provides basic functionality such as delivering the operation status
--  in  terms of completeness percentage. The abstract procedure Show is
--  to be defined by the derived type. 
--
with Ada.Calendar;  use Ada.Calendar;

with Generic_Unbounded_Array;
with Generic_Stack;

package Indicator.Advance is
   pragma Elaborate_Body (Indicator.Advance);
--
-- Progress_Object -- Abstract progress indicator
--
   type Progress_Object is abstract new Indicator_Object with private;
--
-- Get -- Status of the operation
--
--    Viewer - The indicator
--
-- The progress is indicated by values starting from 0.0. Values greater
-- then  1.0  are  legal and depend on scaling provided by the parameter
-- Total used in Reset. 
--
-- Returns :
--
--    Progress 0.0...
--
   function Get (Viewer : Progress_Object) return Float;
--
-- Get -- Time of the operation start
--
--    Viewer - The indicator
--
-- Returns :
--
--    The time operation started
--
   function Get (Viewer : Progress_Object) return Time;
--
-- Get_Refresh
--
--    Viewer  - The indicator 
--
-- Returns :
--
--    Current refresh period
--
   function Get_Refresh (Viewer : Progress_Object) return Duration;
--
-- Reset -- Overrides Indicator...
--
   procedure Reset
             (  Viewer : in out Progress_Object;
                Total  : Natural := 0
             );
--
-- On_Check -- Called as a part of Check implementation
--
--    Viewer - The indicator
--    Update - Update flag
--
-- This procedure is called from an implementation of Check. When Update
-- is returned as True the caller should update the indicatior's  visual
-- appearance. 
--
   procedure On_Check
             (  Viewer : in out Progress_Object;
                Update : out Boolean
             );
--
-- On_Done -- Called as a part of Done implementation
--
--    Viewer - The indicator
--    Update - Update flag
--
-- This procedure is called from an implementation of Done. When  Update
-- is returned as True the caller should update the indicatior's  visual
-- appearance. 
--
   procedure On_Done
             (  Viewer : in out Progress_Object;
                Update : out Boolean
             );
--
-- Set_Refresh -- Refresh period of an indicator
--
--    Viewer  - The indicator 
--    Refresh - The period of
--
-- This procedure is used to change the refresh period. This  influences
-- The  frequency  of  updates  of  the indicator's visual appearance as
-- reported by procedures On_Check and On_Done. 
--
   procedure Set_Refresh
             (  Viewer  : in out Progress_Object;
                Refresh : Duration
             );

private
   type State is record
      Step  : Float := 1.0;
      Count : Float := 0.0;
   end record;
   type State_Array is array (Integer range <>) of State;
   package Unbounded_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type         => Integer,
             Object_Type        => State,
             Object_Array_Type  => State_Array,
             Null_Element       => (1.0, 0.0)
          );
   use Unbounded_Arrays;
   package Counter_Stacks is
      new Generic_Stack
          (  Index_Type   => Integer,
             Object_Type  => State,
             Array_Type   => Unbounded_Arrays.Unbounded_Array,
             Null_Element => (1.0, 0.0)
          );
   use Counter_Stacks;

   type Progress_Object is abstract new Indicator_Object with record
      Empty     : Boolean  := True;
      Refresh   : Duration := 0.5;
      Start     : Time;
      That_Time : Time;
      Now       : State;
      History   : Stack;
   end record;

end Indicator.Advance;
