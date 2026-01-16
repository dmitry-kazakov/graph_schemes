--                                                                    --
--  package Indicator.Text_IO       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
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
--  This  package  provides  progress  indicators  based on text I/O for
--  output devices supporting backspace.  
--
with Indicator.Advance;

package Indicator.Text_IO is
   pragma Elaborate_Body (Indicator.Text_IO);
--
-- Text_Indicator_Object -- Abstract text indicator
--
   type Text_Indicator_Object is
      abstract new Indicator.Advance.Progress_Object with private;
--
-- Get_Text -- To show the indicator state
--
--    Viewer - The indicator
--
-- This is an abstract function to be implemented by a derived type.
--
-- Returns :
--
--    The text to show
--
   function Get_Text (Viewer : Text_Indicator_Object)
      return String is abstract;
--
-- Bar -- The bar indicator
--
--    Size - Of the bar in characters
--
-- The progress is shown in the form '|HHHH  | 60.05%'. The discriminant
-- Size specifies the size of the bar, i.e.  the  number  of  characters
-- 'H', shown when the progress is 100%.   
--
   type Bar (Size : Positive) is
      new Text_Indicator_Object with private;
--
-- Timed_Bar -- The bar indicator with time to go
--
--    Size - Of the bar in characters
--
-- The  progress  is  shown  in  the  form  '|HHHH   | 60.05% +10s'. The
-- discriminant Size specifies the size of the bar, i.e. the  number  of
-- characters 'H', shown when the progress is 100%. This type is derived
-- from Bar. 
--
   type Timed_Bar is new Bar with private;
--
-- Counter -- The counter indicator
--
--    Suffix - The text to be added to the number
--
-- The  progress  is  shown  as  a  plain number followed by the text in
-- Suffix.  
--
   type Counter (Suffix : not null access String) is
      new Text_Indicator_Object with private;
--
-- Get_Text -- Implements Get_Text of Text_Indicator
--
   function Get_Text (Viewer : Bar)       return String;
   function Get_Text (Viewer : Timed_Bar) return String;
   function Get_Text (Viewer : Counter)   return String;
--
-- Check -- Overrides Indicator...
--
   procedure Check (Viewer : in out Bar);
   procedure Check (Viewer : in out Timed_Bar);
   procedure Check (Viewer : in out Counter);
--
-- Done -- Overrides Indicator...
--
   procedure Done (Viewer : in out Bar);
   procedure Done (Viewer : in out Timed_Bar);
   procedure Done (Viewer : in out Counter);

private
   type Text_Indicator_Object is
      abstract new Indicator.Advance.Progress_Object with
   record
      Written : Natural := 0;
   end record;

   type Bar (Size : Positive) is
      new Text_Indicator_Object with null record;
   
   type Timed_Bar is new Bar with null record;

   type Counter (Suffix : not null access String) is
      new Text_Indicator_Object with null record;
   
end Indicator.Text_IO;
