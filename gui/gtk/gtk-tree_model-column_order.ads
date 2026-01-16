--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Column_Order                 Luebeck            --
--  Interface                                      Summer, 2006       --
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

with Ada.Finalization;

package Gtk.Tree_Model.Column_Order is
--
-- Gtk_Column_Order -- In which columns are sorted
--
   type Gtk_Column_Order is private;
--
-- Set_First -- Make a column the first in the order
--
--    Order  - The order
--    Column - The column ID (0..Length - 1)
--    Length - The total number of columns
--
-- Column  is  moved  to  the  first position. When called first or when
-- Length differs from a one given in the previous call,  the  order  is
-- initialized by ascending columns IDs, except  for  Column,  which  is
-- placed at the first position. Usually, when a sort function is called
-- it  calls  Set_First  with  the current sort column. Which causes the
-- order to change so that the sort column  would  be  the  first.  This
-- procedure does nothing if Column is negative or not in 0..Length - 1,
-- or else Length is not positive. 
--
   procedure Set_First
             (  Order  : in out Gtk_Column_Order;
                Column : GInt;
                Length : GInt
             );
--
-- Get -- The column ID by its order
--
--    Order - The order 
--    Index - The position in the order 1..
--
-- Returns :
--
--    [-]  There is no column for this position
--    [*]  The column ID corresponding to Index
--
   function Get (Order : Gtk_Column_Order; Index : Positive)
      return GInt;

private
   type Sort_Order is array (Positive range <>) of GInt;
   type Sort_Order_Ptr is access Sort_Order;
   type Gtk_Column_Order is new Ada.Finalization.Controlled with record
      Path : Sort_Order_Ptr;
   end record;

   procedure Adjust   (Order : in out Gtk_Column_Order);
   procedure Finalize (Order : in out Gtk_Column_Order);
   
end Gtk.Tree_Model.Column_Order;
