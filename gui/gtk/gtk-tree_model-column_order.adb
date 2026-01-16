--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Tree_Model.Column_Order                Luebeck            --
--  Implementation                                 Summer, 2006       --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Glib.Messages;   use Glib.Messages;
with Gtk.Missed;      use Gtk.Missed;

with Ada.Unchecked_Deallocation;

package body Gtk.Tree_Model.Column_Order is

   procedure Free is
      new Ada.Unchecked_Deallocation (Sort_Order, Sort_Order_Ptr);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Tree_Model.Column_Order." & Name;
   end Where;

   procedure Adjust (Order : in out Gtk_Column_Order) is
   begin
      if Order.Path /= null then
         Order.Path := new Sort_Order'(Order.Path.all);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Adjust")
         )  );
   end Adjust;

   procedure Finalize (Order : in out Gtk_Column_Order) is
   begin
      Free (Order.Path);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   procedure Set_first
             (  Order  : in out Gtk_Column_Order;
                Column : GInt;
                Length : GInt
             )  is
   begin
      if Column >= 0 and then Column < Length and then Length > 0 then
         if (  Order.Path = null
            or else
               Order.Path'Length /= Integer (Length)
            )
         then
            Free (Order.Path);
            Order.Path := new Sort_Order (1..Positive (Length));
            Order.Path (Order.Path'First) := Column;
            declare
               ID : GInt := 0;
            begin
               for Index in Order.Path'First + 1..Order.Path'Last loop
                  if ID = Column then
                     ID := ID + 1;
                  end if;
                  Order.Path (Index) := ID;
                  ID := ID + 1;
               end loop;
            end;
         else
            for Index in Order.Path'Range loop
               if Order.Path (Index) = Column then
                  Order.Path (2..Index) := Order.Path (1..Index - 1);
                  Order.Path (1) := Column;
                  exit;
               end if;
            end loop;
         end if;
      end if;
   end Set_first;

   function Get (Order : Gtk_Column_Order; Index : Positive)
      return GInt is
   begin
      if Order.Path = null or else Index > Order.Path'Length then
         return -1;
      else
         return Order.Path (Index);
      end if;
   end Get;

end Gtk.Tree_Model.Column_Order;
