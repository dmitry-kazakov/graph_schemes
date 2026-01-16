--                                                                    --
--  package Indicator.Handle        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Indicator.Handle is

   procedure Cancel (Viewer : Indicator_Handle) is
   begin
      if Is_Valid (Viewer) then
         Cancel (Ptr (Viewer).all);
      end if;
   end Cancel;

   procedure Check (Viewer : Indicator_Handle) is
   begin
      if Is_Valid (Viewer) then
         Check (Ptr (Viewer).all);
      end if;
   end Check;

   procedure Done (Viewer : Indicator_Handle) is
   begin
      if Is_Valid (Viewer) then
         Done (Ptr (Viewer).all);
      end if;
   end Done;

   procedure Invalidate (Viewer : in out Indicator_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Viewer));
   end Invalidate;

   function Is_Valid (Viewer : Indicator_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Viewer));
   end Is_Valid;

   function Ptr (Viewer : Indicator_Handle)
      return Indicator_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Viewer));
   end Ptr;

   function Ref (Viewer : Indicator_Object_Ptr)
      return Indicator_Handle is
   begin
      return (Handles.Ref (Viewer) with null record);
   end Ref;

   procedure Ref
             (  Handle : in out Indicator_Handle;
                Viewer : Indicator_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Viewer);
   end Ref;

   procedure Reset (Viewer : Indicator_Handle; Total  : Natural := 0) is
   begin
      if Is_Valid (Viewer) then
         Reset (Ptr (Viewer).all, Total);
      end if;
   end Reset;

end Indicator.Handle;
