--                                                                    --
--  package Gtk.Indicators          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Gdk.Event;

package body Gtk.Indicators is

   procedure Do_Cancel
             (  Widget : access Gtk_Widget_Record'Class;
                Viewer : Indicator_Object_Ptr
             )  is
   begin
      Cancel (Viewer.all);
   end Do_Cancel;

   function Do_Delete
            (  Widget : access Gtk_Indicator_Record'Class;
               Event  : Gdk.Event.Gdk_Event
            )  return Boolean is
   begin
      Cancel (Ptr (Widget.Viewer).all);
      return False;
   end Do_Delete;


   procedure On_Destroy (Widget : access Gtk_Indicator_Record'Class) is
   begin
      if Is_Valid (Widget.Viewer) then
         Cancel (Ptr (Widget.Viewer).all);
      end if;
   end On_Destroy;

   procedure Destroy (Viewer : not null access Gtk_Indicator_Record) is
   begin
      On_Destroy (Viewer);
      Gtk.Table.Destroy (Gtk_Table_Record (Viewer.all)'Access);
   end Destroy;

   function Get_Button
            (  Widget : not null access Gtk_Indicator_Record
            )  return Gtk_Button is
   begin
      return Widget.Cancel_Button;
   end Get_Button;

   function Get_Indicator
            (  Widget : not null access Gtk_Indicator_Record
            )  return Indicator_Handle is
   begin
      return Widget.Viewer;
   end Get_Indicator;

   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button_Position : Gtk_Rectangle;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             )  is
   begin
      Widget.Viewer := Viewer;
      Initialize (Widget, Size.Height, Size.Width, Homogeneous);
      Set_Row_Spacings (Widget, Spacing.Width);
      Set_Col_Spacings (Widget, Spacing.Height);
      Function_Callbacks.Connect
      (  Widget,
         "delete_event",
         Function_Callbacks.To_Marshaller (Do_Delete'Access)
      );
      Widget_Callbacks.Connect
      (  Widget,
         "destroy",
         Widget_Callbacks.To_Marshaller (On_Destroy'Access)
      );
   end Initialize;

   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button          : not null access
                                  Gtk_Button_Record'Class;
                Button_Position : Gtk_Rectangle;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             )  is
      X_Size : Guint := Guint'Max (1, Size.Width);
      Y_Size : Guint := Guint'Max (1, Size.Height);
   begin
      Widget.Viewer := Viewer;
      X_Size := Guint'Max (X_Size, Button_Position.Right);
      Y_Size := Guint'Max (Y_Size, Button_Position.Bottom);
      Widget.Cancel_Button := Button.all'Unchecked_Access;
      Set_Use_Underline (Widget.Cancel_Button, True);
      Initialize (Widget, Y_Size, X_Size, Homogeneous);
      Set_Row_Spacings (Widget, Spacing.Height);
      Set_Col_Spacings (Widget, Spacing.Width);
      Attach
      (  Widget,
         Widget.Cancel_Button,
         Button_Position.Left,
         Button_Position.Right,
         Button_Position.Top,
         Button_Position.Bottom,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink
      );
      if Is_Valid (Viewer) then
         Indicator_Callbacks.Connect
         (  Widget.Cancel_Button,
            "clicked",
            Indicator_Callbacks.To_Marshaller (Do_Cancel'Access),
            Ptr (Viewer)
         );
      end if;
      Show (Widget.Cancel_Button);
      Function_Callbacks.Connect
      (  Widget,
         "delete_event",
         Function_Callbacks.To_Marshaller (Do_Delete'Access)
      );
      Widget_Callbacks.Connect
      (  Widget,
         "destroy",
         Widget_Callbacks.To_Marshaller (On_Destroy'Access)
      );
   end Initialize;

   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button          : not null access
                                  Gtk_Button_Record'Class;
                Button_Position : Gtk_Position_Type;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             )  is
   begin
      case Button_Position is
         when Pos_Left =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Button          => Button,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position => (0, 1, 0, Size.Height)
            );
         when Pos_Right =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Button          => Button,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position =>
                  (Size.Width - 1, Size.Width, 0, Size.Height)
            );
         when Pos_Top =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Button          => Button,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position => (0, Size.Width, 0, 1)
            );
         when Pos_Bottom =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Button          => Button,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position =>
                  (0, Size.Width, Size.Height - 1, Size.Height)
            );
      end case;
   end Initialize;

   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button_Position : Gtk_Position_Type;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             )  is
   begin
      case Button_Position is
         when Pos_Left =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position => (0, 1, 0, Size.Height)
            );
         when Pos_Right =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position =>
                  (Size.Width - 1, Size.Width, 0, Size.Height)
            );
         when Pos_Top =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position => (0, Size.Width, 0, 1)
            );
         when Pos_Bottom =>
            Initialize
            (  Widget          => Widget,
               Viewer          => Viewer,
               Size            => Size,
               Spacing         => Spacing,
               Homogeneous     => Homogeneous,
               Button_Position =>
                  (0, Size.Width, Size.Height - 1, Size.Height)
            );
      end case;
   end Initialize;

   procedure Set_Viewer
             (  Widget : not null access Gtk_Indicator_Record;
                Viewer : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Widget.Viewer := Viewer;
         if Widget.Cancel_Button /= null then
            Indicator_Callbacks.Connect
            (  Widget.Cancel_Button,
               "clicked",
               Indicator_Callbacks.To_Marshaller (Do_Cancel'Access),
               Ptr (Viewer)
            );
         end if;
      else
         if Widget.Cancel_Button /= null then
            Remove (Widget, Widget.Cancel_Button);
            Widget.Cancel_Button := null;
         end if;
      end if;
   end Set_Viewer;

end Gtk.Indicators;
