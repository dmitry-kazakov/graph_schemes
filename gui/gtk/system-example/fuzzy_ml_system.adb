--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy_ML_System                             Luebeck            --
--  Implementation                                 Summer, 2009       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with Gdk.Color;               use Gdk.Color;
with Gtk.Enums;               use Gtk.Enums;
with Gdk.Event;               use Gdk.Event;
with GLib;                    use GLib;
with GLib.Main;               use GLib.Main;
with GLib.Messages;           use GLib.Messages;
with Gtk.Abstract_Browser;    use Gtk.Abstract_Browser;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Fuzzy_Catalogue;     use Gtk.Fuzzy_Catalogue;
with Gtk.Main.Router;         use Gtk.Main.Router;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Persistent.Handle;       use Persistent.Handle;
with Strings_Edit.Integers;   use Strings_Edit.Integers;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Ada.Task_Identification;
with Ada.Unchecked_Conversion;
with Fuzzy_ML_Version;
with GNAT.Traceback.Symbolic;
with Gtk.Fuzzy_Object;
with Gtk.Handlers;
with Gtk.Main.Router.GNAT_Stack;
with Interfaces;
with Image_Fuzzy_16_XPM;
with Image_Logo_XPM.Image;
with Name_Tables;

with GNAT.Exception_Actions;  use GNAT.Exception_Actions;

procedure Fuzzy_ML_System is
------------------------------------------------------------------------
-- Exception_Tracer -- Debugging purpose stuff for  tracing  exceptions.
--                     The  following  defines   an   exception   action
-- procedure which prints exception information at each point  where  an
-- exception  is  raised.  The  handler is activated below right next to
-- begin.
--
   procedure Exception_Tracer (Occurence : Exception_Occurrence) is
      use Ada.Text_IO, Ada.Task_Identification;
   begin
      Put_Line
      (  "Traced exception in "
      &  Image (Current_Task)
      &  ": "
      &  Exception_Information (Occurence)
      );
   end Exception_Tracer;

   procedure Storage_Error_Tracer (Occurence : Exception_Occurrence) is
      use Ada.Text_IO, GNAT.Traceback, GNAT.Traceback.Symbolic;
      use Ada.Task_Identification;
   begin
      Put_Line
      (  "Traced Storage_Error in "
      &  Image (Current_Task)
      &  ": "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Storage_Error_Tracer;

   procedure Constraint_Error_Tracer
             (  Occurence : Exception_Occurrence
             )  is
      use Ada.Text_IO, GNAT.Traceback, GNAT.Traceback.Symbolic;
      use Ada.Task_Identification;
   begin
      Put_Line
      (  "Traced Constraint_Error in "
      &  Image (Current_Task)
      &  ": "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Constraint_Error_Tracer;

   procedure Program_Error_Tracer
             (  Occurence : Exception_Occurrence
             )  is
      use Ada.Text_IO, GNAT.Traceback, GNAT.Traceback.Symbolic;
      use Ada.Task_Identification;
   begin
      Put_Line
      (  "Traced Program_Error in "
      &  Image (Current_Task)
      &  ": "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Program_Error_Tracer;

   type Exception_Tracer_Ptr is access
      procedure (Occurence : Exception_Occurrence);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Exception_Tracer_Ptr,
             Exception_Action
          );
------------------------------------------------------------------------
   Pane      : Gtk_Paned;
   Window    : Gtk_Window;
   Catalogue : Gtk_Fuzzy_Catalogue;

   type Local is access function return Boolean;
   function "+" is new Ada.Unchecked_Conversion (Local, G_Source_Func);
   function Restore_Layout return Boolean is
   begin
      Move
      (  Window,
         GInt (Integer'(Value (Restore (Catalogue, "x", "")))),
         GInt (Integer'(Value (Restore (Catalogue, "y", ""))))
      );
      Set_Position
      (  Catalogue,
         GInt (Integer'(Value (Restore (Catalogue, "x-separator", ""))))
      );
      Set_Position
      (  Pane,
         GInt (Integer'(Value (Restore (Catalogue, "y-separator", ""))))
      );
      return False;
   exception
      when others =>
         return False;
   end Restore_Layout;

   function Store_Layout return Boolean is
      X, Y : GInt;
      Width, Height : GInt;
   begin
      Get_Position (Window, X, Y);
      Store (Catalogue, "x", Image (Integer (X)));
      Store (Catalogue, "y", Image (Integer (Y)));
      Get_Size (Window, Width, Height);
      Store (Catalogue, "width",  Image (Integer (Width)));
      Store (Catalogue, "height", Image (Integer (Height)));
      Store
      (  Catalogue,
         "x-separator",
         Image (Integer (Get_Position (Catalogue)))
      );
      Store
      (  Catalogue,
         "y-separator",
         Image (Integer (Get_Position (Pane)))
      );
      return True;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Failed to store window layout: "
            &  Exception_Information (Error)
         )  );
         return False;
   end Store_Layout;

   use Gtk.Main.Router.GNAT_Stack;

   type Messages_Filter is new Log_Filter with null record;
   overriding function Ignore
                       (  Filter  : not null access Messages_Filter;
                          Domain  : String;
                          Level   : Log_Level_Flags;
                          Message : UTF8_String
                       )  return Boolean;

   Clipboard_Message : constant String :=
      "inner_clipboard_window_procedure: assertion `success' failed";

   function Ignore
            (  Filter  : not null access Messages_Filter;
               Domain  : String;
               Level   : Log_Level_Flags;
               Message : UTF8_String
            )  return Boolean is
   begin
      return
      (  Message'Length >= Clipboard_Message'Length
      and then
         (  Clipboard_Message
         =  Message
            (  Message'Last - Clipboard_Message'Length + 1
            .. Message'Last
      )  )  );
   end Ignore;

   Filter : Messages_Filter;
begin
--   Register_Global_Action (+Exception_Tracer'Access);
   Register_Id_Action
   (  Storage_Error'Identity,
      +Storage_Error_Tracer'Access
   );
--     Register_Id_Action
--     (  Constraint_Error'Identity,
--       +Constraint_Error_Tracer'Access
--     );
   Register_Id_Action
   (  Program_Error'Identity,
      +Program_Error_Tracer'Access
   );
   Gtk.Main.Init;
   --
   -- The following lines are meant for debugging under GNAT. They cause
   -- stack  tracing upon errors in the libraries specified. Remove them
   -- if you are using another compiler.
   --
   Set_Log_Trace ("Gtk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("Gdk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("GLib-GObject", GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("Gtk",          GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("GtkAda+",      GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("Fuzzy",        GLib.Messages.Log_Level_Flags'Last);
   Set_Log_Trace ("FuzzyML",      GLib.Messages.Log_Level_Flags'Last);
--   Gtk.Fuzzy_Object.Trace_Checks := True;

-- Gtk.RC.Parse ("test_fuzzy_gtk_widgets.rc");
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title
   (  "Fuzzy Machine Learning System v"
   &  Fuzzy_ML_Version.Value
   );
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
--   Set_Border_Width (Window, 10);

   Name_Tables.Name_Body := Name_Tables.Name_Body or To_Set ("-");

   declare
      Box      : Gtk_VBox;
      Greeting : Gtk_Item_Box;
      Surface  : Gtk_Event_Box;
   begin
      Gtk_New_VPaned (Pane);
      Gtk_New_VBox (Box);
      Gtk_New
      (  Widget   => Catalogue,
         Dock     => Box,
         Tracing  => Trace_Nothing
-- Tracing => Trace_IO or Trace_To_Output_Only or Trace_Set_Directory
      );
      Pane.Pack1 (Catalogue, True, False);
      Pane.Pack2 (Box, True);

      Gtk_New (Greeting, "greeting", Catalogue, False);
      Gtk_New (Surface);
      Surface.Add (Image_Logo_XPM.Image);
      Surface.Modify_Bg (State_Normal, Parse ("white"));
      Greeting.Pack_Start (Surface);
      Add_Item (Catalogue, "Wellcome", "gtk-about", Greeting);

      Window.Add (Pane);
      Pane.Show_All;
   end;
      -- Restore window size
   begin
      Gtk.Window.Set_Default_Size
      (  Window,
         GInt (Integer'(Value (Restore (Catalogue, "width", "")))),
         GInt (Integer'(Value (Restore (Catalogue, "height", ""))))
      );
   exception
      when others =>
         Gtk.Window.Set_Default_Size (Window, 800, 600);
   end;
   if 0 = Idle_Add (+Restore_Layout'Access) then
      null;
   end if;
   if 0 = Timeout_Add (2_000, +Store_Layout'Access) then
      null;
   end if;
   Set_Icon (Window, Image_Fuzzy_16_XPM.Get_Pixbuf);
   Show (Window);
   Gtk.Main.Main;

exception
   when Error : others =>
      Gtk.Main.Router.Trace (Error);
end Fuzzy_ML_System;
