--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Renaming_Tools         Luebeck            --
--  Implementation                                 Summer, 2009       --
--                                                                    --
--                                Last revision :  11:45 29 May 2020  --
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

with GLib.Messages;                use GLib.Messages;
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Separator;                use Gtk.Separator;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;
with Strings_Edit;                 use Strings_Edit;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Renaming_Tools is
   use type Deposit_Handle;
   use Move_Set_Stacks;
   use Redo_Buttons;
   use Undo_Buttons;

   type Relation is record
      Left  : Boolean;
      Right : Boolean;
   end record;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Renaming_Tools." & Name;
   end Where;

   procedure Move
             (  Tool   : not null access Renaming_Tool_Record;
                From   : Item_Path;
                To     : Item_Path;
                Items  : in out Name_List;
                Failed : out Boolean
             )  is
      Widget         : constant Gtk_Fuzzy_Catalogue := Tool.Catalogue;
      From_Storage   : Storage_Handle;
      To_Storage     : Storage_Handle;
      From_Directory : Deposit_Handle;
      To_Directory   : Deposit_Handle;
      List           : Gtk_Fuzzy_Objects_View;
   begin
      Browse
      (  Store   => Widget.Cache,
         Path    => From,
         Storage => From_Storage,
         Object  => From_Directory
      );
      Browse
      (  Store   => Widget.Cache,
         Path    => To,
         Storage => To_Storage,
         Object  => To_Directory
      );
      if From_Storage = To_Storage then
         for Index in Items'Range loop
            if Items (Index) /= null then
               declare
                  Name : Item_Name renames Items (Index).all;
                  Path : constant Item_Path :=
                                  Get_Path (Widget.Cache, From, Name);
               begin
                  Rename
                  (  Storage    => From_Storage,
                     Old_Name   => String (Name),
                     Old_Parent => From_Directory,
                     New_Name   => String (Name),
                     New_Parent => To_Directory
                  );
                  Deleted (Tool.Catalogue.Cache, Path);
               exception
                  when Error : Ada.IO_Exceptions.Data_Error =>
                     case Query
                          (  Widget,
                             Style_Get (Widget, "error-title"),
                             Style_Get (Widget, "error-icon"),
                             Style_Get (Widget, "error-icon-size"),
                             (  Style_Get
                                (  Tool.Catalogue,
                                   "inconsistent-storage-error"
                                )
                             &  Exception_Message (Error)
                          )  )  is
                        when others =>
                           for Rest in Index..Items'Last loop
                              Free (Items (Rest));
                           end loop;
                           exit;
                     end case;
                  when Ada.IO_Exceptions.Name_Error => -- Name conflict
                     case Query
                          (  Tool.Catalogue,
                             Style_Get (Widget, "error-title"),
                             Style_Get (Widget, "error-icon"),
                             Style_Get (Widget, "error-icon-size"),
                             (  Style_Get
                                (  Widget,
                                   "duplicated-error-begin"
                                )
                             &  String (Name)
                             &  Style_Get
                                (  Widget,
                                   "duplicated-error-end"
                             )  ),
                             True
                          )  is
                        when Gtk_Response_OK =>
                           Free (Items (Index));
                        when others =>
                           for Rest in Index..Items'Last loop
                              Free (Items (Rest));
                           end loop;
                           exit;
                     end case;
                     Failed := Index = Items'First;
                  when others =>
                     Free (Items (Index));
               end;
            end if;
         end loop;
         Changed (Tool.Catalogue.Cache, To);
      end if;
      Failed := True;
      List := Get_Current (Widget).List;
      if Get_Directory_Object (List) = To_Directory then
         Switch_To (Widget, Tool.Catalogue.Mode);
      else
         List := Get_Current (Tool.Catalogue).List;
         if Get_Directory_Object (List) /= To_Directory then
            Set_Current_Directory (Tool.Catalogue.Tree, To);
            List := Get_Current (Tool.Catalogue).List;
         end if;
      end if;
      if List /= null then
         Reset_Selection (List);
      end if;
      for Index in Items'Range loop
         if Items (Index) /= null then
            if List /= null then
               Set_Selection (List, Items (Index).all, True);
            end if;
            Failed := False;
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move")
         )  );
   end Move;

   function Compare
            (  Left, Right : Gtk_Fuzzy_Objects_View
            )  return Relation is
      function Is_In
               (  From : Gtk_Fuzzy_Objects_View;
                  Path : Item_Path;
                  To   : Gtk_Fuzzy_Objects_View
               )  return Boolean is
         List : Selection renames Get_Selection (From);
      begin
         for Index in List'Range loop
            declare
               Item : constant Item_Path :=
                               Get_Path (From, List (Index));
            begin
               if (  Is_Prefix (String (Item), String (Path))
                  or else
                     (  Get_Index
                        (  To,
                           Get_Name (Get_Cache (From), Item)
                        )
                     >  0
                  )  )
               then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Is_In;
      Left_Directory  : constant Item_Path := Get_Directory (Left);
      Right_Directory : constant Item_Path := Get_Directory (Right);
      Left_Size       : constant Natural := Get_Selection_Size (Left);
      Right_Size      : constant Natural := Get_Selection_Size (Right);
   begin
      if (  Left_Directory = Right_Directory
         or else
            Get_Storage (Left) /= Get_Storage (Right)
         )
      then
         return (False, False);
      else
         return
         (  Left =>
               (  Left_Size  > 0
               and then
                  not Is_In (Left, Right_Directory, Right)
               ),
            Right =>
               (  Right_Size > 0
               and then
                  not Is_In (Right, Left_Directory, Left)
         )     );
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Compare")
         )  );
         return (False, False);
   end Compare;

   function Create
            (  From, To : Gtk_Fuzzy_Objects_View
            )  return Move_Set_Handles.Handle is
      Selected : Selection renames Get_Selection (From);
      Result   : constant Move_Set_Handles.Handle :=
                    Ref (new Move_Set (Selected'Length));
      This     : Move_Set'Class renames Ptr (Result).all;
   begin
      This.From := new Item_Path'(Get_Directory (From));
      This.To   := new Item_Path'(Get_Directory (To));
      for Index in Selected'Range loop
         This.List (Index) :=
            new Item_Name'(Get_Name (From, Selected (Index)));
      end loop;
      return Result;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Create")
         )  );
         return Result;
   end Create;

   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
      Action : constant Move_Set_Handles.Handle :=
                        Create
                        (  Get_Current (Tool.Browser).List,
                           Get_Current (Tool.Catalogue).List
                        );
      Data   : Move_Set'Class renames Ptr (Action).all;
      Failed : Boolean;
   begin
      Data.Into := False;
      Move
      (  Tool   => Tool,
         From   => Data.From.all,
         To     => Data.To.all,
         Items  => Data.List,
         Failed => Failed
      );
      if not Failed then
         Push (Tool.Browser.Renaming.Undo, Action);
         Set_Sensitive (Tool.Undo, True);
      end if;
      Erase (Tool.Browser.Renaming.Redo);
      Set_Sensitive (Tool.Redo, False);
   end Down;

   procedure Gtk_New
             (  Item    : out Renaming_Tool;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Mode    : Filter_Mode
             )  is
   begin
      Item := new Renaming_Tool_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Mode);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item : not null access Renaming_Tool_Record'Class;
                Mode : Filter_Mode
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Alignment : Gtk_Alignment;
      Separator : Gtk_Separator;
      procedure Connect (To : Panel; Index : Positive) is
      begin
         if To /= null then
            Set
            (  Item.Handlers (Index),
               Renaming_Tool_Handlers.Connect
               (  To.List,
                  "directory-changed",
                  Renaming_Tool_Handlers.To_Marshaller
                  (  State_Changed'Access
                  ),
                  Item.all'Unchecked_Access
            )  );
            Set
            (  Item.Handlers (Index + 1),
               Renaming_Tool_Handlers.Connect
               (  To.List,
                  "selection-changed",
                  Renaming_Tool_Handlers.To_Marshaller
                  (  State_Changed'Access
                  ),
                  Item.all'Unchecked_Access
            )  );
         end if;
      end Connect;
      procedure Connect (To : Panel) is
      begin
         if To /= null then
            Renaming_Tool_Handlers.Connect
            (  To.List,
               "directory-changed",
               State_Changed'Access,
               Item.all'Unchecked_Access
            );
            Renaming_Tool_Handlers.Connect
            (  To.List,
               "selection-changed",
               State_Changed'Access,
               Item.all'Unchecked_Access
            );
         end if;
      end Connect;
   begin
      Initialize (Item, "renaming box", False);

      Gtk_New_HBox (Item.Actions_Box);
      Set_Spacing (Item.Actions_Box, GInt (Button_Spacing));
      Gtk_New (Alignment, 0.5, 0.5, 0.2, 0.2);
      Pack_Start (Item, Alignment, False, False);
      Add (Alignment, Item.Actions_Box);

      Gtk_New (Item.Down);

      Pack_Start (Item.Actions_Box, Item.Down, False, False);
      Renaming_Tool_Handlers.Connect
      (  Item.Down,
         "clicked",
         Down'Access,
         Item.all'Unchecked_Access
      );

      Gtk_New (Item.Up);
      Pack_Start (Item.Actions_Box, Item.Up, False, False);
      Renaming_Tool_Handlers.Connect
      (  Item.Up,
         "clicked",
         Up'Access,
         Item.all'Unchecked_Access
      );

      Gtk_New_Vseparator (Separator);
      Pack_Start (Item.Actions_Box, Separator, False, False);

      Gtk_New (Item.Swap);
      Pack_Start (Item.Actions_Box, Item.Swap, False, False);
      Renaming_Tool_Handlers.Connect
      (  Item.Swap,
         "clicked",
         Swap'Access,
         Item.all'Unchecked_Access
      );

      Gtk_New_Vseparator (Separator);
      Pack_Start (Item.Actions_Box, Separator, False, False);

      Gtk_New (Item.Undo);
      Pack_Start (Item.Actions_Box, Item.Undo, False, False);
      Set_Sensitive
      (  Item.Undo,
         not Is_Empty (Item.Browser.Renaming.Undo)
      );
      Renaming_Tool_Handlers.Connect
      (  Item.Undo,
         "clicked",
         Undo'Access,
         Item.all'Unchecked_Access
      );

      Gtk_New (Item.Redo);
      Set_Sensitive
      (  Item.Redo,
         not Is_Empty (Item.Browser.Renaming.Redo)
      );
      Pack_Start (Item.Actions_Box, Item.Redo, False, False);
      Renaming_Tool_Handlers.Connect
      (  Item.Redo,
         "clicked",
         Redo'Access,
         Item.all'Unchecked_Access
      );

      begin
         Gtk_New
         (  Widget   => Item.Catalogue,
            Dock     => Item.Browser.Dock,
            Path     => Get_Current_Directory (Item.Browser.Tree),
            Mode     => Mode,
            Buttons  => False,
            Store    => Item.Browser.Cache,
            Manager  => Get_Manager (Item.Browser.Cache)
         );
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Gtk_New
            (  Widget   => Item.Catalogue,
               Dock     => Item.Browser.Dock,
               Mode     => Mode,
               Buttons  => False,
               Store    => Item.Browser.Cache,
               Manager  => Get_Manager (Item.Browser.Cache)
            );
      end;

      Pack_Start (Item, Item.Catalogue);
      Set_Position
      (  Item.Catalogue,
         Get_Position (Item.Browser) - 2 * GInt (Column_Spacing)
      );

      Show_All (Item);
      Connect (Item.Browser.Classifiers, 1);
      Connect (Item.Browser.Features, 3);
      Connect (Item.Browser.Lectures, 5);
      Connect (Item.Catalogue.Classifiers);
      Connect (Item.Catalogue.Features);
      Connect (Item.Catalogue.Lectures);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-move-label"),
         Style_Get (Item.Browser, "tab-move-icon"),
         Item
      );
      State_Changed (Item, Item.all'Unchecked_Access);
   end Initialize;

   procedure Redo
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
      Action : Move_Set_Handles.Handle;
   begin
      if Is_Empty (Tool.Browser.Renaming.Redo) then
         Set_Sensitive (Tool.Redo, False);
      else
         Action := Top (Tool.Browser.Renaming.Redo);
         declare
            Data   : Move_Set'Class renames Ptr (Action).all;
            Failed : Boolean;
         begin
            Data.Into := not Data.Into;
            if Data.Into then
               Move
               (  Tool   => Tool,
                  From   => Data.From.all,
                  To     => Data.To.all,
                  Items  => Data.List,
                  Failed => Failed
               );
            else
               Move
               (  Tool   => Tool,
                  From   => Data.From.all,
                  To     => Data.To.all,
                  Items  => Data.List,
                  Failed => Failed
               );
            end if;
            Pop (Tool.Browser.Renaming.Redo);
            Set_Sensitive
            (  Tool.Redo,
               not Is_Empty (Tool.Browser.Renaming.Redo)
            );
            if not Failed then
               Push (Tool.Browser.Renaming.Undo, Action);
               Set_Sensitive (Tool.Undo, True);
            end if;
         end;
      end if;
   end Redo;

   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
      Result : Relation := (False, False);
   begin
      if Is_Current (Tool.Browser.Classifiers) then
         if Is_Current (Tool.Catalogue.Classifiers) then
            Result :=
               Compare
               (  Tool.Browser.Classifiers.List,
                  Tool.Catalogue.Classifiers.List
               );
         end if;
      elsif Is_Current (Tool.Browser.Features) then
         if Is_Current (Tool.Catalogue.Features) then
            Result :=
               Compare
               (  Tool.Browser.Features.List,
                  Tool.Catalogue.Features.List
               );
         end if;
      elsif Is_Current (Tool.Browser.Lectures) then
         if Is_Current (Tool.Catalogue.Lectures) then
            Result :=
               Compare
               (  Tool.Browser.Lectures.List,
                  Tool.Catalogue.Lectures.List
               );
         end if;
      end if;
      Set_Sensitive (Tool.Down, Result.Left);
      Set_Sensitive (Tool.Up,   Result.Right);
   end State_Changed;

   procedure Swap
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
   begin
      declare
         Upper : constant Item_Path :=
                          Get_Current_Directory (Tool.Browser.Tree);
         Lower : constant Item_Path :=
                          Get_Current_Directory (Tool.Catalogue.Tree);
      begin
         if Upper /= Lower then
            Set_Current_Directory (Tool.Browser.Tree,  Lower);
            Set_Current_Directory (Tool.Catalogue.Tree, Upper);
         end if;
      end;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Swap fault:"
            &  Exception_Information (Error)
            &  Where ("Swap")
         )  );
   end Swap;

   procedure Undo
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
      Action : Move_Set_Handles.Handle;
   begin
      if Is_Empty (Tool.Browser.Renaming.Undo) then
         Set_Sensitive (Tool.Undo, False);
      else
         Action := Top (Tool.Browser.Renaming.Undo);
         declare
            Data   : Move_Set'Class renames Ptr (Action).all;
            Failed : Boolean;
         begin
            Data.Into := not Data.Into;
            if Data.Into then
               Move
               (  Tool   => Tool,
                  From   => Data.To.all,
                  To     => Data.From.all,
                  Items  => Data.List,
                  Failed => Failed
               );
            else
               Move
               (  Tool   => Tool,
                  From   => Data.To.all,
                  To     => Data.From.all,
                  Items  => Data.List,
                  Failed => Failed
               );
            end if;
            Pop (Tool.Browser.Renaming.Undo);
            Set_Sensitive
            (  Tool.Undo,
               not Is_Empty (Tool.Browser.Renaming.Undo)
            );
            if not Failed then
               Push (Tool.Browser.Renaming.Redo, Action);
               Set_Sensitive (Tool.Redo, True);
            end if;
         end;
      end if;
   end Undo;

   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                Tool   : Renaming_Tool
             )  is
      Action : constant Move_Set_Handles.Handle :=
                        Create
                        (  Get_Current (Tool.Catalogue).List,
                           Get_Current (Tool.Browser).List
                        );
      Data   : Move_Set'Class renames Ptr (Action).all;
      Failed : Boolean;
   begin
      Data.Into := True;
      Move
      (  Tool   => Tool,
         From   => Data.From.all,
         To     => Data.To.all,
         Items  => Data.List,
         Failed => Failed
      );
      if not Failed then
         Push (Tool.Browser.Renaming.Undo, Action);
         Set_Sensitive (Tool.Undo, True);
      end if;
      Erase (Tool.Browser.Renaming.Redo);
      Set_Sensitive (Tool.Redo, False);
   end Up;

end Gtk.Fuzzy_Catalogue.Renaming_Tools;
