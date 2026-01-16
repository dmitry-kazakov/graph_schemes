--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Gtk_Icon_Factory                      Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with Gdk.Pixbuf;            use Gdk.Pixbuf;
with Gtk.Icon_Factory;      use Gtk.Icon_Factory;
with Gtk.Icon_Set;          use Gtk.Icon_Set;
with Gtk.Icon_Source;       use Gtk.Icon_Source;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Stock;             use Gtk.Stock;
with Name_Tables;           use Name_Tables;

with Image_Bookmark_Add_16_XPM,        Image_Bookmark_Add_18_XPM;
with Image_Bookmark_Delete_16_XPM,     Image_Bookmark_Delete_18_XPM;
with Image_Classifier_16_XPM,          Image_Classifier_18_XPM;
with Image_Classifier_View_16_XPM,     Image_Classifier_View_18_XPM;
with Image_Classify_16_XPM,            Image_Classify_18_XPM;
with Image_Classify_64_XPM;
with Image_Classify_Input_16_XPM,      Image_Classify_Input_18_XPM;
with Image_Collapse_16_XPM,            Image_Collapse_18_XPM;
with Image_Compare_Lectures_16_XPM,    Image_Compare_Lectures_18_XPM;
with Image_Delete_16_XPM,              Image_Delete_18_XPM;
with Image_Down_16_XPM,                Image_Down_18_XPM;
with Image_Edit_16_XPM,                Image_Edit_18_XPM;
with Image_Exchange_Folders_16_XPM,    Image_Exchange_Folders_18_XPM;
with Image_Explain_16_XPM,             Image_Explain_18_XPM;
with Image_Feature_16_XPM,             Image_Feature_18_XPM;
with Image_Feature_Copy_16_XPM,        Image_Feature_Copy_18_XPM;
with Image_Feature_Delete_16_XPM,      Image_Feature_Delete_18_XPM;
with Image_Feature_View_16_XPM,        Image_Feature_View_18_XPM;
with Image_Find_16_XPM,                Image_Find_18_XPM;
with Image_Insert_16_XPM,              Image_Insert_18_XPM;
with Image_Learn_16_XPM,               Image_Learn_18_XPM;
with Image_Lecture_16_XPM,             Image_Lecture_18_XPM;
with Image_Lecture_Add_Example_16_XPM, Image_Lecture_Add_Example_18_XPM;
with Image_Lecture_Copy_16_XPM,        Image_Lecture_Copy_18_XPM;
with Image_Lecture_Edit_16_XPM,        Image_Lecture_Edit_18_XPM;
with Image_Lecture_Edited_16_XPM,      Image_Lecture_Edited_18_XPM;
with Image_Lecture_View_16_XPM,        Image_Lecture_View_18_XPM;
with Image_New_Classifier_16_XPM,      Image_New_Classifier_18_XPM;
with Image_New_Feature_16_XPM,         Image_New_Feature_18_XPM;
with Image_New_Folder_16_XPM,          Image_New_Folder_18_XPM;
with Image_New_From_FCL_16_XPM,        Image_New_From_FCL_18_XPM;
with Image_New_Lecture_16_XPM,         Image_New_Lecture_18_XPM;
with Image_Parent_16_XPM,              Image_Parent_18_XPM;
with Image_Preview_16_XPM,             Image_Preview_18_XPM;
with Image_Rename_16_XPM,              Image_Rename_18_XPM;
with Image_Rename_Item_16_XPM,         Image_Rename_Item_18_XPM;
with Image_Save_As_FCL_16_XPM,         Image_Save_As_FCL_18_XPM;
with Image_Save_As_Text_16_XPM,        Image_Save_As_Text_18_XPM;
with Image_Up_16_XPM,                  Image_Up_18_XPM;
with Image_Verify_16_XPM,              Image_Verify_18_XPM;

with Image_Cancel_XPM;
with Image_Classificatory_Feature_XPM;
with Image_Compare_XPM;
with Image_Confirm_XPM;
with Image_Dependent_Binary_Feature_XPM;
with Image_Empty_Lecture_XPM;
with Image_Float_Feature_XPM;
with Image_General_Lecture_XPM;
with Image_Graph_Branch_XPM;
with Image_Graph_Leaf_XPM;
with Image_Graph_Node_XPM;
with Image_Graph_Scheme_Classifier_XPM;
with Image_Has_In_XPM;
with Image_Has_Not_Out_XPM;
with Image_Has_Not_XPM;
with Image_Has_Out_XPM;
with Image_Independent_Binary_Feature_XPM;
with Image_Integer_Feature_XPM;
with Image_Isosceles_Feature_XPM;
with Image_Lecture_Add_Random_Example_16_XPM;
with Image_Lecture_Add_Random_Example_18_XPM;
with Image_Lecture_Copy_Example_16_XPM;
with Image_Lecture_Copy_Example_18_XPM;
with Image_Lecture_Delete_Example_16_XPM;
with Image_Lecture_Delete_Example_18_XPM;
with Image_Linear_Feature_XPM;
with Image_Negative_Example_XPM;
with Image_New_Lecture_From_Text_16_XPM;
with Image_New_Lecture_From_Text_18_XPM;
with Image_Nominal_Feature_XPM;
with Image_Number_XPM;
with Image_ODBC_Lecture_XPM;
with Image_Output_Feature_XPM;
with Image_Positive_Example_XPM;
with Image_Separator_Classifier_XPM;
with Image_SQLite_Lecture_XPM;
with Image_Subrange_Lecture_XPM;
with Image_Windowize_XPM;

package body Fuzzy.Gtk_Icon_Factory is
   use String_Maps;

   Icons           : Gtk.Icon_Factory.Gtk_Icon_Factory;
   Feature_Classes : String_Maps.Table;
   Panel_Size      : Gtk_Icon_Size := 0;

   type Pixbuf_Array is array (Positive range <>) of Gdk_Pixbuf;

   procedure Add_Stock
             (  Pictures : Pixbuf_Array;
                Name     : String;
                Label    : String
             )  is
      Set    : Gtk_Icon_Set;
      Source : Gtk_Icon_Source;
   begin
      Gtk_New (Set);
      for Index in Pictures'Range loop
         case Get_Width (Pictures (Index)) is
             when 16 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Menu);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when 18 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Small_Toolbar);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when 64 =>
                Gtk_New (Source);
                Source.Set_Pixbuf (Pictures (Index));
                Source.Set_Size (Icon_Size_Panel);
                Source.Set_Size_Wildcarded (False);
                Set.Add_Source (Source);
                Free (Source);
             when others =>
                null;
         end case;
         Add_Named (Name, Pictures (Index));
      end loop;
      Gtk_New (Source);
      Set_Pixbuf (Source, Pictures (Pictures'First));
      Set.Add_Source (Source);
      Free (Source);
      for Index in Pictures'Range loop
         Unref (Pictures (Index));
      end loop;
      Add (Icons, Name, Set);
      Unref (Set);
      declare
         Item : Gtk_Stock_Item;
      begin
         Gtk_New
         (  Item     => Item,
            Stock_ID => Name,
            Label    => Label,
            Modifier => 0,
            KeyVal   => 0,
            Translation_Domain => "fuzzy-ml-domain"
         );
         Add_Static ((1 => Item));
      end;
   end Add_Stock;

   function Escape_Name (Name : UTF8_String) return UTF8_String is
      Count : Natural := 0;
   begin
      for Index in Name'Range loop
         if Name (Index) = '_' then
            Count := Count + 1;
         end if;
      end loop;
      if Count = 0 then
         return Name;
      end if;
      declare
         Result : String (1..Name'Length + Count);
      begin
         Count := Result'First;
         for Index in Name'Range loop
            if Name (Index) = '_' then
               Result (Count) := '_';
               Count := Count + 1;
            end if;
            Result (Count) := Name (Index);
            Count := Count + 1;
         end loop;
         return Result;
      end;
   end Escape_Name;

   function Get_Feature_Icon (Class : UTF8_String) return UTF8_String is
   begin
      if IsIn (Feature_Classes, Class) then
         return Class;
      else
         return Stock_File;
      end if;
   end Get_Feature_Icon;

   function Icon_Size_Panel return Gtk_Icon_Size is
   begin
      if Panel_Size = 0 then
         Panel_Size := Icon_Size_Register ("Panel", 64, 64);
      end if;
      return Panel_Size;
   end Icon_Size_Panel;

   procedure Init is
   begin
      if Icons = null then
--           Gtk.RC.Parse_String
--           (  "style ""FuzzyBorderlessButton"" { "
--           &  "GtkButton::inner-border = {0,0,0,0} "
--           &  "GtkButton::focus-line-width = 0 "
--           &  "GtkButton::focus-padding = 0 "
--           &  "}"
--           );

         Gtk_New (Icons);
         Add_Default (Icons);
         Add_Stock
         (  (  Image_Bookmark_Add_18_XPM.Get_Pixbuf,
               Image_Bookmark_Add_16_XPM.Get_Pixbuf
            ),
            Bookmark_Add_Icon,
            "_Bookmark_Add"
         );
         Add_Stock
         (  (  Image_Bookmark_Delete_18_XPM.Get_Pixbuf,
               Image_Bookmark_Delete_16_XPM.Get_Pixbuf
            ),
            Bookmark_Delete_Icon,
            "_Bookmark_Delete"
         );
         Add_Stock
         (  (1 => Image_Graph_Branch_XPM.Get_Pixbuf),
            Graph_Branch_Icon,
            "_Branch"
         );
         Add_Stock
         (  (1 => Image_Cancel_XPM.Get_Pixbuf),
            Cancel_Icon,
            "_Cancel"
         );
         Add_Stock
         (  (1 => Image_Classificatory_Feature_XPM.Get_Pixbuf),
            Classificatory_Feature_Icon,
            "_Classificatory_Feature"
         );
         Add_Stock
         (  (  Image_Classify_18_XPM.Get_Pixbuf,
               Image_Classify_16_XPM.Get_Pixbuf,
               Image_Classify_64_XPM.Get_Pixbuf
            ),
            Classify_Icon,
            "_Classify"
         );
         Add_Stock
         (  (  Image_Classify_Input_18_XPM.Get_Pixbuf,
               Image_Classify_Input_16_XPM.Get_Pixbuf
            ),
            Classify_Input_Icon,
            "_Classify_Input"
         );
         Add_Stock
         (  (  Image_Classifier_18_XPM.Get_Pixbuf,
               Image_Classifier_16_XPM.Get_Pixbuf
            ),
            Classifier_Icon,
            "_Classifier"
         );
         Add_Stock
         (  (  Image_Classifier_View_18_XPM.Get_Pixbuf,
               Image_Classifier_View_16_XPM.Get_Pixbuf
            ),
            Classifier_View_Icon,
            "_Classifier_View"
         );
         Add_Stock
         (  (  Image_Collapse_18_XPM.Get_Pixbuf,
               Image_Collapse_16_XPM.Get_Pixbuf
            ),
            Collapse_Icon,
            "_Collapse"
         );
         Add_Stock
         (  (1 => Image_Compare_XPM.Get_Pixbuf),
            Compare_Icon,
            "_Compare"
         );
         Add_Stock
         (  (  Image_Compare_Lectures_18_XPM.Get_Pixbuf,
               Image_Compare_Lectures_16_XPM.Get_Pixbuf
            ),
            Compare_Lectures_Icon,
            "_Compare_Lectures"
         );
         Add_Stock
         (  (1 => Image_Confirm_XPM.Get_Pixbuf),
            Confirm_Icon,
            "_Confirm"
         );
         Add_Stock
         (  (1 => Image_Negative_Example_XPM.Get_Pixbuf),
            Negative_Icon,
            "_Contra"
         );
         Add_Stock
         (  (  Image_Delete_18_XPM.Get_Pixbuf,
               Image_Delete_16_XPM.Get_Pixbuf
            ),
            Delete_Icon,
            "_Delete"
         );
         Add_Stock
         (  (1 => Image_Dependent_Binary_Feature_XPM.Get_Pixbuf),
            Dependent_Binary_Feature_Icon,
            "_Dependent_Binary_Feature"
         );
         Add_Stock
         (  (  Image_Down_18_XPM.Get_Pixbuf,
               Image_Down_16_XPM.Get_Pixbuf
            ),
            Down_Icon,
            "_Down"
         );
         Add_Stock
         (  (  Image_Edit_18_XPM.Get_Pixbuf,
               Image_Edit_16_XPM.Get_Pixbuf
            ),
            Edit_Icon,
            "_Edit"
         );
         Add_Stock
         (  (1 => Image_Empty_Lecture_XPM.Get_Pixbuf),
            Empty_Lecture_Icon,
            "_Empty_Lecture"
         );
         Add_Stock
         (  (  Image_Explain_18_XPM.Get_Pixbuf,
               Image_Explain_16_XPM.Get_Pixbuf
            ),
            Explain_Icon,
            "_Explain"
         );
         Add_Stock
         (  (  Image_Feature_18_XPM.Get_Pixbuf,
               Image_Feature_16_XPM.Get_Pixbuf
            ),
            Feature_Icon,
            "_Feature"
         );
         Add_Stock
         (  (  Image_Feature_Copy_18_XPM.Get_Pixbuf,
               Image_Feature_Copy_16_XPM.Get_Pixbuf
            ),
            Feature_Copy_Icon,
            "_Feature_Copy"
         );
         Add_Stock
         (  (  Image_Feature_Delete_18_XPM.Get_Pixbuf,
               Image_Feature_Delete_16_XPM.Get_Pixbuf
            ),
            Feature_Delete_Icon,
            "_Feature_Delete"
         );
         Add_Stock
         (  (  Image_Feature_View_18_XPM.Get_Pixbuf,
               Image_Feature_View_16_XPM.Get_Pixbuf
            ),
            Feature_View_Icon,
            "_Feature_View"
         );
         Add_Stock
         (  (  Image_Find_18_XPM.Get_Pixbuf,
               Image_Find_16_XPM.Get_Pixbuf
            ),
            Find_Icon,
            "_Find"
         );
         Add_Stock
         (  (1 => Image_Float_Feature_XPM.Get_Pixbuf),
            Float_Feature_Icon,
            "_Float_Feature"
         );
         Add_Stock
         (  (1 => Image_General_Lecture_XPM.Get_Pixbuf),
            General_Lecture_Icon,
            "_General_Lecture"
         );
         Add_Stock
         (  (1 => Image_Graph_Scheme_Classifier_XPM.Get_Pixbuf),
            Graph_Scheme_Classifier_Icon,
            "_Graph_Scheme_Classifier"
         );
         Add_Stock
         (  (1 => Image_Has_In_XPM.Get_Pixbuf),
            Has_In_Icon,
            "_In"
         );
         Add_Stock
         (  (1 => Image_Independent_Binary_Feature_XPM.Get_Pixbuf),
            Independent_Binary_Feature_Icon,
            "_Independent_Binary_Feature"
         );
         Add_Stock
         (  (  Image_Insert_18_XPM.Get_Pixbuf,
               Image_Insert_16_XPM.Get_Pixbuf
            ),
            Insert_Icon,
            "_Insert"
         );
         Add_Stock
         (  (1 => Image_Integer_Feature_XPM.Get_Pixbuf),
            Integer_Feature_Icon,
            "_Integer_Feature"
         );
         Add_Stock
         (  (1 => Image_Isosceles_Feature_XPM.Get_Pixbuf),
            Isosceles_Feature_Icon,
            "_Isosceles_Feature"
         );
         Add_Stock
         (  (1 => Image_Graph_Leaf_XPM.Get_Pixbuf),
            Graph_Leaf_Icon,
            "_Leaf"
         );
         Add_Stock
         (  (  Image_Learn_18_XPM.Get_Pixbuf,
               Image_Learn_16_XPM.Get_Pixbuf
            ),
            Learn_Icon,
            "_Learn"
         );
         Add_Stock
         (  (  Image_Lecture_18_XPM.Get_Pixbuf,
               Image_Lecture_16_XPM.Get_Pixbuf
            ),
            Lecture_Icon,
            "_Lecture"
         );
         Add_Stock
         (  (  Image_Lecture_Add_Example_18_XPM.Get_Pixbuf,
               Image_Lecture_Add_Example_16_XPM.Get_Pixbuf
            ),
            Lecture_Add_Example_Icon,
            "_Lecture_Add_Example"
         );
         Add_Stock
         (  (  Image_Lecture_Add_Random_Example_18_XPM.Get_Pixbuf,
               Image_Lecture_Add_Random_Example_16_XPM.Get_Pixbuf
            ),
            Lecture_Add_Random_Example_Icon,
            "_Lecture_Add_Random_Example"
         );
         Add_Stock
         (  (  Image_Lecture_Copy_18_XPM.Get_Pixbuf,
               Image_Lecture_Copy_16_XPM.Get_Pixbuf
            ),
            Lecture_Copy_Icon,
            "_Lecture_Copy"
         );
         Add_Stock
         (  (  Image_Lecture_Copy_Example_18_XPM.Get_Pixbuf,
               Image_Lecture_Copy_Example_16_XPM.Get_Pixbuf
            ),
            Lecture_Copy_Example_Icon,
            "_Lecture_Copy_Example"
         );
         Add_Stock
         (  (  Image_Lecture_Delete_Example_18_XPM.Get_Pixbuf,
               Image_Lecture_Delete_Example_16_XPM.Get_Pixbuf
            ),
            Lecture_Delete_Example_Icon,
            "_Lecture_Delete_Example"
         );
         Add_Stock
         (  (  Image_Lecture_Edit_18_XPM.Get_Pixbuf,
               Image_Lecture_Edit_16_XPM.Get_Pixbuf
            ),
            Lecture_Edit_Icon,
            "_Lecture_Edit"
         );
         Add_Stock
         (  (  Image_Lecture_Edited_18_XPM.Get_Pixbuf,
               Image_Lecture_Edited_16_XPM.Get_Pixbuf
            ),
            Lecture_Edited_Icon,
            "_Lecture_Edited"
         );
         Add_Stock
         (  (  Image_Lecture_View_18_XPM.Get_Pixbuf,
               Image_Lecture_View_16_XPM.Get_Pixbuf
            ),
            Lecture_View_Icon,
            "_Lecture_View"
         );
         Add_Stock
         (  (1 => Image_Linear_Feature_XPM.Get_Pixbuf),
            Linear_Feature_Icon,
            "_Linear_Feature"
         );
         Add_Stock
         (  (  Image_New_Classifier_18_XPM.Get_Pixbuf,
               Image_New_Classifier_16_XPM.Get_Pixbuf
            ),
            New_Classifier_Icon,
            "_New_Classifier"
         );
         Add_Stock
         (  (  Image_New_Feature_18_XPM.Get_Pixbuf,
               Image_New_Feature_16_XPM.Get_Pixbuf
            ),
            New_Feature_Icon,
            "_New_Feature"
         );
         Add_Stock
         (  (  Image_New_Folder_18_XPM.Get_Pixbuf,
               Image_New_Folder_16_XPM.Get_Pixbuf
            ),
            New_Folder_Icon,
            "_New_Folder"
         );
         Add_Stock
         (  (  Image_New_From_FCL_18_XPM.Get_Pixbuf,
               Image_New_From_FCL_16_XPM.Get_Pixbuf
            ),
            New_From_FCL_Icon,
            "_New_From_FCL"
         );
         Add_Stock
         (  (  Image_New_Lecture_18_XPM.Get_Pixbuf,
               Image_New_Lecture_16_XPM.Get_Pixbuf
            ),
            New_Lecture_Icon,
            "_New_Lecture"
         );
         Add_Stock
         (  (  Image_New_Lecture_From_Text_18_XPM.Get_Pixbuf,
               Image_New_Lecture_From_Text_16_XPM.Get_Pixbuf
            ),
            New_Lecture_From_Text_Icon,
            "_New_Lecture_From_Text"
         );
         Add_Stock
         (  (1 => Image_Graph_Node_XPM.Get_Pixbuf),
            Graph_Node_Icon,
            "_Node"
         );
         Add_Stock
         (  (1 => Image_Nominal_Feature_XPM.Get_Pixbuf),
            Nominal_Feature_Icon,
            "_Nominal_Feature"
         );
         Add_Stock
         (  (1 => Image_Has_Not_XPM.Get_Pixbuf),
            Has_Not_Icon,
            "_Not_in"
         );
         Add_Stock
         (  (1 => Image_Has_Not_Out_XPM.Get_Pixbuf),
            Has_Not_Out_Icon,
            "_Not_out"
         );
         Add_Stock
         (  (1 => Image_Number_XPM.Get_Pixbuf),
            Number_Icon,
            "_Number"
         );
         Add_Stock
         (  (1 => Image_ODBC_Lecture_XPM.Get_Pixbuf),
            ODBC_Lecture_Icon,
            "_ODBC_Lecture"
         );
         Add_Stock
         (  (1 => Image_Has_Out_XPM.Get_Pixbuf),
            Has_Out_Icon,
            "_Out"
         );
         Add_Stock
         (  (1 => Image_Output_Feature_XPM.Get_Pixbuf),
            Output_Feature_Icon,
            "_Output_Feature"
         );
         Add_Stock
         (  (  Image_Parent_18_XPM.Get_Pixbuf,
               Image_Parent_16_XPM.Get_Pixbuf
            ),
            Parent_Icon,
            "_Parent"
         );
         Add_Stock
         (  (  Image_Preview_18_XPM.Get_Pixbuf,
               Image_Preview_16_XPM.Get_Pixbuf
            ),
            Preview_Icon,
            "_Preview"
         );
         Add_Stock
         (  (1 => Image_Positive_Example_XPM.Get_Pixbuf),
            Positive_Icon,
            "_Pro"
         );
         Add_Stock
         (  (  Image_Rename_18_XPM.Get_Pixbuf,
               Image_Rename_16_XPM.Get_Pixbuf
            ),
            Rename_Icon,
            "_Rename"
         );
         Add_Stock
         (  (  Image_Rename_Item_18_XPM.Get_Pixbuf,
               Image_Rename_Item_16_XPM.Get_Pixbuf
            ),
            Rename_Item_Icon,
            "_Rename_Item"
         );
         Add_Stock
         (  (  Image_Save_As_FCL_18_XPM.Get_Pixbuf,
               Image_Save_As_FCL_16_XPM.Get_Pixbuf
            ),
            Save_As_FCL_Icon,
            "_Save_As_FCL"
         );
         Add_Stock
         (  (  Image_Save_As_Text_18_XPM.Get_Pixbuf,
               Image_Save_As_Text_16_XPM.Get_Pixbuf
            ),
            Save_As_Text_Icon,
            "_Save_As_Text"
         );
         Add_Stock
         (  (1 => Image_Separator_Classifier_XPM.Get_Pixbuf),
            Separator_Classifier_Icon,
            "_Separator_Classifier"
         );
         Add_Stock
         (  (  Image_Exchange_Folders_18_XPM.Get_Pixbuf,
               Image_Exchange_Folders_16_XPM.Get_Pixbuf
            ),
            Swap_Folders_Icon,
            "_Swap_Folders"
         );
         Add_Stock
         (  (1 => Image_SQLite_Lecture_XPM.Get_Pixbuf),
            SQLite_Lecture_Icon,
            "_SQLite_Lecture"
         );
         Add_Stock
         (  (1 => Image_Subrange_Lecture_XPM.Get_Pixbuf),
            Subrange_Lecture_Icon,
            "_Subrange_Lecture"
         );
         Add_Stock
         (  (  Image_Verify_18_XPM.Get_Pixbuf,
               Image_Verify_16_XPM.Get_Pixbuf
            ),
            Verify_Icon,
            "_Verify"
         );
         Add_Stock
         (  (1 => Image_Windowize_XPM.Get_Pixbuf),
            Windowize_Icon,
            "_Windowize"
         );
         Add_Stock
         (  (  Image_Up_18_XPM.Get_Pixbuf,
               Image_Up_16_XPM.Get_Pixbuf
            ),
            Up_Icon,
            "_Up"
         );
      end if;
   end Init;

   procedure Set_Button_Style (Class  : UTF8_String) is
   begin
      Init;
--        Gtk.RC.Parse_String
--        (  "class """
--        &  Class
--        &  """ style ""FuzzyBorderlessButton"""
--        );
   end Set_Button_Style;

begin
   Add (Feature_Classes, Classificatory_Feature_Icon,     1);
   Add (Feature_Classes, Dependent_Binary_Feature_Icon,   2);
   Add (Feature_Classes, Float_Feature_Icon,              3);
   Add (Feature_Classes, Independent_Binary_Feature_Icon, 4);
   Add (Feature_Classes, Integer_Feature_Icon,            5);
   Add (Feature_Classes, Isosceles_Feature_Icon,          6);
   Add (Feature_Classes, Linear_Feature_Icon,             7);
   Add (Feature_Classes, Nominal_Feature_Icon,            8);
   Add (Feature_Classes, Output_Feature_Icon,             9);
end Fuzzy.Gtk_Icon_Factory;
