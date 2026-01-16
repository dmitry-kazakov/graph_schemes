--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.HTML                          Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  10:08 22 Nov 2014  --
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

with Fuzzy.Feature;                use Fuzzy.Feature;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Intuitionistic;         use Fuzzy.Intuitionistic;
with HTML;                         use HTML;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Units;                        use Units;

package body Fuzzy.Lecture.Html is

   Top_Cell_Beg  : constant String := "<td valign=""top"">";

   procedure Put
             (  File       : File_Type;
                Lesson     : Lecture_Object'Class;
                From       : Positive := 1;
                To         : Positive := Positive'Last;
                Parameters : HTML_Parameters'Class
             )  is
      Columns   : constant Natural  := Get_Features_Number (Lesson);
      Last      : constant Positive :=
                     Positive'Min
                     (  Get_Examples_Number (Lesson),
                        To
                     );
      Title_Beg : constant String :=
                    "<td bgcolor=" & To_String (Parameters.Title) & ">";

      function Is_Row_Known
               (  Example     : Positive;
                  Possibility : Image_Type;
                  Necessity   : Image_Type
               )  return Boolean is
      begin
         for Index in 1..Columns loop
            declare
               Feature : Feature_Object'Class renames
                  Ptr (Get_Feature (Lesson, Index)).all;
            begin
               if (  Is_Known (Lesson, Example, Feature, Possibility)
                  or else
                     Is_Known (Lesson, Example, Feature, Necessity)
                  )
               then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Is_Row_Known;

      procedure Put_Row
                (  Example     : Positive;
                   Possibility : Image_Type;
                   Necessity   : Image_Type
                ) is
      begin
         Put_Line (File, Row_Beg);
         if Possibility = Has_In then
            Put_Line
            (  File,
               (  Top_Cell_Beg
               &  Align_Right_Beg
               &  Image (Example)
               &  Align_End
               &  Cell_End
            )  );
         else
            Put_Line
            (  File,
               (  Top_Cell_Beg
               &  Align_Right_Beg
               &  '-'
               &  Image (Example)
               &  Align_End
               &  Cell_End
            )  );
         end if;
         for Index in 1..Columns loop
            declare
               Feature : Feature_Object'Class renames
                   Ptr (Get_Feature (Lesson, Index)).all;
            begin
               Put_Line (File, Cell_Beg);
               declare
                  Value : Classification (Feature.Cardinality);
               begin
                  Value.Possibility :=
                     Get (Lesson, Example, Feature, Possibility);
                  Value.Necessity :=
                     not Get (Lesson, Example, Feature, Necessity);
                  Put (File, Feature, Value, Parameters);
               end;
               Put_Line (File, Cell_End);
            end;
         end loop;
         Put_Line (File, Row_End);
      end Put_Row;

      procedure Put_Scale (Scale : String) is
      begin
         if Scale'Length > 0 then
            Put (File, " ");
            case Parameters.Mode is
               when ASCII_Set | Latin1_Set =>
                  Put_Latin1 (File, Scale);
               when UTF8_Set =>
                  Put_UTF8 (File, Scale, Parameters.UTF8_Error);
            end case;
         end if;
      end Put_Scale;

   begin
      if From > Last or Columns = 0 then
         return;
      end if;
      Put_Line
      (  File,
         "<table cellpadding=""3"" cellspacing=""0"" border=""3"">"
      );
      --
      -- Make the title
      --
      Put_Line (File, Row_Beg);
         Put_Line
         (  File,
            (  Title_Beg
            &  Bold_Beg
            &  Align_Right_Beg
            &  "No."
            &  Align_End
            &  Bold_End
            &  Cell_End
         )  );
         for Index in 1..Columns loop
            declare
               Feature : Feature_Object'Class renames
                  Ptr (Get_Feature (Lesson, Index)).all;
            begin
               Put
               (  File,
                  Title_Beg & Bold_Beg & Align_Center_Beg
               );
               case Parameters.Mode is
                  when ASCII_Set | Latin1_Set =>
                     Put_Latin1 (File, Feature.Name.all);
                  when UTF8_Set =>
                     Put_UTF8
                     (  File,
                        Feature.Name.all,
                        Parameters.UTF8_Error
                     );
               end case;
               if (  not Parameters.Put_Units
                  and then
                     Feature in Domain_Feature_Object'Class
                  )
               then
                  Put_Scale
                  (  Get_Scale_Text
                     (  Domain_Feature_Object'Class (Feature),
                        Parameters
                  )  );
               end if;
               Put_Line (File, Align_End & Bold_End & Cell_End);
            end;
         end loop;
      Put_Line (File, Row_End);
      --
      -- Make the table body
      --
      for Example in From..Last loop
         if Is_Row_Known (Example, Has_In, Has_Not) then
            Put_Row (Example, Has_In,  Has_Not);
            if Is_Row_Known (Example, Has_Out, Has_Not_Out) then
               Put_Row (Example, Has_Out, Has_Not_Out);
            end if;
         elsif Is_Row_Known (Example, Has_Out, Has_Not_Out) then
            Put_Row (Example, Has_Out, Has_Not_Out);
         else
            Put_Row (Example, Has_In,  Has_Not);
         end if;
      end loop;
      Put_Line (File, Table_End);
   end Put;

   procedure Put
             (  Lesson     : Lecture_Object'Class;
                From       : Positive := 1;
                To         : Positive := Positive'Last;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Lesson, From, To, Parameters);
   end Put;

end Fuzzy.Lecture.HTML;
