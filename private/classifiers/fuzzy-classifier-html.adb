--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Scheme.Handle.HTML              Luebeck            --
--  Implementation                                 Spring, 2003       --
--                                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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

with Confidence_Factors.Edit;     use Confidence_Factors.Edit;
with Fuzzy.Classifier.Identity;   use Fuzzy.Classifier.Identity;
with Fuzzy.Classifier.Handle;     use Fuzzy.Classifier.Handle;
with Fuzzy.Classifier.Separator;  use Fuzzy.Classifier.Separator;
with Fuzzy.Graph.Handle;          use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Handle.HTML;     use Fuzzy.Graph.Handle.HTML;
with Fuzzy.Graph.Scheme;          use Fuzzy.Graph.Scheme;

package body Fuzzy.Classifier.HTML is

   procedure Put
             (  File       : File_Type;
                Classifier : Classifier_Object'Class;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      if Classifier in Graph_Scheme_Object'Class then
         declare
            Roots : Root_Nodes_Array renames
                    Graph_Scheme_Object'Class (Classifier).Roots;
         begin
            Put (File, "<p>");
            for Image in Roots'Range loop
               if Is_Valid (Roots (Image)) then
                  Put_Line
                  (  File,
                     (  "Deduced from "
                     &  Image_Type'Image (Image)
                     &  " images:<br>"
                  )  );
                  Put (File, Roots (Image), Parameters);
               else
                  Put_Line
                  (  File,
                     (  "Nothing deduced from "
                     &  Image_Type'Image (Image)
                     &  " images<br>"
                  )  );
               end if;
            end loop;
            Put_Line (File, "</p>");
         end;
      elsif Classifier in Identity_Classifier_Object'Class then
         Put (File, "<p>Identity");
         Put_Line (File, "</p>");
      elsif Classifier in Separator_Object'Class then
         declare
            Object : Separator_Object'Class renames
                     Separator_Object'Class (Classifier);
         begin
            Put (File, "<p>Separation: ");
            Put (File, Image (Object.Separation));
            Put (File, ", Completeness: ");
            Put (File, Image (Object.Completeness));
            Put_Line (File, ", classifier:</p>");
            Put (File, Ptr (Object.Classifier).all, Parameters);
         end;
      else
         Put (File, "<p>Unsupported classifier: ");
         Put (File, Get_Class (Classifier));
         Put_Line (File, "</p>");
      end if;
   end Put;

   procedure Put
             (  Classifier : Classifier_Object'Class;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Classifier, Parameters);
   end Put;
end Fuzzy.Classifier.HTML;
