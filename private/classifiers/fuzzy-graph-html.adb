--                                                                    --
--  package Fuzzy.Graph.HTML        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
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

with Fuzzy.Feature.Edit;         use Fuzzy.Feature.Edit;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with HTML;                       use HTML;
with Integer_Intervals;          use Integer_Intervals;
with Units;                      use Units;

package body Fuzzy.Graph.HTML is

   procedure Show
             (  File    : File_Type;
                Feature : Feature_Object'Class;
                Node    : Graph_Node'Class;
                Child   : Graph_Node_Ptr;
                Pointer : Positive;
                Skip    : in out Domain_Subset
             )  is
      First : Boolean := True;

      procedure Flush (From, To : Integer) is
      begin
         if First then
            First := False;
         else
            Put (File, ", ");
         end if;
         Put
         (  File,
            Image (Feature, Interval'(From, To), Output_Defaults)
         );
      end Flush;

      From     : Integer := Pointer;
      Range_On : Boolean := True;
   begin   
      for Index in Pointer + 1..Skip'Last loop
         if Get_Child (Node, Index) = Child then
            Skip (Index) := True;
            if not Range_On then
               From := Index;
               Range_On := True;
            end if;
         else
            if Range_On then
               Flush (From, Index - 1);
               Range_On := False;
            end if;
         end if;
      end loop;
      if Range_On then
         Flush (From, Skip'Last);
      end if;
   end Show;

   procedure Show
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Node       : Graph_Node'Class;
                Child      : Graph_Node_Ptr;
                Pointer    : Positive;
                Parameters : HTML_Parameters'Class;
                Value      : in out Classification
             )  is
      Weight : Classification := Value;
   begin   
      Value.Possibility (Pointer) := Confidence'First;
      Value.Necessity   (Pointer) := Confidence'First;
      for Index in Pointer + 1..Value.Cardinality loop
         if Get_Child (Node, Index) = Child then
            Value.Possibility (Index) := Confidence'First;
            Value.Necessity   (Index) := Confidence'First;
         else
            Weight.Possibility (Index) := Confidence'First;
            Weight.Necessity   (Index) := Confidence'First;
         end if;
      end loop;
      Put (File, Feature, Weight, Parameters);
   end Show;

   procedure Put
             (  Node       : Graph_Node'Class;
                File       : File_Type;
                Parameters : HTML_Parameters'Class
             )  is
      Feature : Feature_Object'Class renames Get_Feature (Node).all;
   begin
      case Get_Type (Node) is
         when Tree_Leaf =>
            Put_Line
            (  File,
               (  Align_Center_Beg
               &  Bold_Beg
               &  Get_Name (Feature)
               &  Bold_End
               &  Align_End
            )  );
            Put
            (  File,
               Feature,
               Get_Distribution (Node),
               Parameters
            );
         when Tree_Cluster =>
            Put_Line
            (  File,
               (  "<table cellpadding=""3"" "
            &  "cellspacing=""0"" border=""3"">"
            )  );
            declare
               Value : Classification := Get_Distribution (Node);
               Child : Graph_Node_Ptr;
            begin
               for Index in 1..Value.Cardinality loop
                  if (  Value.Possibility (Index) /= Confidence'First
                     or else
                        Value.Necessity (Index) /= Confidence'First
                     )
                  then
                     Child := Get_Child (Node, Index);
                     if Child /= null then
                        Put_Line (File, Row_Beg);
                        -- Cell 1
                        Put
                        (  File,
                           Cell_Beg & Align_Center_Beg & Bold_Beg
                        );
                        case Parameters.Mode is
                           when ASCII_Set | Latin1_Set =>
                              Put_Latin1 (File, Get_Name (Feature));
                           when UTF8_Set =>
                              Put_UTF8
                              (  File,
	                         Get_Name (Feature),
	                         Parameters.UTF8_Error
                              );
                        end case;
                        Put (File, Bold_End & " in" & Break);
                        Show
                        (  File,
                           Feature,
                           Node,
                           Child,
                           Index,
                           Parameters,
                           Value
                        );
                        Put_Line (File, Align_End & Cell_End);
                        -- Cell 2
                        Put_Line (File, Cell_Beg);
                        Put (Child.all, File, Parameters);
                        Put_Line (File, Cell_End);
                        Put_Line (File, Row_End);
                     end if;
                  end if;
               end loop;
            end;
            Put_Line (File, "</table>");
         when Tree_Branch =>
            Put_Line
            (  File,
               (  "<table cellpadding=""3"" "
               &  "cellspacing=""0"" border=""3"">"
            )  );
            declare
               Skip  : Domain_Subset (1..Node.Cardinality) :=
		              (others => False);
               Child : Graph_Node_Ptr;
            begin
               for Index in Skip'Range loop
                  if not Skip (Index) then
                     Child := Get_Child (Node, Index);
                     if Child /= null then
                        Put_Line (File, Row_Beg);
                        -- Cell 1
                        Put
                        (  File,
                           Cell_Beg & Align_Center_Beg & Bold_Beg
                        );
                        case Parameters.Mode is
                           when ASCII_Set | Latin1_Set =>
                              Put_Latin1 (File, Get_Name (Feature));
                           when UTF8_Set =>
                              Put_UTF8
                              (  File,
	                         Get_Name (Feature),
	                         Parameters.UTF8_Error
                              );
                        end case;
                        Put (File, Bold_End & " in" & Break);
                        Show (File, Feature, Node, Child, Index, Skip);
                        Put_Line (File, Align_End & Cell_End);
                        -- Cell 2
                        Put_Line (File, Cell_Beg);
                        Put (Child.all, File, Parameters);
                        Put_Line (File, Cell_End);
                        Put_Line (File, Row_End);
                     end if;
                  end if;
               end loop;
            end;
            Put_Line (File, "</table>");
      end case;
   end Put;

   procedure Put
             (  File       : File_Type;
                Node       : Graph_Node'Class;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      case Get_Type (Node) is
         when Tree_Leaf =>
            Put_Line
            (  File,
               (  "<table cellpadding=""3"" "
               &  "cellspacing=""0"" border=""3"">"
            )  );
            Put_Line (File, Row_Beg);
            Put_Line (File, Cell_Beg);
            Put (Node, File, Parameters);
            Put_Line (File, Cell_End);
            Put_Line (File, Row_End);
            Put_Line (File, "</table>");
         when Tree_Cluster | Tree_Branch =>
            Put (Node, File, Parameters);
      end case;
   end Put;

   procedure Put
             (  Node       : Graph_Node'Class;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Node, Parameters);
   end Put;

end Fuzzy.Graph.HTML;
