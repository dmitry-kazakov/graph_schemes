--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.HTML                          Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Fuzzy.Feature.Edit;  use Fuzzy.Feature.Edit;
with Integer_Intervals;   use Integer_Intervals;
with Units;               use Units;

package body Fuzzy.Feature.HTML is
   Resolution : constant := 50;

   function To_Integer (Value : Confidence; Max : Positive)
      return Natural is
   begin
      return
         Natural'Min
         (  Natural (Float (Value) * Float (Max + 1)),
            Max
         );
   end To_Integer;

   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Set;
                Parameters : HTML_Parameters'Class
             )  is
      Limit   : constant String :=
                   Color_Beg (Parameters.Limits) & '|' & Color_End;
      On_Beg  : constant String :=
                   Color_Beg (Parameters.Possibility_Bar);
      Off_Beg : constant String := Color_Beg (Parameters.Background);
      Point   : Positive := 1;
      Length  : Natural;
   begin
      if Value'Length /= Feature.Cardinality then
         raise Constraint_Error;
      end if;
      Put_Line (File, Table_Beg);
      for Index in Value'Range loop
         Put_Line (File, Row_Beg);
            --
            -- Domain point in the first column
            --
            Put (File, Cell_Beg);
               Put (File, Align_Right_Beg);
               case Parameters.Mode is
                  when ASCII_Set | Latin1_Set =>
                     Put_Latin1
                     (  File,
                        Image
                        (  Feature,
                           Interval'(Point, Point),
                           Parameters
                     )  );
                  when UTF8_Set =>
                     Put_UTF8
                     (  File,
                        Image
                        (  Feature,
                           Interval'(Point, Point),
                           Parameters
                        ),
                        Parameters.UTF8_Error
                     );
               end case;
               Put (File, Align_End);
            Put_Line (File, Cell_End);
            --
            -- Confidence in the next column
            --
            Put (File, Cell_Beg);
               Put (File, Space);
               Put (File, Limit);
               Length := To_Integer (Value (Index), Resolution);
               if 0 /= Length then
                  Put (File, On_Beg);
                  Put (File, Length * '|');
                  Put (File, Color_End);
               end if;
               Length := Resolution - Length;
               if 0 /= Length then
                  Put (File, Off_Beg);
                  Put (File, Length * '|');
                  Put (File, Color_End);
               end if;
               Put (File, Limit);
            Put_Line (File, Cell_End);
         Put_Line (File, Row_End);
         Point := Point + 1;
      end loop;
      Put_Line (File, Table_End);
   end Put; 

   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Set;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Feature, Value, Parameters);
   end Put;

   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class
             )  is
      Limit   : constant String :=
                   Color_Beg (Parameters.Limits) & '|' & Color_End;
      P_Beg   : constant String :=
                   Color_Beg (Parameters.Possibility_Bar);
      N_Beg   : constant String :=
                   Color_Beg (Parameters.Necessity_Bar);
      Err_Beg : constant String :=
                   Color_Beg (Parameters.Conflicting_Bar);
      Off_Beg : constant String := Color_Beg (Parameters.Background);
      Nec_Len : Integer;
      Pos_Len : Integer;

      procedure Put_Two (L1, L2 : Integer; C1, C2 : String) is
      begin
         if L1 > 0 then
            Put (File, C1);
            if L1 > 1 and then L2 = 1 then
               Put (File, (L1 - 1) * '|');
            else
               Put (File, L1 * '|');
            end if;
            Put (File, Color_End);
         end if;
         if (L1 > 1 and then L2 = 1) or else (L1 > 0 and L2 > 1) then
            Put (File, Limit);
         end if;
         if L2 > 0 then
            Put (File, C2);
            if L1 > 0 and then L2 > 1 then
               Put (File, (L2 - 1) * '|');
            else
               Put (File, L2 * '|');
            end if;
            Put (File, Color_End);
         end if;
      end Put_Two;
   begin
      Put_Line (File, Table_Beg);
      for Index in 1..Value.Cardinality loop
         Put_Line (File, Row_Beg);
            --
            -- Domain point in the first column
            --
            Put (File, Cell_Beg);
               Put (File, Align_Right_Beg);
               case Parameters.Mode is
                  when ASCII_Set | Latin1_Set =>
                     Put_Latin1
                     (  File,
                        Image
                        (  Feature,
                           Interval'(Index, Index),
                           Parameters
                     )  );
                  when UTF8_Set =>
                     Put_UTF8
                     (  File,
                        Image
                        (  Feature,
                           Interval'(Index, Index),
                           Parameters
                        ),
                        Parameters.UTF8_Error
                     );
               end case;
               Put (File, Align_End);
            Put_Line (File, Cell_End);
            --
            -- Confidence in the next column
            --
            Put (File, Cell_Beg);
               Put (File, Space);
               Put (File, Limit);
               Nec_Len :=
                  To_Integer (Value.Necessity (Index), Resolution);
               Pos_Len :=
                  To_Integer (Value.Possibility (Index), Resolution);
               if Pos_Len >= Nec_Len then
                  Put_Two (Nec_Len, Pos_Len - Nec_Len, N_Beg, P_Beg);
                  Nec_Len := Resolution - Pos_Len;
               else
                  Put_Two (Pos_Len, Nec_Len - Pos_Len, N_Beg, Err_Beg);
                  Nec_Len := Resolution - Nec_Len;
               end if;
               if Nec_Len > 0 then
                  Put (File, Off_Beg);
                  Put (File, Nec_Len * '|');
                  Put (File, Color_End);
               end if;
               Put (File, Limit);
            Put_Line (File, Cell_End);
         Put_Line (File, Row_End);
      end loop;
      Put_Line (File, Table_End);
   end Put;

   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put
      (  File,
         Feature,
         Intuitionistic.Classification (Value),
         Parameters
      );
   end Put;

   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Feature, Value, Parameters);
   end Put;

   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class
             )  is
   begin
      Put (Standard_Output, Feature, Value, Parameters);
   end Put;

end Fuzzy.Feature.HTML;
