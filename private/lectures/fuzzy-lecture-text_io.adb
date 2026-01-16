--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Text_IO                       Luebeck            --
--  Implementation                                 Summer, 2002       --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Fuzzy.Feature.Edit;    use Fuzzy.Feature.Edit;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Strings_Edit;          use Strings_Edit;

package body Fuzzy.Lecture.Text_IO is

   Increment : constant := 1024;

   function Location
            (  File_Name : String;
               Line_No   : Integer;
               Pointer   : Integer
            )  return String is
   begin
      return
      (  "[File: " & File_Name
      &  " Ln:" & Integer'Image (Line_No)
      &  " Col:" & Integer'Image (Pointer)
      &  "]"
      );
   end Location;
--
-- Read_Line -- One line of the file
--
--    File   - To read
--    Line   - A pointer to
--    Length - Of the line
--
-- The procedure reallocates Line as necessary. Upon end of file Line is
-- freed.
--
   procedure Read_Line
             (  File   : in out File_Type;
                Line   : in out String_Ptr;
                Length : out Natural
             )  is
      Size : Natural;
   begin
      Get_Line (File, Line.all, Size);
      Length := Size;
      while Size = Line'Last loop
         declare
            Old_Line : String_Ptr := Line;
         begin
            Line := new String (1..Old_Line'Length + Increment);
            Line (1..Old_Line'Length) := Old_Line.all;
            Free (Old_Line);
         end;
         Get_Line (File, Line (Length + 1..Line'Last), Size);
         Length := Length + Size;
      end loop;
   exception
      when End_Error =>
         Free (Line);
         return;
   end Read_Line;

   generic
      type Array_Type (<>) is private;
      with function Get (Container : Array_Type; Index : Integer)
         return Feature_Object_Ptr is <>;
      with function First (Container : Array_Type)
         return Integer is <>;
      with function Last (Container : Array_Type)
         return Integer is <>;
   procedure Read_From_File
             (  File_Name  : String;
                Lesson     : in out Lecture_Object'Class;
                Features   : Array_Type;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );

   procedure Read_From_File
             (  File_Name  : String;
                Lesson     : in out Lecture_Object'Class;
                Features   : Array_Type;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  is
      File    : aliased File_Type;
      Feature : Feature_Object_Ptr;
      Line    : String_Ptr := new String (1..Increment);
      Line_No : Natural    := 0;
      Example : Natural    := 0;
      Mode    : Keyword    := Comment;
      Length  : Natural;

      procedure Read (Row : String; Pointer : in out Integer) is
         pragma Inline (Read);
         Value : Classification (Feature.Cardinality);
      begin
         Get (Row, Pointer, Feature.all, Value, Parameters);
         if Mode /= Positive_Example then
            Put
            (  Lesson,
               Example,
               Feature.all,
               Has_Out,
               Value.Possibility
            );
            Put
            (  Lesson,
               Example,
               Feature.all,
               Has_Not_Out,
               not Value.Necessity
            );
         end if;
         if Mode /= Negative_Example then
            Put
            (  Lesson,
               Example,
               Feature.all,
               Has_In,
               Value.Possibility
            );
            Put
            (  Lesson,
               Example,
               Feature.all,
               Has_Not,
               not Value.Necessity
            );
         end if;
      exception
         when Constraint_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Range error in "
               &  Get_Name (Feature.all)
               &  " "
               &  Location (File_Name, Line_No, Pointer)
            )  );
         when End_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Missing "
               &  Get_Name (Feature.all)
               &  " "
               &  Location (File_Name, Line_No, Pointer)
            )  );
         when Data_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               (  "Syntax error in "
               &  Get_Name (Feature.all)
               &  " "
               &  Location (File_Name, Line_No, Pointer)
            )  );
      end Read;
   begin
      Open (File, In_File, File_Name);
      Reset (Viewer.all);
      loop
         Read_Line (File, Line, Length);
         exit when Line = null;
         Line_No := Line_No + 1;
         declare
            Row     : String renames Line (1..Length);
            Empty   : Boolean := True;
            Pointer : Integer := Row'First;
         begin
            --
            -- Reading  the  example type and determining if it is a new
            -- example, or a second part of the previous one.
            --
            Get (Row, Pointer, Parameters.Blanks);
            declare
               Offset : Natural;
               Start  : constant Integer := Pointer;
            begin
               Locate (Row, Pointer, Parameters.Keywords, Offset);
               if Offset = 0 then
                  Example := Example + 1;
                  Mode    := Parameters.Default_Example;
               else
                  case GetTag (Parameters.Keywords, Offset) is
                     when Comment =>
                        Pointer := Row'Last + 1;
                     when Positive_Example =>
                        Example := Example + 1;
                        Mode    := Positive_Example;
                     when Negative_Example =>
                        if Mode /= Positive_Example then
                           Example := Example + 1;
                        end if;
                        Mode := Negative_Example;
                     when Full_Example =>
                        Example := Example + 1;
                        Mode    := Full_Example;
                     when Undefined_Example =>
                        Pointer := Start;
                        Example := Example + 1;
                        Mode    := Parameters.Default_Example;
                  end case;
               end if;
            end;
            if Pointer <= Row'Last then -- Not a comment or empty line
               --
               -- Reading the features
               --
               for Index in First (Features)..Last (Features) loop
                  Get (Row, Pointer, Parameters.Blanks);
                  Feature := Get (Features, Index);
                  if Feature /= null then
                     if Pointer > Row'Last then
                        if not Parameters.Allow_Empty_Fields then
                           Raise_Exception
                           (  Data_Error'Identity,
                              (  "Missing "
                              &  Get_Name (Feature.all)
                              &  " "
                              &  Location (File_Name, Line_No, Pointer)
                           )  );
                        end if;
                        exit;
                     end if;
                     --
                     -- Reading a  feature  from  the  list.  First  the
                     -- delimiter is recognized if there was  a  feature
                     -- value before this.
                     --
                     if (  not Empty
                        and then
                           Parameters.Delimiters /= Null_Set
                        )
                     then
                        if (  Pointer > Row'Last
                           or else
                              not Is_In
                                  (  Row (Pointer),
                                     Parameters.Delimiters
                           )      )
                        then
                           Raise_Exception
                           (  Data_Error'Identity,
                              (  "Features delimiter expected "
                              &  Location (File_Name, Line_No, Pointer)
                           )  );
                        end if;
                        Pointer := Pointer + 1;
                        Get (Row, Pointer, Parameters.Blanks);
                     end if;
                     --
                     -- Recognizing an implicitly omitted feature values
                     -- for the case when a next delimiter follows.
                     --
                     if (  Pointer <= Row'Last
                        and then
                           Is_In
                           (  Row (Pointer),
                              Parameters.Delimiters
                        )  )
                     then
                        --
                        -- A delimiter detected
                        --
                        if not Parameters.Allow_Empty_Fields then
                           Raise_Exception
                           (  Data_Error'Identity,
                              (  "Missing "
                              &  Get_Name (Feature.all)
                              &  " "
                              &  Location (File_Name, Line_No, Pointer)
                           )  );
                        end if;
                     else
                        --
                        -- Checking for a comment, an explicitly omitted
                        -- value.  If  none  then  reading  the  current
                        -- feature
                        --
                        declare
                           Offset : Natural;
                           Start  : constant Integer := Pointer;
                        begin
                           Locate
                           (  Row,
                              Pointer,
                              Parameters.Keywords,
                              Offset
                           );
                           if Offset = 0 then
                              Read (Row, Pointer);
                              Empty := False;
                           else
                              case GetTag
                                   (  Parameters.Keywords,
                                      Offset
                                   )  is
                                 when Comment =>
                                    Pointer := Row'Last + 1;
                                 when Example_Type =>
                                    Pointer := Start;
                                    Read (Row, Pointer);
                                    Empty := False;
                                 when Undefined_Example =>
                                    Empty := False;
                              end case;
                           end if;
                        end;
                     end if;
                  end if;
               end loop;
               --
               -- Finishing reading the line
               --
               Get (Row, Pointer, Parameters.Blanks);
               if (  not Empty
                  and then
                     Pointer <= Row'Last
                  and then
                     not Is_In (Row (Pointer), Parameters.Delimiters)
                  )
               then -- Skip trailing delimiter
                  Pointer := Pointer + 1;
                  Get (Row, Pointer, Parameters.Blanks);
                  declare
                     Offset : Natural;
                     Start  : constant Integer := Pointer;
                  begin
                     Locate (Row, Pointer, Parameters.Keywords, Offset);
                     if Offset /= 0 then
                        case GetTag (Parameters.Keywords, Offset) is
                           when Comment =>
                              Pointer := Row'Last + 1;
                           when Example_Type | Undefined_Example =>
                              Pointer := Start;
                        end case;
                     end if;
                  end;
               end if;
               if Pointer <= Row'Last then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Unrecognized "
                     &  Location (File_Name, Line_No, Pointer)
                  )  );
               end if;
            end if;
         end;
         Check (Viewer.all);
      end loop;
      Done (Viewer.all);
      Free (Line);
      Close (File);
   exception
      when others =>
         Free (Line);
         Close (File);
         Done (Viewer.all);
         raise;
   end Read_From_File;

   generic
      type Array_Type (<>) is private;
      with function Get (Container : Array_Type; Index : Integer)
         return Feature_Object_Ptr is <>;
      with function First (Container : Array_Type)
         return Integer is <>;
      with function Last (Container : Array_Type)
         return Integer is <>;
   procedure Write_To_File
             (  File_Name  : String;
                Lesson     : Lecture_Object'Class;
                Features   : Array_Type;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );

   procedure Write_To_File
             (  File_Name  : String;
                Lesson     : Lecture_Object'Class;
                Features   : Array_Type;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  is
      File : aliased File_Type;
      Line : String_Ptr := new String (1..Increment);
      Stop : constant Natural :=
                Natural'Min (To, Get_Examples_Number (Lesson));
      Pointer : Integer;

      procedure Expand is
         Old_Line : String_Ptr := Line;
      begin
         Line := new String (1..Line'Length + Increment);
         Line (1..Pointer - 1) := Old_Line (1..Pointer - 1);
         Free (Old_Line);
      end Expand;

      procedure Put
                (  Feature : Feature_Object'Class;
                   Possibility, Necessity : Set
                )  is
         Value : constant Classification :=
                          (  Cardinality => Possibility'Length,
                             Possibility => Possibility,
                             Necessity   => Necessity
                          );
      begin
         loop
            begin
               Put
               (  Destination => Line.all,
                  Pointer     => Pointer,
                  Feature     => Feature,
                  Value       => Value,
                  Parameters  => Parameters
               );
               return;
            exception
               when Layout_Error =>
                  Expand;
            end;
         end loop;
      end Put;

      procedure Put (Text : String) is
      begin
         loop
            begin
               Put (Line.all, Pointer, Text);
               return;
            exception
               when Layout_Error =>
                  Expand;
            end;
         end loop;
      end Put;

      function Is_Defined
               (  Example  : Positive;
                  Pos, Nec : Image_Type
               )  return Boolean is
         Feature : Feature_Object_Ptr;
      begin
         if Parameters.Skip_Undefined then
            for Index in First (Features)..Last (Features) loop
               Feature := Get (Features, Index);
               if Feature /= null then
                  if (  Is_Known (Lesson, Example, Feature.all, Pos)
                     or else
                        Is_Known (Lesson, Example, Feature.all, Nec)
                     )
                  then
                     return True;
                  end if;
               end if;
            end loop;
            return False;
         else
            return True;
         end if;
      end Is_Defined;

      Feature  : Feature_Object_Ptr;
      Leftmost : Boolean;
   begin
      Create (File, Out_File, File_Name);
      Reset (Viewer.all, Integer'Max (0, Stop - From + 1));
      for Example in From..Stop loop
         -- Positive examples, when required and defined
         if (  Parameters.Output_Positive
            and then
               Is_Defined (Example, Has_In, Has_Not)
            )
         then
            Pointer  := 1;
            Leftmost := True;
            Put (Parameters.Positive_Example);
            for Index in First (Features)..Last (Features) loop
               Feature := Get (Features, Index);
               if Feature /= null then
                  if Leftmost then
                     Leftmost := False;
                  else
                     Put (Parameters.Delimiter);
                  end if;
                  Put
                  (  Feature.all,
                     Get (Lesson, Example, Feature.all, Has_In),
                     not Get (Lesson, Example, Feature.all, Has_Not)
                  );
               end if;
            end loop;
            Put_Line (File, Line (1..Pointer - 1));
         end if;
         -- Negative examples, when required and defined
         if (  Parameters.Output_Negative
            and then
               Is_Defined (Example, Has_Out, Has_Not_Out)
            )
         then
            Pointer  := 1;
            Leftmost := True;
            Put (Parameters.Positive_Example);
            for Index in First (Features)..Last (Features) loop
               Feature := Get (Features, Index);
               if Feature /= null then
                  if Leftmost then
                     Leftmost := False;
                  else
                     Put (Parameters.Delimiter);
                  end if;
                  Put
                  (  Feature.all,
                     Get (Lesson, Example, Feature.all, Has_Out),
                     not Get (Lesson, Example, Feature.all, Has_Not_Out)
                  );
               end if;
            end loop;
            Put_Line (File, Line (1..Pointer - 1));
         end if;
         Check (Viewer.all);
      end loop;
      Free (Line);
      Done (Viewer.all);
      Close (File);
   exception
      when End_Error =>
         Free (Line);
         Done (Viewer.all);
         Delete (File);
         raise;
      when others =>
         Free (Line);
         Done (Viewer.all);
         Delete (File);
         raise;
   end Write_To_File;

   function Get (Container : Feature_Array; Index : Integer)
      return Feature_Object_Ptr;
   pragma Inline (Get);

   function First (Container : Bounded_Array) return Integer;
   function First (Container : Feature_Array) return Integer;
   pragma Inline (First);

   function Last (Container : Bounded_Array) return Integer;
   function Last (Container : Feature_Array) return Integer;
   pragma Inline (Last);

   function Get (Container : Feature_Array; Index : Integer)
      return Feature_Object_Ptr is
   begin
      return Ptr (Container (Index));
   end Get;

   function First (Container : Bounded_Array) return Integer is
   begin
      return Container.First;
   end First;

   function First (Container : Feature_Array) return Integer is
   begin
      return Container'First;
   end First;

   function Last (Container : Bounded_Array) return Integer is
   begin
      return Container.Last;
   end Last;

   function Last (Container : Feature_Array) return Integer is
   begin
      return Container'Last;
   end Last;

   procedure Read_With_Bounded_Array is
      new Read_From_File (Bounded_Array);

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Object'Class;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  renames Read_With_Bounded_Array;

   procedure Read_With_Feature_Array is
      new Read_From_File (Feature_Array);

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Object'Class;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  renames Read_With_Feature_Array;

   procedure Write_With_Bounded_Array is
      new Write_To_File (Bounded_Array);

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Object'Class;
                Features   : Bounded_Array;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  renames Write_With_Bounded_Array;

   procedure Write_With_Feature_Array is
      new Write_To_File (Feature_Array);

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Object'Class;
                Features   : Feature_Array;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             )  renames Write_With_Feature_Array;

end Fuzzy.Lecture.Text_IO;
