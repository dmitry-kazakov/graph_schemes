--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.Text_IO                Luebeck            --
--  Interface                                      Summer, 2002       --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Fuzzy.Lecture.Text_IO;   use Fuzzy.Lecture.Text_IO;
with Fuzzy.Logic;             use Fuzzy.Logic;
with Strings_Edit;            use Strings_Edit;

with Fuzzy.Lecture.Text_IO.Keywords;
with Units;

package Fuzzy.Lecture.Handle.Text_IO is
--
-- Read_Defaults -- File read parameters used by default
--
-- (o)  Allow_Empty_Fields  is  set  to  allow features to be implicitly
--      omitted. This includes specifying a lesser  number  of  features
--      values  than  expected  and  using  empty  fields  separated  by
--      delimiters;
-- (o)  Base is set to decimal integer values;
-- (o)  Blanks  includes  space,  tabulation,  vertical tabulation (VT),
--      line feed (LF), carriage return (CR). The latter two might  slip
--      through system I/O, for example when  a  Windows  file  is  read
--      under Linux.  In  general,  line  ends  are  determined  by  the
--      operating system;
-- (o)  Default  is  set  to  certain  true, i.e. when truth values in a
--      classification  are  omitted  both  are assumed to be 1. If only
--      necessity is omitted it is assumed equal to the possibility;
-- (o)  Default_Example  defines  the  classifications  to  be  positive
--      training examples by default;
-- (o)  Delimiters is an empty set, i.e. features values are separated
--      only by the characters from the set Blanks;
-- (o)  Get_Units is set to false, i.e. explicit unit specifications are
--      not obligatory;
-- (o)  Mode is the Latin-1 character set to be assumed;
-- (o)  Keywords contains the following tokens:
--         --  introduces a comment,
--         <+> specifies a positive training example,
--         <-> does a negative training example,
--         <*> does a full example.
--         * and ? are used to explicitly omit a feature classification
-- (o)  Quote_Units is set to require dimension specifications to appear
--      in []-brackets.
--
   Read_Defaults : constant Read_Parameters :=
      (  Allow_Empty_Fields => True,
         Base               => 10,
         Blanks             => To_Set (' ' & HT & LF & CR & VT),
         Default            => Fuzzy.Logic.Certain_True,
         Default_Example    => Positive_Example,
         Delimiters         => Null_Set,
         Get_Units          => False,
         Mode               => Units.Latin1_Set,
         Keywords           => Keywords.Default,
         Quote_Units        => True
      );
--
-- Write_Defaults -- File write parameters used by default
--
-- (o)  Abs_Small and Rel_Small is set to maximal precision;
-- (o)  Base is set to decimal integer values;
-- (o)  Default  is  set  to  certain  true, i.e. when truth values in a
--      classification  are  omitted  both  are assumed to be 1. If only
--      necessity is omitted it is assumed equal to the possibility;
-- (o)  Delimiter is space;
-- (o)  Mode is to use the Latin-1 character set;
-- (o)  Negative_Example is <->;
-- (o)  Output_Negative is true;
-- (o)  Output_Positive is true;
-- (o)  Positive_Example is <+>;
-- (o)  Put_Plus is  false  to  drop  plus  sign  when  output  positive
--      numbers;
-- (o)  Put_Units is false to drop units when output dimensioned values.
--      Quote_Units, Use_Derived, Use_SI are irrelevant;
-- (o)  Put_Plus is  false  to  drop  plus  sign  when  output  positive
--      numbers;
-- (o)  Skip_Undefined is set to  true  in  order  to  ignore  undefined
--      examples.
--
   Write_Defaults : constant Write_Parameters :=
      (  Abs_Small               =>-MaxSmall,
         Base                    => 10,
         Default                 => Certain_True,
         Delimiter               => " ",
         Delimiter_Length        => 1,
         Mode                    => Units.Latin1_Set,
         Negative_Example        => "<->",
         Negative_Example_Length => 3,
         Output_Positive         => True,
         Output_Negative         => True,
         Positive_Example        => "<+>",
         Positive_Example_Length => 3,
         Put_Plus                => False,
         Put_Units               => False,
         Quote_Units             => False,
         Rel_Small               => MaxSmall,
         Skip_Undefined          => True,
         Use_Derived             => True,
         Use_SI                  => False
      );
--
-- Read -- Read a training set from a text file
--
--    File_Name  - The name of the file to read from
--    Lesson     - The training set
--    Features   - The list of features
--    Parameters - The parameters controlling features input
--    Viewer     - A progress indication object or a handle to
--
-- These procedures are  used to read training  examples  from the  file
-- specified by its name (the parameter File_Name). The file is  opened,
-- read and closed by these procedures. The examples  successfully  read
-- from the file are put into the training  set  Lesson.  Each  training
-- example  occupies up to two lines of the file. A line consists of the
-- classifications  of features from the list specified by the parameter
-- Features. The order of the features is defined by the list. They  are
-- separated  by  the  characters  from  the  set Parameters.Blanks. The
-- syntax  of  a  classification  depends  on   the   feature.   Missing
-- classification  can  be  specified  explicitly if Parameters.Keywords
-- contains  Undefined_Example  tokens.  It  also   can   be   specified
-- implicitly    by    omitting     feature     specifications.     When
-- Parameters.Delimiters  is not empty then features specifications must
-- be separated  by  one  character  from  that  set  (and  possibly  an
-- arbitrary   number    of    characters    from    Parameters.Blanks).
-- Parameters.Allow_Empty_Fields     additionally     allows     feature
-- specifications to be considered missing when  one  delimiter  follows
-- another,  possibly  separated by Parameters.Blanks. Lines of the file
-- may contain comments starting with a token Comment and continuing  to
-- the   end   of   the   line.  A  line  may  start  with  one  of  the
-- {Positive|Negative|Full}_Example  keyword   indicating   whether   it
-- specifies  a  positive,  a negative part of the example or both. When
-- nothing specified  Parameters.Default_Example  is  assumed.  If  both
-- parts specified the positive part has to precede  the  negative  one.
-- Otherwise, they are assumed to be of different (consequent) examples.
-- The fields of Parameters determine the  way  features classifications
-- are input as described in Fuzzy.Features.Handle.Edit.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - Operation was aborted
--    I/O exceptions
--
   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : Indicator_Handle
             );
   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : Indicator_Handle
             );
   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Read -- Read a training set from a text file
--
--    File_Name  - The name of the file to read from
--    Features   - The list of features
--    Parameters - The parameters controlling features input
--    Viewer     - A progress indication object or a handle to
--
-- These functions return a training set read from the file specified by
-- its name (the parameter File_Name). The syntax of the file is same as
-- in the procedure Read. Viewer can be an invalid handle.
--
-- Returns :
--
--    Handle to the training set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - Operation was aborted
--    I/O exceptions
--
   function Read
            (  File_Name  : String;
               Features   : Bounded_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : Indicator_Handle
            )  return Lecture_Handle;
   function Read
            (  File_Name  : String;
               Features   : Feature_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : Indicator_Handle
            )  return Lecture_Handle;
   function Read
            (  File_Name  : String;
               Features   : Bounded_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access
            )  return Lecture_Handle;
   function Read
            (  File_Name  : String;
               Features   : Feature_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access
            )  return Lecture_Handle;
--
-- Write -- Write a training set into a text file
--
--    File_Name  - The name of the file to write into
--    Lesson     - The training set
--    Features   - The list of features
--    From       - The first example to output
--    To         - The last example to output
--    Parameters - The parameters controlling features output
--    Viewer     - A progress indication object
--
-- These procedures are used to write training examples  of  a  training
-- set  into a file specified by its name (the parameter File_Name). The
-- file is created, for  write  and  closed  by  these  procedures.  The
-- examples  are  written per line. Each training example occupies up to
-- two  lines  of  the  file.  A line consists of the classifications of
-- features from the list specified by the parameter Features. The order
-- of the features is  defined  by  the  list.  They  are  separated  by
-- Parameters.Delimiter.  The  syntax of a classification depends on the
-- feature. Each line is  prefixed  by  Parameters.Positive_Example  and
-- Parameters.Negative_Example  depending  on  whether  the  example  is
-- positive  or  negative.  The  fields  of Parameters determine the way
-- features    classifications    are    output    as    described    in
-- Fuzzy.Feature.Handle.Edit.  The parameters  From..To define the range
-- of examples to output. Either may exceed the last example number.
--
-- Exceptions :
--
--    End_Error - Writing was aborted by indicator object
--    I/O exceptions
--
   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Bounded_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : Indicator_Handle
             );
   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Feature_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : Indicator_Handle
             );
   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Bounded_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Feature_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
private
   pragma Inline (Read);
   pragma Inline (Write);

end Fuzzy.Lecture.Handle.Text_IO;
