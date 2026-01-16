--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Text_IO                       Luebeck            --
--  Interface                                      Summer, 2002       --
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

with Ada.Strings.Maps;  use Ada.Strings.Maps;

with Tables;

package Fuzzy.Lecture.Text_IO is
--
-- Keyword -- Types of keywords recognized in a file
--
-- (o)  Comment  is  a  combination of characters that starts a comment.
--      The comment continues to the end of the current line;
-- (o)  Positive_Example is a chain  of  characters  that  introduces  a
--      positive training example. The feature values of the example are
--      the classifications of the event behind  the  example.  I.e.  it
--      defines the has_in and has_not images;
-- (o)  Negative_Example is a chain  of  characters  that  introduces  a
--      negative training example. The feature values of the example are
--      the  classifications  of the event's complement. I.e. it defines
--      the has_out and has_not_out;
-- (o)  Full_Example is a chain of characters that introduces an example
--      that  is both positive and negative, i.e. each classification of
--      the  example  defines  has_in=has_out  and   has_not=has_not_out
--      images;
-- (o)  Undefined_Example  is  a  chain  of  characters  that denotes an
--      explicitly omitted feature classification.
--
   type Keyword is
        (  Comment,
           Positive_Example,
           Negative_Example,
           Full_Example,
           Undefined_Example
        );
   subtype Example_Type is Keyword range Positive_Example..Full_Example;
--
-- File_Tokens -- The package Table instantiation with Keyword
--
   package File_Tokens is new Tables (Keyword);
   use File_Tokens;
--
-- Read_Parameters -- The text input parameters
--
-- (o)  Allow_Empty_Fields if  true  allows  implicit  omitting  feature
--      specifications.   For   example  in:  1;;3  the  second  feature
--      specification is  omitted,  provided  that  ";"  is  used  as  a
--      delimiter;
-- (o)  Blanks  is  the set  of  characters  that  can separate features
--      values. Usually it contains space, tabulation etc;
-- (o)  Default_Example is example type  assumed  by  default,  when  no
--      keyword appears in the line beginning;
-- (o)  Delimiters is the set of characters that must  separate  values.
--      If this set is not empty each two values have  to  separated  by
--      exactly one  character  from  this  set.  Each  extra  delimiter
--      indicates   an  implicitly  omitted  feature  specifications  if
--      Allow_Empty_Fields is true. Usually  characters  like  semicolon
--      are  used as delimiters. Delimiters are not used when Delimiters
--      is empty;
-- (o)  Keywords is the table of the keywords. Each token in  the  table
--      is associated with a value of the type Keyword. It is allowed to
--      have more than one chain of characters to be associated with one
--      type of keywords.
--
   type Read_Parameters is new Input_Parameters with record
      Blanks             : Character_Set;
      Delimiters         : Character_Set;
      Default_Example    : Example_Type := Positive_Example;
      Allow_Empty_Fields : Boolean      := True;
      Keywords           : Table;
   end record;
--
-- Write_Parameters -- The text output parameters
--
-- (o)  Delimiter_Length is the length of the delimiters between feature
--      values. A feature value is a fuzzy classification of the example
--      or of its complement;
-- (o)  Delimiter is one to place betwen the values of features;
-- (o)  Output_Negative is true when negative examples are to output;
-- (o)  Output_Positive is true when positive examples are to output;
-- (o)  Positive_Example_Length  is  length  of  the prefix of the lines
--      containing positive examples;
-- (o)  Positive_Example  is  the  prefix  of  the  lines   representing
--      positive examples;
-- (o)  Negative_Example_Length  is  length  of  the prefix of the lines
--      containing negative examples;
-- (o)  Negative_Example  is  the  prefix  of  the  lines   representing
--      negative examples;
-- (o)  Skip_Undefined instructs to ignore example that are undefined.
--
   type Write_Parameters
        (  Delimiter_Length        : Positive;
           Positive_Example_Length : Natural;
           Negative_Example_Length : Natural
        )  is new Output_Parameters with
   record
      Skip_Undefined   : Boolean := True;
      Output_Positive  : Boolean := True;
      Output_Negative  : Boolean := True;
      Delimiter        : String (1..Delimiter_Length);
      Positive_Example : String (1..Positive_Example_Length);
      Negative_Example : String (1..Negative_Example_Length);
   end record;
--
-- Read -- Read a training set from a text file
--
--    File_Name  - The name of the file to read from
--    Lesson     - The training set
--    Features   - The list of features
--    Parameters - The parameters controlling features input
--    Viewer     - A progress indication object
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
                Lesson     : in out Lecture_Object'Class;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );
   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Object'Class;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );
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
                Lesson     : Lecture_Object'Class;
                Features   : Bounded_Array;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );
   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Object'Class;
                Features   : Feature_Array;
                From       : Positive;
                To         : Positive;
                Parameters : Write_Parameters'Class;
                Viewer     : not null access Indicator_Object'Class
             );
end Fuzzy.Lecture.Text_IO;
