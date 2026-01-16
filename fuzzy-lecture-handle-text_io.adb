--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.Text_IO                Luebeck            --
--  Implementation                                 Summer, 2002       --
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

with Fuzzy.Lecture.General;  use Fuzzy.Lecture.General;

package body Fuzzy.Lecture.Handle.Text_IO is

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : Indicator_Handle
             )  is
   begin
      if not Is_Valid (Lesson) then
         Lesson := Create;
      end if;
      if Is_Valid (Viewer) then
         Read
         (  File_Name,
            Ptr (Lesson).all,
            Features,
            Parameters,
            Ptr (Viewer)
         );
      else
         Read
         (  File_Name,
            Ptr (Lesson).all,
            Features,
            Parameters,
            Negleter'Access
         );
      end if;
   end Read;

   function Read
            (  File_Name  : String;
               Features   : Bounded_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : Indicator_Handle
            )  return Lecture_Handle is
      Result : constant Lecture_Handle := Create;
   begin
      if Is_Valid (Viewer) then
         Read
         (  File_Name,
            Ptr (Result).all,
            Features,
            Parameters,
            Ptr (Viewer)
         );
      else
         Read
         (  File_Name,
            Ptr (Result).all,
            Features,
            Parameters,
            Negleter'Access
         );
      end if;
      return Result;
   end Read;

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : Indicator_Handle
             )  is
   begin
      if not Is_Valid (Lesson) then
         Lesson := Create;
      end if;
      if Is_Valid (Viewer) then
         Read
         (  File_Name,
            Ptr (Lesson).all,
            Features,
            Parameters,
            Ptr (Viewer)
         );
      else
         Read
         (  File_Name,
            Ptr (Lesson).all,
            Features,
            Parameters,
            Negleter'Access
         );
      end if;
   end Read;

   function Read
            (  File_Name : String;
               Features  : Feature_Array;
               Parameters : Read_Parameters'Class  := Read_Defaults;
               Viewer     : Indicator_Handle
            )  return Lecture_Handle is
      Result : constant Lecture_Handle := Create;
   begin
      if Is_Valid (Viewer) then
         Read
         (  File_Name,
            Ptr (Result).all,
            Features,
            Parameters,
            Ptr (Viewer)
         );
      else
         Read
         (  File_Name,
            Ptr (Result).all,
            Features,
            Parameters,
            Negleter'Access
         );
      end if;
      return Result;
   end Read;

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Bounded_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access
             )  is
   begin
      if not Is_Valid (Lesson) then
         Lesson := Create;
      end if;
      Read (File_Name, Ptr (Lesson).all, Features, Parameters, Viewer);
   end Read;

   function Read
            (  File_Name  : String;
               Features   : Bounded_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access
            )  return Lecture_Handle is
      Result : constant Lecture_Handle := Create;
   begin
      Read (File_Name, Ptr (Result).all, Features, Parameters, Viewer);
      return Result;
   end Read;

   procedure Read
             (  File_Name  : String;
                Lesson     : in out Lecture_Handle;
                Features   : Feature_Array;
                Parameters : Read_Parameters'Class := Read_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      if not Is_Valid (Lesson) then
         Lesson := Create;
      end if;
      Read (File_Name, Ptr (Lesson).all, Features, Parameters, Viewer);
   end Read;

   function Read
            (  File_Name  : String;
               Features   : Feature_Array;
               Parameters : Read_Parameters'Class := Read_Defaults;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access
            )  return Lecture_Handle is
      Result : constant Lecture_Handle := Create;
   begin
      Read (File_Name, Ptr (Result).all, Features, Parameters, Viewer);
      return Result;
   end Read;

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Bounded_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Write
         (  File_Name  => File_Name,
            Lesson     => Ptr (Lesson).all,
            Features   => Features,
            From       => From,
            To         => To,
            Parameters => Parameters,
            Viewer     => Ptr (Viewer)
         );
      else
         Write
         (  File_Name  => File_Name,
            Lesson     => Ptr (Lesson).all,
            Features   => Features,
            From       => From,
            To         => To,
            Parameters => Parameters,
            Viewer     => Negleter'Access
         );
      end if;
   end Write;

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Feature_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : Indicator_Handle
             )  is
   begin
      if Is_Valid (Viewer) then
         Write
         (  File_Name  => File_Name,
            Lesson     => Ptr (Lesson).all,
            Features   => Features,
            From       => From,
            To         => To,
            Parameters => Parameters,
            Viewer     => Ptr (Viewer)
         );
      else
         Write
         (  File_Name  => File_Name,
            Lesson     => Ptr (Lesson).all,
            Features   => Features,
            From       => From,
            To         => To,
            Parameters => Parameters,
            Viewer     => Negleter'Access
         );
      end if;
   end Write;

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Bounded_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      Write
      (  File_Name  => File_Name,
         Lesson     => Ptr (Lesson).all,
         Features   => Features,
         From       => From,
         To         => To,
         Parameters => Parameters,
         Viewer     => Viewer
      );
   end Write;

   procedure Write
             (  File_Name  : String;
                Lesson     : Lecture_Handle;
                Features   : Feature_Array;
                From       : Positive               := 1;
                To         : Positive               := Positive'Last;
                Parameters : Write_Parameters'Class := Write_Defaults;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is
   begin
      Write
      (  File_Name  => File_Name,
         Lesson     => Ptr (Lesson).all,
         Features   => Features,
         From       => From,
         To         => To,
         Parameters => Parameters,
         Viewer     => Viewer
      );
   end Write;

end Fuzzy.Lecture.Handle.Text_IO;
