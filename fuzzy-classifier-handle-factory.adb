--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Handle.Factory             Luebeck            --
--  Implementation                                 Spring, 2003       --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Fuzzy.Graph.Learning;  use Fuzzy.Graph.Learning;

package body Fuzzy.Classifier.Handle.Factory is

   function Learn
            (  Context  : not null access Training_Data'Class;
               Features : Feature_Array;
               From     : Positive := 1;
               To       : Positive := Positive'Last
            )  return Classifier_Handle is
   begin
      if Context.all in Graph_Training_Data'Class then
         return
            Fuzzy.Graph.Scheme.Create
            (  Graph_Training_Data'Class (Context.all)'Access,
               Features,
               From,
               To
            );
      else
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unsupported training data"
         );
      end if;
   end Learn;

   function Learn
            (  Lesson      : Lecture_Handle;
               Name        : String;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access Fuzzy.Graph.
                             Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle is
   begin
      if Is_Valid (Viewer) then
         return
            Fuzzy.Graph.Scheme.Create
            (  Lesson      => Lesson,
               Name        => Name,
               Features    => Features,
               Classes     => Classes,
               From        => From,
               To          => To,
               Threshold   => Threshold,
               Equivalence => Equivalence,
               Viewer      => Ptr (Viewer),
               Factory     => Factory
            );
      else
         return
            Fuzzy.Graph.Scheme.Create
            (  Lesson      => Lesson,
               Name        => Name,
               Features    => Features,
               Classes     => Classes,
               From        => From,
               To          => To,
               Threshold   => Threshold,
               Equivalence => Equivalence,
               Factory     => Factory
            );
      end if;
   end Learn;

   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access Fuzzy.Graph.
                             Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle is
   begin
      if Is_Valid (Viewer) then
         return
            Fuzzy.Graph.Scheme.Create
            (  Lesson      => Lesson,
               Features    => Features,
               Classes     => Classes,
               From        => From,
               To          => To,
               Threshold   => Threshold,
               Equivalence => Equivalence,
               Viewer      => Ptr (Viewer),
               Factory     => Factory
            );
      else
         return
            Fuzzy.Graph.Scheme.Create
            (  Lesson      => Lesson,
               Features    => Features,
               Classes     => Classes,
               From        => From,
               To          => To,
               Threshold   => Threshold,
               Equivalence => Equivalence,
               Factory     => Factory
            );
      end if;
   end Learn;

end Fuzzy.Classifier.Handle.Factory;
