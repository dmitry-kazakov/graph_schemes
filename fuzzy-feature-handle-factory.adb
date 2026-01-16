--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Factory                Luebeck            --
--  Implementation                                 Spring, 2002       --
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

with Fuzzy.Feature.Binary.Mutually_Independent;
with Fuzzy.Feature.Discrete;

package body Fuzzy.Feature.Handle.Factory is

   function Create_Binary (Feature : Feature_Handle)
      return Bounded_Array
         renames Fuzzy.Feature.Binary.Create;

   function Create_Independent_Binary (Feature : Feature_Handle)
      return Bounded_Array
         renames Fuzzy.Feature.Binary.Mutually_Independent.Create;

   function Create_Binary (Feature : Feature_Handle)
      return Feature_Array is
   begin
      return To_Feature_Array (Create_Binary (Feature));
   end Create_Binary;

   function Create_Independent_Binary (Feature : Feature_Handle)
      return Feature_Array is
   begin
      return To_Feature_Array (Create_Independent_Binary (Feature));
   end Create_Independent_Binary;

   function Create_Binary
            (  Name    : String;
               Feature : Feature_Handle
            )  return Feature_Handle
      renames Fuzzy.Feature.Binary.Create;

   function Create_Binary
            (  Name         : String;
               Feature      : Feature_Handle;
               Bit_Position : Natural
            )  return Feature_Handle
      renames Fuzzy.Feature.Binary.Create;

   function Create_Classificatory
            (  Name       : String;
               Classifier : Classifier_Handle;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Feature_Handle
      renames Fuzzy.Feature.Classificatory.Create;

   function Create_Clustering
            (  Name        : String;
               Classifier  : Classifier_Handle;
               Generalize  : Generalization_Mode := Linear;
               Threshold   : Confidence          := Confidence'First;
               Equivalence : Cluster_Frequency   := Cluster_Equivalence
            )  return Feature_Handle
      renames Fuzzy.Feature.Classificatory.Clustering.Create;

   function Create_Discrete (Name : String; Domain : String)
      return Feature_Handle renames Fuzzy.Feature.Discrete.Create;

   function Create_Discrete
            (  Name   : String;
               Domain : Domain_Description'Class
            )  return Feature_Handle
      renames Fuzzy.Feature.Discrete.Create;

   function Create_Independent_Binary
            (  Name    : String;
               Feature : Feature_Handle
            )  return Feature_Handle
      renames Fuzzy.Feature.Binary.Mutually_Independent.Create;

   function Create_Independent_Binary
            (  Name         : String;
               Feature      : Feature_Handle;
               Bit_Position : Natural
            )  return Feature_Handle
      renames Fuzzy.Feature.Binary.Mutually_Independent.Create;

end Fuzzy.Feature.Handle.Factory;
