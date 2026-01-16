--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Classificatory.               Luebeck            --
--        Clustering                               Autumn, 2006       --
--  Interface                                                         --
--                                Last revision :  14:48 30 May 2014  --
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

with Generic_Set;

package Fuzzy.Feature.Classificatory.Clustering is
   pragma Elaborate_Body (Fuzzy.Feature.Classificatory.Clustering);
--
-- Cluster_Frequency -- Training set cluster frequency
--
   type Cluster_Frequency is new Float;
   Cluster_Equivalence : constant Cluster_Frequency := 0.1;
--
-- Frequency_To_Value_Maps -- Instantiation of the generic  set  package
--                            to provide maps of frequency to the values
--                            of a feature domain set.
--
   type Frequency_To_Value is record
      Frequency : Cluster_Frequency;
      Value     : Positive;
   end record;
   function "<" (Left, Right : Frequency_To_Value) return Boolean;
   package Frequency_To_Value_Maps is
      new Generic_Set
          (  Object_Type  => Frequency_To_Value,
             Null_Element => (Cluster_Frequency'First, 1)
          );
--
-- Create -- Create a clustering feature
--
--    Name        - Of the feature
--    Classifier  - A handle to classifier
--    Generalize  - Generalization mode
--    Threshold   - For classifications
--    Equivalence - The confidence interval
--
-- This  function  creates  a  clustering  feature  which  will  use   a
-- Classifier for clustering. The domain set of the feature is  the  set
-- of classes of the classifier. The parameter Equivalence specifies the
-- interval in which frequencies are considered equivalent.
--
-- Returns :
--
--    Pointer to the created object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--
   function Create
            (  Name        : String;
               Classifier  : Classifier_Handle;
               Generalize  : Generalization_Mode := Linear;
               Threshold   : Confidence          := Confidence'First;
               Equivalence : Cluster_Frequency   := Cluster_Equivalence
            )  return Feature_Handle;
--
-- Get_Equivalence -- Check handle
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The equivalence interval of frequencies
--
-- Exceptions :
--
--      Constraint_Error - Invalid handle, object or classifier
--
   function Get_Equivalence (Feature : Feature_Handle)
      return Cluster_Frequency;
   function Get_Equivalence (Feature : Feature_Object'Class)
      return Cluster_Frequency;
   pragma Inline (Get_Equivalence);
--
-- Is_Clustering -- Check handle
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature refers a valid classificatory feature
--
   function Is_Clustering (Feature : Feature_Handle) return Boolean;
   function Is_Clustering (Feature : Feature_Object'Class)
      return Boolean;
   pragma Inline (Is_Clustering);

private
--
-- Clustering_Feature_Object -- The classificatory feature type
--
   type Clustering_Feature_Object is
      new Classificatory_Feature_Object with
   record
      Equivalence : Cluster_Frequency := 0.0;
   end record;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Clustering_Feature_Object)
      return String;
   pragma Inline (Get_Class);
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             );
--
-- Clustering_Class -- The class name
--
   Clustering_Class : constant String :=
      Classificatory_Class & "Classificatory.Clustering";

end Fuzzy.Feature.Classificatory.Clustering;
