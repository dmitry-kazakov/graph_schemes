--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Separator                  Luebeck            --
--  Interface                                      Winter, 2005       --
--                                                                    --
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

with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;

package Fuzzy.Classifier.Separator is
   pragma Elaborate_Body (Fuzzy.Classifier.Separator);

   Class : constant String := Classifier_Class & "Separator";
--
-- Create -- Create a separator classifier
--
--    Classifier   - A handle to a classifier
--    Separation   - The separation level of classes
--    Completeness - The level of classes' completenes
--
-- This  function  creates  a  classifier  that separates classes. It is
-- built upon another classifier specified by the parameter  Classifier.
-- A separator classifier uses Classifier to classify classes  known  to
-- be separate and complete to the levels specified  to  the  parameters
-- Separation   and   Completeness.  The  parameter  Separation  is  the
-- confidence level of class separation. That is the possibility of  the
-- intersection of  the  classes  or  its  estimation  from  above.  The
-- parameter Completeness is the confidence level of class completeness,
-- i.e. the necessity of class union or its estimation from  below.  The
-- built classifier uses the  knowledge  about  classes  to  refine  the
-- classifications given by the original classifier.
--
-- Returns :
--
--    Pointer to the created object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create
            (  Classifier   : Classifier_Handle;
               Separation   : Confidence := Confidence'First;
               Completeness : Confidence := Confidence'Last
            )  return Classifier_Handle;
--
-- Get_Classifier
--
--    Classifier - A handle to a classifier
--
-- Returns :
--
--    A handle to the original classifier
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, not a separator
--
   function Get_Classifier (Classifier : Classifier_Handle)
      return Classifier_Handle;
--
-- Is_Separator -- Check for separator classifier
--
--    Classifier - A handle to a classifier
--
-- Returns :
--
--    True if Classifier was created using Create
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Is_Separator (Classifier : Classifier_Handle)
      return Boolean;
--
-- Separator_Object -- A separating classifier
--
   type Separator_Object is new Classifier_Object with record
      Classifier   : Classifier_Handle;
      Separation   : Confidence;
      Completeness : Confidence;
   end record;

private
   pragma Inline (Get_Classifier);
   pragma Inline (Is_Separator);
--
-- Classify -- Overrides Fuzzy.Classifier...
--
   overriding
   function Classify
            (  Classifier : Separator_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification;
--
-- Estimate -- Overrides Fuzzy.Classifier...
--
   overriding
   function Estimate
            (  Classifier : Separator_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Intuitionistic.Set;
--
-- Get_Class -- Overrides Object.Arhived...
--
   overriding
   function Get_Class (Classifier : Separator_Object) return String;
--
-- Get_Classes -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Classes (Classifier : Separator_Object)
      return Feature_Handle;
--
-- Get_Examples -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Get_Examples
             (  Classifier : Separator_Object;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Get_Features --Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Features (Classifier : Separator_Object)
      return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Classifier : Separator_Object;
                List       : in out Deposit_Container'Class
             );
--
-- Get_Training_Set_From -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_From (Classifier : Separator_Object)
      return Positive;
--
-- Get_Training_Set_Length -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Length (Classifier : Separator_Object)
      return Natural;
--
-- Get_Training_Set_Name -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Name (Classifier : Separator_Object)
      return String;
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Classifier : Separator_Object) return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Classifier : in out Separator_Object);
--
-- Learn -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Learn
             (  Classifier : in out Separator_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Classifier  : Separator_Object
             );

   pragma Inline (Get_Class);
   pragma Inline (Get_Classes);
   pragma Inline (Get_Examples);
   pragma Inline (Get_Features);
   pragma Inline (Get_Training_Set_Name);
   pragma Inline (Is_Modified);
   pragma Inline (Reset_Modified);
end Fuzzy.Classifier.Separator;
