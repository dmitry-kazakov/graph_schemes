--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Identity                   Luebeck            --
--  Interface                                      Autumn, 2006       --
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
--
--  The  package  defines  the  type  Identity_Classifier_Object   which
--  represents  a trivial classifier which just translates the values of
--  the class-feature into classifications.
--
with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;

package Fuzzy.Classifier.Identity is
   pragma Elaborate_Body (Fuzzy.Classifier.Identity);
--
-- Identity_Classifier_Object -- A trivial classifier
--
   type Identity_Classifier_Object is new Classifier_Object with record
      Classes : Feature_Handle;
   end record;
--
-- Classify -- Overrides Fuzzy.Classifier...
--
   overriding
   function Classify
            (  classifier : Identity_Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification;
--
-- Create -- A new classifier
--
--    Classes - The class feature
--
-- Returns :
--
--    The classifier (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create (Classes : Feature_Handle) return Classifier_Handle;
--
-- Estimate -- Overrides Fuzzy.Classifier...
--
   overriding
   function Estimate
            (  Classifier : Identity_Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set;
--
-- Get_Class -- Overrides Object.Arhived...
--
   overriding
   function Get_Class (Classifier : Identity_Classifier_Object)
      return String;
--
-- Get_Classes -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Classes (Classifier : Identity_Classifier_Object)
      return Feature_Handle;
--
-- Get_Examples -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Get_Examples
             (  Classifier : Identity_Classifier_Object;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Get_Features -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Features (Classifier : Identity_Classifier_Object)
      return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Referents -- Overrides Object.Arhived...
--
   overriding
   procedure Get_Referents
             (  Classifier : Identity_Classifier_Object;
                List       : in out Deposit_Container'Class
             );
--
-- Get_Training_Set_From -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_From
            (  Classifier : Identity_Classifier_Object
            )  return Positive;
--
-- Get_Training_Set_Length -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Length
            (  Classifier : Identity_Classifier_Object
            )  return Natural;
--
-- Get_Training_Set_Name -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Name
            (  Classifier : Identity_Classifier_Object
            )  return String;
--
-- Is_Identity -- Check for identity classifier
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
   function Is_Identity (Classifier : Classifier_Handle)
      return Boolean;
--
-- Learn -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Learn
             (  Classifier : in out Identity_Classifier_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             );
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Classifier : Identity_Classifier_Object)
      return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified
             (  Classifier : in out Identity_Classifier_Object
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Classifier  : Identity_Classifier_Object
             );

private
   pragma Inline (Get_Class);
   pragma Inline (Get_Classes);
   pragma Inline (Is_Identity);
   pragma Inline (Is_Modified);
   pragma Inline (Reset_Modified);
end Fuzzy.Classifier.Identity;
