--                                                                    --
--  package Fuzzy.Feature.Context   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2003       --
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
--  This  package  defines  a  concrete  context  type   Lecture_Context
--  (derived from the abstract Context_Object). The context is  used  to
--  buffer teaching set data and their constraints.
--
with Fuzzy.Lecture;  use Fuzzy.Lecture;

package Fuzzy.Feature.Context is
--
-- Lecture_Context -- Context dependent data to evaluate features
--
--    Lesson - The training set associated with the context
--
-- The set associated with a context is not destroyed till  the  context
-- exists.
--
   type Lecture_Context
        (  Lesson : not null access Lecture_Object'Class
        )  is new Context_Object with
   record
      Example : Positive := 1;
   end record;
--
-- Finalize -- Destructor
--
--    Context - The data context
--
   overriding
   procedure Finalize (Context : in out Lecture_Context);
--
-- Get -- Get image
--
--    Context - The data context
--    Example - The number of an example in the set
--    Feature - In the example
--    Image   - The requested image
--
-- This function returns the image specified by the parameter Image  for
-- Feature in the example Example.
--
-- Returns :
--
--    Fuzzy subset of the feature domain
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--
   function Get
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get -- Get conditional possibility of a value
--
--    Context - The data context
--    Example - The number of an example in the set
--    Feature - In the example
--    Image   - The requested image
--    Value   - The feature domain value
--
-- This function is an equivalent to:
--
--    Get (Context, Example, Feature, Image) (Value)
--
-- Returns :
--
--    The possibility of Value in the specified Image
--
-- Exceptions :
--
--    Constraint_Error - Illegal Value or an invalid handle
--
   function Get
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence;
--
-- Initialize -- Constructor
--
--    Context - The data context
--
-- All derived types are required to  call  this  procedure  from  their
-- versions of Initialize.
--
   overriding
   procedure Initialize (Context : in out Lecture_Context);
--
-- Is_Defined - Check if image is defined
--
--    Context - The data context
--    Example - The number of an example in the set
--    Feature - In the example
--    Image   - The characteristic to check
--
-- This  function can be used to query whether an image of is defined by
-- the teaching set.
--
-- Returns :
--
--    True if the image is present in the lecture example
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--
   function Is_Defined
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Check image is known
--
--    Context - The data context
--    Example - The number of the example in the set
--    Feature - In the example
--    Image   - The characteristic to check
--
-- This  function checks if the specified image is unknown, i.e. has all
-- values of the membership function 1.
--
-- Returns :
--
--    False if Get (Context, Example, Feature, Image) would return all 1
--
-- Exceptions :
--
--    Constraint_Error - Invalid feature handle
--
   function Is_Known
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Select_Example -- Change the example number
--
--    Context - The data context
--    Example - To be selected
--
-- This procedure is used to select  an  example  of  the  teaching  set
-- associated with the context.  It  does  nothing  if  the  example  is
-- already selected. Otherwise all cached context data are invalidated.
--
   procedure Select_Example
             (  Context : in out Lecture_Context;
                Example : Positive
             );

private
--
-- Get_Data -- Overrides Fuzzy.Feature...
--
   overriding
   function Get_Data
            (  Context : not null access Lecture_Context;
               Feature : Feature_Object'Class
            )  return Feature_Data_Ptr;
--
-- Set_Data -- Overrides Fuzzy.Feature...
--
   procedure Set_Data
             (  Context : in out Lecture_Context;
                Feature : Feature_Object'Class;
                Data    : in out Feature_Data_Ptr
             );

   pragma Inline (Is_Defined);
   pragma Inline (Is_Known);
   pragma Inline (Get_Data);
   pragma Inline (Set_Data);

end Fuzzy.Feature.Context;
