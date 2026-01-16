--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Independent           Luebeck            --
--  Interface                                      Winter, 2002       --
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
-- This package defines the type Independent_Feature_Object derived from
-- Feature_Object, which is a base for all independent feature types.
--
with Fuzzy.Feature.Data;  use Fuzzy.Feature.Data;

generic
   type Feature_Type is abstract new Feature_Object with private;
package Fuzzy.Feature.Generic_Independent is
   pragma Elaborate_Body (Fuzzy.Feature.Generic_Independent);

   type Independent_Feature_Object is abstract
      new Feature_Type with null record;
--
-- Create_Data -- Overrides Fuzzy.Feature...
--
   overriding
   function Create_Data (Feature : Independent_Feature_Object)
      return Feature_Data_Ptr;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get -- Overrides Fuzzy.Feature...
--
   overriding
   function Get
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence;
--
-- Is_Computed -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Computed
            (  Feature : Independent_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean;
--
-- Is_Defined -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Defined
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Known
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Query -- Value of
--
--    Feature - The feature
--    Context - The context
--    Image   - To get
--    Data    - The feature data
--
-- This procedure queries the value of a feature.
--
   procedure Query
             (  Feature : Independent_Feature_Object;
                Context : in out Context_Object'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data'Class
             );

private
   pragma Inline (Get);
end Fuzzy.Feature.Generic_Independent;
