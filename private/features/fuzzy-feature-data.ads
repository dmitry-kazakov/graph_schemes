--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Data                          Luebeck            --
--  Interface                                      Spring, 2003       --
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
-- This  package  defines  the  type  Cached_Feature_Data  derived  from
-- Feature_Data, which can be used for caching feature data.
--
package Fuzzy.Feature.Data is
   pragma Elaborate_Body (Fuzzy.Feature.Data);

   type Image_Flags is array (Image_Type) of Boolean;
--
-- Cached_Feature_Data -- Feature data cache
--
--    Cardinality - Of the feature
--
   type Cached_Feature_Data (Cardinality : Positive) is
      new Feature_Data with
   record
      Known       : Image_Flags := (others => False);
      Defined     : Image_Flags := (others => False);
      Has_In      : Set (1..Cardinality);
      Has_Out     : Set (1..Cardinality);
      Has_Not     : Set (1..Cardinality);
      Has_Not_Out : Set (1..Cardinality);
   end record;
--
-- Undefine -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Undefine
             (  Data    : in out Cached_Feature_Data;
                Context : in out Context_Object'Class
             );

private
   pragma Inline (Undefine);

end Fuzzy.Feature.Data;
