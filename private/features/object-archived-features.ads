--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Archived.Features                    Luebeck            --
--  Interface                                      Summer, 2009       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

package Object.Archived.Features is
--
-- Feature_Link -- Backward link to a feature
--
   type Feature_Link is
      abstract new Backward_Link with null record;
--
-- Renamed -- Notification about renaming
--
--    Feature  - The object
--    Old_Name - The old name of the feature
--    New_Name - The new name of the feature
--
-- This procedure is used when Object has been renamed.
--
   procedure Renamed
             (  Feature  : in out Deposit'Class;
                Old_Name : String;
                New_Name : String
             );
--
-- Renamed -- Notification about object renaming
--
--    Link     - From the renamed object
--    Old_Name - The old name of the feature
--    New_Name - The new name of the feature
--
-- This  procedure  is  used when a feature object has been renamed from
-- Old_Name to New_Name.
--
   procedure Renamed
             (  Link     : in out Feature_Link;
                Old_Name : String;
                New_Name : String
             )  is abstract;

end Object.Archived.Features;
