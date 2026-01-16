--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Day_Of_Week_Feature                    Luebeck            --
--  Interface                                      Spring, 2002       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Fuzzy.Feature.Handle;   use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Generic_Discrete;

package Test_Day_Of_Week_Feature is
   type Day_Of_Week is (Mo, Tu, We, Th, Fr, Sa, Su);
   package Day_Of_Week_Features is
      new Fuzzy.Feature.Generic_Discrete (Day_Of_Week);
   function Create_Day_Of_Week_Feature (Name : String)
      return Feature_Handle renames Day_Of_Week_Features.Create;

end Test_Day_Of_Week_Feature;
