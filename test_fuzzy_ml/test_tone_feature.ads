--                                                                    --
--  package Test_Tone_Feature       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  12:48 30 Aug 2010  --
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

package Test_Tone_Feature is
   type Tone is (C, D, E, A, G);
   package Tone_Features is new Fuzzy.Feature.Generic_Discrete (Tone);
   function Create_Tone_Feature (Name : String) return Feature_Handle
      renames Tone_Features.Create;

end Test_Tone_Feature;
