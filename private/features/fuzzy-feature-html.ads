--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.HTML                          Luebeck            --
--  Interface                                      Spring, 2002       --
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
--
--  This package provides HTML output for fuzzy feature values. A  fuzzy
--  feature value is a pair of fuzzy sets over the fuzzy feature domain.  
--
with Ada.Text_IO;        use Ada.Text_IO;
with HTML;               use HTML;
with Strings_Edit.UTF8;  use Strings_Edit.UTF8;

with Fuzzy.Intuitionistic;

package Fuzzy.Feature.HTML is
   pragma Elaborate_Body (Fuzzy.Feature.HTML);
--
-- HTML_Parameters -- Parameters controlling HTML output:
--
-- (o)  Background  is  the  color  of the background, i.e. of the truth
--      levels which are neither possible nor necessary; 
-- (o)  Conflictiong_Bar is the color of the necessity bar that does not
--      cover  the  possibility  bar, i.e. it is the color of the levels
--      which are necessary, but impossible;
-- (o)  Limits is the color of the border values;
-- (o)  Necessity_Bar  is  the color of the necessity bar when it covers
--      the  possibility  bar,  i.e. it is the color of the levels which
--      are possible and necessary; 
-- (o)  Possibility_Bar  is  the  color of the possibility bar, when the
--      necessity does not cover it, i.e. it is the color of the  levels
--      which are possible, but unnecessary;
-- (o)  Resolution is the number of | representing in 1.0 truth level;
-- (o)  Title is the background color used for titles;
-- (o)  UTF8_Error is the code position to substitute improperly encoded
--      UTF-8 code points.
--
-- The color parameters control output of sets as follows:
--
--        ___ Limits ______
--       /                 \
--      | |||||||||||       |
--               /       \
--   Possibility_Bar    Background
--     Necessity_Bar
--   Conflicting_Bar
--
   type HTML_Parameters is new Output_Parameters with record
      Resolution      : Positive := 50;
      Background      : Color    := (16#FF#, 16#FF#, 16#FF#);
      Conflicting_Bar : Color    := (16#FF#, 16#00#, 16#00#);
      Limits          : Color    := (16#00#, 16#00#, 16#00#);
      Necessity_Bar   : Color    := (16#A0#, 16#A0#, 16#00#);
      Possibility_Bar : Color    := (16#00#, 16#80#, 16#00#);
      Title           : Color    := (16#A0#, 16#A0#, 16#FF#);
      UTF8_Error      : UTF8_Code_Point := Character'Pos ('?');
   end record;
--
-- Put -- Output a fuzzy set
--
--  [ File ]     - The output file (the standard output if omitted)
--    Feature    - The feature
--    Value      - The value (set, intuitionistic set, classification)
--    Parameters - The output parameters
--
-- This procedure writes Value built over the domain set of Feature into
-- the file File in the HTML format. 
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality of Value
--    I/O exceptions
--
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Set;
                Parameters : HTML_Parameters'Class
             );
   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Set;
                Parameters : HTML_Parameters'Class
             );
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class
             );
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class
             );
   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class
             );
   procedure Put
             (  Feature    : Feature_Object'Class;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class
             );
end Fuzzy.Feature.HTML;
