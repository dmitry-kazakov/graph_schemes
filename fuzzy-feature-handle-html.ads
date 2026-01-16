--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.HTML                   Luebeck            --
--  Interface                                      Spring, 2002       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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
with Ada.Text_IO;         use Ada.Text_IO;
with HTML;                use HTML;
with Fuzzy.Feature.HTML;  use Fuzzy.Feature.HTML;
with Fuzzy.Intuitionistic;

package Fuzzy.Feature.Handle.HTML is
   pragma Elaborate_Body (Fuzzy.Feature.Handle.HTML);
--
-- HTML_Defaults -- The parameters used by default
--
   HTML_Defaults : constant HTML_Parameters :=
      (  Abs_Small       =>-MaxSmall,
         Background      => White,
         Base            => 10,
         Conflicting_Bar => (16#FF#, 16#00#, 16#00#),
         Default         => Certain_True,
         Mode            => Units.UTF8_Set,
         Limits          => Black,
         Necessity_Bar   => (16#A0#, 16#A0#, 16#00#),
         Rel_Small       => MaxSmall,
         Resolution      => 50,
         Possibility_Bar => (16#00#, 16#80#, 16#00#),
         Put_Plus        => False,
         Put_Units       => False,
         Quote_Units     => True,
         Title           => (16#A0#, 16#A0#, 16#FF#),
         Use_Derived     => True,
         Use_SI          => False,
         UTF8_Error      => Character'Pos ('?')
      );
--
-- Put -- Output a fuzzy set
--
--  [ File ]     - The output file (the standard output if omitted)
--    Feature    - The feature handle
--    Value      - The feature value to be written
--    Parameters - The output parameters
--
-- This procedure writes fuzzy set, intuitionistic set or classification
-- Value  built over the domain set of Feature into the file File in the
-- HTML format. The color parameters are used as follows: 
--
--        ___ Limits ______
--       /                 \
--      | |||||||||||       |
--               /       \
--            Bar         Background
--
-- Exceptions :
--
--    Constraint_Error - Illegal handle or cardinality of Value
--    I/O exceptions
--
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Handle;
                Value      : Set;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  Feature    : Feature_Handle;
                Value      : Set;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Handle;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  Feature    : Feature_Handle;
                Value      : Intuitionistic.Set;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  File       : File_Type;
                Feature    : Feature_Handle;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  Feature    : Feature_Handle;
                Value      : Intuitionistic.Classification;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
end Fuzzy.Feature.Handle.HTML;
