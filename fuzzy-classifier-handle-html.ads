--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Handle.HTML                Luebeck            --
--  Interface                                      Spring, 2003       --
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
--  This package provides HTML output for fuzzy classifiers.
--
with Ada.Text_IO;                use Ada.Text_IO;
with Fuzzy.Feature.HTML;         use Fuzzy.Feature.HTML; 
with Fuzzy.Feature.Handle.HTML;  use Fuzzy.Feature.Handle.HTML;

package Fuzzy.Classifier.Handle.HTML is
--
-- Put -- Output a classifer
--
--  [ File ]     - The output file
--    Classifier - The classifier
--    Parameters - The output parameters
--
-- This  procedure  writes  the  classifier  specified  by  the   handle
-- Classifier  into  File  in  the HTML format. The fields of Parameters
-- control   the   output   of   feature   values   as   described    in
-- Fuzzy.Feature.Handle.HTML. 
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    I/O exceptions
--
   procedure Put
             (  File       : File_Type;
                Classifier : Classifier_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
   procedure Put
             (  Classifier : Classifier_Handle;
                Parameters : HTML_Parameters'Class := HTML_Defaults
             );
end Fuzzy.Classifier.Handle.HTML;
