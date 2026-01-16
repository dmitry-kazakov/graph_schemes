--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Compiler.                       Luebeck            --
--         Get_Variables.Get_Real                  Summer, 2005       --
--  Separate body implementation                                      --
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
--  This  procedure  recognizes a linguistic variable definition. It has
--  the syntax: 
--
--     <name> : real
--     <name> : real [<scale>]
--
--  The values in the list have the syntax of an  identifier.  They  may
--  not repeat. 
--
separate (Parsers.FCL.Compiler.Get_Variables)
   procedure Get_Real
             (  Name     : String;
                Location : Parsers.Multiline_Source.Location
             )  is
   Text  : Unbounded_String;
   Scale : Measure;
begin
   Get_Scale (Parser, Code, Scale, Text);
   Add
   (  In_Vars,
      Name,
      (  Kind_Of    => Real_Var,
         Feature    => No_Feature,
         Location   => Location & Link (Code),
         Scale      => Text,
         Fuzzify_At => ((0,0),(0,0))
   )  );
end Get_Real;
