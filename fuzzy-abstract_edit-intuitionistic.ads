--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Abstract_Edit.Intuitionistic          Luebeck            --
--  Instantiation                                  Summer, 2003       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Fuzzy.Logic;  use Fuzzy.Logic;

with Fuzzy.Generic_Edit.Intuitionistic;
with Fuzzy.Intuitionistic;

package Fuzzy.Abstract_Edit.Intuitionistic is
--
-- Get -- Instantiated from Fuzzy.Generic_Edit.Intuitionistic...
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Certain_True
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Certain_True
             );
--
-- Image -- Instantiated from Fuzzy.Generic_Edit.Intuitionistic...
--
   function Image
            (  Data    : User_Data'Class;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Certain_True
            )  return String;
   function Image
            (  Data    : User_Data'Class;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Certain_True
            )  return String;
--
-- Put -- Instantiated from Fuzzy.Generic_Edit.Intuitionistic...
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data'Class;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data'Class;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Instantiated from Fuzzy.Generic_Edit.Intuitionistic...
--
   function Value
            (  Source  : String;
               Data    : User_Data'Class;
               Default : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Set;
   function Value
            (  Source  : String;
               Data    : User_Data'Class;
               Default : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Classification;

private
   pragma Inline (Get);
   pragma Inline (Image);
   pragma Inline (Put);
   pragma Inline (Value);
--
-- Intuitionistic_Edit -- Implementation through an instantiation
--
   package Intuitionistic_Edit is new Set_Edit.Intuitionistic;

end Fuzzy.Abstract_Edit.Intuitionistic;
