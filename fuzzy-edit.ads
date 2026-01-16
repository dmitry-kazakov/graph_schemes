--                                                                    --
--  package Fuzzy.Edit              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2000       --
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

with Fuzzy.Generic_Edit;
with Strings_Edit;        use Strings_Edit;

package Fuzzy.Edit is
--
-- Get -- An instantiation of Fuzzy.Generic_Edit...
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Set
             );
--
-- Value -- An instantiation of Fuzzy.Generic_Edit...
--
   function Value
            (  Source : String;
               First  : Integer;
               Last   : Integer
            )  return Set;
--
-- Put -- An instantiation of Fuzzy.Generic_Edit...
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Set;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Image -- An instantiation of Fuzzy.Generic_Edit...
--
   function Image (Value : Set) return String;

private
   type User_Data (First, Last : Integer) is null record;

   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : User_Data;
                From      : out Integer;
                To        : out Integer;
                Exclusive : out Boolean
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );

   package Set_Edit is
      new Fuzzy.Generic_Edit (User_Data, Get, Put);

end Fuzzy.Edit;
