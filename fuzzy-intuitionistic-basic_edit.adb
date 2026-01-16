--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Intuitionistic.Basic_Edit             Luebeck            --
--  Interface                                      Summer, 2003       --
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

with Fuzzy.Basic_Edit;  use Fuzzy.Basic_Edit;

package body Fuzzy.Intuitionistic.Basic_Edit is

   procedure Get_Weight
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Boolean;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      Index : Integer := Pointer;
   begin
      Get_Weight
      (  Source,
         Index,
         Value.Possibility,
         Default.Possibility
      );
      if Index /= Pointer then
         Get_Weight
         (  Source,
            Index,
            Value.Necessity,
            Default.Necessity and Value.Possibility
         );
         Pointer := Index;
      else
         Value.Necessity := Default.Necessity and Value.Possibility;
      end if;
   end Get_Weight;

end Fuzzy.Intuitionistic.Basic_Edit;
