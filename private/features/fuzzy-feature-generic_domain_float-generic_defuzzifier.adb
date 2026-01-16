--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier                      Autumn, 2005       --
--  Implementation                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier is

   type General_Defuzzifier_Object_Ptr is
      access all Defuzzifier_Object'Class;

   function To_Ptr is
      new Ada.Unchecked_Conversion
          (  General_Defuzzifier_Object_Ptr,
             Defuzzifier_Object_Ptr
          );

   function Is_Modified (Defuzzifier : Defuzzifier_Object)
      return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Reset_Modified (Defuzzifier : in out Defuzzifier_Object) is
   begin
      null;
   end Reset_Modified;

   function To_Defuzzifier_Ptr is
      new Ada.Unchecked_Conversion
          (  Deposit_Ptr,
             Defuzzifier_Object_Ptr
          );

   function To_Defuzzifier_Object_Ptr (Ptr : Deposit_Ptr)
      return Defuzzifier_Object_Ptr is
   begin
      if Ptr.all in Defuzzifier_Object'Class then
         return To_Defuzzifier_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Defuzzifier_Object_Ptr;

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier;
