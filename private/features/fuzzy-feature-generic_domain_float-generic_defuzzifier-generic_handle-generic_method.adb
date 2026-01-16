--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Defuzzifier.          Luebeck            --
--        Generic_Handle.Generic_Method            Autumn, 2005       --
--           Generic_Method                                           --
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

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
             Generic_Handle.Generic_Method is

   Class : constant String := Defuzzifier_Class & "Method." & Name;

   function Create return Defuzzifier_Handle is
   begin
      return Ref (new Defuzzification_Method_Object);
   end Create;

   function Defuzzify
            (  Defuzzifier : Defuzzification_Method_Object;
               Value       : Variable_Measures.Variable_Measure
            )  return Fuzzy_Measures.Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Method (Value.Gain),
         Offset => Value.Offset
      );
   end Defuzzify;

   function Get_Class (Defuzzifier : Defuzzification_Method_Object)
      return String is
   begin
      return Class;
   end Get_Class;

   function Get_Method_Name
            (  Defuzzifier : Defuzzification_Method_Object
            )  return String is
   begin
      return Name;
   end Get_Method_Name;

   procedure Restore
             (  Source      : String;
                Pointer     : in out Integer;
                Class       : String;
                List        : Deposit_Container'Class;
                Defuzzifier : out Deposit_Ptr
             )  is
   begin
      Defuzzifier := new Defuzzification_Method_Object;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Defuzzifier : Defuzzification_Method_Object
             )  is
   begin
      null;
   end Store;

begin
   if not Is_Registered (Class) then
      Register_Class (Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle.Generic_Method;
