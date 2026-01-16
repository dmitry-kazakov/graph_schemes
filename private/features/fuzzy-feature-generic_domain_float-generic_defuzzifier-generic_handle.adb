--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier.Generic_Handle       Autumn, 2005       --
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
             Generic_Handle is

   function Defuzzify
            (  Defuzzifier : Defuzzifier_Handle;
               Value       : Variable_Measure
            )  return Measure is
   begin
      return Defuzzify (Ptr (Defuzzifier).all, Value);
   end Defuzzify;

   procedure Delete (Defuzzifier : in out Defuzzifier_Handle) is
   begin
      if Is_Valid (Defuzzifier) then
         Object.Archived.Delete (Ptr (Defuzzifier).all);
         Invalidate (Defuzzifier);
      end if;
   end Delete;

   function Get_Class (Defuzzifier : Defuzzifier_Handle)
      return String is
   begin
      return Get_Class (Ptr (Defuzzifier).all);
   end Get_Class;

   function Get_Method_Name
            (  Defuzzifier : Defuzzifier_Handle
            )  return String is
   begin
      return Get_Method_Name (Ptr (Defuzzifier).all);
   end Get_Method_Name;

   procedure Invalidate (Defuzzifier : in out Defuzzifier_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Defuzzifier));
   end Invalidate;

   function Is_Valid (Defuzzifier : Defuzzifier_Handle)
      return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Defuzzifier));
   end Is_Valid;

   function Ptr (Defuzzifier : Defuzzifier_Handle)
      return Defuzzifier_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Defuzzifier));
   end Ptr;

   function Ref (Defuzzifier : Defuzzifier_Object_Ptr)
      return Defuzzifier_Handle is
   begin
      return (Handles.Ref (Defuzzifier) with null record);
   end Ref;

   procedure Set
             (  Handle : in out Defuzzifier_Handle;
                Defuzzifier : Defuzzifier_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Defuzzifier);
   end Set;

   function To_Defuzzifier_Handle
            (  Defuzzifier : Deposit_Handles.Handle
            )  return Defuzzifier_Handle is
   begin
      return
         Ref
         (  To_Defuzzifier_Object_Ptr
            (  Deposit_Handles.Ptr (Defuzzifier)
         )  );
   end To_Defuzzifier_Handle;

   function To_Deposit_Handle (Defuzzifier : Defuzzifier_Handle)
      return Deposit_Handles.Handle is
   begin
      return Deposit_Handles.Ref (To_Deposit_Ptr (Ptr (Defuzzifier)));
   end To_Deposit_Handle;

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle;
