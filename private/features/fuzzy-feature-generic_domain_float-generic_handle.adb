--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Handle                           Autumn, 2005       --
--  Implementation                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Handle is

   function Accumulate
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set
            )  return Variable_Measure is
   begin
      return
         Accumulate
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Accumulate;

   function Classify
            (  Feature : Feature_Handle;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Classify;

   function Classify
            (  Feature : Feature_Handle;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Classify;

   function Classify
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Classify;

   function Classify
            (  Feature : Feature_Handle;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Classify;

   function Get_Scale (Feature : Feature_Handle) return Measure is
   begin
      return
         Get_Scale
         (  Domain_Feature_Object'Class (Ptr (Feature).all)
         );
   end Get_Scale;

   function Get_Scale_Text
            (  Feature    : Feature_Handle;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return
         Get_Scale_Text
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Parameters
         );
   end Get_Scale_Text;

   function Get_Unit (Feature : Feature_Handle) return Unit is
   begin
      return
         Get_Unit
         (  Domain_Feature_Object'Class (Ptr (Feature).all)
         );
   end Get_Unit;

   function Get_Variable
            (  Feature : Feature_Handle;
               Value   : Positive
            )  return Variable_Measure is
   begin
      return
         Get_Variable
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end Get_Variable;

   function Is_Domain_Float (Feature : Feature_Handle) return Boolean is
   begin
      return
      (  Feature.Is_Valid
      and then
         Ptr (Feature).all in Domain_Feature_Object'Class
      );
   end Is_Domain_Float;

   function Is_Domain_Linguistic
            (  Feature : Feature_Handle
            )  return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
      (  This /= null
      and then
         This.all in Domain_Feature_Object'Class
      and then
         Is_Domain_Linguistic (Domain_Feature_Object'Class (This.all))
      );
   end Is_Domain_Linguistic;

   function To_Set
            (  Feature : Feature_Handle;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end To_Set;

   function To_Set
            (  Feature : Feature_Handle;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end To_Set;

   function To_Set
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end To_Set;

   function To_Set
            (  Feature : Feature_Handle;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Domain_Feature_Object'Class (Ptr (Feature).all),
            Value
         );
   end To_Set;

end Fuzzy.Feature.Generic_Domain_Float.Generic_Handle;
