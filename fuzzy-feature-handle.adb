--                                                                    --
--  package Fuzzy.Feature.Handle    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
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

package body Fuzzy.Feature.Handle is

   procedure Create_Constraint
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             ) is
   begin
      Create_Constraint (Ptr (Feature).all, Context, Allowed);
   end Create_Constraint;

   procedure Delete (Feature : in out Feature_Handle) is
   begin
      if Feature.Is_Valid then
         Object.Archived.Delete (Ptr (Feature).all);
         Invalidate (Feature);
      end if;
   end Delete;

   function Get_Cardinality (Feature : Feature_Handle)
      return Positive is
   begin
      return Ptr (Feature).Cardinality;
   end Get_Cardinality;

   function Get_Class (Feature : Feature_Handle) return String is
   begin
      return Get_Class (Ptr (Feature).all);
   end Get_Class;

   function Get_Constraint
            (  Feature : Feature_Handle;
               Context : access Context_Object'Class
            )  return Domain_Subset is
   begin
      return Get_Constraint (Ptr (Feature).all, Context);
   end Get_Constraint;

   function Get_ID (Feature : Feature_Handle)
      return Feature_ID is
   begin
      return Get_ID (Ptr (Feature).all);
   end Get_ID;

   function Get_Name (Feature : Feature_Handle)
      return String is
   begin
      return Get_Name (Ptr (Feature).all);
   end Get_Name;

   procedure Invalidate (Feature : in out Feature_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Feature));
   end Invalidate;

   function Is_Computed
            (  Feature : Feature_Handle;
               Source  : Feature_Handle
            )  return Boolean is
   begin
      return Is_Computed (Ptr (Feature).all, Ptr (Source).all);
   end Is_Computed;

   function Is_Valid (Feature : Feature_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Feature));
   end Is_Valid;

   function Ptr (Feature : Feature_Handle)
      return Feature_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Feature));
   end Ptr;

   function Ref (Feature : Feature_Object_Ptr) return Feature_Handle is
   begin
      return (Handles.Ref (Feature) with null record);
   end Ref;

   procedure Ref
             (  Handle  : in out Feature_Handle;
                Feature : Feature_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Feature);
   end Ref;

   procedure Set_Constraint
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                Value   : Positive;
                Allowed : Boolean := False
             )  is
   begin
      Set_Constraint (Ptr (Feature).all, Context, Value, Allowed);
   end Set_Constraint;

   procedure Set_Constraint_Range
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             )  is
   begin
      Set_Constraint_Range
      (  Ptr (Feature).all,
         Context,
         From,
         To,
         Allowed
      );
   end Set_Constraint_Range;

   procedure Set_Name
             (  Feature : Feature_Handle;
                Name    : String
             )  is
   begin
      Set_Name (Ptr (Feature).all, Name);
   end Set_Name;

   procedure Set_Range
             (  Feature : Feature_Handle;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             )  is
   begin
      Set_Range (Ptr (Feature).all, Context, From, To);
   end Set_Range;

   function To_Feature_Handle
            (  Feature : Deposit_Handles.Handle
            )  return Feature_Handle is
   begin
      return
         Ref
         (  To_Feature_Object_Ptr
            (  Deposit_Handles.Ptr (Feature)
         )  );
   end To_Feature_Handle;

   function To_Deposit_Handle (Feature : Feature_Handle)
      return Deposit_Handles.Handle is
   begin
      return Deposit_Handles.Ref (To_Deposit_Ptr (Ptr (Feature)));
   end To_Deposit_Handle;

   function "<" (Left, Right : Feature_Handle) return Boolean is
   begin
      return
         Handles."<" (Handles.Handle (Left), Handles.Handle (Right));
   end "<";

   function "<=" (Left, Right : Feature_Handle) return Boolean is
   begin
      return
         Handles."<=" (Handles.Handle (Left), Handles.Handle (Right));
   end "<=";

   function "="  (Left, Right : Feature_Handle) return Boolean is
   begin
      return
         Handles."=" (Handles.Handle (Left), Handles.Handle (Right));
   end "=";

   function ">=" (Left, Right : Feature_Handle) return Boolean is
   begin
      return
         Handles.">=" (Handles.Handle (Left), Handles.Handle (Right));
   end ">=";

   function ">"  (Left, Right : Feature_Handle) return Boolean is
   begin
      return
         Handles.">" (Handles.Handle (Left), Handles.Handle (Right));
   end ">";

end Fuzzy.Feature.Handle;
