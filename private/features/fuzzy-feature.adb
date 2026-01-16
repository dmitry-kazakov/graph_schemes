--                                                                    --
--  package Fuzzy.Feature           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
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

with Object.Archived.Features;  use Object.Archived.Features;

package body Fuzzy.Feature is
   use Context_Stack_Objects;
   use ID_Stacks.Segmented_Stack;

   Free_ID : Feature_ID := Feature_ID'First; -- First unused ID
   Number_Of_Features : Integer := 0;
   Unused_IDs         : Stack;

   function "<" (Left, Right : Feature_Object_Ptr) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (  Left = null
         or else
            Left.ID < Right.ID
      )  );
   end "<";

   procedure Create_Constraint
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             )  is
      Ptr : constant Value_Constraint_Ptr :=
               new Value_Constraint
                   (  Feature.Cardinality,
                      Get_Data (Context'Unchecked_Access, Feature)
                   );
      Result : Value_Constraint'Class renames Ptr.all;
   begin
      if Result.Data.Constraint = null then
         Result.Allowed := (others => Allowed);
      else
         Result.Parent := Result.Data.Constraint;
         if Allowed then
            Result.Allowed := Result.Data.Constraint.Allowed;
         else
            Result.Allowed := (others => False);
         end if;
      end if;
      Result.Data.Constraint := Ptr;
   end Create_Constraint;

   procedure Finalize (Feature : in out Feature_Object) is
   begin
      Object.Archived.Close (Feature);
      if Feature.ID + 1 = Free_ID then
         Free_ID := Free_ID - 1;
      else
         Push (Unused_IDs, Feature.ID);
      end if;
      Object.Archived.Finalize (Deposit (Feature));
      Number_Of_Features := Number_Of_Features - 1;
      Free (Feature.Name);
   end Finalize;

   procedure Finalize (Constraint : in out Value_Constraint) is
   begin
      Constraint.Data.Constraint := Constraint.Parent;
      Finalize (Pool_Object (Constraint));
   end Finalize;

   function Get_Constraint
            (  Feature : Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset is
      Data : Feature_Data'Class renames Get_Data (Context, Feature).all;
   begin
      if Data.Constraint = null then
         return (1..Feature.Cardinality => True);
      else
         return Data.Constraint.Allowed;
      end if;
   end Get_Constraint;

   function Get_ID (Feature : Feature_Object'Class)
      return Feature_ID is
   begin
      return Feature.ID;
   end Get_ID;

   function Get_Name (Feature : Feature_Object) return String is
   begin
      if Feature.Name = null then
         return "";
      else
         return Feature.Name.all;
      end if;
   end Get_Name;

   function Get_Number_Of_Features return Natural is
   begin
      return Number_Of_Features;
   end Get_Number_Of_Features;

   procedure Initialize (Feature : in out Feature_Object) is
   begin
      Object.Archived.Initialize (Deposit (Feature));
      if Is_Empty (Unused_IDs) then
         Feature.ID := Free_ID;
         Free_ID    := Free_ID + 1;
      else
         Feature.ID := Top (Unused_IDs);
         Pop (Unused_IDs);
      end if;
      Number_Of_Features := Number_Of_Features + 1;
   end Initialize;

   function Is_Modified (Feature : Feature_Object) return Boolean is
   begin
      return Feature.Modified;
   end Is_Modified;

   procedure Reset_Modified (Feature : in out Feature_Object) is
   begin
      Feature.Modified := False;
   end Reset_Modified;

   procedure Set_Constraint
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                Value   : Positive;
                Allowed : Boolean := False
             )  is
      Data : Feature_Data'Class renames
                Get_Data (Context'Unchecked_Access, Feature).all;
   begin
      if Data.Constraint = null then
         Create_Constraint (Feature, Context, not Allowed);
      end if;
      if Allowed then
         --
         -- The parent's constraint on the value cannot be  removed.  So
         -- only if the parent allows the value, we allow it too.
         --
         declare
            Parent : constant Value_Constraint_Ptr :=
                        Data.Constraint.Parent;
         begin
            if Parent = null or else Parent.Allowed (Value) then
               --
               -- It is safe to check for  the  immediate  parent  only,
               -- because all operations enforce the constraint  at  any
               -- time.
               --
               Data.Constraint.Allowed (Value) := True;
            end if;
         end;
      else
         Data.Constraint.Allowed (Value) := False;
      end if;
   end Set_Constraint;

   procedure Set_Constraint_Range
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             )  is
   begin
      if To >= From then
         declare
            Data : Feature_Data'Class renames
                      Get_Data (Context'Unchecked_Access, Feature).all;
         begin
            if Data.Constraint = null then
               Create_Constraint (Feature, Context, not Allowed);
            end if;
            if Allowed then
               declare
                  Parent : constant Value_Constraint_Ptr :=
                              Data.Constraint.Parent;
               begin
                  --
                  -- It is enough to  check  only  the  direct  parent's
                  -- contsraint  because  all  operations  enforce   the
                  -- constraint at any time.
                  --
                  if Parent = null then
                     Data.Constraint.Allowed (From..To) :=
                        (others => True);
                  else
                     Data.Constraint.Allowed (From..To) :=
                        Parent.Allowed (From..To);
                  end if;
               end;
            else
               Data.Constraint.Allowed (From..To) := (others => False);
            end if;
         end;
      end if;
   end Set_Constraint_Range;

   procedure Set_Name
             (  Feature : in out Feature_Object;
                Name    : String
             )  is
   begin
      if Feature.Name = null then
         Feature.Name := new String'(Name);
         Feature.Modified := True;
         Renamed (Feature, "", Name);
      elsif Name = Feature.Name.all then
         return;
      else
         declare
            Old_Name : constant String := Feature.Name.all;
         begin
            Free (Feature.Name);
            Feature.Name := new String'(Name);
            Feature.Modified := True;
            Renamed (Feature, Old_Name, Name);
         end;
      end if;
   end Set_Name;

   procedure Set_Range
             (  Feature : Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             )  is
      Data : Feature_Data'Class renames
                Get_Data (Context'Unchecked_Access, Feature).all;
   begin
      if Data.Constraint = null then
         Create_Constraint (Feature, Context);
      end if;
      declare
         Constraint : Value_Constraint'Class renames
                         Data.Constraint.all;
      begin
         Constraint.From := From;
         Constraint.To   := To;
      end;
   end Set_Range;

   function To_Feature_Ptr is
      new Ada.Unchecked_Conversion
          (  Deposit_Ptr,
             Feature_Object_Ptr
          );

   function To_Feature_Object_Ptr (Ptr : Deposit_Ptr)
      return Feature_Object_Ptr is
   begin
      if Ptr.all in Feature_Object'Class then
         return To_Feature_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Feature_Object_Ptr;

end Fuzzy.Feature;
