--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Container              Luebeck            --
--  Implementation                                 Winter, 2005       --
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

package body Fuzzy.Feature.Handle.Container is

   procedure Add (Container : in out Set; Item : Feature_Object_Ptr) is
   begin
      Handles_Set.Add (Handles_Set.Set (Container), Item);
   end Add;

   procedure Add (Container : in out Set; Item : Feature_Handle) is
   begin
      Add (Container, Ptr (Item));
   end Add;

   procedure Add (Container : in out Set; Items : Set) is
   begin
      Handles_Set.Add
      (  Handles_Set.Set (Container),
         Handles_Set.Set (Items)
      );
   end Add;

   function Create return Set is
   begin
      return (Handles_Set.Create with null record);
   end Create;

   procedure Erase (Container : in out Set) is
   begin
      Handles_Set.Erase (Handles_Set.Set (Container));
   end Erase;

   function Find (Container : Set; Item : Feature_Object'Class)
      return Integer is
   begin
      return Handles_Set.Find (Handles_Set.Set (Container), Item);
   end Find;

   function Find (Container : Set; Item : Feature_Object_Ptr)
      return Integer is
   begin
      return Handles_Set.Find (Handles_Set.Set (Container), Item);
   end Find;

   function Find (Container : Set; Item : Feature_Handle)
      return Integer is
   begin
      return Find (Container, Ptr (Item));
   end Find;

   function Get (Container : Set; Index : Positive)
      return Feature_Object_Ptr is
   begin
      return Handles_Set.Get (Handles_Set.Set (Container), Index);
   end Get;

   function Get_Size (Container : Set) return Natural is
   begin
      return Handles_Set.Get_Size (Handles_Set.Set (Container));
   end Get_Size;

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Handles_Set.Is_Empty (Handles_Set.Set (Container));
   end Is_Empty;

   function Is_In (Container : Set; Item : Feature_Object'Class)
      return Boolean is
   begin
      return Handles_Set.Is_In (Handles_Set.Set (Container), Item);
   end Is_In;

   function Is_In (Container : Set; Item : Feature_Object_Ptr)
      return Boolean is
   begin
      return Handles_Set.Is_In (Handles_Set.Set (Container), Item);
   end Is_In;

   function Is_In (Container : Set; Item : Feature_Handle)
      return Boolean is
   begin
      return Is_In (Container, Ptr (Item));
   end Is_In;

   function Ref (Container : Set; Index : Positive)
      return Feature_Handle is
   begin
      return Ref (Get (Container, Index));
   end Ref;

   procedure Remove (Container : in out Set; Index : Positive) is
   begin
      Handles_Set.Remove (Handles_Set.Set (Container), Index);
   end Remove;

   procedure Remove
             (  Container : in out Set;
                Item      : Feature_Object'Class
             )  is
   begin
      Handles_Set.Remove (Handles_Set.Set (Container), Item);
   end Remove;

   procedure Remove
             (  Container : in out Set;
                Item      : Feature_Object_Ptr
             )  is
   begin
      Handles_Set.Remove (Handles_Set.Set (Container), Item);
   end Remove;

   procedure Remove (Container : in out Set; Item : Feature_Handle) is
   begin
      Remove (Container, Ptr (Item));
   end Remove;

   procedure Remove (Container : in out Set; Items : Set) is
   begin
      Handles_Set.Remove
      (  Handles_Set.Set (Container),
         Handles_Set.Set (Items)
      );
   end Remove;

   function "and" (Left, Right : Set) return Set is
   begin
      return
      (  Handles_Set."and"
         (  Handles_Set.Set (Left),
            Handles_Set.Set (Right)
         )
      with null record
      );
   end "and";

   function "or" (Left, Right : Set) return Set is
   begin
      return
      (  Handles_Set."or"
         (  Handles_Set.Set (Left),
            Handles_Set.Set (Right)
         )
      with null record
      );
   end "or";

   function "xor" (Left, Right : Set) return Set is
   begin
      return
      (  Handles_Set."xor"
         (  Handles_Set.Set (Left),
            Handles_Set.Set (Right)
         )
      with null record
      );
   end "xor";

   function "=" (Left, Right : Set) return Boolean is
   begin
      return
         Handles_Set."="
         (  Handles_Set.Set (Left),
            Handles_Set.Set (Right)
         );
   end "=";

end Fuzzy.Feature.Handle.Container;
