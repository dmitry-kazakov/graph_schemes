--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Unbounded_Arrays       Luebeck            --
--  Implementation                                 Autumn, 2008       --
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

package body Fuzzy.Feature.Handle.Unbounded_Arrays is
   use Unbounded_Arrays;

   procedure Adjust (Container : in out Unbounded_Array) is
   begin
      Adjust (Unbounded_Arrays.Unbounded_Array (Container));
   end Adjust;

   procedure Erase (Container : in out Unbounded_Array) is
   begin
      Erase (Unbounded_Arrays.Unbounded_Array (Container));
   end Erase;

   procedure Finalize (Container : in out Unbounded_Array) is
   begin
      Finalize (Unbounded_Arrays.Unbounded_Array (Container));
   end Finalize;

   function First (Container : Unbounded_Array) return Integer is
   begin
      return First (Unbounded_Arrays.Unbounded_Array (Container));
   end First;

   function Get
            (  Container : Unbounded_Array;
               Index     : Integer
            )  return Feature_Object_Ptr is
   begin
      return Get (Unbounded_Arrays.Unbounded_Array (Container), Index);
   end Get;

   function Last (Container : Unbounded_Array) return Integer is
   begin
      return Last (Unbounded_Arrays.Unbounded_Array (Container));
   end Last;

   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Integer;
                Element   : Feature_Object_Ptr
             )  is
   begin
      Put
      (  Unbounded_Arrays.Unbounded_Array (Container),
         Index,
         Element
      );
   end Put;

   procedure Put
             (  Container : in out Unbounded_Array;
                Index     : Integer;
                Element   : Feature_Handle
             ) is
   begin
      Put
      (  Unbounded_Arrays.Unbounded_Array (Container),
         Index,
         Element
      );
   end Put;

   function Ref
            (  Container : Unbounded_Array;
               Index     : Integer
            )  return Feature_Handle is
   begin
      return Ref (Unbounded_Arrays.Unbounded_Array (Container), Index);
   end Ref;

   function To_Unbounded_Array (Container : Feature_Array)
      return Unbounded_Array is
      Result : Unbounded_Array;
   begin
      for Index in Container'Range loop
         Put
         (  Result,
            Index,
            Ptr (Container (Index))
         );
      end loop;
      return Result;
   end To_Unbounded_Array;

   function To_Feature_Array (Container : Unbounded_Array)
      return Feature_Array is
      From : Integer := First (Container);
      To   : Integer := Last  (Container);
   begin
      while From <= To and Get (Container, From) = null loop
         From := From + 1;
      end loop;
      while From <= To and Get (Container, To) = null loop
         To := To - 1;
      end loop;
      declare
         Result : Feature_Array (From..To);
      begin
         for Index in Result'Range loop
            Set (Result (Index), Get (Container, Index));
         end loop;
         return Result;
      end;
   end To_Feature_Array;

end Fuzzy.Feature.Handle.Unbounded_Arrays;
