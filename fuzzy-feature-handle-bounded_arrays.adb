--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Bounded_Arrays         Luebeck            --
--  Implementation                                 Summer, 2004       --
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

package body Fuzzy.Feature.Handle.Bounded_Arrays is
   use Bounded_Arrays;

   procedure Adjust (Container : in out Bounded_Array) is
   begin
      Adjust (Bounded_Arrays.Bounded_Array (Container));
   end Adjust;

   function Append
            (  Container : Bounded_Array;
               Element   : Feature_Object_Ptr := null;
               Count     : Natural            := 1
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  Append
                  (  Bounded_Arrays.Bounded_Array (Container),
                     Element,
                     Count
                  );
   begin
      return (Result with Result.First, Result.Last);
   end Append;

   function Append
            (  Container : Bounded_Array;
               Element   : Feature_Handle;
               Count     : Natural := 1
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  Append
                  (  Bounded_Arrays.Bounded_Array (Container),
                     Element,
                     Count
                  );
   begin
      return (Result with Result.First, Result.Last);
   end Append;

   function Delete
            (  Container : Bounded_Array;
               From      : Integer;
               Count     : Natural := 1
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                 Delete
                 (  Bounded_Arrays.Bounded_Array (Container),
                    From,
                    Count
                 );
   begin
      return (Result with Result.First, Result.Last);
   end Delete;

   procedure Finalize (Container : in out Bounded_Array) is
   begin
      Finalize (Bounded_Arrays.Bounded_Array (Container));
   end Finalize;

   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Element   : Feature_Object_Ptr
             )  is
   begin
      Fill
      (  Bounded_Arrays.Bounded_Array (Container),
         From,
         To,
         Element
      );
   end Fill;

   procedure Fill
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Element   : Feature_Handle
             )  is
   begin
      Fill
      (  Bounded_Arrays.Bounded_Array (Container),
         From,
         To,
         Element
      );
   end Fill;

   function Get
            (  Container : Bounded_Array;
               Index     : Integer
            )  return Feature_Object_Ptr is
   begin
      return Get (Bounded_Arrays.Bounded_Array (Container), Index);
   end Get;

   function Get
            (  Container : Bounded_Array;
               From      : Integer;
               To        : Integer
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  Get
                  (  Bounded_Arrays.Bounded_Array (Container),
                     From,
                     To
                  );
   begin
      return (Result with Result.First, Result.Last);
   end Get;

   function Prepend
            (  Container : Bounded_Array;
               Element   : Feature_Object_Ptr := null;
               Count     : Natural            := 1
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  Prepend
                  (  Bounded_Arrays.Bounded_Array (Container),
                     Element,
                     Count
                  );
   begin
      return (Result with Result.First, Result.Last);
   end Prepend;

   function Prepend
            (  Container : Bounded_Array;
               Element   : Feature_Handle;
               Count     : Natural := 1
            )  return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  Prepend
                  (  Bounded_Arrays.Bounded_Array (Container),
                     Element,
                     Count
                  );
   begin
      return (Result with Result.First, Result.Last);
   end Prepend;

   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Integer;
                Element   : Feature_Object_Ptr
             )  is
   begin
      Put (Bounded_Arrays.Bounded_Array (Container), Index, Element);
   end Put;

   procedure Put
             (  Container : in out Bounded_Array;
                Index     : Integer;
                Element   : Feature_Handle
             )  is
   begin
      Put (Bounded_Arrays.Bounded_Array (Container), Index, Element);
   end Put;

   procedure Put
             (  Container : in out Bounded_Array;
                From      : Integer;
                To        : Integer;
                Elements  : Bounded_Array
             )  is
   begin
      Put
      (  Bounded_Arrays.Bounded_Array (Container),
         From,
         To,
         Bounded_Arrays.Bounded_Array (Elements)
      );
   end Put;

   function Ref
            (  Container : Bounded_Array;
               Index     : Integer
            )  return Feature_Handle is
   begin
      return Ref (Bounded_Arrays.Bounded_Array (Container), Index);
   end Ref;

   function To_Bounded_Array (Container : Feature_Array)
      return Bounded_Array is
      Result : Bounded_Array (Container'First, Container'Last);
   begin
      for Index in Container'Range loop
         Put
         (  Bounded_Arrays.Bounded_Array (Result),
            Index,
            Container (Index)
         );
      end loop;
      return Result;
   end To_Bounded_Array;

   function To_Feature_Array (Container : Bounded_Array)
      return Feature_Array is
      Result : Feature_Array (Container.First..Container.Last);
   begin
      for Index in Result'Range loop
         Set
         (  Result (Index),
            Get (Bounded_Arrays.Bounded_Array (Container), Index)
         );
      end loop;
      return Result;   
   end To_Feature_Array;

   function "&" (Left, Right : Bounded_Array) return Bounded_Array is
      Result : Bounded_Arrays.Bounded_Array renames
                  "&"
                  (  Bounded_Arrays.Bounded_Array (Left),
                     Bounded_Arrays.Bounded_Array (Right)
                  );
   begin
      return (Result with Result.First, Result.Last);
   end "&";

end Fuzzy.Feature.Handle.Bounded_Arrays;