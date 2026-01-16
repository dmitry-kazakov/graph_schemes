--                                                                    --
--  package Fuzzy.Graph.Separators  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2006       --
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

with Ada.Unchecked_Deallocation;
with Intervals;
with Strings_Edit.Floats;

package body Fuzzy.Graph.Separators is
   use Classification_Sets;
   use Node_Classifications;
   use Stack_Storage;
   use type Divergence_Range;

   procedure Allocate
             (  Cache : in out Quality_Cache;
                Ptr   : out Classification_Ptr
             )  is
   begin
      if Is_Empty (Cache.Free) then
         declare
            type Ptr_Type is access Classification;
            for Ptr_Type'Storage_Pool use Cache.Arena;
            Arena_Ptr : constant Ptr_Type :=
                           new Classification (Cache.Cardinality);
         begin
            Ptr := Arena_Ptr.all'Unchecked_Access;
         end;
      else
         Ptr := Top (Cache.Free);
         Pop (Cache.Free);
      end if;
   end Allocate;

   procedure Add
             (  Left  : in out Classification;
                Right : Classification
             )  is
      pragma Inline (Add);
   begin
      Or_At  (Left.Possibility, Right.Possibility);
      And_At (Left.Necessity,   Right.Necessity);
   end Add;

   procedure Add
             (  Left  : in out Classification_Ptr_Array;
                Right : Classification;
                Cache : in out Quality_Cache
             )  is
      pragma Inline (Add);
   begin
      for Index in Left'Range loop
         if Left (Index) = null then
            Allocate (Cache, Left (Index));
            Left (Index).all := Right;
         else
            Add (Left (Index).all, Right);
         end if;
      end loop;
   end Add;

   procedure Add
             (  Left  : in out Classification_Ptr_Array;
                Right : Classification_Ptr_Array;
                Cache : in out Quality_Cache
             )  is
      pragma Inline (Add);
   begin
      for Index in Right'Range loop
         if Right (Index) /= null then
            if Left (Index) = null then
               Allocate (Cache, Left (Index));
               Left (Index).all := Right (Index).all;
            else
               Add (Left (Index).all, Right (Index).all);
            end if;
         end if;
      end loop;
   end Add;

   procedure Add
             (  Left  : in out Classification_Ptr_Array;
                Right : Graph_Node'Class;
                Cache : access Quality_Cache
             )  is
      Child : Graph_Node_Ptr;
   begin
      for Index in 1..Right.Cardinality loop
         Child := Get_Child (Right, Index);
         if Child /= null then
            if Left (Index) = null then
               Allocate (Cache.all, Left (Index));
               Left (Index).all := Get_Distribution (Child, Cache);
            else
               Add (Left (Index).all, Get_Distribution (Child, Cache));
            end if;
         end if;
      end loop;
   end Add;

   function Compare
            (  Left      : Divergence_Range;
               Right     : Divergence_Range;
               Threshold : Confidence
            )  return Quality_Comparison is
      pragma Inline (Compare);
      Step      : constant Divergence := Divergence (Threshold);
      From_Diff : constant Divergence := Left.From - Right.From;
      To_Diff   : constant Divergence := Left.To   - Right.To;
   begin
      if From_Diff > Step then
         if To_Diff >= -Step then
            return Higher;
         end if;
      elsif From_Diff < -Step then
         if To_Diff <= Step then
            return Lower;
         end if;
      else
         if To_Diff > Step then
            return Higher;
         elsif To_Diff < -Step then
            return Lower;
         end if;
      end if;
      return Same;
   end Compare;

   procedure Deleted
             (  Link  : in out Node_Classification;
                Temps : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Deleted;

   procedure Destroyed (Link : in out Node_Classification) is
   begin
      Remove
      (  Link.Cache.Map,
         Graph_Node'Class (This (Link).all)'Access
      );
   end Destroyed;

   procedure Free
             (  Cache : in out Quality_Cache;
                Ptr   : in out Classification_Ptr
             )  is
   begin
      if Ptr /= null then
         Push (Cache.Free, Ptr);
         Ptr := null;
      end if;
   end Free;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Node_Classification,
             Node_Classification_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Feature_Distribution,
             Feature_Distribution_Ptr
          );

   procedure Finalize (Link : in out Node_Classification) is
      This : Feature_Distribution_Ptr;
   begin
      Free (Link.Cache.all, Link.Universe);
      for Feature in 1..Get_Size (Link.Distributions) loop
         This := Get (Link.Distributions, Feature);
         for Index in This.Distribution'Range loop
            Free (Link.Cache.all, This.Distribution (Index));
         end loop;
         Free (This);
      end loop;
      Finalize (Backward_Link (Link));
   end Finalize;

   procedure Finalize (Cache : in out Quality_Cache) is
      Ptr : Node_Classification_Ptr;
   begin
      for Index in 1..Get_Size (Cache.Map) loop
         Ptr := Get (Cache.Map, Index);
         Free (Ptr);
      end loop;
   end Finalize;

   function Get_Data
            (  Node  : Graph_Node_Ptr;
               Cache : access Quality_Cache
            )  return Node_Classification_Ptr is
      Index : constant Integer :=
                 Find (Cache.Map, Graph_Node_Ref (Node));
   begin
      if Index > 0 then
         return Get (Cache.Map, Index);
      else
         declare
            Result : Node_Classification_Ptr :=
                        new Node_Classification
                            (  Cardinality => Cache.Cardinality,
                               Cache       => Cache
                            );
         begin
            Add (Cache.Map, Graph_Node_Ref (Node), Result);
            Attach
            (  To_Backward_Link_Ptr (Result),
               To_Deposit_Ptr (Node)
            );
            Result.Quality_Sequence  := Node.Sequence - 1;
            Result.Universe_Sequence := Result.Quality_Sequence;
            return Result;
         exception
            when others =>
               Remove (Cache.Map, Graph_Node_Ref (Node));
               Free (Result);
               raise;
         end;
      end if;
   end Get_Data;

   function Get_Distribution
            (  Node  : Graph_Node_Ptr;
               Cache : access Quality_Cache
            )  return Classification_Ptr is
      Data : Node_Classification renames Get_Data (Node, Cache).all;
   begin
      if Data.Universe = null then
         Allocate (Cache.all, Data.Universe);
      elsif Node.Sequence = Data.Universe_Sequence then
         return Data.Universe;
      end if;
      Data.Universe.Possibility := (others => Confidence'First);
      Data.Universe.Necessity   := (others => Confidence'Last);
      declare
         Child : Graph_Node_Ptr;
      begin
         for Index in 1..Node.Cardinality loop
            Child := Get_Child (Node.all, Index);
            if Child /= null then
               Add
               (  Data.Universe.all,
                  Get_Distribution (Child, Cache)
               );
            end if;
         end loop;
      end;
      Data.Universe_Sequence := Node.Sequence;
      return Data.Universe;
   end Get_Distribution;

   function Get_Distribution
            (  Node  : Graph_Node_Ptr;
               Cache : not null access Quality_Cache
            )  return Classification is
   begin
      case Get_Type (Node.all) is
         when Tree_Leaf =>
            return Get_Distribution (Node.all);
         when Tree_Cluster | Tree_Branch =>
            return Get_Distribution (Node, Cache).all;
      end case;
   end Get_Distribution;

   function Get_Distribution
            (  Node    : Graph_Node_Ptr;
               Feature : Feature_Object'Class;
               Data    : access Node_Classification;
               Cache   : access Quality_Cache
            )  return Feature_Distribution_Ptr is
      Result : Feature_Distribution_Ptr;
      Index  : constant Integer :=
                  Find (Data.Distributions, Feature.Self);
   begin
      if Index > 0 then
         Result := Get (Data.Distributions, Index);
         if Result.Distribution_Sequence = Node.Sequence then
            return Result;
         end if;
      else
         Result := new Feature_Distribution (Feature.Cardinality);
         begin
            Result.Quality_Sequence := Node.Sequence - 1;
            Add (Data.Distributions, Feature.Self, Result);
         exception
            when others =>
               Free (Result);
               raise;
         end;
      end if;
      declare
         Child : Graph_Node_Ptr;
      begin
         for Index in 1..Node.Cardinality loop
            Child := Get_Child (Node.all, Index);
            if Child /= null then
               if Get_Type (Child.all) = Tree_Leaf then
                  Add
                  (  Result.Distribution,
                     Get_Distribution (Child.all),
                     Cache.all
                  );
               elsif Get_Feature (Child.all) = Feature.Self then
                  Add (Result.Distribution, Child.all, Cache);
               else
                  Add
                  (  Result.Distribution,
                     Get_Distribution
                     (  Child,
                        Feature,
                        Get_Data (Child, Cache),
                        Cache
                     ) .Distribution,
                     Cache.all
                  );
               end if;
            end if;
         end loop;
      end;
      Result.Distribution_Sequence := Node.Sequence;
      return Result;
   end Get_Distribution;
--
-- Get_Quality -- Quality of a feature space classification
--
--    Distribution - Feature space
--    Cardinality  - Number of classes
--
-- The  parameter  Distribution  is an array of classifications given to
-- each  feature  domain value. The formula used to estimate the quality
-- is  a mean for classification qualities of each class. The quality of
-- a class is defined  the  maximum  quality  over  the  feature  domain
-- values.  The  class i quality in a feature domain value is defiend as
-- follows. Let [Nk, Pk] be the classifications of the classes k in  the
-- point. Then
--     _  _                               _  _          _   _
--    [N, P] = Max [Nk, Pk]      W = max {P, N} / (1 + |P - N|)
--            k/=i
--
--         [min {Ni, Pi}, Pi]
--    Qi = ------------------
--             1 + 100 W
--
   function Get_Quality
            (  Distribution : Classification_Ptr_Array;
               Cardinality  : Positive
            )  return Divergence_Range is
      Factor  : constant := 100.0;
      Quality : array (1..Cardinality) of Divergence_Range :=
                  (others => Zero);
   begin
      for Value in Distribution'Range loop
         if Distribution (Value) /= null then
            declare
               This : Classification renames Distribution (Value).all;
            begin
               for I in 1..This.Cardinality loop
                  declare
                     Against : Fuzzy_Boolean := Certain_False;
                     Max     : Divergence_Range;
                  begin
                     for J in 1..This.Cardinality loop
                        if I /= J then
                           Against :=
                              (  Against
                              or (  Possibility => This.Possibility (J),
                                    Necessity   => This.Necessity   (J)
                              )  );
                        end if;
                     end loop;
                     Max.From := Divergence (This.Necessity   (I));
                     Max.To   := Divergence (This.Possibility (I));
                     if Max.To < Max.From then
                        Max.From := Max.To;
                     end if;
                     Max :=
                        (  Max
                        /  (  1.0
                           +  (  Factor
                              *  Divergence
                                 (  Against.Possibility
                                 or Against.Necessity
                                 )
                              /  (  1.0
                                 +  abs
                                    (  Divergence (Against.Possibility)
                                    -  Divergence (Against.Necessity)
                        )  )  )  )  );
                     case Quality (I) < Max is
                        when Intervals.True =>
                           Quality (I) := Max;
                        when Intervals.False =>
                           null;
                        when Intervals.Uncertain =>
                           Quality (I) := (Max + Quality (I)) / 2.0;
                     end case;
                  end;
               end loop;
            end;
         end if;
      end loop;
      declare
         Result : Divergence_Range := Zero;
      begin
         for I in Quality'Range loop
            Result := Result + Quality (I);
         end loop;
         return Result / Divergence (Cardinality);
      end;
   end Get_Quality;

   function Get_Quality
            (  Node    : Graph_Node_Ptr;
               Feature : Feature_Object'Class;
               Cache   : not null access Quality_Cache
            )  return Divergence_Range is
   begin
      if Get_Type (Node.all) = Tree_Leaf then
         return Zero;
      end if;
      declare
         Node_Data : constant Node_Classification_Ptr :=
                     Get_Data (Node, Cache);
      begin
         if Get_Feature (Node.all) = Feature.Self then
            if Node_Data.Quality_Sequence /= Node.Sequence then
               declare
                  Distribution : Classification_Ptr_Array
                                    (1..Node.Cardinality);
               begin
                  Add (Distribution, Node.all, Cache);
                  Node_Data.Node_Quality :=
                     Get_Quality (Distribution, Cache.Cardinality);
                  for Index in Distribution'Range loop
                     Free (Cache.all, Distribution (Index));
                  end loop;
               exception
                  when others =>
                     for Index in Distribution'Range loop
                        Free (Cache.all, Distribution (Index));
                     end loop;
                     raise;
               end;
               Node_Data.Quality_Sequence := Node.Sequence;
            end if;
            return Node_Data.Node_Quality;
         else
            declare
               Feature_Data : Feature_Distribution renames
                  Get_Distribution
                  (  Node,
                     Feature,
                     Node_Data,
                     Cache
                  ) .all;
            begin
               if Feature_Data.Quality_Sequence = Node.Sequence then
                  return Feature_Data.Feature_Quality;
               end if;
               Feature_Data.Feature_Quality :=
                  Get_Quality
                  (  Feature_Data.Distribution,
                     Cache.Cardinality
                  );
               Feature_Data.Quality_Sequence := Node.Sequence;
               return Feature_Data.Feature_Quality;
            end;
         end if;
      end;
   end Get_Quality;

   function Get_Quality
            (  Node    : Graph_Node_Ptr;
               Feature : Feature_Handle;
               Cache   : not null access Quality_Cache
            )  return Divergence_Range is
   begin
      return Get_Quality (Node, Ptr (Feature).all, Cache);
   end Get_Quality;

   function Image (Value : Divergence_Range) return String is
   begin
      return
      (  "["
      &  Strings_Edit.Floats.Image (Float (Value.From))
      &  ", "
      &  Strings_Edit.Floats.Image (Float (Value.To))
      &  "]"
      );
   end Image;

   function To_Quality (Value : Fuzzy_Boolean)
      return Divergence_Range is
   begin
      return
      (  From => Divergence (Value.Necessity),
         To   => Divergence (Value.Possibility)
      );
   end To_Quality;
--
-- Cov -- Covariance of two values
--
--    X, Y - Fuzzy logical values
--
-- The covariance is defined as conditional equivalence:
--
--    (X <= Y and Y <= X) and (X or Y)
--
-- Returns :
--
--    The covariance of
--
-- function Cov (X, Y : Fuzzy_Boolean) return Divergence_Range is
--    pragma Inline (Cov);
--    Result : Divergence_Range;
-- begin
--    Result.From := Divergence (X.Necessity   xor Y.Necessity);
--    Result.To   := Divergence (X.Possibility xor Y.Possibility);
--    if Result.From > Result.To then
--       return (From => Result.To, To => Result.From);
--    else
--       return Result;
--    end if;
-- end Cov;
--
-- function Cov (X, Y : Fuzzy_Boolean) return Divergence_Range is
--    pragma Inline (Cov);
--    Result : Divergence_Range;
-- begin
--    Result.From :=
--       Divergence
--       (  (X.Necessity or not Y.Necessity)
--       and
--          (not X.Necessity or Y.Necessity)
--       and
--          (X.Necessity or Y.Necessity)
--       );
--    Result.To :=
--       Divergence
--       (  (X.Possibility or not Y.Possibility)
--       and
--          (not X.Possibility or Y.Possibility)
--       and
--          (X.Possibility or Y.Possibility)
--       );
--    return Result;
-- end Cov;
--
-- function Cov (X : Fuzzy_Boolean) return Divergence_Range is
--    pragma Inline (Cov);
--    Result : Divergence_Range;
-- begin
--    Result.From := Divergence (X.Necessity and not X.Necessity);
--    Result.To   := Divergence (X.Possibility);
--    return Result;
-- end Cov;
--
-- function Cov (Left, Right : Classification)
--    return Divergence_Range is
--    pragma Inline (Cov);
--    Norm   : constant Divergence := Divergence (Left.Cardinality);
--    Result : Divergence_Range    := Zero;
--    Equals : Divergence_Range;
-- begin
--    for Index in 1..Left.Cardinality loop
--       Equals :=
--          Cov
--          (  Fuzzy_Boolean'
--             (  Possibility => Left.Possibility (Index),
--                Necessity   => Left.Necessity (Index)
--             ),
--             Fuzzy_Boolean'
--             (  Possibility => Right.Possibility (Index),
--                Necessity   => Right.Necessity (Index)
--          )  );
--       Result.From := Result.From + Equals.From;
--       Result.To   := Result.To   + Equals.To;
--    end loop;
--    Result.From := Result.From / Norm;
--    Result.To   := Result.To   / Norm;
--    return Result;
-- end Cov;
--
-- function Cov (Left : Classification) return Divergence_Range is
--    pragma Inline (Cov);
--    Norm   : constant Divergence := Divergence (Left.Cardinality);
--    Result : Divergence_Range    := Zero;
--    Equals : Divergence_Range;
-- begin
--    for Index in 1..Left.Cardinality loop
--       Equals :=
--          Cov
--          (  Fuzzy_Boolean'
--             (  Possibility => Left.Possibility (Index),
--                Necessity   => Left.Necessity   (Index)
--          )  );
--       Result.From := Result.From + Equals.From;
--       Result.To   := Result.To   + Equals.To;
--    end loop;
--    Result.From := Result.From / Norm;
--    Result.To   := Result.To   / Norm;
--    return Result;
-- end Cov;
--
-- function Class_Difference
--          (  Distribution : Classification_Ptr_Array;
--             Cardinality  : Positive
--          )  return Divergence_Range is
--    Norm   : constant Divergence :=
--                      (  Divergence (Distribution'Length)
--                      *  Divergence (Distribution'Length - 1)
--                      );
--    Result : Divergence_Range := Zero;
-- begin
--    for I in Distribution'Range loop
--       if Distribution (I) = null then
--          for J in I + 1..Distribution'Last loop
--             if Distribution (J) /= null then
--                Result := Result + Cov (Distribution (J).all);
--             end if;
--          end loop;
--       else
--          for J in I + 1..Distribution'Last loop
--             if Distribution (J) = null then
--                Result := Result + Cov (Distribution (I).all);
--             else
--                Result :=
--                   (  Result
--                   +  Cov (Distribution (I).all, Distribution (J).all)
--                   );
--             end if;
--          end loop;
--       end if;
--    end loop;
--    Result.From := 1.0 - Result.To / Norm;
--    Result.To   := 1.0 - Result.From / Norm;
--    return Result;
-- end Class_Difference;
--
-- function Class_Difference
--          (  Distribution : Classification_Ptr_Array;
--             Cardinality  : Positive
--          )  return Divergence_Range is
--    Result : Divergence_Range := (0.0, 0.0);
-- begin
--    for Class in 1..Cardinality loop
--       declare
--          Max : Divergence_Range := (0.0, 0.0);
--       begin
--          for Value in Distribution'Range loop
--             if Distribution (Value) /= null then
--                declare
--                   This  : Classification renames
--                              Distribution (Value).all;
--                   Ratio : Divergence_Range := (1.0, 1.0);
--                begin
--                   for Other in 1..Cardinality loop
--                      if Other /= Class then
--                         Ratio :=
--                            Ratio + To_Divergence_Range (This, Other);
--                      end if;
--                   end loop;
--                   Ratio := To_Divergence_Range (This, Class) / Ratio;
--                   case Ratio > Max is
--                      when Intervals.True =>
--                         Max := Ratio;
--                      when Intervals.False =>
--                         null;
--                      when Intervals.Uncertain =>
--                         Max := (Max + Ratio) / 2.0;
--                   end case;
--                end;
--             end if;
--          end loop;
--          Result := Result + Max;
--       end;
--    end loop;
--    return Result / Divergence (Cardinality);
-- end Class_Difference;

end Fuzzy.Graph.Separators;
