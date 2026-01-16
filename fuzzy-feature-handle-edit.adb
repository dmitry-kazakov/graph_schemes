--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Edit                   Luebeck            --
--  Implementation                                 Spring, 2002       --
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

with Fuzzy.Feature.Edit;  use Fuzzy.Feature.Edit;

package body Fuzzy.Feature.Handle.Edit is

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Interval;
                Parameters : Input_Parameters'Class := Input_Defaults
             )  is
   begin
      Get (Source, Pointer, Ptr (Feature).all, Value, Parameters);
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Fuzzy.Set;
                Parameters : Input_Parameters'Class := Input_Defaults
             )  is
   begin
      Get (Source, Pointer, Ptr (Feature).all, Value, Parameters);
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Intuitionistic.Set;
                Parameters : Input_Parameters'Class := Input_Defaults
             )  is
   begin
      Get (Source, Pointer, Ptr (Feature).all, Value, Parameters);
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Intuitionistic.Classification;
                Parameters : Input_Parameters'Class := Input_Defaults
             )  is
   begin
      Get (Source, Pointer, Ptr (Feature).all, Value, Parameters);
   end Get;

   procedure Get_Domain
             (  Feature    : Feature_Handle;
                Domain     : out Domain_Description'Class;
                Parameters : Output_Parameters'Class := Output_Defaults
             )  is
      Object : Feature_Object'Class renames Ptr (Feature).all;
   begin
      Erase (Domain);
      for Index in 1..Object.Cardinality loop
         Add
         (  Domain,
            Image (Object, Interval'(Index, Index), Parameters),
            Index,
            Unchecked => True
         );
      end loop;
   end Get_Domain;

   function Image
            (  Feature    : Feature_Handle;
               Value      : Interval;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Feature).all, Value, Parameters);
   end Image;

   function Image
            (  Feature    : Feature_Handle;
               Value      : Fuzzy.Set;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Feature).all, Value, Parameters);
   end Image;

   function Image
            (  Feature    : Feature_Handle;
               Value      : Domain_Subset;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Feature).all, Value, Parameters);
   end Image;

   function Image
            (  Feature    : Feature_Handle;
               Value      : Intuitionistic.Set;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Feature).all, Value, Parameters);
   end Image;

   function Image
            (  Feature    : Feature_Handle;
               Value      : Intuitionistic.Classification;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Feature).all, Value, Parameters);
   end Image;

   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Interval is
   begin
      return Value (Source, Ptr (Feature).all, Parameters);
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Fuzzy.Set is
   begin
      return Value (Source, Ptr (Feature).all, Parameters);
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Intuitionistic.Set is
   begin
      return Value (Source, Ptr (Feature).all, Parameters);
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Intuitionistic.Classification is
   begin
      return Value (Source, Ptr (Feature).all, Parameters);
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Interval;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Ptr (Feature).all,
         Value,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Domain_Subset;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Ptr (Feature).all,
         Value,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Fuzzy.Set;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Ptr (Feature).all,
         Value,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Intuitionistic.Set;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Ptr (Feature).all,
         Value,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Intuitionistic.Classification;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Ptr (Feature).all,
         Value,
         Parameters,
         Field,
         Justify,
         Fill
      );
   end Put;

end Fuzzy.Feature.Handle.Edit;
