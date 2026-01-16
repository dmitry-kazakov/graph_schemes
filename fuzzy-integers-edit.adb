--                                                                    --
--  package Fuzzy.Integers.Edit     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Fuzzy.Numbers.Edit;

package body Fuzzy.Integers.Edit is
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Number;
                Base    : in NumberBase := 10
             )  is
   begin
      Integer_Edit.Get (Source, Pointer, Value, Base);
   end Get;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Number;
                Base        : in NumberBase := 10;
                RelSmall    : in Positive   := MaxSmall;
                AbsSmall    : in Integer    := -MaxSmall;
                Field       : in Natural    := 0;
                Justify     : in Alignment  := Left;
                Fill        : in Character  := ' '
             )  is
   begin
      Integer_Edit.Put
      (  Destination,
         Pointer,
         Value,
         Base,
         Field   => Field,
         Justify => Justify,
         Fill    => Fill
      );
   end Put;

   package Edit is new Fuzzy_Numbers.Edit;

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Fuzzy_Integer;
                Base    : in NumberBase := 10
             )  renames Edit.Get;

   function Value
            (  Source : in String;
               Base   : in NumberBase := 10
            )  return Fuzzy_Integer renames Edit.Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Fuzzy_Integer;
                Base        : in NumberBase := 10;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
   begin
      Edit.Put
      (  Destination,
         Pointer,
         Value,
         Base,
         Field   => Field,
         Justify => Justify,
         Fill    => Fill
      );
   end Put;

   function Image
            (  Value : in Fuzzy_Integer;
               Base  : in NumberBase := 10
            )  return String is
   begin
      return Edit.Image (Value, Base);
   end Image;

end Fuzzy.Integers.Edit;
