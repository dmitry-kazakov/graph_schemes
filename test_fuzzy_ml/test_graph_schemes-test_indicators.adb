--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Indicators          Luebeck            --
--  Separate body implementation                   Winter, 2003       --
--                                                                    --
--                                Last revision :  09:46 08 Oct 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--
--
--  This is a small test procedure for testing indicators.
--
with Indicator.Text_IO;  use Indicator.Text_IO;

separate (Test_Graph_Schemes) procedure Test_Indicators is
begin
   declare
      Progress : Timed_Bar (30);
   begin
      New_Line;
      Put ("Testing bar: ");
      Reset (Progress, 300);
      for Index in 1..300 loop
         delay 0.01;
         Check (Progress);
      end loop;
      Done (Progress);
   end;
   declare
      Progress : Timed_Bar (30);
   begin
      New_Line;
      Put ("Testing bar: ");
      Reset (Progress, 7);      -- Main operation
         Reset (Progress, 70);     -- Suboperation
         for Index in 1..80 loop
            delay 0.1;
            Check (Progress);
         end loop;
         Done (Progress);          -- End of suboperation
      for Index in 1..5 loop
         delay 0.5;
         Check (Progress);
      end loop;
         Reset (Progress, 50);     -- Suboperation
         for Index in 1..50 loop
            delay 0.2;
            Check (Progress);
         end loop;
         Done (Progress);          -- End of suboperation
      Done (Progress);          -- End of the main operation
   end;
   declare
      Text     : aliased String := " Done";
      Progress : Counter (Text'Access);
   begin
      New_Line;
      Put ("Testing counter: ");
      Reset (Progress);
      for Index in 1..10 loop
         delay 0.1;
         Check (Progress);
      end loop;
      Done (Progress);
      New_Line;
   end;
end Test_Indicators;
