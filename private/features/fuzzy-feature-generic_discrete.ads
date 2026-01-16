--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Discrete              Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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
--
--  This  generic  package is used to create discrete feature types. The
--  package parameter is an  discrete  type  to  represent  the  feature
--  domain.
--
--      Enumeration - A discrete type
--
--  All   generic   discrete  features  uses  the  class  registered  in
--  Fuzzy.Feature.Discrete. Such features being stored and then restored
--  mutate to Fuzzy.Feature.Discrete.Discrete_Feature_Object.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;

with Fuzzy.Feature.Independent;

generic
   type Enumeration is (<>);
package Fuzzy.Feature.Generic_Discrete is
   subtype Domain_Type is Enumeration;
--
-- Create -- Create a discrete feature
--
--   Name    - Of the feature
--
-- Returns :
--
--   A handle to the newly created feature
--
   function Create (Name : String) return Feature_Handle;
--
-- Is_Discrete -- Check if the feature is a discrete feature
--
--    Feature - Object or a handle to
--
-- Returns :
--
--    True if Feature is a valid handle to a discrete feature
--
   function Is_Discrete (Feature : Feature_Object'Class) return Boolean;
   function Is_Discrete (Feature : Feature_Handle) return Boolean;

private
   use Independent;
   use Object.Archived;

   type Discrete_Feature_Object is
      new Independent_Feature_Object with null record;
   type Discrete_Feature_Ptr is access Discrete_Feature_Object'Class;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Discrete_Feature_Object)
      return String;
   pragma Inline (Get_Class);
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Discrete_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object
             );

end Fuzzy.Feature.Generic_Discrete;
