--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Discrete                      Luebeck            --
--  Interface                                      Spring, 2002       --
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
--  This  package  is  used  to  create  discrete  feature types. Unlike
--  Fuzzy.Feature.Generic_Discrete it is not generic. Thus  features  of
--  this type can be created dynamically.
--
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;

with Fuzzy.Feature.Independent;

package Fuzzy.Feature.Discrete is
   pragma Elaborate_Body (Fuzzy.Feature.Discrete);

   Discrete_Class : constant String := Feature_Class & "Discrete";
--
-- Create -- Create a discrete feature
--
--    Name   - Of the feature
--    Domain - Of the domain set of the feature
--
-- The parameter Domain is the list  of  feature  domain  element  names
-- separated  by  commas.  Names are case-insensitive, they start from a
-- letter and contain only letters, digits,  spaces  and  underlines.  A
-- name  may  not  end  with  a  space  or underline. Also if a space or
-- underline appears the next character shall be different.  Commas  can
-- be surrounded by spaces and tabs in Domain. The whole string shall be
-- matched.  Otherwise,  Data_Error is propagated. It is also propagated
-- if Domain contains duplicated element names.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Data_Error - Syntax error of the string Domain
--    Name_Error - Duplicated elements
--    End_Error  - No elements found in the string Domain
--
   function Create (Name : String; Domain : String)
      return Feature_Handle;
--
-- Create -- Create a discrete feature
--
--    Name   - Of the feature
--    Domain - Of the domain set of the feature
--
-- The   parameter   Domain  is  the  feature  domain  as  described  in
-- Fuzzy.Abstract_Edit.Named.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Constraint_Error - No elements found in Domain
--
   function Create (Name : String; Domain : Domain_Description'Class)
      return Feature_Handle;
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

   type Domain_Description_Ptr is access Domain_Description;
   type Discrete_Feature_Object is
      new Independent_Feature_Object with
   record
      Domain : Domain_Description_Ptr;
   end record;
--
-- Finalize -- Destructor
--
--    Feature - To be destroyed
--
   overriding
   procedure Finalize (Feature : in out Discrete_Feature_Object);
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
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
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

end Fuzzy.Feature.Discrete;
