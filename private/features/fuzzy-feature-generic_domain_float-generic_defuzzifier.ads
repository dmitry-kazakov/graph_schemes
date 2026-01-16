--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier                      Autumn, 2005       --
--  Interface                                                         --
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
-- This   generic   child   package   defines  the  abstract  base  type
-- Defuzzifier_Object  for  all  defuzzifier  types  compatible with the
-- floating point features provided by the parent package.
--
with Object.Archived;  use Object.Archived;

with Ada.Unchecked_Conversion;
with Fuzzy.Feature.Generic_Domain_Float;
with Object.Handle;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier is
   pragma Elaborate_Body
          (  Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier
          );
   use Fuzzy_Measures;
   use Variable_Measures;
--
-- Defuzzifier_Class -- The prefix of all feature object classes
--
   Defuzzifier_Class : constant String := "Defuzzifier." & Suffix;
--
-- Defuzzifier_Object -- The abstract base type for defuzzifier types
--
   type Defuzzifier_Object is abstract new Deposit with null record;
   type Defuzzifier_Object_Ptr is access Defuzzifier_Object'Class;
   for Defuzzifier_Object_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
--
-- Defuzzify -- Defuzzify a linguistic variable
--
--    Defuzzifier - The defuzzifier object
--    Value       - A linguistic variable
--
-- Returns :
--
--    Defuzzifier value
--
-- Exceptions :
--
--    Constraint_Error - Defuzzification failure
--
   function Defuzzify
            (  Defuzzifier : Defuzzifier_Object;
               Value       : Variable_Measure
            )  return Measure is abstract;
--
-- Get_Method_Name -- Get the method name
--
--    Defuzzifier - The defuzzifier object
--
-- Returns :
--
--    The name of
--
   function Get_Method_Name
            (  Defuzzifier : Defuzzifier_Object
            )  return String is abstract;
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Defuzzifier : Defuzzifier_Object)
      return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Defuzzifier : in out Defuzzifier_Object);
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Defuzzifier_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Defuzzifier_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a defuzzifier
--
   function To_Defuzzifier_Object_Ptr (Ptr : Deposit_Ptr)
      return Defuzzifier_Object_Ptr;

private
--
-- Handles -- To Feature_Objects
--
   package Handles is
      new Object.Handle (Defuzzifier_Object, Defuzzifier_Object_Ptr);

   pragma Inline (To_Defuzzifier_Object_Ptr);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier;
