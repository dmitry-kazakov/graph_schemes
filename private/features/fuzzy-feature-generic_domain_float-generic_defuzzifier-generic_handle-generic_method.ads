--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier.Generic_Handle.      Autumn, 2005       --
--           Generic_Method                                           --
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
-- This generic package provides defuzzifiers based on a defuzzification
-- method. It has two generic parameters:
--
-- (o)  Name is the name of the method;
-- (o)  Method is the defuzzification function which  takes  a  variable
--      and returns the defuzzified number.
--
generic
   Name : String;
   with function Method (Value : Variable) return Domain_Number;
package Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
        Generic_Handle.Generic_Method is
   pragma Elaborate_Body
          (  Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
             Generic_Handle.Generic_Method
          );
   Name_Of : String renames Name;
--
-- Create -- Create a defuzzifier
--
-- Returns :
--
--    The handle to the defuzzifier
--
   function Create return Defuzzifier_Handle;

private
--
-- Defuzzification_Method_Object -- The object
--
   type Defuzzification_Method_Object is
      new Defuzzifier_Object with null record;
--
-- Defuzzify -- Overrides Fuzzy.Defuzzifier
--
   overriding
   function Defuzzify
            (  Defuzzifier : Defuzzification_Method_Object;
               Value       : Variable_Measures.Variable_Measure
            )  return Fuzzy_Measures.Measure;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Defuzzifier : Defuzzification_Method_Object)
      return String;
--
-- Get_Method_Name -- Overrides Object.Archived...
--
   overriding
   function Get_Method_Name
            (  Defuzzifier : Defuzzification_Method_Object
            )  return String;
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source      : String;
                Pointer     : in out Integer;
                Class       : String;
                List        : Deposit_Container'Class;
                Defuzzifier : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Defuzzifier : Defuzzification_Method_Object
             );

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

   pragma Inline (Get_Class);
   pragma Inline (Get_Method_Name);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle.Generic_Method;
