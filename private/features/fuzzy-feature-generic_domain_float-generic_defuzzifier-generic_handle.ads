--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float.         Luebeck            --
--        Generic_Defuzzifier.Generic_Handle       Autumn, 2005       --
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
--  This  package  defines  the type Defuzzifier_Handle which is used to
--  access defuzzifier objects. It defines the  public  interface  of  a
--  defuzzifier.
--
with Deposit_Handles;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
        Generic_Handle is
   pragma Elaborate_Body
          (  Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
             Generic_Handle
          );
   use Variable_Measures;
   use Fuzzy_Measures;
--
-- Defuzzifier_Handle -- A handle to feature
--
   type Defuzzifier_Handle is tagged private;
--
-- Defuzzify -- Defuzzify a linguistic variable
--
--    Defuzzifier - A handle to defuzzifier
--    Value       - A linguistic variable
--
-- Returns :
--
--    Defuzzifier value
--
-- Exceptions :
--
--    Constraint_Error - Defuzzification failure, or an invalid handle
--
   function Defuzzify
            (  Defuzzifier : Defuzzifier_Handle;
               Value       : Variable_Measure
            )  return Measure;
--
-- Delete -- Deletion request
--
--    Defuzzifier - A handle to the feature
--
-- This  procedure requests deletion of Defuzzifier. Defuzzifier becomes
-- an  invalid  handle.  The  defuzzifier  object  itself  is deleted if
-- possible. Nothing happens if Defuzzifier is not a valid handle.
--
   procedure Delete (Defuzzifier : in out Defuzzifier_Handle);
--
-- Get_Class -- Get the defuzzifier class
--
--    Defuzzifier - The defuzzifier handle
--
-- Returns :
--
--    The defuzzifier class
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Class (Defuzzifier : Defuzzifier_Handle) return String;
--
-- Get_Method_Name -- Get the method name
--
--    Defuzzifier - The defuzzifier handle
--
-- Returns :
--
--    The name of
--
   function Get_Method_Name
            (  Defuzzifier : Defuzzifier_Handle
            )  return String;
--
-- Invalidate -- Detach handle from the object
--
--    Defuzzifier - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Defuzzifier : in out Defuzzifier_Handle);
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Defuzzifier - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Defuzzifier : Defuzzifier_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Defuzzifier - The handle
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Defuzzifier : Defuzzifier_Handle)
      return Defuzzifier_Object_Ptr;
--
-- Ref -- Get handle to a feature object
--
--    Defuzzifier - The defuzzifier object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Defuzzifier : Defuzzifier_Object_Ptr)
      return Defuzzifier_Handle;
--
-- Set -- Set a handle to a defuzzifier object
--
--    Handle      - The handle to set
--    Defuzzifier - The node object
--
   procedure Set
             (  Handle : in out Defuzzifier_Handle;
                Defuzzifier : Defuzzifier_Object_Ptr
             );
--
-- To_Defuzzifier_Handle -- Handle conversion
--
--    Defuzzifier - A handle to
--
-- Returns :
--
--    A handle to the defuzzifier
--
-- Exceptions :
--
--    Constraint_Error - The object is not a defuzzifier, or an  invalid
--                       handle
--
   function To_Defuzzifier_Handle
            (  Defuzzifier : Deposit_Handles.Handle
            )  return Defuzzifier_Handle;
--
-- To_Deposit_Handle -- Pointer conversion
--
--    Defuzzifier - A handle to
--
-- Returns :
--
--    Handle to archived object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Deposit_Handle (Defuzzifier : Defuzzifier_Handle)
      return Deposit_Handles.Handle;
--
-- No_Defuzzifier -- An invalid defuzzifier handle
--
   No_Defuzzifier : constant Defuzzifier_Handle;

private
   pragma Inline (Get_Class);
   pragma Inline (Get_Method_Name);
   pragma Inline (Invalidate);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Set);
   pragma Inline (To_Defuzzifier_Handle);
   pragma Inline (To_Deposit_Handle);

   type Defuzzifier_Handle is new Handles.Handle with null record;

   No_Defuzzifier : constant Defuzzifier_Handle :=
                       (Handles.Handle with null record);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Defuzzifier.
    Generic_Handle;
