--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Sets                      Luebeck            --
--  Interface                                      Summer, 2003       --
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
--
--  This generic package is  used  to  create  and  deal  with  sets  of
--  linguistic  variables.  It  is  generic but has no parameters of its
--  own.  A set of linguistic variables is ordered. Each variable in the
--  set has  unique  name  and  number  1...  The  names  of  linguistic
--  variables are case-insensitive when  matched.  A  valid  name  shall
--  start with a letter and contain only letters, numbers, space,  minus
--  and  underline.  It may not end with a space, minus or underline. If
--  one  of  them  appears  in  the  name  the  next  character shall be
--  different.
--
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Generic_Unbounded_Array;
with Object.Handle;

generic
package Fuzzy.Linguistics.Sets is
--
-- Linguistic_Set -- A set of lingustic variables
--
   type Linguistic_Set is private;
   type Domain_Description_Ptr is access constant Domain_Description;
--
-- Defuzzification_Result_Class -- Types of results
--
--    Empty     - The result is an empty set
--    Singleton - The result is a numeric number (point)
--    Segment   - The result is a finite interval of numbers
--    Subset    - Any other
--
   type Defuzzification_Result_Class is
        (  Empty,
           Singleton,
           Segment,
           Subset
        );
--
-- Defuzzification_Result -- A result of defuzzification process
--
--    Class - The type of
--
   type Defuzzification_Result
        (  Class : Defuzzification_Result_Class
        )  is
   record
      case Class is
         when Empty     => null;
         when Singleton => Singleton : Number;
         when Segment   => Segment   : Interval;
         when Subset    => Subset    : Variable;
      end case;
   end record;
--
-- Accumulate -- A combination of weighted variables from a set
--
--    Set          - The set of linguistic variables
--    Distribution - A fuzzy set (weights)
--
-- The  result  is  a  linguistic  variable  evaluated   as   union   of
-- intersections Set (i) and Distribution (i). The result  is  empty  if
-- Set  is empty. Constraint_Error is propagated if the cardinalities of
-- Set and the domain set of Distribution differ.
--
-- Result :
--
--    Accumulated result
--
-- Exceptions :
--
--    Constraint_Error - Cardinality error
--
   function Accumulate
            (  Set          : Linguistic_Set;
               Distribution : Fuzzy.Set
            )  return Variable;
--
-- Add -- A variable into a set
--
--    Set   - The set to be modified
--    Name  - The name of the variable
--    Value - The variable
--
-- The variable Value is  added  to  the  end  of  Set  under  the  name
-- specified by the parameter Name.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name
--    Name_Error       - The name is already used by another variable
--
   procedure Add
             (  Set   : in out Linguistic_Set;
                Name  : String;
                Value : Variable
             );
--
-- Classify -- A value
--
--    Set   - A set of linguistic variables
--    Value - The value to be classified (fuzzified)
--
-- The result is a classification of Value in terms of Set elements. The
-- value can be either a crisp number or a crisp  interval  or  a  fuzzy
-- number or a variable.
--
-- Returns :
--
--    The classification of Value
--
-- Exceptions :
--
--    Constraint_Error - Empty set
--
   function Classify
            (  Set   : Linguistic_Set;
               Value : Number
            )  return Classification;
   function Classify
            (  Set   : Linguistic_Set;
               Value : Interval
            )  return Classification;
   function Classify
            (  Set   : Linguistic_Set;
               Value : Fuzzy_Float
            )  return Classification;
   function Classify
            (  Set   : Linguistic_Set;
               Value : Variable
            )  return Classification;
--
-- Defuzzify -- A classification of a lingustic set
--
--    Set   - The set of linguistic variables
--    Value - A classification on Set
--
-- The  result  of  the  function  is  defuzzified  Value, i.e. a set of
-- numbers  which  being  fuzzified  (see Classify) might produce Value.
-- Constraint_Error is propagated when the cardinality of Value  differs
-- from one of Set.
--
-- Returns :
--
--    Defuzzified Value
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality
--
   function Defuzzify
            (  Set   : Linguistic_Set;
               Value : Classification
            )  return Defuzzification_Result;
--
-- Empty -- Empty set
--
-- Returns :
--
--    Empty set
--
   function Empty return Linguistic_Set;
--
-- Erase -- Make a set empty
--
--    Set - To be erased
--
   procedure Erase (Set : in out Linguistic_Set);
--
-- Equal -- Compare two sets (equality)
--
--    Left  - The first argument
--    Right - The second argument
--    Eps   - Tolerance level
--
-- Two sets are equal  if  they  have  same  number  of  variables,  the
-- corresponding variables have same names (case-insensitive comparison)
-- and  the  variables are equivalent up to Eps (see Equal in the parent
-- package).
--
-- Returns :
--
--    True if Left and Right are same
--
   function Equal
            (  Left, Right : Linguistic_Set;
               Eps         : Number'Base := 0.0
            )  return Boolean;
--
-- Get -- Get a linguistic variable by its domain value index
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    Constraint_Error - There is no such variable
--
   function Get
            (  Set   : Linguistic_Set;
               Index : Positive
            )  return Variable;
--
-- Get -- Get index of a variable by its name
--
--    Set  - A set of linguistic variables
--    Name - Of the variable
--
-- Returns :
--
--    The domain value index of Name
--
-- Exceptions :
--
--    End_Error - No variable matched
--
   function Get
            (  Set  : Linguistic_Set;
               Name : String
            )  return Positive;
--
-- Get -- Get a linguistic variable by its domain value name
--
--    Set  - A set of linguistic variables
--    Name - Of the variable
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    End_Error - No variable matched
--
   function Get (Set : Linguistic_Set; Name : String)
      return Variable;
--
-- Get -- Get index of a variable by its name
--
--    Source  - String to parse
--    Pointer - Starting string position
--    Set     - A set of linguistic variables
--    Index   - Of the variable
--
-- This procedure parses the string Source. If a name  of  a  linguistic
-- variable from  Set  is  successfully  matched  starting  from  Source
-- (Pointer),  Pointer is advanced to the position following the matched
-- name and Index is set to respond to the domain value associated  with
-- the variable.
--
-- Exceptions :
--
--    End_Error    - No variable matched
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Index   : out Positive
             );
--
-- Get_Cardinality -- Get number of elements
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    The number of variables in the set
--
   function Get_Cardinality (Set : Linguistic_Set) return Natural;
--
-- Get_Domain -- Get the domain set description
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    Pointer to the domain set description
--
-- Exceptions :
--
--    Constraint_Error - An empty set
--
   function Get_Domain (Set : Linguistic_Set)
      return Domain_Description_Ptr;
--
-- Get_Name -- Get name of a linguistic variable
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Returns :
--
--    The name of the variable
--
-- Exceptions :
--
--    Constraint_Error - There is no such variable
--
   function Get_Name (Set : Linguistic_Set; Index : Positive)
      return String;
--
-- Get_Use_Count -- Get the share count of the body
--
--    Set   - A set of linguistic variables
--
-- Returns :
--
--    The number of references to the set
--
   function Get_Use_Count (Set : Linguistic_Set) return Natural;
--
-- Insert -- A variable into a set
--
--    Set   - The set to be modified
--    Index - The value (domain set value index 1..)
--    Name  - The name of the variable
--    Value - The variable
--
-- The variable Value is inserted into Set under the name  specified  by
-- the parameter Name and domain value Index.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name or illegal index
--    Name_Error       - The name is already used by another variable
--
   procedure Insert
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String;
                Value : Variable
             );
--
-- Is_Empty -- Empty set test
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    True if the set is empty
--
   function Is_Empty (Set : Linguistic_Set) return Boolean;
--
-- Move -- Move a variable in the set
--
--    Set  - A set of linguistic variables
--    From - The value (domain set value index 1..)
--    To   - The new value
--
-- This procedure moves the variable from the domain set index  From  to
-- To. The indices of the variables in the range From..To will change as
-- well.
--
-- Exceptions :
--
--    Contraint_Error - Wrong indices
--
   procedure Move
             (  Set  : in out Linguistic_Set;
                From : Positive;
                To   : Positive
             );
--
-- Remove -- Remove variable from the set
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove (Set : in out Linguistic_Set; Index : Positive);
--
-- Remove -- Remove variable from the set by its name
--
--    Set  - A set of linguistic variables
--    Name - To be removed
--
-- If the Name is not in the set, nothing happens.
--
   procedure Remove (Set : in out Linguistic_Set; Name : String);
--
-- Rename -- The domain value
--
--    Set   - The set to be modified
--    Index - The value (domain set value index 1..)
--    Name  - The name of the variable
--
-- This procedure renames the domain value specified by Index.
--
-- Exceptions :
--
--    Contraint_Error - Wrong index or name
--    Name_Error      - The name is already used by another variable
--
   procedure Rename
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String
             );
--
-- Replace -- Replace a variable
--
--    Set     - The set to be modified
--    Index   - The value (domain set value index 1..)
--  [ Name  ] - The new name of the domain value
--    Value   - The new domain value
--
-- This  procedure  replaces  the domain value of a variable of the set.
-- When the parameter Name is not specified the domain value name is not
-- changed. Otherwise both the name and the value are changed.
--
-- Exceptions :
--
--    Contraint_Error - Wrong index or name
--    Name_Error      - The name is already used by another variable
--
   procedure Replace
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Name  : String;
                Value : Variable
             );
   procedure Replace
             (  Set   : in out Linguistic_Set;
                Index : Positive;
                Value : Variable
             );
--
-- Swap -- Variables in the set
--
--    Set     - The set to be modified
--    Index_1 - The value (domain set value index 1..)
--    Index_2 - The value (domain set value index 1..)
--
-- This  procedure  swaps  two  variables of Set specified by the domain
-- value indices. Nothing happens when Index_1 = Index_2.
--
-- Exceptions :
--
--    Constraint_Error - Illegal indices
--
   procedure Swap
             (  Set     : in out Linguistic_Set;
                Index_1 : Positive;
                Index_2 : Positive
             );
--
-- To_Set -- Convert a value to a subset of linguistic variables
--
--    Set   - A set of linguistic variables
--    Value - The value to be converted
--
-- The result is an estimation of Value in terms of  Set  elements.  The
-- value can be either a crisp number or a crisp  interval  or  a  fuzzy
-- number or a variable.
--
-- Returns :
--
--    The estimation of Value
--
-- Exceptions :
--
--    Constraint_Error - Empty set
--
   function To_Set
            (  Set   : Linguistic_Set;
               Value : Number
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Linguistic_Set;
               Value : Interval
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Linguistic_Set;
               Value : Fuzzy_Float
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Linguistic_Set;
               Value : Variable
            )  return Fuzzy.Intuitionistic.Set;
--
-- Array_Of_Variables -- Array of variables
--
   type Array_Of_Variables  is array (Positive range <>) of Variable;
--
-- Unbounded_Arrays -- Unbounded array of variables
--
   package Unbounded_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Variable,
             Object_Array_Type => Array_Of_Variables,
             Null_Element      => Empty
          );
private
   pragma Inline (Classify);
   pragma Inline (Empty);
   pragma Inline (Get);
   pragma Inline (Get_Cardinality);
   pragma Inline (Get_Domain);
   pragma Inline (Get_Name);
   pragma Inline (Is_Empty);
   pragma Inline (To_Set);

   use Unbounded_Arrays;
--
-- Set_Body -- The body of a set
--
   type Set_Body is new Object.Entity with record
      List   : Unbounded_Array;
      Domain : aliased Domain_Description;
   end record;
   type Set_Body_Ptr is access Set_Body'Class;
--
-- Clone -- A body of a set
--
   function Clone (Set : Set_Body) return Set_Body_Ptr;

   package Handles is new Object.Handle (Set_Body'Class, Set_Body_Ptr);
--
-- Linguistic_Set -- The set of linguistic variables
--
-- It is a handle to an instance of Set_Body.
--
   type Linguistic_Set is new Handles.Handle with null record;
--
-- Ref -- Overrides Object.Handle...
--
   function Ref (Thing : Set_Body_Ptr) return Linguistic_Set;
   pragma Inline (Ref);

end Fuzzy.Linguistics.Sets;
