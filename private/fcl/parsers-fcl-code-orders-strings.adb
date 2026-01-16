--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Strings                                  Winter, 2005       --
--  Implementation                                                    --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

with Parsers.FCL.Code.Logic.Plain;

package body Parsers.FCL.Code.Orders.Strings is

   function Set_Dimension
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Text;
               Right    : Measure
            )  return Constant_Value'Class is
      Result : Logic.Plain.Truth_Value;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "String value at "
         &  Image (Left.Location)
         &  " cannot have dimension as found at "
         &  Image (Location)
      )  );
      return Result;
   end Set_Dimension;

   function Stretch
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Text;
               Right    : Ordered'Class;
               Inversed : Boolean
            )  return Lattice'Class is
      Result : Logic.Plain.Truth_Value;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Range at "
         &  Image (Left.Location)
         &  " has a string bound as found at "
         &  Image (Left.Location)
      )  );
      return Result;
   end Stretch;

   function Get_Preference (Left : Literal_Text) return Preference is
   begin
      return Literal_String_Preference;
   end Get_Preference;

   function Get_Preference (Left : Result_Text) return Preference is
   begin
      return Result_String_Preference;
   end Get_Preference;

   function To_Text (Node : String_Literal'Class) return Text'Class is
   begin
      return
         Literal_Text'
         (  Node.Location,
            Node'Unchecked_Access
         );
   end To_Text;

   function To_Text (Node : Character_Literal'Class)
      return Text'Class is
      Value : constant String := Image (Node.Value);
   begin
      return Result_Text'(Value'Length, Node.Location, Value);
   end To_Text;

   function Do_Text
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Text'Class is
   begin
      case Tree.Operation is
         when Concatenate =>
            return Concatenate
                   (  Tree.Location,
                      Get_Text (Context, Tree.Operands (1).all),
                      Get_Text (Context, Tree.Operands (2).all)
                   );
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "A string operator is expected at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Text;

   function Get_Text
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Text'Class is
   begin
      if Tree in Expression'Class then
         return Do_Text (Context, Expression'Class (Tree));
      elsif Tree in String_Literal'Class then
         return To_Text (String_Literal'Class (Tree));
      elsif Tree in Character_Literal'Class then
         return To_Text (Character_Literal'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      else
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "A string or character operand is expected at "
            &  Image (Term'Class (Tree).Location)
         )  );
      end if;
   end Get_Text;

   function Concatenate
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Literal_Text;
               Right    : Text'Class
            )  return Text'Class is
      Result : constant String := Left.Value.Value & Get_Value (Right);
   begin
      return
         Result_Text'
         (  Result'Length,
            Left.Location & Right.Location & Location,
            Result
         );
   end Concatenate;

   function Concatenate
            (  Location : Parsers.Multiline_Source.Location;
               Left     : Result_Text;
               Right    : Text'Class
            )  return Text'Class is
      Result : constant String := Left.Value & Get_Value (Right);
   begin
      return
         Result_Text'
         (  Result'Length,
            Left.Location & Right.Location & Location,
            Result
         );
   end Concatenate;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value.Value = To_String (Right))
         );
   end EQ;

   function EQ
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Constant_Value'Class
            )  return Constant_Value'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value = To_String (Right))
         );
   end EQ;

   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value.Value > To_String (Right))
         );
   end GT;

   function GT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value > To_String (Right))
         );
   end GT;

   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Literal_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value.Value >= To_String (Right))
         );
   end GE;

   function GE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Result_Text;
               Right    : Ordered'Class
            )  return Logic.Logical'Class is
   begin
      return
         Logic.Plain.Truth_Value'
         (  Left.Location & Right.Location & Location,
            To_Confidence (Left.Value >= To_String (Right))
         );
   end GE;

   function Image
            (  Feature : Feature_Handle;
               Item    : Literal_Text;
               Mode    : Code_Set
            )  return String is
   begin
      return Quote (Item.Value.Value);
   end Image;

   function Image
            (  Feature : Feature_Handle;
               Item    : Result_Text;
               Mode    : Code_Set
            )  return String is
   begin
      return Quote (Item.Value);
   end Image;

   function To_String (Left : Constant_Value'Class) return String is
   begin
      if Left in Literal_Text'Class then
         return Literal_Text'Class (Left).Value.Value;
      elsif Left in Result_Text'Class then
         return Result_Text'Class (Left).Value;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Text value is expected at "
         &  Image (Left.Location)
      )  );
   end To_String;

   function To_String (Tree : Literal'Class) return String is
   begin
      return Tree.Value;
   end To_String;

   function Get_Value (Tree : Literal_Text) return String is
   begin
      return Tree.Value.Value;
   end Get_Value;

   function Get_Value (Tree : Result_Text) return String is
   begin
      return Tree.Value;
   end Get_Value;

   function Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term is
      Result : Logical_Term;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "String operand at "
         &  Image (Value.Location)
         &  " is not allowed for '"
         &  Get_Name (Context.Expected.Feature)
         &  "' in the operation at "
         &  Image (Location)
      )  );
      return Result;
   end Equal_Logical_Term;

   function Greater_Or_Equal_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term renames Equal_Logical_Term;

   function Greater_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term renames Equal_Logical_Term;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Text
            )  return Logical_Term renames Equal_Logical_Term;

end Parsers.FCL.Code.Orders.Strings;
