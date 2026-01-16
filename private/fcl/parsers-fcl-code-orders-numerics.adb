--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders.                    Luebeck            --
--        Numerics                                 Winter, 2005       --
--  Implementation                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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

with Ada.Exceptions;  use Ada.Exceptions;

with Parsers.FCL.Code.Orders.Numerics.Integers;
with Parsers.FCL.Code.Orders.Numerics.Reals;

package body Parsers.FCL.Code.Orders.Numerics is
   use Fuzzy.Feature.Domain_Floats.Float_Edit;
   use Fuzzy.Feature.Domain_Integers.Integer_Edit;

   function Do_Numeric
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Numeric'Class is
   begin
      if Tree.Operands'Length = 2 then
         declare
            Left : Numeric'Class renames
               Get_Numeric (Context, Tree.Operands (1).all);
            Right : Numeric'Class renames
               Get_Numeric (Context, Tree.Operands (2).all);
         begin
            if Get_Preference (Left) < Get_Preference (Right) then
               case Tree.Operation is
                  when Add =>
                     return Add (Tree.Location, Right, Left);
                  when Div =>
                     return Div_Inv (Tree.Location, Right, Left);
                  when Mul =>
                     return Mul (Tree.Location, Right, Left);
                  when Pow | Postfix_Pow =>
                     return Pow_Inv (Tree.Location, Right, Left);
                  when Sub =>
                     return Sub_Inv (Tree.Location, Right, Left);
                  when others =>
                     null;
               end case;
            else
               case Tree.Operation is
                  when Add =>
                     return Add (Tree.Location, Left, Right);
                  when Div =>
                     return Div (Tree.Location, Left, Right);
                  when Mul =>
                     return Mul (Tree.Location, Left, Right);
                  when Pow | Postfix_Pow =>
                     return Pow (Tree.Location, Left, Right);
                  when Sub =>
                     return Sub (Tree.Location, Left, Right);
                  when others =>
                     null;
               end case;
            end if;
         end;
      else
         declare
            Left : Numeric'Class renames
               Get_Numeric (Context, Tree.Operands (1).all);
         begin
            case Tree.Operation is
               when Abs_Value =>
                  return Abs_Value (Tree.Location, Left);
               when Minus =>
                  return Minus (Tree.Location, Left);
               when Plus =>
                  return Plus (Tree.Location, Left);
               when others =>
                  null;
            end case;
         end;
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric operation is expected at "
         &  Image (Tree.Location)
      )  );
   end Do_Numeric;

   function Get_Numeric
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Numeric'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames
                      Expression'Class (Tree);
         begin
            case Node.Operation is
               when Left_Bracket =>
                  if Node.Operands'Length = 1 then
                     return
                        Get_Numeric (Context, Node.Operands (1).all);
                  else
                     Raise_Exception
                     (  Syntax_Error'Identity,
                        (  "Unexpected operand at "
                        &  Image (Get_Location (Node.Operands (3).all))
                        &  " in brackets starting at "
                        &  Image (Node.Location)
                     )  );
                  end if;
               when Left_Dimension =>
                  declare
                     Result : Constant_Value'Class renames
                                 Do_Dimension (Context, Node);
                  begin
                     if Result in Numeric'Class then
                        return Numeric'Class (Result);
                     end if;
                  end;
               when Arithmetic =>
                  return Do_Numeric (Context, Node);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Numeric operand is expected at "
                     &  Image (Get_Location (Node))
                  )  );
            end case;
         end;
      elsif Tree in Numeric_Literal'Class then
         return To_Numeric (Numeric_Literal'Class (Tree));
      elsif Tree in Power_Literal'Class then
         return To_Numeric (Power_Literal'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      elsif Tree in Identifier'Class then
         return Reals.To_Real (Context, Identifier'Class (Tree));
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "A numeric operand is expected at "
         &  Image (Term'Class (Tree).Location)
      )  );
   end Get_Numeric;

   function Get_Numeric (Tree : Node'Class) return Numeric'Class is
      Features : aliased Dictionary;
      Context  : Resolution_Context;
   begin
      Context.Features := Features'Unchecked_Access;
      return Get_Numeric (Context, Tree);
   end Get_Numeric;

   function Subset_Logical_Term
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Value    : Numeric
            )  return Logical_Term is
      Result : Logical_Term;
   begin
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Numeric operand at "
         &  Image (Value.Location)
         &  " found, whereas a subset of '"
         &  Get_Name (Context.Expected.Feature)
         &  "' is expected by membership test at "
         &  Image (Location)
      )  );
      return Result;
   end Subset_Logical_Term;

   function To_Numeric (Literal : Numeric_Literal'Class)
      return Numeric'Class is
   begin
      if Literal.Exponent = Integer'First then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Too small value at "
            &  Image (Literal.Location)
         )  );
      elsif Literal.Exponent = Integer'Last then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Too big value at "
            &  Image (Literal.Location)
         )  );
      end if;
      if (  Literal in Real_Literal'Class
         or else
            Literal.Dimension /= Np
         or else
            Literal.Exponent < 0
         )
      then
         if Literal.Malformed then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Malformed dimensioned value at "
               &  Image (Literal.Location)
            )  );
         end if;
         declare
            Result : Reals.Real;
         begin
            Result.Location := Literal.Location;
            Result.Value :=
               (  Value (Literal.Value, Literal.Base)
               *  (  Domain_Float (Literal.Base)
                  ** Domain_Float (Literal.Exponent)
                  )
               *  Literal.Dimension
               );
            if Result.Value.Gain'Valid then
               return Result;
            end if;
         exception
            when others =>
               null;
         end;
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Out of range dimensioned value at "
            &  Image (Literal.Location)
         )  );
      else
         if Literal.Malformed then
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Malformed integer value at "
               &  Image (Literal.Location)
            )  );
         end if;
         declare
            Result : Integers.Int;
         begin
            Result.Location := Literal.Location;
            Result.Value :=
               (  Value (Literal.Value, Literal.Base)
               *  Domain_Integer (Literal.Base) ** Literal.Exponent
               );
            return Result;
         exception
            when others =>
               Raise_Exception
               (  Syntax_Error'Identity,
                  (  "Too large integer value at "
                  &  Image (Literal.Location)
               )  );
         end;
      end if;
   end To_Numeric;

   function To_Numeric (Literal : Power_Literal'Class)
      return Numeric'Class is
      Result : Integers.Int;
   begin
       Result.Location := Literal.Location;
       Result.Value    := Literal.Value;
       return Result;
   end To_Numeric;

end Parsers.FCL.Code.Orders.Numerics;
