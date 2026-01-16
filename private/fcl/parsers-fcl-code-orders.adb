--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Orders                     Luebeck            --
--  Implementation                                 Winter, 2005       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Parsers.FCL.Code.Logic;  use Parsers.FCL.Code.Logic;

with Parsers.FCL.Code.Orders.Discrete;
with Parsers.FCL.Code.Orders.Numerics;
with Parsers.FCL.Code.Orders.Strings;

package body Parsers.FCL.Code.Orders is

   function LT
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Ordered'Class;
               Right    : Ordered'Class
            )  return Logical'Class is
      pragma Inline (LT);
   begin
      return not GE (Location, Context, Left, Right);
   end LT;

   function LE
            (  Location : Parsers.Multiline_Source.Location;
               Context  : Resolution_Context;
               Left     : Ordered'Class;
               Right    : Ordered'Class
            )  return Logical'Class is
      pragma Inline (LE);
   begin
      return not GT (Location, Context, Left, Right);
   end LE;

   function Do_Ordered
            (  Context : Resolution_Context;
               Tree    : Expression'Class
            )  return Logical'Class is
   begin
      case Tree.Operation is
         when GE =>
            declare
               Left  : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (1).all);
               Right : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return LE (Tree.Location, Context, Right, Left);
               else
                  return GE (Tree.Location, Context, Left, Right);
               end if;
            end;
         when GT =>
            declare
               Left  : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (1).all);
               Right : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return LT (Tree.Location, Context, Right, Left);
               else
                  return GT (Tree.Location, Context, Left, Right);
               end if;
            end;
         when LE =>
            declare
               Left  : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (1).all);
               Right : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return GE (Tree.Location, Context, Right, Left);
               else
                  return LE (Tree.Location, Context, Left, Right);
               end if;
            end;
         when LT =>
            declare
               Left  : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (1).all);
               Right : Ordered'Class renames
                  Get_Ordered (Context, Tree.Operands (2).all);
            begin
               if Get_Preference (Left) < Get_Preference (Right) then
                  return LE (Tree.Location, Context, Right, Left);
               else
                  return GE (Tree.Location, Context, Left, Right);
               end if;
            end;
         when others =>
            Raise_Exception
            (  Syntax_Error'Identity,
               (  "Relational operator is expected at "
               &  Image (Tree.Location)
            )  );
      end case;
   end Do_Ordered;

   function Get_Ordered
            (  Context : Resolution_Context;
               Tree    : Node'Class
            )  return Ordered'Class is
   begin
      if Tree in Expression'Class then
         declare
            Node : Expression'Class renames
                      Expression'Class (Tree);
         begin
            case Node.Operation is
               when Left_Bracket =>
                  declare
                     Result : Constant_Value'Class renames
                        Do_Bracket (Context, Node);
                  begin
                     if Result in Ordered'Class then
                        return Ordered'Class (Result);
                     else
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "Ordered value is expected at "
                           &  Image (Node.Location)
                        )  );
                     end if;
                  end;
               when Left_Dimension =>
                  declare
                     Result : Constant_Value'Class renames
                        Do_Dimension (Context, Node);
                  begin
                     if Result in Ordered'Class then
                        return Ordered'Class (Result);
                     else
                        Raise_Exception
                        (  Syntax_Error'Identity,
                           (  "Ordered value is expected at "
                           &  Image (Node.Location)
                        )  );
                     end if;
                  end;
               when Arithmetic =>
                  return Numerics.Do_Numeric (Context, Node);
               when Concatenate =>
                  return Strings.Do_Text (Context, Node);
               when others =>
                  Raise_Exception
                  (  Syntax_Error'Identity,
                     (  "Numeric-valued operator is expected at "
                     &  Image (Node.Location)
                  )  );
            end case;
         end;
      elsif Tree in Numeric_Literal'Class then
         return Numerics.To_Numeric (Numeric_Literal'Class (Tree));
      elsif Tree in String_Literal'Class then
         return Strings.To_Text (String_Literal'Class (Tree));
      elsif Tree in Character_Literal'Class then
         return Strings.To_Text (Character_Literal'Class (Tree));
      elsif Tree in Identifier'Class then
         return Discrete.Get_Point (Context, Identifier'Class (Tree));
      elsif Tree in Missing_Operand'Class then
         Raise_Exception
         (  Syntax_Error'Identity,
            (  "Missing operand at "
            &  Image (Missing_Operand'Class (Tree).Location)
         )  );
      end if;
      Raise_Exception
      (  Syntax_Error'Identity,
         (  "Ordered operand is expected at "
         &  Image (Term'Class (Tree).Location)
      )  );
   end Get_Ordered;

end Parsers.FCL.Code.Orders;
