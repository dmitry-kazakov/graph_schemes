--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Boolean_Drawing                   Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Gdk;                            use Gdk;
with Gdk.Color.IHLS;                 use Gdk.Color.IHLS;
with Gdk.Pixbuf;                     use Gdk.Pixbuf;
with GLib.Messages;                  use GLib.Messages;
with GLib.Properties;                use GLib.Properties;
with GLib.Values.Confidence_Factors; use GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Logic;        use GLib.Values.Fuzzy.Logic;
with GtkAda.Types;                   use GtkAda.Types;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Style;                      use Gtk.Style;
with Gtk.Style_Context;              use Gtk.Style_Context;
with Strings_Edit;                   use Strings_Edit;
with System;                         use System;

with Ada.IO_Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Gtk.Fuzzy_Boolean_Drawing;

package body Gtk.Fuzzy_Boolean_Drawing is
   use Cairo;
   use Confidence_Factors;
   use Gdk.Color;
   use Gdk.RGBA;
   use GLib;
   use GLib.Generic_Properties;
   use Gtk.Widget.Styles;

   Def_Pos_Color : constant Gdk_Color := Parse ("#A0C080");
   Def_Nec_Color : constant Gdk_Color := Parse ("#E0D080");
   Def_Err_Color : constant Gdk_Color := Parse ("#FF5040");

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Boolean_Drawing." & Name;
   end Where;

   function Get_Look
            (  Widget : not null access Gtk_Widget_Record'Class
            )  return Look_And_Feel;

   procedure Adjust (Data : in out Fuzzy_Boolean_Data) is
   begin
      Data.Possibility := null;
      Data.Necessity   := null;
      Data.Image       := null;
      Data.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Adjust")
         )  );
   end Adjust;

   procedure Free is
      new Ada.Unchecked_Deallocation (RGB_Image, RGB_Image_Ptr);

   function To_Color (Brightness : Integer) return GUInt16 is
      pragma Inline (To_Color);
   begin
      if Brightness <= Integer (GUInt16'First) then
         return GUInt16'First;
      elsif Brightness >= Integer (GUInt16'Last) then
         return GUInt16'Last;
      else
         return GUInt16 (Brightness);
      end if;
   end To_Color;

   procedure Get_Color_Style
             (  Widget    : not null access Gtk_Widget_Record'Class;
                Look      : out Look_And_Feel;
                Pos_Color : out Gdk_Color;
                Nec_Color : out Gdk_Color;
                Err_Color : out Gdk_Color
             )  is
   begin
      Pos_Color :=
         Style_Get (Widget, "possibility-color", Def_Pos_Color);
      Nec_Color :=
         Style_Get (Widget, "necessity-color", Def_Nec_Color);
      Err_Color :=
         Style_Get (Widget, "error-color", Def_Err_Color);
      Look := Get_Look (Widget);
   end Get_Color_Style;

   function Lighten (Color : Gdk_Color; Factor : Float)
      return Gdk_Color is
      pragma Inline (Lighten);
      Inc    : constant Float := 1.0 + Factor * 0.3;
      Result : Gdk_Color;
   begin
      Set_RGB
      (  Result,
         To_Color (Integer (Float (Red   (Color)) * Inc)),
         To_Color (Integer (Float (Green (Color)) * Inc)),
         To_Color (Integer (Float (Blue  (Color)) * Inc))
      );
      return Result;
   end Lighten;

   procedure Bar
             (  Context : Cairo_Context;
                Left    : GInt;
                Right   : GInt;
                Width   : GInt;
                Height  : GInt;
                Color   : Gdk_Color;
                Look    : Look_And_Feel
             )  is
      procedure Draw_Rectangle
                (  X      : GInt;
                   Y      : GInt;
                   Width  : GInt;
                   Height : GInt;
                   Color  : Gdk_Color
               )  is
         pragma Inline (Draw_Rectangle);
      begin
         Cairo.Rectangle
         (  Context,
            GDouble (X),
            GDouble (Y),
            GDouble (Width),
            GDouble (Height)
         );
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (GUInt16'Last),
            GDouble (Green (Color)) / GDouble (GUInt16'Last),
            GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
         );
         Cairo.Fill (Context);
      end Draw_Rectangle;

      X : GInt := Left;
      Y : GInt := Right;
      W : GInt := Width;
      H : GInt := Height;
   begin
      if W < 1 or else H < 1 then
         return;
      end if;
      case Look is
         when Relief =>
            H := H - 1;
            W := W - 1;
            Draw_Rectangle (X, Y, W, H, Color);
            X := X + 1;
            Y := Y + 1;
            W := W - 2;
            H := H - 2;
            if W < 1 or else H < 1 then
               return;
            end if;
            Draw_Rectangle (X, Y, W, H, Lighten (Color, 1.0));
            X := X + 1;
            Y := Y + 1;
            W := W - 1;
            H := H - 1;
            if W < 1 or else H < 1 then
               return;
            end if;
            Draw_Rectangle (X, Y, W, H, Lighten (Color, -1.0));
            W := W - 1;
            H := H - 1;
            if W < 1 or else H < 1 then
               return;
            end if;
            for Index in 1..H loop
               Draw_Rectangle
               (  X,
                  Y,
                  W,
                  1,
                  Lighten (Color, 2.0 * Float (Index) / Float (H) - 1.0)
               );
               Y := Y + 1;
            end loop;
         when Flat =>
            Draw_Rectangle (X, Y, W, H, Color);
      end case;
   end Bar;

   procedure Draw_Bar
             (  Data    : in out Fuzzy_Boolean_Data;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Area    : Gdk_Rectangle;
                State   : Gtk_Cell_Renderer_State
             )  is
      pragma Inline (Draw_Bar);
      Gap         : constant := 3;
      Bar_Height  : GInt;
      Left_Width  : GInt;
      Left_Text   : Pango_Layout;
      Right_Width : GInt;
      Right_Text  : Pango_Layout;
      Text_Y      : GDouble;
      Text_Width  : GInt;
      Text_Height : GInt;
      Text_State  : Gtk_State_Flags;
      Look        : Look_And_Feel;
      Pos_Color   : Gdk_Color;
      Nec_Color   : Gdk_Color;
      Err_Color   : Gdk_Color;
      Style       : constant Gtk_Style_Context :=
                    Get_Style_Context (Widget);
   begin
      Get_Color_Style (Widget, Look, Pos_Color, Nec_Color, Err_Color);
      if (  Data.Invalid
         or else
            Area.Width < 1
         or else
            Area.Height < 1
         )
      then
         return;
      end if;
      if 0 /= (State and Cell_Renderer_Selected) then
         Text_State := Gtk_State_Flag_Selected;
      else
         Text_State := Gtk_State_Flag_Normal;
      end if;
      Bar_Height := Area.Height;
      if Data.Value.Possibility = Data.Value.Necessity then
         Left_Width :=
            GInt
            (  Float (Data.Value.Possibility) * Float (Area.Width)
            /  Float (Confidence'Last)
            );
         Right_Width := 0;
         Bar
         (  Context,
            Area.X,
            Area.Y,
            Left_Width,
            Bar_Height,
            Nec_Color,
            Look
         );
         Right_Text := Data.Possibility;
      elsif Data.Value.Possibility > Data.Value.Necessity or else
            Data.Confidence then
         Left_Text  := Data.Necessity;
         Left_Width :=
            GInt
            (  Float (Data.Value.Necessity) * Float (Area.Width)
            /  Float (Confidence'Last)
            );
         Right_Width :=
            (  GInt
               (  Float (Data.Value.Possibility) * Float (Area.Width)
               /  Float (Confidence'Last)
               )
            -  Left_Width
            );
         Right_Text := Data.Possibility;
         Bar
         (  Context,
            Area.X + Left_Width,
            Area.Y,
            Right_Width,
            Bar_Height,
            Pos_Color,
            Look
         );
         Bar
         (  Context,
            Area.X,
            Area.Y,
            Left_Width,
            Bar_Height,
            Nec_Color,
            Look
         );
      else
         Left_Text  := Data.Possibility;
         Left_Width :=
            GInt
            (  Float (Data.Value.Possibility) * Float (Area.Width)
            /  Float (Confidence'Last)
            );
         Right_Width :=
            (  GInt
               (  Float (Data.Value.Necessity) * Float (Area.Width)
               /  Float (Confidence'Last)
               )
            -  Left_Width
            );
         Right_Text := Data.Necessity;
         Bar
         (  Context,
            Area.X,
            Area.Y,
            Left_Width,
            Bar_Height,
            Nec_Color,
            Look
         );
         Bar
         (  Context,
            Area.X + Left_Width,
            Area.Y,
            Right_Width,
            Bar_Height,
            Err_Color,
            Look
         );
      end if;
      Get_Pixel_Size (Right_Text, Text_Width, Text_Height);
      Text_Y :=
         GDouble (Area.Y) + GDouble (Area.Height - Text_Height) / 2.0;
      Text_Width := Text_Width + 2 * Gap;
      if Text_Width <= Area.Width - Left_Width - Right_Width then
         Style.Set_State (Text_State);
         Style.Render_Layout
         (  Context,
            GDouble (Area.X + Left_Width + Right_Width + Gap),
            Text_Y,
            Right_Text
         );
         if not Data.Confidence and then Left_Text /= null then
            Get_Pixel_Size (Left_Text, Text_Width, Text_Height);
            Text_Width := Text_Width + 2 * Gap;
            if Text_Width <= Right_Width then
               Style.Set_State (Gtk_State_Flag_Normal);
               Style.Render_Layout
               (  Context,
                  GDouble (Area.X + Left_Width + Gap),
                  Text_Y,
                  Left_Text
               );
            elsif Text_Width <= Left_Width then
               Style.Render_Layout
               (  Context,
                  GDouble (Area.X + Left_Width - Text_Width + Gap),
                  Text_Y,
                  Left_Text
               );
            end if;
         end if;
      elsif Text_Width <= Right_Width then
         Style.Set_State (Gtk_State_Flag_Normal);
         Style.Render_Layout
         (  Context,
            GDouble
            (  Area.X
            +  Left_Width
            +  Right_Width
            -  Text_Width
            +  Gap
            ),
            Text_Y,
            Right_Text
         );
         if not Data.Confidence and then Left_Text /= null then
            Get_Pixel_Size (Left_Text, Text_Width, Text_Height);
            Text_Width := Text_Width + 2 * Gap;
            if Text_Width <= Left_Width then
               Render_Layout
               (  Style,
                  Context,
                  GDouble (Area.X + Left_Width - Text_Width + Gap),
                  Text_Y,
                  Left_Text
               );
            end if;
         end if;
      elsif Text_Width <= Left_Width then
         Style.Set_State (Gtk_State_Flag_Normal);
         Style.Render_Layout
         (  Context,
            GDouble (Area.X + Left_Width - Text_Width + Gap),
            Text_Y,
            Right_Text
         );
      end if;
   end Draw_Bar;

   function "*" (Color : Gdk_Color; Factor : Float) return Gdk_Color is
      Result : Gdk_Color;
   begin
      Set_RGB
      (  Result,
         To_Color (Integer (Float (Red   (Color)) * Factor)),
         To_Color (Integer (Float (Green (Color)) * Factor)),
         To_Color (Integer (Float (Blue  (Color)) * Factor))
      );
      return Result;
   end "*";

   function Mix
            (  C1 : Gdk_Color; F1 : Float;
               C2 : Gdk_Color; F2 : Float
            )  return Gdk_Color is
   begin
      return Average ((C1*F1, C2*F2));
   end Mix;

   procedure Draw_Bullet
             (  Data       : in out Fuzzy_Boolean_Data;
                Context    : Cairo_Context;
                Widget     : not null access Gtk_Widget_Record'Class;
                Area       : Gdk_Rectangle;
                Background : Gdk_RGBA
             )  is
      pragma Inline (Draw_Bullet);
      use Ada.Numerics.Elementary_Functions;
      Size      : constant GInt := GInt'Min (Area.Height, Area.Width);
      Look      : Look_And_Feel;
      Color     : Gdk_Color;
      Pos_Color : Gdk_Color;
      Nec_Color : Gdk_Color;
      Err_Color : Gdk_Color;
   begin
      if (  not Data.Invalid
         and then
            Area.Width > 0
         and then
            Area.Height > 0
         )
      then
         if Data.Image = null then
            Data.Image := new RGB_Image (1..Size, 1..Size);
         elsif (  Data.Image'Length (1) < Size
               or else
                  Data.Image'Length (2) < Size
               )
         then
            Free (Data.Image);
            Data.Image := new RGB_Image (1..Size, 1..Size);
         end if;
         declare
            Shape : RGB_Image renames Data.Image.all;
            This  : RGB_Pixel;
            BG    : constant RGB_Pixel := To_RGB_Pixel (Background);
            D     : constant Float := Float (Size);
            R     : constant Float := D / 2.0;
            X0    : constant Float := Float (Shape'First (2)) + R;
            Y0    : constant Float := Float (Shape'First (1)) + R;
            RR    : constant Float := R*R;
            CS    : constant Float := 0.707106781186547524400844362105;
         begin
            Get_Color_Style
            (  Widget,
               Look,
               Pos_Color,
               Nec_Color,
               Err_Color
            );
            if Data.Value.Possibility >= Data.Value.Necessity then
               Color :=
                  Mix
                  (  Pos_Color, Float (Data.Value.Possibility),
                     Nec_Color, Float (Data.Value.Necessity)
                  );
            else
               Color :=
                  Mix
                  (  Pos_Color, Float (Data.Value.Possibility),
                     Err_Color, Float (Data.Value.Necessity)
                  );
            end if;
            declare
               S : Float;
            begin
               S :=
                  abs
                  (  Float (Data.Value.Possibility)
                  -  Float (Data.Value.Necessity)
                  );
               S := (S * 0.4 - 0.1) * Float (Gdk_Saturation'Last);
               if S < 0.0 then
                  Color := Impurify (Color, Gdk_Saturation (-2.0 * S));
               else
                  Color := Purify (Color, Gdk_Saturation (S));
               end if;
            end;
            case Look is
               when Relief =>
                  for I in 1..Size loop
                     for J in 1..Size loop
                        declare
                           X  : Float := Float (J) - X0;
                           Y  : constant Float := Float (I) - Y0;
                           CX : Float;
                           CY : Float;
                           L  : constant Float := X*X + Y*Y - RR;
                           C  : Gdk_Color;
                        begin
                           if L > 4.0 then
                              Shape (I, J) := BG;
                           else
                              CX := (X * CS + Y * CS) + 2.0 * R / 3.0;
                              CY := X * CS - Y * CS;
                              X := sqrt (0.5*(CX*CX + CY*CY)/RR) - 0.5;
                              if X > 1.0 then
                                 X := 2.0 - X;
                                 if X < 0.5 then
                                    X := 0.5;
                                 end if;
                              end if;
                              X := X * Float (Gdk_Luminance'Last);
                              if X < 0.0 then
                                 C :=
                                    Lighten
                                    (  Color,
                                       Gdk_Luminance (-X),
                                       True
                                    );
                              else
                                 C := Darken (Color, Gdk_Luminance (X));
                              end if;
--                                if L > -4.0 then
--                                   C := Average ((C, Back));
--                                end if;
                              Shape (I, J) := To_RGB_Pixel (C);
                           end if;
                        end;
                     end loop;
                  end loop;
               when Flat =>
                  This := To_RGB_Pixel (Color);
                  for I in 1..Size loop
                     for J in 1..Size loop
                        declare
                           X : constant Float := Float (J) - X0;
                           Y : constant Float := Float (I) - Y0;
                        begin
                           if X*X + Y*Y > RR then
                              Shape (I, J) := BG;
                           else
                              Shape (I, J) := This;
                           end if;
                        end;
                     end loop;
                  end loop;
            end case;
            declare
               Icon  : Gdk_Pixbuf;
               Style : constant Gtk_Style_Context :=
                       Get_Style_Context (Widget);
               function To_GUChar_Array is
                  new Ada.Unchecked_Conversion
                      (  RGB_Image_Ptr,
                         GUChar_Array_Access
                      );
            begin
               Icon :=
                  Gdk_New_From_Data
                  (  Data      => To_GUChar_Array (Data.Image),
                     Width     => Size,
                     Height    => Size,
                     Has_Alpha => True,
                     Rowstride => Shape'Length (2) * 4,
                     Auto_Destroy_Data => False
                  );
               Style.Render_Icon
               (  Cr     => Context,
                  Pixbuf => Icon,
                  X      => GDouble (Area.X),
                  Y      => GDouble (Area.Y)
               );
               Icon.Unref;
            end;
         end;
      end if;
   end Draw_Bullet;

   procedure Draw_Text
             (  Data    : in out Fuzzy_Boolean_Data;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Area    : Gdk_Rectangle;
                State   : Gtk_Cell_Renderer_State
             )  is
      pragma Inline (Draw_Text);
      Style : constant Gtk_Style_Context := Get_Style_Context (Widget);
   begin
      if (  Data.Invalid
         or else
            Area.Width < 1
         or else
            Area.Height < 1
         )
      then
         return;
      end if;
      if Data.Possibility /= null then
         Style.Render_Layout
         (  Context,
            GDouble (Area.X),
            GDouble (Area.Y),
            Data.Possibility
         );
      end if;
   end Draw_Text;

   procedure Draw
             (  Data       : in out Fuzzy_Boolean_Data;
                Context    : Cairo_Context;
                Widget     : not null access Gtk_Widget_Record'Class;
                Area       : Gdk_Rectangle;
                Background : Gdk_RGBA;
                State      : Gtk_Cell_Renderer_State := 0
             )  is
   begin
      case Get_Shape (Data'Access, Widget) is
         when Bar =>
            Draw_Bar (Data, Context, Widget, Area, State);
         when Bullet =>
            Draw_Bullet (Data, Context, Widget, Area, Background);
         when Text =>
            Draw_Text (Data, Context, Widget, Area, State);
      end case;
   end Draw;

   procedure Finalize (Data : in out Fuzzy_Boolean_Data) is
   begin
      if Data.Possibility /= null then
         Unref (Data.Possibility);
         Data.Possibility := null;
      end if;
      if Data.Necessity /= null then
         Unref (Data.Necessity);
         Data.Necessity := null;
      end if;
      if Data.Image /= null then
         Free (Data.Image);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   function Get (Data : Fuzzy_Boolean_Data) return GValue is
      Value : GValue;
   begin
      if Data.Confidence then
         Init (Value, GType_Confidence);
         if Data.Invalid then
            GLib.Values.Confidence_Factors.Set_Undefined (Value);
         else
            Set (Value, Data.Value.Possibility);
         end if;
      else
         Init (Value, GType_Fuzzy_Boolean);
         if Data.Invalid then
            GLib.Values.Fuzzy.Logic.Set_Undefined (Value);
         else
            Set (Value, Data.Value);
         end if;
      end if;
      return Value;
   end Get;

   function Get_Look
            (  Widget : not null access Gtk_Widget_Record'Class
            )  return Look_And_Feel is
   begin
      return Look_And_Feel_Style.Style_Get (Widget, "look");
   end Get_Look;

   procedure Get_Property
             (  Data          : in out Fuzzy_Boolean_Data;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Intuitionistic_Value_ID =>
            Init (Value, GType_Fuzzy_Boolean);
            if Data.Invalid then
               GLib.Values.Confidence_Factors.Set_Undefined (Value);
            else
               Set (Value, Data.Value);
            end if;
         when Truth_Value_ID =>
            Init (Value, GType_Confidence);
            if Data.Invalid then
               GLib.Values.Confidence_Factors.Set_Undefined (Value);
            else
               Set (Value, Data.Value.Possibility);
            end if;
         when others =>
            null; -- GType_Invalid
      end case;
   end Get_Property;

   function Get_Shape
            (  Data   : not null access Fuzzy_Boolean_Data;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Shape is
   begin
      if not (Data.Shape_Fixed and then Data.Shape_Set) then
         Data.Form      := Shape_Style.Style_Get (Widget, "shape");
         Data.Shape_Set := True;
      end if;
      return Data.Form;
   end Get_Shape;

   function Get_Size
            (  Data    : Fuzzy_Boolean_Data;
               Form    : Shape;
               Area    : Gdk_Rectangle;
               X_Align : GFloat;
               Y_Align : GFloat;
               X_Pad   : GUInt;
               Y_Pad   : GUInt
            )  return Gdk_Rectangle is
      Result : Gdk_Rectangle := Get_Size (Data, Form, X_Pad, Y_Pad);
   begin
      Result.X :=
         GInt'Max
         (  GInt (X_Align * GFloat (Area.Width - Result.Width)),
            0
         );
      Result.Y :=
         GInt'Max
         (  GInt (Y_Align * GFloat (Area.Height - Result.Height)),
            0
         );
      return Result;
   end Get_Size;

   function Get_Size
            (  Data  : Fuzzy_Boolean_Data;
               Form  : Shape;
               X_Pad : GUInt;
               Y_Pad : GUInt
            )  return Gdk_Rectangle is
      Area : Gdk_Rectangle;
   begin
      case Form is
         when Bar =>
            if Data.Invalid or else Data.Possibility = null then
               Area.Width  := 1;
               Area.Height := 1;
            else
               Data.Possibility.Get_Pixel_Size
               (  Area.Width,
                  Area.Height
               );
               if (  not Data.Confidence
                  and then
                     Data.Value.Possibility /= Data.Value.Necessity
                  and then
                     Data.Necessity /= null
                  )
               then
                  Data.Necessity.Get_Pixel_Size (Area.X, Area.Y);
                  Area.Width  := Area.Width + Area.X;
                  Area.Height := GInt'Max (Area.Height, Area.Y);
               end if;
               Area.Width  := Area.Width  + GInt (X_Pad) * 2;
               Area.Height := Area.Height + GInt (Y_Pad) * 2;
            end if;
         when Bullet =>
            Area.Width  := 5;
            Area.Height := 5;
         when Text =>
            if Data.Invalid or else Data.Possibility = null then
               Area.Width  := 1;
               Area.Height := 1;
            else
               Data.Possibility.Get_Pixel_Size
               (  Area.Width,
                  Area.Height
               );
            end if;
      end case;
      Area.X := 0;
      Area.Y := 0;
      return Area;
   end Get_Size;

   procedure Parse
             (  Source : String;
                Data   : in out Fuzzy_Boolean_Data;
                Retype : Boolean
             )  is
      Invalid    : Boolean := False;
      Confidence : Boolean := False;
      Value      : Fuzzy_Boolean;
   begin
      declare
         Pointer : Integer := Source'First;
      begin
         Get (Source, Pointer);
         Get (Source, Pointer, Value.Possibility);
         Get (Source, Pointer);
         if Pointer > Source'Last then
            Confidence := True;
         elsif Source (Pointer) = ':' then
            Pointer := Pointer + 1;
            Get (Source, Pointer);
            Get (Source, Pointer, Value.Necessity);
            Get (Source, Pointer);
            Invalid := Pointer <= Source'Last;
         elsif Pointer + 2 > Source'Last then
            Invalid := True;
         elsif Source (Pointer..Pointer + 1) = ".." then
            Value.Necessity := Value.Possibility;
            Pointer := Pointer + 2;
            if Source (Pointer) = '.' then
               Pointer := Pointer + 1;
            end if;
            Get (Source, Pointer);
            Get (Source, Pointer, Value.Possibility);
            Get (Source, Pointer);
            Invalid := Pointer <= Source'Last;
         else
            Invalid := True;
         end if;
      exception
         when Ada.IO_Exceptions.End_Error | Constraint_Error |
              Data_Error =>
            Invalid := True;
      end;
      if Retype then
         if Invalid then
            Data.Updated := Data.Updated or not Data.Invalid;
            Data.Invalid := True;
         elsif Confidence then
            Put (Data, Value.Possibility);
         else
            Put (Data, Value);
         end if;
      else
         if Invalid then
            raise Constraint_Error;
         elsif Confidence then
            if Data.Confidence then
               Put (Data, Value.Possibility);
            else
               Value.Necessity := Value.Possibility;
               Put (Data, Value);
            end if;
         else
            if Data.Confidence then
               raise Constraint_Error;
            else
               Put (Data, Value);
            end if;
         end if;
      end if;
   end Parse;

   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : Confidence
             )  is
      New_Value : constant Fuzzy_Boolean :=
                     (Possibility => Value, Necessity => 0.0);
   begin
      Data.Updated :=
         (  Data.Updated
         or else
            Data.Invalid
         or else
            not Data.Confidence
         or else
            Data.Value /= New_Value
         );
      Data.Invalid    := False;
      Data.Confidence := True;
      Data.Value      := New_Value;
   end Put;

   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : Fuzzy.Logic.Fuzzy_Boolean
             )  is
   begin
      Data.Updated :=
         (  Data.Updated
         or else
            Data.Invalid
         or else
            Data.Confidence
         or else
            Data.Value /= Value
         );
      Data.Invalid    := False;
      Data.Confidence := False;
      Data.Value      := Value;
   end Put;

   procedure Put
             (  Data  : in out Fuzzy_Boolean_Data;
                Value : GValue
             )  is
   begin
      if Is_Confidence (Value) then
         if GLib.Values.Confidence_Factors.Is_Defined (Value) then
            Put (Data, Confidence'(Get (Value)));
         else
            Put_Undefined_Confidence (Data);
         end if;
      elsif Is_Fuzzy_Boolean (Value) then
         if GLib.Values.Fuzzy.Logic.Is_Defined (Value) then
            Put (Data, Fuzzy.Logic.Fuzzy_Boolean'(Get (Value)));
         else
            Put_Undefined_Fuzzy_Boolean (Data);
         end if;
      else
         Parse (Get_String (Value), Data, True);
      end if;
   end Put;

   procedure Put_Undefined_Confidence
             (  Data : in out Fuzzy_Boolean_Data
             )  is
   begin
      Data.Updated :=
         (  Data.Updated
         or else
            Data.Invalid
         or else
            not Data.Confidence
         );
      Data.Invalid    := False;
      Data.Confidence := True;
      Data.Value      := Certain_False;
   end Put_Undefined_Confidence;

   procedure Put_Undefined_Fuzzy_Boolean
             (  Data : in out Fuzzy_Boolean_Data
             )  is
   begin
      Data.Updated :=
         (  Data.Updated
         or else
            Data.Invalid
         or else
            Data.Confidence
         );
      Data.Invalid    := False;
      Data.Confidence := False;
      Data.Value      := Certain_False;
   end Put_Undefined_Fuzzy_Boolean;

   procedure Set_Property
             (  Data          : in out Fuzzy_Boolean_Data;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Intuitionistic_Value_ID =>
            begin
               Put (Data, Fuzzy_Boolean'(Get (Value)));
            exception
               when Constraint_Error =>
                  Data.Updated := Data.Updated or not Data.Invalid;
                  Data.Invalid := True;
                  Data.Value   := Certain_False;
            end;
         when Truth_Value_ID =>
            begin
               Put (Data, Confidence'(Get (Value)));
            exception
               when Constraint_Error =>
                  Data.Updated := Data.Updated or not Data.Invalid;
                  Data.Invalid := True;
                  Data.Value   := Certain_False;
            end;
         when others =>
            null;
      end case;
   end Set_Property;

   function To_RGB_Pixel (Color : Gdk_Color) return RGB_Pixel is
   begin
      return
      (  Red   => GUChar (Red   (Color) / 256),
         Green => GUChar (Green (Color) / 256),
         Blue  => GUChar (Blue  (Color) / 256),
         Alpha => GUChar'Last
      );
   end To_RGB_Pixel;

   function To_RGB_Pixel (Color : Gdk_RGBA) return RGB_Pixel is
      function "+" (Value : GDouble) return GUChar is
      begin
         if Value <= 0.0 then
            return 0;
         elsif Value >= 1.0 then
            return GUChar'Last;
         else
            return GUChar (Value * GDouble (GUChar'Last));
         end if;
      end "+";
   begin
      return
      (  Red   => +Color.Red,
         Green => +Color.Green,
         Blue  => +Color.Blue,
         Alpha => +Color.Alpha
      );
   end To_RGB_Pixel;

   procedure Update
             (  Data   : in out Fuzzy_Boolean_Data;
                Widget : not null access Gtk_Widget_Record'Class
             )  is
   begin
      if Data.Updated then
         case Get_Shape (Data'Access, Widget) is
            when Bar =>
               if Data.Possibility = null then
                  Data.Possibility := Create_Pango_Layout (Widget);
               end if;
               if Data.Invalid then
                  Data.Possibility.Set_Text ("");
                  if Data.Necessity /= null then
                     Data.Necessity.Set_Text ("");
                  end if;
                  Set_Tip (Widget);
               elsif Data.Confidence or else
                     Data.Value.Possibility = Data.Value.Necessity then
                  declare
                     Text : constant String :=
                               Image (Data.Value.Possibility);
                  begin
                     Data.Possibility.Set_Text (Text);
                     Widget.Set_Tooltip_Text (Text);
                  end;
               else
                  if Data.Necessity = null then
                     Data.Necessity := Create_Pango_Layout (Widget);
                  end if;
                  declare
                     Pos : constant String :=
                              Image (Data.Value.Possibility);
                     Nec : constant String :=
                              Image (Data.Value.Necessity);
                  begin
                     Data.Possibility.Set_Text (Pos);
                     Data.Necessity.Set_Text (Nec);
                     Widget.Set_Tooltip_Text (Pos & ':' & Nec);
                  end;
               end if;
            when Bullet =>
               if Data.Invalid then
                  Set_Tip (Widget);
               elsif Data.Confidence or else
                     Data.Value.Possibility = Data.Value.Necessity then
                  Widget.Set_Tooltip_Text
                  (  Image (Data.Value.Possibility)
                  );
               else
                  Widget.Set_Tooltip_Text
                  (  Image (Data.Value.Possibility)
                  &  ':'
                  &  Image (Data.Value.Necessity)
                  );
               end if;
            when Text =>
               if Data.Possibility = null then
                  Data.Possibility := Create_Pango_Layout (Widget);
               end if;
               if Data.Invalid then
                  Data.Possibility.Set_Text ("");
                  Set_Tip (Widget);
               elsif Data.Confidence or else
                     Data.Value.Possibility = Data.Value.Necessity
                     then
                  declare
                     Text : constant String :=
                               Image (Data.Value.Possibility);
                  begin
                     Data.Possibility.Set_Text (Text);
                     Widget.Set_Tooltip_Text (Text);
                  end;
               else
                  declare
                     Text : constant String :=
                            (  Image (Data.Value.Possibility)
                            &  ':'
                            &  Image (Data.Value.Necessity)
                            );
                  begin
                     Data.Possibility.Set_Text (Text);
                     Widget.Set_Tooltip_Text (Text);
                  end;
               end if;
         end case;
         Data.Updated := False;
      end if;
   end Update;

end Gtk.Fuzzy_Boolean_Drawing;
