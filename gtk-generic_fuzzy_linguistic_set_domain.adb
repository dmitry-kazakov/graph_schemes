--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain     Luebeck            --
--  Implementation                                 Winter, 2007       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Numerics;              use Ada.Numerics;
with GLib.Messages;             use GLib.Messages;
with GLib.Object;               use GLib.Object;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Style;              use GtkAda.Style;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango;                     use Pango;

with Ada.Unchecked_Conversion;
with Gdk.Window;
with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;
with Interfaces.C.Strings;
with Strings_Edit.Floats;
with Strings_Edit.Integers.Superscript;
with System;

package body Gtk.Generic_Fuzzy_Linguistic_Set_Domain is
   use Float_Edit;
   use Fuzzy_Linguistic_Sets.Unbounded_Arrays;
   use Fuzzy.Logic;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
      (  0 => Interfaces.C.Strings.New_String ("zoomed"),
         1 => Interfaces.C.Strings.New_String ("marked")
      );

   Eps             : constant := 1.0E-6;
   Color_Cycle     : constant := 9;
   Def_First_Color : constant Gdk_IHLS_Color :=
      (  Hue        => 0,
         Luminance  => Gdk_Luminance'Last - Gdk_Luminance'Last / 4,
         Saturation => Gdk_Saturation'Last / 5
      );

   Page_Inc : constant GDouble := 0.75;

   function "not" (Pixel : RGB_Pixel) return RGB_Pixel is
   begin
      return
      (  Red   => Pixel.Red   + 128,
         Green => Pixel.Green + 128,
         Blue  => Pixel.Blue  + 128
      );
   end "not";

   package Context_Marshaller is
      new Return_Boolean_Callback.Marshallers.Generic_Marshaller
          (  Cairo.Cairo_Context,
             Cairo.Get_Context
          );

   type Preferred_Size_Ptr is access procedure
        (  Widget       : System.Address;
           Minimum_Size : out Gint;
           Natural_Size : out Gint
        );
   pragma Convention (C, Preferred_Size_Ptr);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Preferred_Size_Ptr,
             Preferred_Size_Handler
          );

   procedure Preferred_Height
             (  Widget       : System.Address;
                Minimum_Size : out Gint;
                Natural_Size : out Gint
             );
   pragma Convention (C, Preferred_Height);

   procedure Preferred_Width
             (  Widget       : System.Address;
                Minimum_Size : out Gint;
                Natural_Size : out Gint
             );
   pragma Convention (C, Preferred_Width);

   procedure Elliptic_Arc
             (  Context          : Cairo_Context;
                X, Y             : GDouble;
                Major_Radius     : GDouble;
                Minor_Radius     : GDouble;
                Major_Axis_Angle : GDouble := 0.0;
                From_Angle       : GDouble := 0.0;
                Length_Angle     : GDouble := 2.0 * Pi
             );

   function Get_Color
            (  Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_RGBA is
      pragma Inline (Get_Color);
   begin
      return Color : Gdk_RGBA do
         Get_Style_Context
         (  Widget
         ) .Get_Color (Gtk_State_Flag_Normal, Color);
      end return;
   end Get_Color;

   procedure Install_Set_Properties (Class : GObject_Class);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Generic_Fuzzy_Linguistic_Set_Domain." & Name;
   end Where;

   function "<" (Left, Right : Color_Point) return Boolean is
   begin
      return
      (  Left.Y < Right.Y
      or else
         (  Left.Y = Right.Y
         and then
            Pixel (Left.Color) < Pixel (Right.Color)
      )  );
   end "<";

   function Is_In
            (  Points : Points_Indices.Set;
               Var    : Variable
            )  return Boolean is
   begin
      return
         Get_Size (Points) in 1..Get_Points_Number (Var) - 1;
   end Is_In;

   function Create
            (  Text : UTF8_String
            )  return Text_Handles.Handle is
      Ptr : constant Text_Ptr := new Text_Body (Text'Length);
   begin
      Ptr.Text := Text;
      return Ref (Ptr);
   end Create;

   procedure Create_Palette
             (  Data : in out Linguistic_Set_Data
             )  is
      Color : Color_Position;
   begin
      Data.Last_Used := -1;
      for Index in 1..Data.Cardinality + Data.Accumulated loop
         Get_Color (Data, Color);
         Put
         (  Data.Palette,
            Index,
            (  To_RGB
               (  Val
                  (  Data.First_Color,
                     Natural (Color),
                     Color_Cycle
               )  ),
               Color
         )  );
      end loop;
      Data.Updated := True;
      Data.Repaint := False;
   end Create_Palette;

   procedure Deselect_All
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             )  is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.all;
         begin
            if not Is_Empty (Data.Selected) then
               Erase (Data.Selected);
               Data.Updated := True;
               Queue_Draw (Widget.View);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Deselect_All")
         )  );
   end Deselect_All;

   procedure Destroy
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             )  is
   begin
      if Widget.Data.Ptr.Zooming /= null then
         Widget.Data.Ptr.Zooming.Finalized;
      end if;
      Widget.Data.Invalidate;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy Gtk_Fuzzy_Linguistic_Set_Domain_Record")
         )  );
   end Destroy;

   function Draw
            (  Object  : access GObject_Record'Class;
               Context : Cairo_Context;
               Widget  : Gtk_Fuzzy_Linguistic_Set_Domain
            )  return Boolean is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.all;
            Size : Gdk_Rectangle;
         begin
            Widget.Get_Allocation (Size);
            if (  Size.Width /= Widget.Width
               or else
                  Size.Height /= Widget.Height
               )
            then
               Widget.Resize (Size.Width, Size.Height, True);
               Widget.Width  := Size.Width;
               Widget.Height := Size.Height;
--           Refresh (Widget);
--           return True;
            end if;
            if Data.Repaint then
               Create_Palette (Data);
            end if;
            Data.Top_Offs :=
               (  Y_Axis (Widget.Height)
               -  Data.Bottom_Offs
               -  1
               -  Get_Height (Data.Area)
               );
            Set_Line_Width (Context, 1.0);
            -- Drawing the domain
            Draw_Domain (Data, Context, Y_Axis (Widget.Height));
            -- Drawing the annotation
            if Data.Left_Offs > 0 then
               Widget.Draw_Annotation (Context);
            end if;
            if Widget.Top_Left /= null then
               Widget.Draw_Top_Left (Context);
            end if;
            if Widget.Bottom_Right /= null then
               Widget.Draw_Bottom_Right (Context);
            end if;
         end;
      end if;
      return True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Draw")
         )  );
         return True;
   end Draw;

   procedure Draw_Annotation
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             )  is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data   : Linguistic_Set_Data'Class renames
                     Widget.Data.Ptr.all;
            Color  : constant Gdk_RGBA := Get_Color (Widget);
            Width  : X_Axis;
            Height : Y_Axis;
            Major  : Interval;
         begin
            -- Vertical line
            Draw_Line
            (  Context,
               Data.Left_Offs,
               Data.Top_Offs,
               Data.Top_Offs + Get_Height (Data.Area),
               Data.Line_Color
            );
            -- Vertical ticks
            declare
               Right   : constant X_Axis := Data.Left_Offs;
               Left_1  : constant X_Axis :=
                            Right - X_Axis (Data.Major_Tick_Length);
               Left_2  : constant X_Axis :=
                            Right - X_Axis (Data.Minor_Tick_Length);
               Value   : Truth   := Data.Y_Ticks.Low_Value;
               Minor   : Natural := Data.Y_Ticks.Low_Tick;
               Small   : constant Integer :=
                            Integer'Min (Data.Y_Ticks.Small, 0);
               Top     : Y_Axis  := 0;
               Cut_Off : Clip_Type;
               Pos     : Y_Axis;
               Y       : Y_Axis;
            begin
               if Widget.Top_Left /= null then
                  Top := Top +
                         Y_Axis (Widget.Top_Left.Get_Allocated_Height);
               end if;
               loop
                  To_Y (Data.Area, Value, Y, Cut_Off);
                  Pos := Y + Data.Top_Offs;
                  if Minor = 0 or else Minor > Data.Y_Ticks.Ticks then
                     -- Major tick
                     if Cut_Off = None then
                        Draw_Line
                        (  Context,
                           Left_1,
                           Right,
                           Pos,
                           Data.Line_Color
                        );
                        Data.Text.Set_Text
                        (  Strings_Edit.Floats.Image
                           (  Float (Value),
                              AbsSmall => Small
                        )  );
                        Data.Text.Get_Pixel_Size
                        (  GInt (Width),
                           GInt (Height)
                        );
                        Pos := Pos - Height / 2;
                        if (  (  Left_1 - Width - X_Axis (Data.Tick_Gap)
                              >= 0
                              )
                           and then
                              Pos >= Top
                           )
                        then
                           Draw_Layout
                           (  Context,
                              Color,
                              GInt
                              (  Left_1
                              -  Width
                              -  X_Axis (Data.Tick_Gap)
                              ),
                              GInt (Pos),
                              Data.Text
                           );
                        end if;
                     end if;
                     Minor := 1;
                  else
                     -- Minor tick
                     if Cut_Off = None then
                        Draw_Line
                        (  Context,
                           Left_2,
                           Right,
                           Pos,
                           Data.Line_Color
                        );
                        Minor := Minor + 1;
                     end if;
                  end if;
                  exit when Y <= 1;
                  Value := Value + Data.Y_Ticks.Minor;
               end loop;
            end;
            -- Horizontal line
            Draw_Line
            (  Context,
               Data.Left_Offs,
               Data.Left_Offs + Get_Width (Data.Area) - 1,
               Data.Top_Offs + Get_Height (Data.Area),
               Data.Line_Color
            );
            -- Horizontal ticks
            Data.Draw_X_Ticks (Context, Major);
            if Widget.Bottom_Right = null then
               Data.Draw_X_Annotation
               (  Context => Context,
                  X_Offs  => Data.Left_Offs,
                  Y_Offs  => (  Data.Top_Offs + Get_Height (Data.Area)
                             +  Y_Axis
                                (  Data.Major_Tick_Length
                                +  Data.Tick_Gap
                             )  ),
                  From    => 1 - Data.Left_Offs,
                  To      => Get_Width (Data.Area),
                  Major   => Major,
                  Color   => Color
               );
            else
               Data.Draw_X_Annotation
               (  Context => Context,
                  X_Offs  => Data.Left_Offs,
                  Y_Offs  => (  Data.Top_Offs + Get_Height (Data.Area)
                             +  Y_Axis
                                (  Data.Major_Tick_Length
                                +  Data.Tick_Gap
                             )  ),
                  From    => 1 - Data.Left_Offs,
                  To      => (  Get_Width (Data.Area)
                             +  X_Axis
                                (  Widget.Bottom_Right.
                                   Get_Allocated_Width
                             )  ),
                  Major   => Major,
                  Color   => Color
               );
            end if;
         end;
      end if;
   end Draw_Annotation;

   procedure Draw_Area
             (  Area        : in out Drawing_Area;
                From_X      : X_Axis;
                To_X        : X_Axis;
                Cardinality : Natural;
                Set         : Unbounded_Arrays.Unbounded_Array;
                Selected    : Selection;
                Accumulated : Natural;
                Palette     : Color_Item_Arrays.Unbounded_Array;
                Background  : Gdk_Color;
                Opaque      : Boolean
             )  is
      Selection : Gdk_Color_Array (1..Cardinality);
      Colors    : Gdk_Color_Array (0..Cardinality - 1);
   begin
      for Index in 1..Get_Size (Selected) loop
         declare
            No : constant Positive :=
                 Positive (Selected.Get_Key (Index));
         begin
            Selection (No) := Selection_Color (Get (Palette, No).Color);
         end;
      end loop;
      if Accumulated in 1..Cardinality then
         Selection (Accumulated) :=
            Selection_Color (Get (Palette, Accumulated).Color);
      end if;
      for X in From_X..To_X loop
         --
         -- Sorting variables by values
         --
         declare
            Value : constant Interval := To_Number (Area, X);
            Color : Gdk_Color;
         begin
            Erase (Area.Order);
            for Index in 1..Cardinality loop
               if (  Index = Accumulated
                  or else
                     Selected.Is_In (Variable_Index (Index))
                  )
               then
                  Color := Selection (Index);
               else
                  Color := Get (Palette, Index).Color;
               end if;
               Add
               (  Area.Order,
                  (  To_Y
                     (  Area,
                        Is_In (Value, Get (Set, Index)).Possibility
                     ),
                     Color
               )  );
            end loop;
         end;
         --
         -- Painting variables below the membership functions
         --
         declare
            Y_Top : Y_Axis  := 1;
            Count : Natural := 0;

            procedure Flush (Y_Bottom : Y_Axis) is
               pragma Inline (Flush);
            begin
               if Y_Top <= Y_Bottom then
                  declare
                     Mix : Gdk_Color;
                  begin
                     if Count > 0 then
                        Mix := Average (Colors (0..Count - 1));
                     else
                        if not Opaque then
                           return;
                        end if;
                        Mix := Background;
                     end if;
                     for Y in Y_Top..Y_Bottom loop
                        Area.Pixels.Set (X, Y, Mix);
                     end loop;
                  end;
               end if;
            end Flush;
         begin
            for Index in 1..Area.Order.Get_Size loop
               declare
                  This : Color_Point renames Area.Order.Get (Index);
               begin
                  if This.Y /= Y_Top then
                     Flush (This.Y - 1);
                     Y_Top := This.Y;
                  end if;
                  Colors (Count) := This.Color;
                  Count := Count + 1;
               end;
            end loop;
            Flush (Get_Height (Area));
         end;
      end loop;
   end Draw_Area;

   procedure Draw_Area_Selection
             (  Data     : in out Linguistic_Set_Data;
                Context  : Cairo_Context;
                Inversed : Boolean;
                Y_Offs   : Y_Axis := 0
             )  is
   begin
      Draw_Rectangle_Area
      (  Data     => Data,
         Context  => Context,
         X1       => Data.Area_Selection.X1,
         X2       => Data.Area_Selection.X2,
         Y1       => Data.Area_Selection.Y1 + Y_Offs,
         Y2       => Data.Area_Selection.Y2 + Y_Offs,
         Inversed => Inversed
      );
   end Draw_Area_Selection;

   procedure Draw_Bottom_Right
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             )  is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data  : Linguistic_Set_Data'Class renames
                    Widget.Data.Ptr.all;
            Major : Interval;
         begin
            Data.Draw_X_Ticks (Context, Major);
            Data.Draw_X_Annotation
            (  Context => Context,
               X_Offs  => Data.Left_Offs,
               Y_Offs  => (  Data.Top_Offs + Get_Height (Data.Area)
                          +  Y_Axis
                             (  Data.Major_Tick_Length
                             +  Data.Tick_Gap
                          )  ),
               From    => Data.Left_Offs + Get_Width (Data.Area),
               To      => X_Axis (Widget.Get_Allocated_Width),
               Major   => (Major.To, Major.To),
               Color   => Get_Color (Widget)
            );
         end;
      end if;
   end Draw_Bottom_Right;

   procedure Draw_Domain
             (  Data    : in out Linguistic_Set_Data;
                Context : Cairo_Context;
                Height  : Y_Axis
             )  is
   begin
      if (  Data.Updated
         or else
            Data.Scrolled_X
         or else
            Data.Scrolled_Y
         or else
            Data.Zoomed_X
         or else
            Data.Zoomed_Y
         )
      then
         Draw_Area
         (  Data.Area,
            1,
            Get_Width (Data.Area),
            Data.Cardinality + Data.Accumulated,
            Data.Set,
            Data.Selected,
            Data.Accumulated * (Data.Cardinality + Data.Accumulated),
            Data.Palette,
            Data.Background,
            True
         );
         for Index in 1..Data.Cardinality + Data.Accumulated loop
            Draw_Variable
            (  Data.Area,
               1,
               Get_Width (Data.Area),
               Get (Data.Set, Index),
               To_RGB
               (  Purify
                  (  Darken
                     (  To_IHLS (Get (Data.Palette, Index).Color),
                        Gdk_Luminance'Last / 3
                     ),
                     Gdk_Saturation'Last / 3
            )  )  );
         end loop;
         Data.Updated    := False;
         Data.Scrolled_X := False;
         Data.Scrolled_Y := False;
         Data.Zoomed_X   := False;
         Data.Zoomed_Y   := False;
      end if;
      -- Drawing the background
      Data.Area.Pixels.Draw
      (  Context => Context,
         X  => GInt (Data.Left_Offs),
         Y  => GInt
               (  Height
               -  Data.Bottom_Offs
               -  Get_Height (Data.Area)
               ),
         X1 => 1,
         X2 => Get_Width (Data.Area),
         Y1 => 1,
         Y2 => Get_Height (Data.Area)
      );
      if (  Inactive = Data.Area_Selection.State
         and then
            Data.Area_Selection.X1 in 1..Get_Width (Data.Area)
         and then
            Data.Area_Selection.X2 in 1..Get_Width (Data.Area)
         and then
            Data.Area_Selection.Y1 in 1..Get_Height (Data.Area)
         and then
            Data.Area_Selection.Y2 in 1..Get_Height (Data.Area)
         )
      then
         Data.Draw_Area_Selection (Context, True, Data.Top_Offs + 1);
      else
         Data.Area_Selection.State := None;
      end if;
      -- Drawing selected points
      Data.Draw_Selected (Context, Data.Top_Offs + 1);
   end Draw_Domain;

   procedure Draw_Line
             (  Context : Cairo_Context;
                X1      : X_Axis;
                Y1      : Y_Axis;
                X2      : X_Axis;
                Y2      : Y_Axis
             )  is
      pragma Inline (Draw_Line);
   begin        -- For a ntialiasing + 0.5
      Move_To (Context, GDouble (X1), GDouble (Y1));
      Line_To (Context, GDouble (X2), GDouble (Y2));
   end Draw_Line;

   procedure Draw_Line
             (  Context : Cairo_Context;
                X1, X2  : X_Axis;
                Y       : Y_Axis;
                Color   : Gdk_Color
             )  is
   begin
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Color)) / GDouble (GUInt16'Last),
         GDouble (Green (Color)) / GDouble (GUInt16'Last),
         GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
      );
      Move_To (Context, GDouble (X1), GDouble (Y) + 0.5);
      Line_To (Context, GDouble (X2), GDouble (Y) + 0.5);
      Stroke (Context);
   end Draw_Line;

   procedure Draw_Line
             (  Context : Cairo_Context;
                X       : X_Axis;
                Y1, Y2  : Y_Axis;
                Color   : Gdk_Color
             )  is
   begin
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Color)) / GDouble (GUInt16'Last),
         GDouble (Green (Color)) / GDouble (GUInt16'Last),
         GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
      );
      Move_To (Context, GDouble (X) + 0.5, GDouble (Y1));
      Line_To (Context, GDouble (X) + 0.5, GDouble (Y2));
      Stroke (Context);
   end Draw_Line;

   procedure Draw_Rectangle
             (  Context : Cairo_Context;
                X1, X2  : X_Axis;
                Y1, Y2  : Y_Axis;
                Color   : Gdk_Color
             )  is
      L, R : X_Axis;
      T, B : Y_Axis;
   begin
      if X1 > X2 then
         L := X2;
         R := X1;
      else
         L := X1;
         R := X2;
      end if;
      if Y1 > Y2 then
         T := Y2;
         B := Y1;
      else
         T := Y1;
         B := Y2;
      end if;
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Color)) / GDouble (GUInt16'Last),
         GDouble (Green (Color)) / GDouble (GUInt16'Last),
         GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
      );
      Rectangle
      (  Cr     => Context,
         X      => GDouble (L),
         Y      => GDouble (T),
         Width  => GDouble (R - L),
         Height => GDouble (B - T)
      );
      Stroke (Context);
   end Draw_Rectangle;

   procedure Draw_Rectangle_Area
             (  Data     : in out Linguistic_Set_Data;
                Context  : Cairo_Context;
                X1, X2   : X_Axis;
                Y1, Y2   : Y_Axis;
                Inversed : Boolean
             )  is
      procedure Draw_Line (X1, X2 : X_Axis; Y : Y_Axis) is
         Pixels : RGB_Image renames Data.Area.Pixels;
      begin
         if Inversed then
            declare
               Row : constant Y_Axis := Pixels.Get_Height;
            begin
               for X in X1..X2 loop
                  Pixels.Set (X, Row, not Pixels.Get (X, Y));
               end loop;
               Pixels.Draw
               (  Context => Context,
                  X  => GInt (Data.Left_Offs - 1 + X1),
                  Y  => GInt (Y - 1),
                  X1 => X1,
                  X2 => X2,
                  Y1 => Row,
                  Y2 => Row
               );
            end;
         else
            Pixels.Draw
            (  Context => Context,
               X  => GInt (Data.Left_Offs - 1 + X1),
               Y  => GInt (Y - 1),
               X1 => X1,
               X2 => X2,
               Y1 => Y,
               Y2 => Y
            );
         end if;
      end Draw_Line;

      procedure Draw_Line (X : X_Axis; Y1, Y2 : Y_Axis) is
         Pixels : RGB_Image renames Data.Area.Pixels;
      begin
         if Inversed then
            declare
               Column : constant X_Axis := Pixels.Get_Width;
            begin
               for Y in Y1..Y2 loop
                  Pixels.Set (Column, Y, not Pixels.Get (X, Y));
               end loop;
               Pixels.Draw
               (  Context => Context,
                  X  => GInt (Data.Left_Offs - 1 + X),
                  Y  => GInt (Y1 - 1),
                  X1 => Column,
                  X2 => Column,
                  Y1 => Y1,
                  Y2 => Y2
               );
            end;
         else
            Pixels.Draw
            (  Context => Context,
               X  => GInt (Data.Left_Offs - 1 + X),
               Y  => GInt (Y1 - 1),
               X1 => X,
               X2 => X,
               Y1 => Y1,
               Y2 => Y2
            );
         end if;
      end Draw_Line;

      L, R : X_Axis;
      T, B : Y_Axis;
   begin
      if X1 > X2 then
         L := X2;
         R := X1;
      else
         L := X1;
         R := X2;
      end if;
      if Y1 > Y2 then
         T := Y2;
         B := Y1;
      else
         T := Y1;
         B := Y2;
      end if;
      Draw_Line (L, R, T);
      if L = R then -- Null width
         Draw_Line (L, T, B);
      elsif T = B then -- Null height
         Draw_Line (L, R, T);
      else
         Draw_Line (L, R, T);
         Draw_Line (L, R, B);
         if B - T > 1 then
           Draw_Line (L, T + 1, B - 1);
           Draw_Line (R, T + 1, B - 1);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Draw_Rectangle_XOR")
         )  );
   end Draw_Rectangle_Area;

   procedure Draw_Selected
             (  Area    : Drawing_Area;
                Context : Cairo_Context;
                X       : X_Axis;
                Min     : Confidence;
                Max     : Confidence;
                Radius  : GInt;
                Y_Offs  : Y_Axis
             )  is
      Y1 : constant Y_Axis := To_Y (Area, Max);
      Y2 : constant Y_Axis := To_Y (Area, Min);
   begin
      Elliptic_Arc
      (  Context => Context,
         X => GDouble (X),
         Y => (GDouble (Y1) + GDouble (Y2)) / 2.0 + GDouble (Y_Offs),
         Major_Radius => GDouble (Radius),
         Minor_Radius => GDouble (Radius) + GDouble (Y2 - Y1) / 2.0
      );
      Stroke (Context);
   end Draw_Selected;

   procedure Draw_Selected
             (  Area     : Drawing_Area;
                Context  : Cairo_Context;
                X        : X_Axis;
                Y        : Ordinates;
                Radius   : GInt;
                Distance : Confidence;
                Y_Offs   : Y_Axis
             )  is
      First : Confidence := Y (1);
      Last  : Confidence := First;
   begin
      for Index in Y'First + 1..Y'Last loop
         if Y (Index) - First > Distance then
            Draw_Selected
            (  Area,
               Context,
               X,
               First,
               Last,
               Radius,
               Y_Offs
            );
            First := Y (Index);
         end if;
         Last := Y (Index);
      end loop;
      Draw_Selected (Area, Context, X, First, Last, Radius, Y_Offs);
   end Draw_Selected;

   procedure Draw_Selected
             (  Data    : in out Linguistic_Set_Data;
                Context : Cairo_Context;
                Y_Offs  : Y_Axis := 0
             )  is
   begin
      Save (Context);
      Rectangle
      (  Cr     => Context,
         X      => GDouble (Data.Left_Offs),
         Y      => GDouble (Y_Offs),
         Width  => GDouble (Get_Width (Data.Area)),
         Height => GDouble (Get_Height (Data.Area))
      );
      Clip (Context);
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Data.Selection_Color))/GDouble (GUInt16'Last),
         GDouble (Green (Data.Selection_Color))/GDouble (GUInt16'Last),
         GDouble (Blue  (Data.Selection_Color))/GDouble (GUInt16'Last)
      );
      Set_Line_Width (Context, 1.0);
      for Index in reverse 1..Data.Selected.Get_Size loop
         declare
            Var_No : constant Positive :=
                        Positive (Data.Selected.Get_Key (Index));
         begin
            if Var_No <= Data.Cardinality then
               declare
                  Max_Distance : constant Confidence :=
                     To_Confidence
                     (  Truth (Data.Selection_Length * 3)
                     /  Data.Area.Y_Gain
                     );
                  Var    : Variable renames Get (Data.Set, Var_No);
                  Count  : constant Natural :=
                              Get_Points_Number (Var);
                  Points : Points_Indices.Set renames
                              Data.Selected.Get (Index);
                  Area   : Drawing_Area renames Data.Area;
                  Point  : Positive;
                  Value  : Number'Base;
                  Left   : Confidence;
                  Right  : Confidence;
                  Min    : Confidence;
                  Max    : Confidence;
                  X      : X_Axis;
               begin
                  for Item in reverse 1..Get_Size (Points) loop
                     Point := Positive (Get (Points, Item));
                     if Point <= Count then
                        Get_Point
                        (  Var,
                           Point,
                           Value,
                           Left,
                           Min,
                           Max,
                           Right
                        );
                        X := To_X (Area, Value) + Data.Left_Offs;
                        if Max - Min > Max_Distance then
                           if Left < Right then
                              Draw_Selected
                              (  Area,
                                 Context,
                                 X,
                                 (Min, Left, Right, Max),
                                 Data.Selection_Length,
                                 Max_Distance,
                                 Y_Offs
                              );
                           else
                              Draw_Selected
                              (  Area,
                                 Context,
                                 X,
                                 (Min, Right, Left, Max),
                                 Data.Selection_Length,
                                 Max_Distance,
                                 Y_Offs
                              );
                           end if;
                        else
                           Draw_Selected
                           (  Area,
                              Context,
                              X,
                              Min,
                              Max,
                              Data.Selection_Length,
                              Y_Offs
                           );
                        end if;
                     end if;
                  end loop;
               end;
            else
               Remove (Data.Selected, Index);
            end if;
         end;
      end loop;
      Restore (Context);
   end Draw_Selected;

   procedure Draw_Top_Left
             (  Widget  : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Context : Cairo_Context
             )  is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data    : Linguistic_Set_Data'Class renames
                      Widget.Data.Ptr.all;
            Right   : constant GInt := GInt (Data.Left_Offs);
            Left    : constant GInt := Right - Data.Major_Tick_Length;
            Small   : constant Integer := Integer'Min
                                          (  Data.Y_Ticks.Small,
                                             0
                                          );
            Minor   : Natural  := Data.Y_Ticks.Low_Tick;
            Value   : Truth    := Data.Y_Ticks.Low_Value;
            Color   : constant Gdk_RGBA := Get_Color (Widget.Top_Left);
            Pos     : GInt;
            Y       : Y_Axis;
            Cut_Off : Clip_Type;
            Width   : GInt := Widget.Get_Allocated_Width;
            Height  : GInt := Widget.Get_Allocated_Height;
            Area    : Drawing_Area renames Data.Area;
         begin
            loop
               To_Y (Area, Value, Y, Cut_Off);
               Pos := GInt (Y - 1);
               if Minor = 0 or else Minor > Data.Y_Ticks.Ticks then
                  -- Major tick
                  if Cut_Off = None then
                     Data.Text.Set_Text
                     (  Strings_Edit.Floats.Image
                        (  Float (Value),
                           AbsSmall => Small
                     )  );
                     Data.Text.Get_Pixel_Size (Width, Height);
                     Pos := Pos - Height / 2;
                     if Pos < 0 then
                        Pos := Pos +
                               Widget.Top_Left.Get_Allocated_Height;
                        if Left - Width - Data.Tick_Gap >= 0 then
                           Draw_Layout
                           (  Context,
                              Color,
                              Left - Width - Data.Tick_Gap,
                              Pos,
                              Data.Text
                           );
                        end if;
                     end if;
                  end if;
                  Minor := 1;
               else
                  Minor := Minor + 1;
               end if;
               exit when Y <= 1;
               Value := Value + Data.Y_Ticks.Minor;
            end loop;
         end;
      end if;
   end Draw_Top_Left;

   procedure Draw_Variable
             (  Area   : in out Drawing_Area;
                From_X : X_Axis;
                To_X   : X_Axis;
                Var    : Variable;
                Color  : Gdk_Color
             )  is
      Level        : Fuzzy_Boolean;
      Top_Y        : Y_Axis;
      Bottom_Y     : Y_Axis;
      Old_Top_Y    : Y_Axis    := 0;
      Old_Bottom_Y : Y_Axis    := Get_Height (Area) + 1;
      Cut_Off      : Clip_Type := Above;
      Join         : Boolean   := False;
   begin
      for X in From_X..To_X loop
         Level := Is_In (To_Number (Area, X), Var);
         To_Y (Area, Scale (Level.Possibility), Top_Y, Cut_Off);
         if Cut_Off = Above then
            Join := False;
         else
            To_Y (Area, Scale (Level.Necessity), Bottom_Y, Cut_Off);
            if Cut_Off = Below then
               Join := False;
            else
               if Join then
                  if Top_Y > Old_Bottom_Y then
                     Top_Y := Old_Bottom_Y + 1;
                  end if;
                  if Bottom_Y < Old_Top_Y then
                     Bottom_Y := Old_Top_Y - 1;
                  end if;
               else
                  Join := True;
               end if;
               for Y in Top_Y..Bottom_Y loop
                  Area.Pixels.Set (X, Y, Color);
               end loop;
               Old_Top_Y    := Top_Y;
               Old_Bottom_Y := Bottom_Y;
            end if;
         end if;
      end loop;
   end Draw_Variable;

   procedure Draw_X_Ticks
             (  Data    : Linguistic_Set_Data;
                Context : Cairo_Context;
                Major   : out Interval
             )  is
      Top          : constant Y_Axis :=
                        Data.Top_Offs + Get_Height (Data.Area);
      Minor_Bottom : constant Y_Axis :=
                        Top + Y_Axis (Data.Minor_Tick_Length);
      Major_Bottom : constant Y_Axis :=
                        Top + Y_Axis (Data.Major_Tick_Length);
      Minor   : Natural     := Data.X_Ticks.Low_Tick;
      Value   : Number'Base := Data.X_Ticks.Low_Value;
      Cut_Off : Clip_Type;
      X       : X_Axis;
   begin
      Major.From := Number'Base'Last;
      Major.To   := Number'Base'First;
      declare
         Drawing : Drawing_Area renames Data.Area;
      begin
         loop
            To_X (Drawing, Value, X, Cut_Off);
            if Minor = 0 or else Minor > Data.X_Ticks.Ticks then
               -- Major tick
               if Cut_Off = None then
                  if Major.From = Number'Base'Last then
                     Major.From := Value;
                  end if;
                  Major.To := Value;
                  Draw_Line
                  (  Context,
                     X + Data.Left_Offs - 1,
                     Top,
                     Major_Bottom,
                     Data.Line_Color
                  );
               end if;
               Minor := 1;
            else
               -- Minor tick
               if Cut_Off = None then
                  Draw_Line
                  (  Context,
                     X + Data.Left_Offs - 1,
                     Top,
                     Minor_Bottom,
                     Data.Line_Color
                  );
               end if;
               Minor := Minor + 1;
            end if;
            exit when X >= Get_Width (Drawing);
            Value := Value + Data.X_Ticks.Minor;
         end loop;
      end;
   end Draw_X_Ticks;

   procedure Draw_X_Annotation
             (  Data    : Linguistic_Set_Data;
                Context : Cairo_Context;
                X_Offs  : X_Axis;
                Y_Offs  : Y_Axis;
                From    : X_Axis;
                To      : X_Axis;
                Major   : Interval;
                Color   : Gdk_RGBA
             )  is
      Step    : constant Number'Base :=
                         (  Data.X_Ticks.Minor
                         *  Number'Base (Data.X_Ticks.Ticks + 1)
                         );
      Width   : X_Axis;
      Height  : GInt;
      Note    : constant String := Get_Domain_Note (Data, Data.X_Power);
      Power   : constant Number'Base := 10.0 ** Data.X_Power;
      Small   : constant Integer :=
                   Integer'Min (0, Data.X_Ticks.Small - Data.X_Power);
      Cut_Off : Clip_Type;
      X       : X_Axis;
      X1      : X_Axis   := From;
      X2      : X_Axis   := To;
      Span    : Interval := Major;
      Left    : Boolean  := False;
      Value   : Number'Base;
   begin
      if Note'Length /= 0 then
         -- Drawing the note
         Data.Text.Set_Text (Note);
         Data.Text.Get_Pixel_Size (GInt (Width), Height);
         if X1 + Width - 1 > X2 then
            return;
         end if;
         X2 := X2 - Width;
         Draw_Layout
         (  Context,
            Color,
            GInt (X2 + X_Offs - 1),
            GInt (Y_Offs),
            Data.Text
         );
         X2 := X2 - X_Axis (Data.Tick_Gap * 3);
      end if;
      -- Drawing annotations
      declare
         Drawing : Drawing_Area renames Data.Area;
      begin
         while Span.To - Span.From >= -Step loop
            if Left then
               Value := Span.From;
               Span.From := Span.From + Step;
            else
               Value := Span.To;
               Span.To := Span.To - Step;
            end if;
            To_X (Drawing, Value, X, Cut_Off);
            if X in X1..X2 then
               Data.Text.Set_Text
               (  Float_Edit.Image (Value / Power, AbsSmall => Small)
               );
               Data.Text.Get_Pixel_Size (GInt (Width), Height);
               X := X - Width / 2;
               if X >= X1 and then X + Width - 1 <= X2 then
                  Draw_Layout
                  (  Context,
                     Color,
                     GInt (X + X_Offs - 1),
                     GInt (Y_Offs),
                     Data.Text
                  );
                  if Left then
                     X1 := X + Width + X_Axis (Data.Tick_Gap);
                     Left := False;
                  else
                     X2 := X - X_Axis (Data.Tick_Gap) - 1;
                     Left := True;
                  end if;
               end if;
            end if;
         end loop;
      end;
   end Draw_X_Annotation;

   procedure Elliptic_Arc
             (  Context          : Cairo_Context;
                X, Y             : GDouble;
                Major_Radius     : GDouble;
                Minor_Radius     : GDouble;
                Major_Axis_Angle : GDouble := 0.0;
                From_Angle       : GDouble := 0.0;
                Length_Angle     : GDouble := 2.0 * Pi
             )  is
      To_Angle : constant GDouble := From_Angle + Length_Angle;
   begin
      Save (Context);
      Translate (Context, X, Y);
      Rotate (Context, Major_Axis_Angle);
      Scale (Context, Major_Radius, Minor_Radius);
      if Length_Angle > 0.0 then
         Arc
         (  Cr     => Context,
            Xc     => 0.0,
            Yc     => 0.0,
            Radius => 1.0,
            Angle1 => From_Angle,
            Angle2 => To_Angle
         );
      else
         Arc_Negative
         (  Cr     => Context,
            Xc     => 0.0,
            Yc     => 0.0,
            Radius => 1.0,
            Angle1 => From_Angle,
            Angle2 => To_Angle
         );
      end if;
      Restore (Context);
   end Elliptic_Arc;

   procedure Finalize (Data : in out Linguistic_Set_Data) is
   begin
      Standard.Object.Finalize (Standard.Object.Entity (Data));
      if Data.Text /= null then
         Data.Text.Unref;
         Data.Text := null;
      end if;
      Data.Note_Text.Invalidate;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize Linguistic_Set_Data")
         )  );
   end Finalize;

   procedure Free_Color
             (  Data  : in out Linguistic_Set_Data;
                Color : Color_Position
             )  is
   begin
      Add (Data.Free_Colors, Color);
   end Free_Color;

   function From_X
            (  Area : Drawing_Area;
               X    : X_Axis
            )  return Number'Base is
      Value : constant Interval := To_Number (Area, X);
   begin
      return (Value.From + Value.To) / 2.0;
   end From_X;

   function From_Y
            (  Area : Drawing_Area;
               Y    : Y_Axis
            )  return Truth is
   begin
      return (To_Necessity (Area, Y) + To_Possibility (Area, Y)) / 2.0;
   end From_Y;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record;
               Index  : Positive
            )  return Variable is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if Index > Data.Cardinality then
         raise Constraint_Error;
      end if;
      return Data.Set.Get (Index);
   end Get;

   function Get_Cardinality
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Natural is
   begin
      return Widget.Data.Ptr.Cardinality;
   end Get_Cardinality;

   function Get (Area : Drawing_Area) return Zoom_Undo_Item is
   begin
      return
      (  X_Gain   => Area.X_Gain,
         X_Offset => Area.X_Offset,
         Y_Gain   => Area.Y_Gain,
         Y_Offset => Area.Y_Offset
      );
   end Get;

   procedure Get_Color
             (  Data  : in out Linguistic_Set_Data;
                Color : out Color_Position
             )  is
   begin
      if Is_Empty (Data.Free_Colors) then
         Data.Last_Used := Data.Last_Used + 1;
         Color := Data.Last_Used;
      else
         Color := Get (Data.Free_Colors, 1);
         Remove (Data.Free_Colors, Positive'(1));
      end if;
   end Get_Color;

   function Get_Domain_Note
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return UTF8_String is
   begin
      return Widget.Data.Ptr.Note_Text.Ptr.Text;
   end Get_Domain_Note;

   function Get_Domain_Note
            (  Data  : Linguistic_Set_Data;
               Power : Integer
            )  return String is
  begin
      if Power  = 0 then
         return Data.Note_Text.Ptr.Text;
      elsif Data.Note_Text.Ptr.Length = 0 then
         return
         (  "10"
         &  Strings_Edit.Integers.Superscript.Image (Power)
         );
      else
         return
         (  "10"
         &  Strings_Edit.Integers.Superscript.Image (Power)
         &  " "
         &  Data.Note_Text.Ptr.Text
         );
      end if;
   end Get_Domain_Note;

   function Get_Height (Area : Drawing_Area) return Y_Axis is
   begin
      return Y_Axis'Max (1, Area.Pixels.Get_Height - 1);
   end Get_Height;

   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Selection is
   begin
      return Widget.Data.Ptr.Selected;
   end Get_Selection;

   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Selection_Subtype is
   begin
      if Widget.Data.Is_Valid then
         declare
            Data : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.all;
         begin
            return Data.Get_Selection (Data.Selected);
         end;
      else
         return (Mode => Empty);
      end if;
   end Get_Selection;

   function Get_Selection
            (  Data     : Linguistic_Set_Data;
               Selected : Selection
            )  return Selection_Subtype is
   begin
      case Get_Size (Selected) is
         when 0 =>
            return (Mode => Empty);
         when 1 =>
            declare
               Var_No : constant Variable_Index :=
                           Get_Key (Selected, 1);
            begin
               if Natural (Var_No) > Data.Cardinality then
                  return (Mode => Empty);
               end if;
               declare
                  Points : Points_Indices.Set renames
                              Get (Selected, Positive'(1));
                  Var    : Variable renames
                              Get (Data.Set, Natural (Var_No));
               begin
                  if Is_In (Points, Var) then
                     declare
                        From : constant Point_Index := Get (Points, 1);
                        To   : Point_Index := From;
                     begin
                        for Point in 2..Get_Size (Points) loop
                           To := To + 1;
                           if Get (Points, Point) /= To then
                              return
                              (  Mode     => Complex,
                                 Selected => Selected
                              );
                           end if;
                        end loop;
                        return
                        (  Mode       => Points_Range,
                           Range_At   => Var_No,
                           From_Point => From,
                           To_Point   => To
                        );
                     end;
                  elsif Is_Empty (Points) then
                     return (Single_Variable, Var_No);
                  else
                     return
                     (  Mode       => All_Points,
                        Range_At   => Var_No,
                        From_Point => 1,
                        To_Point   =>
                           Point_Index (Get_Points_Number (Var))
                     );
                  end if;
               end;
            end;
         when others =>
            declare
               From : constant Variable_Index := Selected.Get_Key (1);
               To   : Variable_Index := From;
               Contiguous : Boolean  := True;
            begin
               for Index in 1..Get_Size (Selected) loop
                  Contiguous :=
                     (  Contiguous
                     and then
                        To /= Get_Key (Selected, Index)
                     );
                  if Positive (To) <= Data.Cardinality then
                     if Is_In
                        (  Get (Selected, Index),
                           Get (Data.Set, Positive (To))
                        )
                     then
                        return
                        (  Mode     => Complex,
                           Selected => Selected
                        );
                     end if;
                  end if;
                  To := To + 1;
               end loop;
               if Contiguous then
                  return
                  (  Mode          => Variables_Range,
                     From_Variable => From,
                     To_Variable   => To - 1
                  );
               else
                  return
                  (  Mode     => Complete_Variables,
                     Selected => Selected
                  );
               end if;
            end;
      end case;
   end Get_Selection;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Event_Box.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name & "Domain",
            Signals      => Signals
         )
      then
         Install_Set_Properties (Class_Ref (Class_Record.The_Type));
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   function Get_Width (Area : Drawing_Area) return X_Axis is
   begin
      return X_Axis'Max (1, Area.Pixels.Get_Width - 1);
   end Get_Width;

   function Get_X_Adjustment
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Gtk_Adjustment is
   begin
      if Widget.X_Scroll = null then
         return null;
      else
         return Widget.X_Scroll.Get_Adjustment;
      end if;
   end Get_X_Adjustment;

   function Get_X_Size (Area : Drawing_Area) return Number'Base is
   begin
      return Area.X_Gain * Number'Base (Get_Width (Area) - 1);
   end Get_X_Size;

   function Get_X_Zoom
            (  Data : Linguistic_Set_Data
            )  return Zoom_X_Range is
      Result : constant Zoom_X_Factor :=
                        (  Zoom_X_Factor (Data.Area.X_Gain)
                        *  Zoom_X_Factor (Length (Data.X_Range))
                        /  Zoom_X_Factor (Get_Width (Data.Area) - 1)
                        );
   begin
      if Result < Zoom_X_Range'First then
         return Zoom_X_Range'First;
      elsif Result > Zoom_X_Range'Last then
         return Zoom_X_Range'Last;
      else
         return Result;
      end if;
   end Get_X_Zoom;

   function Get_X_Zoom
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Zoom_X_Range is
   begin
      return Widget.Data.Ptr.Get_X_Zoom;
   end Get_X_Zoom;

   function Get_Y_Adjustment
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Gtk_Adjustment is
   begin
      if Widget.Y_Scroll = null then
         return null;
      else
         return Widget.Y_Scroll.Get_Adjustment;
      end if;
   end Get_Y_Adjustment;

   function Get_Y_Size (Area : Drawing_Area) return Truth is
   begin
      return Area.Y_Gain * Truth (Get_Height (Area) - 1);
   end Get_Y_Size;

   function Get_Y_Zoom
            (  Data : Linguistic_Set_Data
            )  return Zoom_Y_Range is
      Result : constant Zoom_Y_Factor :=
                        (  Zoom_Y_Factor (Data.Area.Y_Gain)
                        /  Zoom_Y_Factor (Get_Height (Data.Area) - 1)
                        );
   begin
      if Result < Zoom_Y_Range'First then
         return Zoom_Y_Range'First;
      elsif Result > Zoom_Y_Range'Last then
         return Zoom_Y_Range'Last;
      else
         return Result;
      end if;
   end Get_Y_Zoom;

   function Get_Y_Zoom
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Zoom_Y_Range is
   begin
      return Widget.Data.Ptr.Get_Y_Zoom;
   end Get_Y_Zoom;

   procedure Gtk_New
             (  Widget      : out Gtk_Fuzzy_Linguistic_Set_Domain;
                Value       : Linguistic_Set;
                Annotated   : Boolean     := True;
                Domain_Note : UTF8_String := "";
                X_Scroll    : Boolean     := True;
                Y_Scroll    : Boolean     := True
             )  is
   begin
      Widget := new Gtk_Fuzzy_Linguistic_Set_Domain_Record;
      begin
         Initialize
         (  Widget,
            Value,
            Annotated,
            Domain_Note,
            X_Scroll,
            Y_Scroll
         );
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Hide_Accumulated (Data : in out Linguistic_Set_Data) is
   begin
      if Data.Accumulated /= 0 then
         Free_Color
         (  Data,
            Data.Palette.Vector (Data.Cardinality + 1).Position
         );
         Data.Accumulated := 0;
         Data.Updated := True;
      end if;
   end Hide_Accumulated;

   procedure Hide_Accumulated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      Data.Hide_Accumulated;
      Widget.Refresh;
   end Hide_Accumulated;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
                Value       : Linguistic_Set;
                Annotated   : Boolean;
                Domain_Note : UTF8_String;
                X_Scroll    : Boolean;
                Y_Scroll    : Boolean
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Event_Box.Initialize (Widget);
      Widget.Data.Set (new Linguistic_Set_Data);
      Gtk_New (Widget.Content);
      Widget.Add (Widget.Content);
      Widget.Content.Show;
      declare
         Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      begin
         Data.Annotated := Annotated;
         Data.Note_Text := Create (Domain_Note);
         Data.Cardinality := Get_Cardinality (Value);
         for Index in 1..Data.Cardinality loop
            Put (Data.Set, Index, Get (Value, Index));
         end loop;
      end;
      --
      -- Set handlers to make the widget truly resizable
      --
      Set_Default_Get_Preferred_Width_Handler
      (  Class_From_Type (Class_Record.The_Type),
         +Preferred_Width'Access
      );
      Set_Default_Get_Preferred_Height_Handler
      (  Class_From_Type (Class_Record.The_Type),
         +Preferred_Height'Access
      );

      Widget_Callback.Connect
      (  Widget,
         "destroy",
         Destroy'Access
      );
      Widget_Callback.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access
      );
      Gtk_New (Widget.View);
      Widget.Content.Put (Widget.View, 0, 0);
      Widget.View.Show;
      Return_Boolean_Callback.Connect
      (  Widget,
         "draw",
         Context_Marshaller.To_Marshaller (Draw'Access),
         Widget.all'Access,
         True
      );
      Style_Updated (Widget);
      Widget.Set_X_Scroll (X_Scroll);
      Widget.Set_Y_Scroll (Y_Scroll);
   end Initialize;

   procedure Insert
             (  Data  : in out Linguistic_Set_Data;
                Index : Positive;
                Value : Variable
             )  is
      Color : Color_Item;
   begin
      Data.Hide_Accumulated;
      Put (Data.Set, Data.Cardinality + 1, Value);
      Get_Color (Data, Color.Position);
      Color.Color :=
         To_RGB
         (  Val
            (  Data.First_Color,
               Natural (Color.Position),
               Color_Cycle
         )  );
      Put (Data.Palette, Data.Cardinality + 1, Color);
      if Index <= Data.Cardinality then
         Data.Set.Vector (Index + 1..Data.Cardinality + 1) :=
            Data.Set.Vector (Index..Data.Cardinality);
         Data.Set.Vector (Index) := Value;
         Data.Palette.Vector (Index + 1..Data.Cardinality + 1) :=
            Data.Palette.Vector (Index..Data.Cardinality);
         Data.Palette.Vector (Index) := Color;
         if not Data.Selected.Is_Empty then
            declare
               Selected : Selection;
               Var_No   : Variable_Index;
            begin
               for Item in 1..Data.Selected.Get_Size loop
                  Var_No := Data.Selected.Get_Key (Item);
                  if Var_No >= Variable_Index (Index) then
                     Var_No := Var_No + 1;
                  end if;
                  Selected.Add (Var_No, Get (Data.Selected, Item));
               end loop;
               Data.Selected := Selected;
            end;
         end if;
      end if;
      Data.Cardinality := Data.Cardinality + 1;
      Data.Updated := True;
   end Insert;

   procedure Install_Set_Properties (Class : GObject_Class) is
   begin
      Install_Style_Property
      (  Class,
         Gnew_Double
         (  Name  => "actions-merge-timeout",
            Nick  => "Merge timeout",
            Blurb => "Time span to merge consequent zooming actions",
            Minimum => 0.0,
            Maximum => 1000.0,
            Default => 0.3
      )  );
      Install_Style_Property
      (  Class,
         Gnew_String
         (  Name    => "x-move-tip",
            Nick    => "X-Move",
            Blurb   => "Tip of the X-move scale",
            Default => "Move along X-axis"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_String
         (  Name    => "x-zoom-tip",
            Nick    => "X-Zoom",
            Blurb   => "Tip of the X-zoom scale",
            Default => "Zoom X-axis"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_String
         (  Name    => "y-zoom-tip",
            Nick    => "Y-Zoom",
            Blurb   => "Tip of the Y-zoom scale",
            Default => "Zoom Y-axis"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_String
         (  Name    => "y-move-tip",
            Nick    => "Y-Move",
            Blurb   => "Tip of the Y-move scale",
            Default => "Move along Y-axis"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "major-tick-length",
            Nick    => "Major tick",
            Blurb   => "The length of major ticks",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 10
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "major-tick-x-step",
            Nick    => "Major tick X step",
            Blurb   => "The minimal horizontal step of major ticks",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 50
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "major-tick-y-step",
            Nick    => "Major tick Y step",
            Blurb   => "The minimal vertical step of major ticks",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 50
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "minor-tick-length",
            Nick    => "Minor tick",
            Blurb   => "The length of minor ticks",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 5
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "tick-gap",
            Nick    => "Tick gap",
            Blurb   => "The gap between tick and annotation",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 4
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Int
         (  Name    => "selection-radius",
            Nick    => "Selection radius",
            Blurb   => "The radius of point selection circle",
            Minimum => 0,
            Maximum => GInt'Last,
            Default => 20
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "first-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "First color",
            Blurb      => "The color of the first variable"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "background-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "background-color",
            Blurb      => "The background color"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "line-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Line color",
            Blurb      => "The color used to draw annotation lines"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "selection-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "selection-color",
            Blurb      => "The color used for points selection"
      )  );
   end Install_Set_Properties;

   function Is_Annotated
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record
            )  return Boolean is
   begin
      return Widget.Data.Ptr.Annotated;
   end Is_Annotated;

   procedure Marked
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             )  is
   begin
      Widget_Callback.Emit_By_Name (Widget, "marked");
   end Marked;

   function Middle_X (Area : Drawing_Area) return Number'Base is
   begin
      return
      (  Number'Base (Get_Width (Area) + 1)
      /  (2.0 * Area.X_Gain)
      +  Area.X_Offset
      );
   end Middle_X;

   function Middle_Y (Area : Drawing_Area) return Truth is
   begin
      return
      (  Area.Y_Offset
      -  Truth (Get_Height (Area) + 1)
      /  (2.0 * Area.Y_Gain)
      );
   end Middle_Y;

   procedure Move
             (  Data : in out Linguistic_Set_Data;
                From : Positive;
                To   : Positive
             )  is
      Var      : Variable;
      Color    : Color_Item;
      Selected : Selection;
      This     : Positive;
   begin
      Data.Hide_Accumulated;
      if From <= Data.Cardinality and then To <= Data.Cardinality then
         if From < To then
            Var := Data.Set.Vector (From);
            Data.Set.Vector (From..To - 1) :=
               Data.Set.Vector (From + 1..To);
            Data.Set.Vector (To) := Var;
            Color := Data.Palette.Vector (From);
            Data.Palette.Vector (From..To - 1) :=
               Data.Palette.Vector (From + 1..To);
            Data.Palette.Vector (To) := Color;
            for Index in 1..Data.Selected.Get_Size loop
               This := Positive (Data.Selected.Get_Key (Index));
               if This in From..To then
                  if This = From then
                     This := To;
                  else
                     This := This - 1;
                  end if;
               end if;
               Selected.Replace
               (  Variable_Index (This),
                  Data.Selected.Get (Index)
               );
            end loop;
         elsif From > To then
            Var := Data.Set.Vector (From);
            Data.Set.Vector (To + 1..From) :=
               Data.Set.Vector (To..From - 1);
            Data.Set.Vector (To) := Var;
            Color := Data.Palette.Vector (From);
            Data.Palette.Vector (To + 1..From) :=
               Data.Palette.Vector (To..From - 1);
            Data.Palette.Vector (To) := Color;
            for Index in 1..Data.Selected.Get_Size loop
               This := Positive (Data.Selected.Get_Key (Index));
               if This in To..From then
                  if This = From then
                     This := To;
                  else
                     This := This + 1;
                  end if;
               end if;
               Selected.Replace
               (  Variable_Index (This),
                  Data.Selected.Get (Index)
               );
            end loop;
         end if;
         Data.Selected := Selected;
      end if;
   end Move;

   procedure Preferred_Height
             (  Widget       : System.Address;
                Minimum_Size : out GInt;
                Natural_Size : out GInt
             )  is
      Stub : Gtk_Fuzzy_Linguistic_Set_Domain_Record;
      This : Gtk_Fuzzy_Linguistic_Set_Domain;
   begin
      This :=
         Gtk_Fuzzy_Linguistic_Set_Domain
         (  Glib.Object.Get_User_Data (Widget, Stub)
         );
      if This.Data.Is_Valid then
         Minimum_Size :=
            GInt'Max (1, GInt (This.Data.Ptr.Left_Offs) + 1);
      else
         Minimum_Size := 1;
      end if;
      Natural_Size := Minimum_Size;
   end Preferred_Height;

   procedure Preferred_Width
             (  Widget       : System.Address;
                Minimum_Size : out GInt;
                Natural_Size : out GInt
             )  is
      Stub : Gtk_Fuzzy_Linguistic_Set_Domain_Record;
      This : Gtk_Fuzzy_Linguistic_Set_Domain;
   begin
      This :=
         Gtk_Fuzzy_Linguistic_Set_Domain
         (  Glib.Object.Get_User_Data (Widget, Stub)
         );
      if This.Data.Is_Valid then
         Minimum_Size :=
            GInt'Max
            (  1,
               This.Width - GInt (This.Data.Ptr.Bottom_Offs) + 1
            );
      else
         Minimum_Size := 1;
      end if;
      Natural_Size := Minimum_Size;
   end Preferred_Width;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Value  : Linguistic_Set
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      Data.Hide_Accumulated;
      for Index in reverse 1..Data.Cardinality loop
         Data.Remove (Index);
      end loop;
      for Index in 1..Get_Cardinality (Value) loop
         Data.Insert (Index, Get (Value, Index));
      end loop;
      Data.Rescale_X (Widget.Get_X_Adjustment);
      Widget.Zoomed (True);
   end Put;

   procedure Refresh
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if Data.New_Small_Y then
         Data.New_Small_Y := False;
         Widget.Resize
         (  Widget.Get_Allocated_Width,
            Widget.Get_Allocated_Height,
            False
         );
      end if;
      if (  Data.Scrolled_X
         or else
            Data.Scrolled_Y
         or else
            Data.Zoomed_X
         or else
            Data.Zoomed_Y
         )
      then
         Widget.View.Queue_Draw;
         if (  Widget.Bottom_Right /= null
            and then
               (Data.Zoomed_X or else Data.Scrolled_X)
            )
         then
            Widget.Bottom_Right.Queue_Draw;
         end if;
         if (  Widget.Top_Left /= null
            and then
               (Data.Zoomed_Y or else Data.Scrolled_Y)
            )
         then
            Widget.Top_Left.Queue_Draw;
         end if;
      elsif Data.Updated then
         Widget.View.Queue_Draw;
      end if;
   end Refresh;

   procedure Remove
             (  Data  : in out Linguistic_Set_Data;
                Index : Positive
             )  is
   begin
      Data.Hide_Accumulated;
      if Index = Data.Cardinality then
         Data.Selected.Remove (Variable_Index (Data.Cardinality));
         Data.Free_Color
         (  Data.Palette.Vector (Data.Cardinality).Position
         );
         Data.Set.Vector (Data.Cardinality) := Empty;
         Data.Cardinality := Data.Cardinality - 1;
         Data.Updated := True;
      elsif Index < Data.Cardinality then
         Data.Set.Vector (Index..Data.Cardinality - 1) :=
            Data.Set.Vector (Index + 1..Data.Cardinality);
         Data.Set.Vector (Data.Cardinality) := Empty;
         Free_Color (Data, Data.Palette.Vector (Index).Position);
         Data.Palette.Vector (Index..Data.Cardinality - 1) :=
            Data.Palette.Vector (Index + 1..Data.Cardinality);
         Data.Cardinality := Data.Cardinality - 1;
         declare
            Selected : Selection;
            Var_No   : Variable_Index;
         begin
            for Item in 1..Get_Size (Data.Selected) loop
               Var_No := Get_Key (Data.Selected, Item);
               if Var_No /= Variable_Index (Index) then
                  if Var_No > Variable_Index (Index) then
                     Var_No := Var_No - 1;
                  end if;
                  Selected.Add (Var_No, Get (Data.Selected, Item));
               end if;
            end loop;
            Data.Selected := Selected;
         end;
         Data.Updated := True;
      end if;
   end Remove;

   procedure Rescale_X
             (  Data   : in out Linguistic_Set_Data;
                Span   : Interval;
                Scroll : Gtk_Adjustment;
                Whole  : Boolean
             )  is
      Scale : Interval := Span;
      Gain  : Number'Base;
   begin
      if Whole then
         Data.X_Range := Scale;
         Gain := Number'Base (Get_Width (Data.Area) - 1) / Length (Scale);
      else
         if Scale.From < Data.X_Range.From then
            Scale.From := Data.X_Range.From;
         elsif Scale.From > Data.X_Range.To then
            Scale.From := Data.X_Range.To;
         end if;
         if Scale.To < Data.X_Range.From then
            Scale.To := Data.X_Range.From;
         elsif Scale.To > Data.X_Range.To then
            Scale.To := Data.X_Range.To;
         end if;
         if (  Length (Data.X_Range)
            >  Length (Scale) * Number'Base (Zoom_X_Range'Last)
            )
         then
            Gain :=
               (  Number'Base (Get_Width (Data.Area) - 1)
               *  Number'Base (Zoom_X_Range'Last)
               /  Length (Data.X_Range)
              );
         else
            Gain :=
               (  Number'Base (Get_Width (Data.Area) - 1)
               /  Length (Scale)
               );
         end if;
      end if;
      Data.Set_X_Scale (Gain, Scale.From, Scroll);
   end Rescale_X;

   procedure Rescale_X
             (  Data   : in out Linguistic_Set_Data;
                Scroll : Gtk_Adjustment
             )  is
      Span  : Interval;
      Value : Number;
      Left  : Confidence;
      Min   : Confidence;
      Max   : Confidence;
      Right : Confidence;
   begin
      Span.From := Number'Last;
      Span.To   := Number'First;
      for Index in 1..Data.Cardinality loop
         declare
            Var : Variable renames Get (Data.Set, Index);
            No  : constant Natural := Get_Points_Number (Var);
         begin
            if No > 0 then
               Get_Point (Var, 1,  Value, Left, Min, Max, Right);
               Span.From := Number'Min (Span.From, Value);
               Get_Point (Var, No, Value, Left, Min, Max, Right);
               Span.To := Number'Max (Span.To, Value);
            end if;
         end;
      end loop;
      if Span.From >= Span.To then
         Data.Rescale_X ((0.0, 1.0), Scroll, True);
      else
         Data.Rescale_X (Span, Scroll, True);
      end if;
   end Rescale_X;

   procedure Rescale_Y
             (  Data   : in out Linguistic_Set_Data;
                From   : Truth;
                To     : Truth;
                Scroll : Gtk_Adjustment
             )  is
      Max    : constant Truth := 1.0;
      Top    : Truth := To;
      Bottom : Truth := From;
      Gain   : Truth;
   begin
      if Bottom < 0.0 then
         Bottom := 0.0;
      elsif Bottom > Max then
         Bottom := Max;
      end if;
      if Top < 0.0 then
         Top := 0.0;
      elsif Top > Max then
         Top := Max;
      end if;
      if 1.0 / Truth (Zoom_Y_Range'Last) > Top - Bottom then
         Gain :=
            Truth (Get_Height (Data.Area) - 1) * Truth (Zoom_Y_Range'Last);
      else
         Gain := Truth (Get_Height (Data.Area) - 1) / Truth (Top - Bottom);
      end if;
      Data.Set_Y_Scale (Gain, Top, Scroll);
   end Rescale_Y;

   procedure Rescale_Y
             (  Data   : in out Linguistic_Set_Data;
                Scroll : Gtk_Adjustment
             ) is
   begin
      Data.Rescale_Y (0.0, 1.0, Scroll);
   end Rescale_Y;

   procedure Resize
             (  Data     : in out Linguistic_Set_Data;
                X_Size   : X_Axis;
                Y_Size   : Y_Axis;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment;
                Resized  : out Boolean
             )  is
      use X_Axis_Ticks;
      use Y_Axis_Ticks;
      Width  : constant X_Axis :=
                  X_Axis'Max (X_Size - Data.Left_Offs,   4);
      Height : constant Y_Axis :=
                  Y_Axis'Max (Y_Size - Data.Bottom_Offs, 4);
   begin
      Resized :=
         (  Get_Width (Data.Area) /= Width
         or else
            Get_Height (Data.Area) /= Height
         );
      if Resized then -- Rescaling an existing drawing area
         Data.Set_Size (Width, Height);
         Data.Rescale_X (X_Scroll);
         Data.Rescale_Y (Y_Scroll);
         Data.Updated := True;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Resize (Linguistic_Set_Data)")
         )  );
   end Resize;

   procedure Resize
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Width  : GInt;
                Height : GInt;
                Force  : Boolean
             )  is
      Data     : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      Size     : Gtk_Requisition;
      X_Offs   : GInt := 0;
      Y_Offs   : GInt := 0;
      Resized  : array (1..2) of Boolean;
      X_Scroll : Gtk_Adjustment;
      Y_Scroll : Gtk_Adjustment;
   begin
      if Widget.X_Scroll /= null then
         -- Determine the height of the horizontal scroll
         Widget.X_Scroll.Size_Request (Size);
         Y_Offs   := Size.Height;
         X_Scroll := Widget.X_SCroll.Get_Adjustment;
      end if;
      if Widget.Y_Scroll /= null then
         -- Determine the width of the vertical scroll
         Widget.Y_Scroll.Size_Request (Size);
         X_Offs   := Size.Width;
         Y_Scroll := Widget.Y_SCroll.Get_Adjustment;
      end if;
      Data.Resize
      (  X_Axis (Width  - X_Offs),
         Y_Axis (Height - Y_Offs),
         X_Scroll,
         Y_Scroll,
         Resized (1)
      );
      Data.Set_Border (Widget);
      Data.Resize                   -- Resize again with border set
      (  X_Axis (Width  - X_Offs),
         Y_Axis (Height - Y_Offs),
         X_Scroll,
         Y_Scroll,
         Resized (2)
      );
      if Force or else Resized (1) or else Resized (2) then
         Widget.View.Set_Size_Request (Width - X_Offs, Height - Y_Offs);
         Widget.Content.Move (Widget.View, 0, Y_Offs);
         if Widget.X_Scroll /= null then
            Widget.X_Scroll.Set_Size_Request
            (  GInt (Get_Width (Data.Area)),
               Y_Offs
            );
            Widget.Content.Move
            (  Widget.X_Scroll,
               GInt (Data.Left_Offs),
               0
            );
            Widget.Top_Left.Set_Size_Request
            (  GInt (Data.Left_Offs),
               Y_Offs
            );
            Widget.Content.Move (Widget.Top_Left, 0, 0);
         end if;
         if Widget.Y_Scroll /= null then
            Widget.Y_Scroll.Set_Size_Request
            (  X_Offs,
               GInt (Get_Height (Data.Area))
            );
            Widget.Content.Move
            (  Widget.Y_Scroll,
               GInt (Get_Width (Data.Area) + Data.Left_Offs),
               Y_Offs
            );
            Widget.Bottom_Right.Set_Size_Request
            (  X_Offs,
               GInt (Data.Bottom_Offs)
            );
            Widget.Content.Move
            (  Widget.Bottom_Right,
               GInt (Get_Width (Data.Area) + Data.Left_Offs),
               GInt (Get_Height (Data.Area)) + Y_Offs
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Resize")
         )  );
   end Resize;

   function Scale (Value : Confidence) return Truth is
   begin
      return Truth (Value) / Truth (Confidence'Last);
   end Scale;

   procedure Scale
             (  Data     : in out Linguistic_Set_Data;
                Item     : Zoom_Undo_Item;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             )  is
   begin
      Data.Set_X_Scale (Item.X_Gain, Item.X_Offset, X_Scroll);
      Data.Set_Y_Scale (Item.Y_Gain, Item.Y_Offset, Y_Scroll);
   end Scale;

   procedure Scroll
             (  Data     : in out Linguistic_Set_Data;
                Value    : Interval;
                Level    : Fuzzy_Boolean;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             )  is
      Zoom_X : Zoom_X_Factor := Zoom_X_Factor'First;
      Zoom_Y : Zoom_Y_Factor := Zoom_Y_Factor'First;
      Gain_X : Number'Base;
      Gain_Y : Truth;
      Max_Y  : Truth;
      Min_Y  : Truth;
      Max_X  : Number'Base;
      Min_X  : Number'Base;
   begin
      if Value.From > Value.To then
         Max_X := Value.From;
         Min_X := Value.To;
      else
         Max_X := Value.To;
         Min_X := Value.From;
      end if;
      if Level.Necessity > Level.Possibility then
         Max_Y := Scale (Level.Necessity);
         Min_Y := Scale (Level.Possibility);
      else
         Max_Y := Scale (Level.Possibility);
         Min_Y := Scale (Level.Necessity);
      end if;
      if not Is_In (Value, Data.X_Range) then
         Data.Rescale_X
         (  (  From => Number'Min (Data.X_Range.From, Min_X),
               To   => Number'Max (Data.X_Range.To,   Max_X)
            ),
            X_Scroll,
            True
          );
      end if;
      Gain_X := (Max_X - Min_X) / Number'Base (Get_Width (Data.Area) - 1);
      if Gain_X < Data.Area.X_Gain then
         Zoom_X :=
            (  Zoom_X_Factor (Gain_X)
            *  Zoom_X_Factor (Length (Data.X_Range))
            /  Zoom_X_Factor (Get_Width (Data.Area) - 1)
            );
      end if;
      Gain_Y := (Max_Y - Min_Y) / Truth (Get_Height (Data.Area) - 1);
      if Gain_Y < Data.Area.Y_Gain then
         Zoom_Y :=
            (  Zoom_Y_Factor (Gain_Y)
            /  Zoom_Y_Factor (Get_Height (Data.Area) - 1)
            );
      end if;
      Data.Zoom (Zoom_X, Zoom_Y, X_Scroll, Y_Scroll);
      Data.Scroll_X (Min_X, Max_X, X_Scroll);
      Data.Scroll_Y (Max_Y, Min_Y, Y_Scroll);
   end Scroll;

   procedure Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Value  : Interval;
                Level  : Fuzzy_Boolean
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      Data.Scroll
      (  Value,
         Level,
         Widget.Get_X_Adjustment,
         Widget.Get_Y_Adjustment
      );
      Widget.Zoomed (True);
   end Scroll;

   procedure Scroll_Left
             (  Data   : in out Linguistic_Set_Data;
                Left   : Number'Base;
                Scroll : Gtk_Adjustment
             )  is
   begin
      Data.Set_X_Scale (Data.Area.X_Gain, Left, Scroll);
   end Scroll_Left;

   procedure Scroll_Top
             (  Data   : in out Linguistic_Set_Data;
                Top    : Truth;
                Scroll : Gtk_Adjustment
             )  is
   begin
      Data.Set_Y_Scale (Data.Area.Y_Gain, Top, Scroll);
   end Scroll_Top;

   procedure Scroll_X
             (  Data   : in out Linguistic_Set_Data;
                Left   : Number'Base;
                Right  : Number'Base;
                Scroll : Gtk_Adjustment
             )  is
   begin
      if Right > Data.X_Range.To then
         Data.Set_X_Scale
         (  Data.Area.X_Gain,
            Data.Area.X_Offset + Data.X_Range.To - Right,
            Scroll
         );
      end if;
      if Left < Data.X_Range.From then
         Data.Set_X_Scale
         (  Data.Area.X_Gain,
            Data.Area.X_Offset + Data.X_Range.From - Left,
            Scroll
         );
      end if;
   end Scroll_X;

   procedure Scroll_Y
             (  Data   : in out Linguistic_Set_Data;
                Top    : Truth;
                Bottom : Truth;
                Scroll : Gtk_Adjustment
             )  is
   begin
      if Top > 1.0 then
         Data.Set_Y_Scale
         (  Data.Area.Y_Gain,
            Data.Area.Y_Offset + 1.0 - Top,
            Scroll
         );
      end if;
      if Bottom < 0.0 then
         Data.Set_Y_Scale
         (  Data.Area.Y_Gain,
            Data.Area.Y_Offset - Bottom,
            Scroll
         );
      end if;
   end Scroll_Y;

   function Selection_Color (Color : Gdk_Color) return Gdk_Color is
   begin
      return
         To_RGB
         (  Darken
            (  Purify (To_IHLS (Color), Gdk_Saturation'Last),
               Gdk_Luminance'Last / 2
         )  );
   end Selection_Color;

   procedure Select_Point
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Index    : Positive;
                Point    : Positive;
                Selected : Boolean
             )  is
      Var_No   : constant Variable_Index := Variable_Index (Index);
      Point_No : constant Point_Index    := Point_Index (Point);
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if (  Index not in 1..Data.Cardinality
         or else
            Point not in 1..Get_Points_Number (Get (Data.Set, Index))
         )
      then
         raise Constraint_Error;
      end if;
      if Selected then
         if Data.Selected.Is_In (Var_No) then
            declare
               List : Points_Indices.Set := Data.Selected.Get (Index);
            begin
               if not List.Is_In (Point_No) then
                  List.Add (Point_No);
                  Data.Selected.Replace (Index, List);
               end if;
               Data.Updated := True;
               Widget.View.Queue_Draw;
            end;
         else
            declare
               List : Points_Indices.Set;
            begin
               List.Add (Point_No);
               Data.Selected.Add (Var_No, List);
            end;
            Data.Updated := True;
            Widget.View.Queue_Draw;
         end if;
      else
         if Data.Selected.Is_In (Var_No) then
            declare
               List : Points_Indices.Set := Data.Selected.Get (Index);
            begin
               if List.Is_In (Point_No) then
                  List.Remove (Point);
                  Data.Selected.Replace (Index, List);
                  Data.Updated := True;
                  Widget.View.Queue_Draw;
               end if;
            end;
         end if;
      end if;
   end Select_Point;

   procedure Select_Variable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Index    : Positive;
                Selected : Boolean
             )  is
      Var_No : constant Variable_Index := Variable_Index (Index);
      Data   : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if Index not in 1..Data.Cardinality then
         raise Constraint_Error;
      end if;
      if Selected then
         if not Is_In (Data.Selected, Var_No) then
            declare
               Empty : Points_Indices.Set;
            begin
               Data.Selected.Add (Var_No, Empty);
            end;
            Data.Updated := True;
            Widget.View.Queue_Draw;
         end if;
      else
         if Data.Selected.Is_In (Var_No) then
            Data.Selected.Remove (Index);
            Data.Updated := True;
            Widget.View.Queue_Draw;
         end if;
      end if;
   end Select_Variable;

   procedure Set_Annotated
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Annotated : Boolean
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if Data.Annotated xor Annotated then
         Data.Annotated := Annotated;
         Data.Scrolled_X := True;
         Widget.Refresh;
      end if;
   end Set_Annotated;

   procedure Set_Border
             (  Data   : in out Linguistic_Set_Data;
                Widget : not null access Gtk_Widget_Record'Class
             )  is
      use X_Axis_Ticks;
      use Y_Axis_Ticks;
   begin
      if Data.Annotated then
         if Data.Text = null then
            Data.Text := Widget.Create_Pango_Layout;
         end if;
         declare
            Width  : GInt;
            Height : GInt;
         begin
            Data.X_Tick_Step :=
               Style_Get (Widget, "major-tick-x-step");
            Data.Y_Tick_Step :=
               Style_Get (Widget, "major-tick-y-step");
            Data.Major_Tick_Length :=
               Style_Get (Widget, "major-tick-length");
            Data.Minor_Tick_Length :=
               Style_Get (Widget, "minor-tick-length");
            Data.Tick_Gap := Style_Get (Widget, "tick-gap");
            Data.Y_Ticks :=
               Y_Axis_Ticks.Create
               (  To_Necessity (Data.Area, Get_Height (Data.Area)),
                  To_Possibility (Data.Area, 1),
                  (  Integer (Get_Height (Data.Area))
                  / (Integer (Data.Major_Tick_Length) * 5)
               )  );
            Data.Text.Set_Text
            (  Strings_Edit.Floats.Image
               (  0.6666666,
                  AbsSmall => Data.Y_Ticks.Small
            )  );
            Data.Text.Get_Pixel_Size (Width, Height);
            Data.Bottom_Offs :=
               Y_Axis
               (  Height
               +  Data.Major_Tick_Length
               +  Data.Tick_Gap
               );
            Data.Left_Offs :=
               X_Axis
               (  Width
               +  Data.Major_Tick_Length
               +  Data.Tick_Gap * 2
               );
         end;
      else
         Data.Left_Offs   := 0;
         Data.Bottom_Offs := 0;
      end if;
      Data.Updated := True;
   end Set_Border;

   procedure Set_Domain_Note
             (  Widget      : not null access
                              Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Domain_Note : UTF8_String
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
   begin
      if Data.Note_Text.Ptr.Text /= Domain_Note then
         Data.Note_Text  := Create (Domain_Note);
         Data.Scrolled_X := True;
         Widget.Refresh;
      end if;
   end Set_Domain_Note;

   procedure Set_Selection
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Selected : Selection
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;

      procedure Add
                (  Var_No : Variable_Index;
                   From   : Points_Indices.Set
                )  is
         Points : Points_Indices.Set;
         Point  : Point_Index;
         Size   : constant Natural :=
                  Get_Points_Number (Get (Data.Set, Positive (Var_No)));

      begin
         for Index in 1..Get_Size (From) loop
            Point := Get (From, Index);
            if Point <= Point_Index (Size) then
               Points.Add (Point);
            end if;
         end loop;
         Data.Selected.Add (Var_No, Points);
      end Add;

      Same : Boolean := Selected.Get_Size = Data.Selected.Get_Size;
   begin
      if Same then
         for Index in 1..Get_Size (Selected) loop
            if (  (  Selected.Get_Key (Index)
                  /= Data.Selected.Get_Key (Index)
                  )
               or else
                  Selected.Get (Index) /= Data.Selected.Get (Index)
               )
            then
               Same := False;
               exit;
            end if;
         end loop;
      end if;
      if not Same then
         Data.Selected.Erase;
         for Index in 1..Selected.Get_Size loop
            declare
               Var_No : constant Variable_Index :=
                                 Selected.Get_Key (Index);
            begin
               if Var_No <= Variable_Index (Data.Cardinality) then
                  Add (Var_No, Selected.Get (Index));
               end if;
            end;
         end loop;
         Data.Updated := True;
         Widget.Refresh;
      end if;
   end Set_Selection;

   procedure Set_Size
             (  Data   : in out Linguistic_Set_Data;
                X_Size : X_Axis;
                Y_Size : Y_Axis
             )  is
   begin
      Data.Area.Pixels.Set_Size (X_Size + 1, Y_Size + 1);
      Data.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Setting size"
            &  X_Axis'Image (X_Size + 1)
            &  " X"
            &  Y_Axis'Image (Y_Size + 1)
            &  " fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Size")
         )  );
   end Set_Size;

   procedure Set_X_Scale
             (  Data   : in out Linguistic_Set_Data;
                Gain   : Number'Base;
                Offset : Number'Base;
                Scroll : Gtk_Adjustment
             )  is
      Area : Drawing_Area renames Data.Area;
   begin
      if Gain <= 0.0 then
         raise Constraint_Error with "Non-positive gain in Set_X_Scale";
      end if;
      if Area.X_Gain /= Gain then
         Area.X_Gain := Gain;
         Data.Zoomed_X := True;
         if Scroll /= null then
            declare
               Gain : constant GDouble := GDouble (Area.X_Gain);
               Page : constant GDouble :=
                               GDouble (Get_Width (Area) - 1) / Gain;
            begin
               Scroll.Set_Lower (GDouble (Data.X_Range.From));
               Scroll.Set_Upper (GDouble (Data.X_Range.To));
               Scroll.Set_Step_Increment (10.0 / Gain);
               Scroll.Set_Page_Increment (Page * Page_Inc);
               Scroll.Set_Page_Size (Page);
               Scroll.Changed;
            end;
         end if;
         -- Compute the new small of one pixel value
         declare
            Size  : Number'Base := 1.0 / Gain;
            Small : Integer := 0;
         begin
            if Size <= 0.1 then
               loop
                  Small := Small - 1;
                  Size  := Size * 10.0;
                  exit when Size > 0.1;
               end loop;
            elsif Size > 1.0 then
               loop
                  Small := Small + 1;
                  Size  := Size / 10.0;
                  exit when Size <= 1.0;
               end loop;
            end if;
            Data.X_Tracker_Small := Small;
         end;
      end if;
      if Area.X_Offset /= Offset then
         Area.X_Offset := Offset;
         Data.Scrolled_X := True;
         if Scroll /= null then
            declare
               Offs : constant GDouble := GDouble (Offset);
            begin
               if Scroll.Get_Value /= Offs then
                  Scroll.Set_Value (Offs);
               end if;
            end;
         end if;
      end if;
      if Data.Zoomed_X or else Data.Scrolled_X then
         -- Compute new power of the middle value
         Data.X_Ticks :=
            X_Axis_Ticks.Create
            (  To_Number (Area, 1).From,
               To_Number (Area, Get_Width (Data.Area)).To,
               Integer (Get_Width (Area)) / Integer (Data.X_Tick_Step)
            );
         declare
            Power : Integer := (Data.X_Ticks.Small / 3) * 3;
            Value : Number'Base := Middle_X (Area) / 10.0 ** Power;
         begin
            while Value >= 1000.0 loop
               Power := Power + 3;
               Value := Value / 1000.0;
            end loop;
            Data.X_Power := Power;
         end;
      end if;
   end Set_X_Scale;

   procedure Set_Y_Scale
             (  Data   : in out Linguistic_Set_Data;
                Gain   : Truth;
                Offset : Truth;
                Scroll : Gtk_Adjustment
             )  is
      Area : Drawing_Area renames Data.Area;
   begin
      if Gain <= 0.0 then
         raise Constraint_Error with "Non-positive gain in Set_Y_Scale";
      end if;
      if Area.Y_Gain /= Gain then
         Area.Y_Gain := Gain;
         Data.Zoomed_Y := True;
         if Scroll /= null then
            declare
               Page : constant GDouble :=
                               (  GDouble (Get_Height (Area) - 1)
                               /  GDouble (Area.Y_Gain)
                               );
            begin
               Scroll.Set_Page_Size (Page);
               Scroll.Set_Page_Increment (Page * Page_Inc);
               Scroll.Changed;
            end;
         end if;
      end if;
      if Area.Y_Offset /= Offset then
         Area.Y_Offset := Offset;
         Data.Scrolled_Y := True;
         if Scroll /= null then
            if Scroll.Get_Value /= 1.0 - GDouble (Area.Y_Offset) then
               Scroll.Set_Value (1.0 - GDouble (Area.Y_Offset));
            end if;
         end if;
      end if;
      if Data.Annotated and then (Data.Zoomed_Y or else Data.Scrolled_Y)
      then
         declare
            Small : constant Integer := Data.Y_Ticks.Small;
         begin
            Data.Y_Ticks :=
               Y_Axis_Ticks.Create
               (  To_Necessity (Area, Get_Height (Area)),
                  To_Possibility (Area, 1),
                  (  Integer (Get_Height (Area))
                  /  Integer (Data.Y_Tick_Step)
               )  );
            if Small /= Data.Y_Ticks.Small then
               Data.New_Small_Y := True;
            end if;
         end;
      end if;
   end Set_Y_Scale;

   procedure Set_X_Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Scroll : Boolean
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      X_Adjustment : Gtk_Adjustment;
   begin
      if Scroll xor Widget.X_Scroll /= null then
         if Scroll then
            declare
               Gain : constant GDouble := GDouble (Data.Area.X_Gain);
               Page : constant GDouble :=
                               GDouble (Get_Width (Data.Area)) / Gain;
            begin
               Gtk_New
               (  Adjustment => X_Adjustment,
                  Value => GDouble (Data.Area.X_Offset),
                  Lower => GDouble (Data.X_Range.From),
                  Upper => GDouble (Data.X_Range.To),
                  Step_Increment => 10.0 / Gain,
                  Page_Increment => Page * Page_Inc,
                  Page_Size      => Page
               );
            end;
            Gtk_New_HScrollbar (Widget.X_Scroll, X_Adjustment);
            Widget.Content.Put (Widget.X_Scroll, 0, 0);
            Gtk_New (Widget.Top_Left);
            Widget.Content.Put (Widget.Top_Left, 0, 0);
            Widget.Top_Left.Show;
            Widget.X_Scroll.Show;
         else
            -- Delete scroll bar
            Widget.Content.Remove (Widget.X_Scroll);
            Widget.Content.Remove (Widget.Top_Left);
            Widget.X_Scroll := null;
            Widget.Top_Left := null;
         end if;
--           Widget.Resize
--           (  Widget.Get_Allocated_Width,
--              Widget.Get_Allocated_Height,
--              True
--           );
      end if;
   end Set_X_Scroll;

   procedure Set_Y_Scroll
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Scroll : Boolean
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      Y_Adjustment : Gtk_Adjustment;
   begin
      if Scroll xor Widget.Y_Scroll /= null then
         if Scroll then
            declare
               Gain : constant GDouble := GDouble (Data.Area.Y_Gain);
               Page : constant GDouble :=
                               GDouble (Get_Height (Data.Area)) / Gain;
            begin
               Gtk_New
               (  Adjustment => Y_Adjustment,
                  Value      => 1.0 - GDouble (Data.Area.Y_Offset),
                  Lower          => 0.0,
                  Upper          => 1.0,
                  Step_Increment => 10.0 / Gain,
                  Page_Increment => Page * Page_Inc,
                  Page_Size      => Page
               );
            end;
            Gtk_New_VScrollbar (Widget.Y_Scroll, Y_Adjustment);
            Widget.Content.Put (Widget.Y_Scroll, 0, 0);
            Gtk_New (Widget.Bottom_Right);
--              Return_Boolean_Callback.Connect
--              (  Widget.Bottom_Right,
--                 "draw",
--                 Context_Marshaller.To_Marshaller
--                 (  Draw_Bottom_Right'Access
--                 ),
--                 Widget.all'Access
--              );
            Widget.Content.Put (Widget.Bottom_Right, 0, 0);
            Widget.Y_Scroll.Show;
            Widget.Bottom_Right.Show;
         else
            -- Delete scroll bar
            Widget.Content.Remove (Widget.Y_Scroll);
            Widget.Content.Remove (Widget.Bottom_Right);
            Widget.Y_Scroll     := null;
            Widget.Bottom_Right := null;
         end if;
--           Widget.Resize
--           (  Widget.Get_Allocated_Width,
--              Widget.Get_Allocated_Height,
--              True
--           );
      end if;
   end Set_Y_Scroll;

   procedure Show_Accumulated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Set    : Fuzzy.Set
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      Var  : Variable;
   begin
      if Data.Cardinality /= Set'Length then
         raise Constraint_Error;
      end if;
      for Index in 1..Data.Cardinality loop
         Var :=
            (  Var
            or (  Get (Data.Set, Index)
               and
                  Set (Set'First + Index - 1)
            )  );
      end loop;
      Data.Show_Accumulated (Var);
      Widget.Refresh;
   end Show_Accumulated;

   procedure Show_Accumulated
             (  Data : in out Linguistic_Set_Data;
                Var  : Variable
             )  is
   begin
      Data.Selected.Erase;
      if Data.Accumulated = 0 then
         if not Is_Empty (Var) then
            Data.Insert (Data.Cardinality + 1, Var);
            Data.Cardinality := Data.Cardinality - 1;
            Data.Accumulated := 1;
            Data.Updated := True;
         end if;
      else
         if Is_Empty (Var) then
            Data.Hide_Accumulated;
         elsif Get (Data.Set, Data.Cardinality + 1) /= Var then
            Data.Set.Put (Data.Cardinality + 1, Var);
            Data.Updated := True;
         end if;
      end if;
   end Show_Accumulated;

   procedure Style_Updated
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             )  is
      use Gdk;
   begin
      if Widget.Data.Is_Valid then
         declare
            Data  : Linguistic_Set_Data'Class renames
                    Widget.Data.Ptr.all;
            Color : Gdk_Color;
         begin
            Set_RGB (Color, GUInt16'Last, 0, 0);
            Data.Background :=
               Style_Get
               (  Widget,
                  "background-color",
                  RGB (1.0, 1.0, 1.0)
               );
            Data.Selection_Color :=
               Style_Get (Widget, "selection-color", Color);
            Data.Line_Color :=
               Style_Get (Widget, "line-color", RGB (0.0, 0.0, 0.0));
            Data.First_Color :=
               To_IHLS
               (  Style_Get
                  (  Widget,
                     "first-color",
                     To_RGB (Def_First_Color)
               )  );
            Data.Selection_Length :=
               Style_Get (Widget, "selection-radius");
            Data.Merge_Threshold :=
               Duration
               (  GDouble'
                  (  Style_Get (Widget, "actions-merge-timeout")
               )  );
            Data.Updated := True;
            Data.Repaint := True;
            Widget.Refresh;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

   function To_Confidence (Value : Truth) return Confidence is
      Result : constant Truth := Value * Truth (Confidence'Last);
   begin
      if Result >= Truth (Confidence'Last) then
         return Confidence'Last;
      elsif Result <= Truth (Confidence'First) then
         return Confidence'First;
      else
         return Confidence (Result);
      end if;
   end To_Confidence;

   function To_Necessity
            (  Area : Drawing_Area;
               Y    : Y_Axis
            )  return Truth is
   begin
      return Area.Y_Offset - (Truth (Y) - 0.5) / Area.Y_Gain;
   end To_Necessity;

   function To_Number
            (  Area : Drawing_Area;
               X    : X_Axis
            )  return Interval is
   begin
      return
      (  Interval'(Number'Base (X) - 1.5, Number'Base (X) - 0.5)
      /  Area.X_Gain
      +  Area.X_Offset
      );
   end To_Number;

   function To_Pixel (Color : Gdk_Color) return RGB_Pixel is
   begin
      return
      (  Red   => GUChar (Red   (Color) / 256),
         Green => GUChar (Green (Color) / 256),
         Blue  => GUChar (Blue  (Color) / 256)
      );
   end To_Pixel;

   function To_Possibility
            (  Area : Drawing_Area;
               Y    : Y_Axis
            )  return Truth is
   begin
      return Area.Y_Offset - (Truth (Y) - 1.5) / Area.Y_Gain;
   end To_Possibility;

   procedure To_X
             (  Area    : Drawing_Area;
                X       : Number'Base;
                Result  : out X_Axis;
                Cut_Off : out Clip_Type
             )  is
      Value : constant Number'Base :=
                  (X - Area.X_Offset) * Area.X_Gain + 1.0;
   begin
      Cut_Off := None;
      if Value < 1.0 then
         if Value < 0.5 then
            Cut_Off := Below;
         end if;
         Result := 1;
      elsif Value > Number'Base (Get_Width (Area)) then
         if Value > Number'Base (Get_Width (Area)) + 0.5 then
            Cut_Off := Above;
         end if;
         Result := Get_Width (Area);
      else
         Result := X_Axis (Value);
      end if;
   end To_X;

   function To_X
            (  Area : Drawing_Area;
               X    : Number'Base
            )  return X_Axis is
      Value : constant Number'Base :=
                  (X - Area.X_Offset) * Area.X_Gain + 1.0;
   begin
      if Value <= 1.0 then
         return 1;
      elsif Value >= Number'Base (Get_Width (Area)) then
         return Get_Width (Area);
      else
         return X_Axis (Value);
      end if;
   end To_X;

   procedure To_Y
             (  Area    : Drawing_Area;
                Y       : Truth;
                Result  : out Y_Axis;
                Cut_Off : out Clip_Type
             )  is
      Value : constant Truth := (Area.Y_Offset - Y) * Area.Y_Gain + 1.0;
   begin
      Cut_Off := None;
      if Value < 1.0 then
         if Value < 0.5 then
            Cut_Off := Below;
         end if;
         Result := 1;
      elsif Value > Truth (Get_Height (Area)) then
         if Value > Truth (Get_Height (Area)) + 0.5 then
            Cut_Off := Above;
         end if;
         Result := Get_Height (Area);
      else
         Result := Y_Axis (Value);
      end if;
   end To_Y;

   function To_Y (Area : Drawing_Area; Y : Truth) return Y_Axis is
      Value : constant Truth := (Area.Y_Offset - Y) * Area.Y_Gain + 1.0;
   begin
      if Value <= 1.0 then
         return 1;
      elsif Value >= Truth (Get_Height (Area)) then
         return Get_Height (Area);
      else
         return Y_Axis (Value);
      end if;
   end To_Y;

   function To_Y (Area : Drawing_Area; Y : Confidence) return Y_Axis is
   begin
      return To_Y (Area, Scale (Y));
   end To_Y;

   procedure Zoom
             (  Data     : in out Linguistic_Set_Data;
                X_Factor : Zoom_X_Factor;
                Y_Factor : Zoom_Y_Factor;
                X_Scroll : Gtk_Adjustment;
                Y_Scroll : Gtk_Adjustment
             )  is
   begin
      if (  X_Factor = Zoom_X_Factor'First
         and then
            Y_Factor = Zoom_Y_Factor'Base'First
         )
      then
         return;
      end if;
      if X_Factor /= Zoom_X_Factor'First then
         declare
            Area   : Drawing_Area renames Data.Area;
            Center : constant Number'Base := Middle_X (Area);
            Factor : Zoom_X_Factor;
         begin
            if X_Factor < Zoom_X_Range'First then
               Factor := Zoom_X_Range'First;
            elsif X_Factor > Zoom_X_Range'Last then
               Factor := Zoom_X_Range'Last;
            else
               Factor := X_Factor;
            end if;
            Data.Set_X_Scale
            (  (  Number'Base (Get_Width (Area) - 1)
               *  Number'Base (Factor)
               /  Length (Data.X_Range)
               ),
               Area.X_Offset,
               X_Scroll
            );
            if Data.Zoomed_X then
               -- Try to keep the middle point in the center
               Data.Set_X_Scale
               (  Area.X_Gain,
                  Area.X_Offset + Center - Middle_X (Area),
                  X_Scroll
               );
               Data.Scroll_X
               (  From_X (Area, 1),
                  From_X (Area, Get_Width (Area)),
                  X_Scroll
               );
            end if;
         end;
      end if;
      if Y_Factor /= Zoom_Y_Factor'First then
         declare
            Area   : Drawing_Area renames Data.Area;
            Center : constant Truth := Middle_Y (Area);
            Factor : Zoom_Y_Factor;
         begin
            if Y_Factor < Zoom_Y_Range'First then
               Factor := Zoom_Y_Range'First;
            elsif Y_Factor > Zoom_Y_Range'Last then
               Factor := Zoom_Y_Range'Last;
            else
               Factor := Y_Factor;
            end if;
            Data.Set_Y_Scale
            (  Truth (Get_Height (Area) - 1) * Truth (Factor),
               Area.Y_Offset,
               Y_Scroll
            );
            if Data.Zoomed_Y then
               -- Try to keep the middle point in the center
               Data.Set_Y_Scale
               (  Area.Y_Gain,
                  Area.Y_Offset + Center - Middle_Y (Area),
                  Y_Scroll
               );
               Data.Scroll_Y
               (  From_Y (Area, 1),
                  From_Y (Area, Get_Height (Area)),
                  Y_Scroll
               );
            end if;
         end;
      end if;
   end Zoom;

   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                X_Factor : Zoom_X_Range;
                Y_Factor : Zoom_Y_Range
             )  is
   begin
      if Widget.Data.Is_Valid then
         Widget.Data.Ptr.Zoom
         (  X_Factor,
            Y_Factor,
            Widget.Get_X_Adjustment,
            Widget.Get_Y_Adjustment
         );
         Widget.Zoomed (True);
      end if;
   end Zoom;

   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                X_Factor : Zoom_X_Range
             )  is
   begin
      if Widget.Data.Is_Valid then
         Widget.Data.Ptr.Zoom
         (  X_Factor,
            Zoom_Y_Factor'First,
            Widget.Get_X_Adjustment,
            Widget.Get_Y_Adjustment
         );
         Widget.Zoomed (True);
      end if;
   end Zoom;

   procedure Zoom
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Y_Factor : Zoom_Y_Range
             )  is
   begin
      if Widget.Data.Is_Valid then
         Widget.Data.Ptr.Zoom
         (  Zoom_X_Range'First,
            Y_Factor,
            Widget.Get_X_Adjustment,
            Widget.Get_Y_Adjustment
         );
         Widget.Zoomed (True);
      end if;
   end Zoom;

   procedure Zoomed
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record;
                Immediate : Boolean := False
             )  is
   begin
      if Widget.Data.Is_Valid then
         if Immediate then
            declare
               Data : Linguistic_Set_Data'Class renames
                      Widget.Data.Ptr.all;
               Emit : constant Boolean :=
                         (  Data.Scrolled_X
                         or else
                            Data.Scrolled_Y
                         or else
                            Data.Zoomed_X
                         or else
                            Data.Zoomed_Y
                         );
            begin
               Widget.Refresh;
               if Emit then
                  Widget_Callback.Emit_By_Name (Widget, "zoomed");
               end if;
            end;
         else
            Widget_Callback.Emit_By_Name (Widget, "zoomed");
         end if;
      end if;
   end Zoomed;

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain;
