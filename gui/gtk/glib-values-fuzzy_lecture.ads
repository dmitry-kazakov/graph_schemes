--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy_Lecture                   Luebeck            --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                Last revision :  14:26 11 Feb 2012  --
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
--  This package provides a GTK+ type, named GType_Lecture.  It  can  be
--  used to place a training set into a GTK+ value.
--
with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;
with GLib.Values.Handle;

package GLib.Values.Fuzzy_Lecture is
--
-- Get_Lesson -- Get training set from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- GType_Lecture.
--
-- Returns :
--
--    A handle to the trainign set
--
-- Exceptions :
--
--    Constraint_Error - The value is not a training set
--
   function Get_Lesson (Value : GValue) return Lecture_Handle;
--
-- GType_Lecture -- The GTK+ type of training sets
--
   function GType_Lecture return GType;
--
-- Set_Lesson -- Set a value
--
--    Value  - To set
--    Lesson - A handle to the training set
--
-- This  procedure  sets  a  training  set  into  GTK+ value, previously
-- initialized using Init with the parameter GType_Lecture.
--
-- Exceptions :
--
--    Constraint_Error - Not an object value, invalid handle
--
   procedure Set_Lesson
             (  Value  : in out GValue;
                Lesson : Lecture_Handle
             );
private
   use Fuzzy.Lecture;
--
-- Gtk_Values -- Training sets as GTK+ values
--
   package Gtk_Values is
      new GLib.Values.Handle
          (  Type_Name       => "GFuzzyLecture",
             Object_Type     => Lecture_Object,
             Object_Type_Ptr => Lecture_Object_Ptr,
             Handle_Type     => Lecture_Handle
          );
   function Get_Lesson (Value : GValue) return Lecture_Handle
      renames Gtk_Values.Get_Handle;
   function GType_Lecture return GType
      renames Gtk_Values.Get_Type;
   procedure Set_Lesson
             (  Value  : in out GValue;
                Lesson : Lecture_Handle
             )  renames Gtk_Values.Set_Handle;

end GLib.Values.Fuzzy_Lecture;
