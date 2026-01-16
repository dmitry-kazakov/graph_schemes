--                                                                    --
--  package Fuzzy.Persistence       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Deposit_Handles;             use Deposit_Handles;
with Fuzzy.Classifier;            use Fuzzy.Classifier;
with Fuzzy.Feature;               use Fuzzy.Feature;
with Fuzzy.Graph;                 use Fuzzy.Graph;
with Fuzzy.Lecture;               use Fuzzy.Lecture;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

package body Fuzzy.Persistence is

   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Classifier_Handle is
      Handle : constant Deposit_Handle :=
                        Get (Ptr (Storage), Name, Parent);
   begin
      return Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Get;

   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Feature_Handle is
      Handle : constant Deposit_Handle :=
                        Get (Ptr (Storage), Name, Parent);
   begin
      return Ref (To_Feature_Object_Ptr (Ptr (Handle)));
   end Get;

   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Lecture_Handle is
      Handle : constant Deposit_Handle :=
                        Get (Ptr (Storage), Name, Parent);
   begin
      return Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Get;

   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Node_Handle is
      Handle : constant Deposit_Handle :=
                        Get (Ptr (Storage), Name, Parent);
   begin
      return Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Get;

   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Classifier_Handle
            )  return String is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Get_Name (Ptr (Storage), Handle);
   end Get_Name;

   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Feature_Handle
            )  return String is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Get_Name (Ptr (Storage), Handle);
   end Get_Name;

   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Lecture_Handle
            )  return String is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Get_Name (Ptr (Storage), Handle);
   end Get_Name;

   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Node_Handle
            )  return String is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Get_Name (Ptr (Storage), Handle);
   end Get_Name;

   function Is_In
            (  Storage : Storage_Handle;
               Object  : Classifier_Handle
            )  return Boolean is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Is_In (Ptr (Storage), Handle);
   end Is_In;

   function Is_In
            (  Storage : Storage_Handle;
               Object  : Feature_Handle
            )  return Boolean is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Is_In (Ptr (Storage), Handle);
   end Is_In;

   function Is_In
            (  Storage : Storage_Handle;
               Object  : Lecture_Handle
            )  return Boolean is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Is_In (Ptr (Storage), Handle);
   end Is_In;

   function Is_In
            (  Storage : Storage_Handle;
               Object  : Node_Handle
            )  return Boolean is
      Handle : constant Deposit_Handle :=
                        Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      return Is_In (Ptr (Storage), Handle);
   end Is_In;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle, Name, Parent);
      Object := Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle, Name, Parent);
      Set_Name (Object, Name);
      Object := Ref (To_Feature_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle, Name, Parent);
      Object := Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle, Name, Parent);
      Object := Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle);
      Object := Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle);
      Object := Ref (To_Feature_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle);
      Object := Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Put (Ptr (Storage).all, Handle);
      Object := Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Put;

   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Classifier_Handle;
                New_Name : String;
                New_Parent : Deposit_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename (Ptr (Storage).all, Handle, New_Name, New_Parent);
      Object := Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Feature_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename (Ptr (Storage).all, Handle, New_Name, New_Parent);
      Object := Ref (To_Feature_Object_Ptr (Ptr (Handle)));
      Set_Name (Object, New_Name);
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Lecture_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename (Ptr (Storage).all, Handle, New_Name, New_Parent);
      Object := Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Node_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename (Ptr (Storage).all, Handle, New_Name, New_Parent);
      Object := Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Rename;

   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Classifier_Handle;
                New_Name : String
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename
      (  Ptr (Storage).all,
         Handle,
         New_Name,
         Get_Parent (Storage, Handle)
      );
      Object := Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Rename;

   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Feature_Handle;
                New_Name : String
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename
      (  Ptr (Storage).all,
         Handle,
         New_Name,
         Get_Parent (Storage, Handle)
      );
      Object := Ref (To_Feature_Object_Ptr (Ptr (Handle)));
      Set_Name (Object, New_Name);
   end Rename;

   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Lecture_Handle;
                New_Name : String
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename
      (  Ptr (Storage).all,
         Handle,
         New_Name,
         Get_Parent (Storage, Handle)
      );
      Object := Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Rename;

   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Node_Handle;
                New_Name : String
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Rename
      (  Ptr (Storage).all,
         Handle,
         New_Name,
         Get_Parent (Storage, Handle)
      );
      Object := Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Rename;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Unname (Ptr (Storage).all, Handle);
      Object := Ref (To_Classifier_Object_Ptr (Ptr (Handle)));
   end Unname;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Unname (Ptr (Storage).all, Handle);
      Object := Ref (To_Feature_Object_Ptr (Ptr (Handle)));
   end Unname;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Unname (Ptr (Storage).all, Handle);
      Object := Ref (To_Lecture_Object_Ptr (Ptr (Handle)));
   end Unname;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle
             )  is
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Ptr (Object)));
   begin
      Unname (Ptr (Storage).all, Handle);
      Object := Ref (To_Graph_Node_Ptr (Ptr (Handle)));
   end Unname;

end Fuzzy.Persistence;
