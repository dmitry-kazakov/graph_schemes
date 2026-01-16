--                                                                    --
--  package Fuzzy.Graph.Scheme      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2003       --
--                                                                    --
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
--
--  The package also defines the type Graph_Scheme which integrates four
--  graphs  into  one  unit.  A  graph-scheme  implements  an   abstract
--  classifier.
--
with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Fuzzy.Feature.Handle;     use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Handle;       use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Learning;     use Fuzzy.Graph.Learning;

with Fuzzy.Feature.Handle.Container;
with Fuzzy.Graph.Memory_Resident;

package Fuzzy.Graph.Scheme is
   pragma Elaborate_Body (Fuzzy.Graph.Scheme);
   Class : constant String := Classifier_Class & "Graph-scheme";

   type Root_Nodes_Array is array (Image_Type) of Node_Handle;
--
-- Graph_Scheme -- A graph-scheme
--
   type Graph_Scheme_Object is new Classifier_Object with record
      Roots    : Root_Nodes_Array;
      Updated  : Boolean  := False;
      From     : Positive := 1;
      Length   : Natural  := 0;
      Set_Name : String_Ptr;
   end record;
--
-- Example_Image -- Get image to classify
--
--    Image      - The image used in training
--    Complement - The example type
--
-- When a graph of the scheme has to classify an example, the image from
-- the training set to select is determined by the image  of  the  graph
-- (i.e.  the images on which the graph was trained) and the type of the
-- example  (positive  vs. negative). The following table summarizes the
-- result of this mapping:
--
--    Image        Complement  To classify
--    -----------  ----------  -----------
--    Has_In       False       Has_In
--    Has_In       True        Has_Not
--    Has_Not      False       Has_Not
--    Has_Not      True        Has_In
--    Has_Out      False       Has_Out
--    Has_Out      True        Has_Not_Out
--    Has_Not_Out  False       Has_Not_Out
--    Has_Not_Out  True        Has_Out
--
   Example_Image : constant array (Image_Type, Boolean)
                      of Image_Type :=
                         (  Has_In      => (Has_In,  Has_Not),
                            Has_Not     => (Has_Not, Has_In),
                            Has_Out     => (Has_Out, Has_Not_Out),
                            Has_Not_Out => (Has_Not_Out, Has_Out)
                         );
--
-- Classify -- Overrides Fuzzy.Classifier...
--
   overriding
   function Classify
            (  Scheme     : Graph_Scheme_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification;
--
-- Create -- Learn from a set of examples
--
--    Lesson      - The training set (a handle to)
--  [ Name ]      - The external name of the training set, UTF-8
--    Features    - The sequence of features to test
--    Classes     - The class feature
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of nodes in terms of quality
--    Viewer      - A progress indication object
--    Factory     - The defaut node factory
--
-- This function builds a  graph-scheme  from  the  fuzzy  training  set
-- Lesson. The parameters From and To specify the range of examples used
-- for learning. Usually it is the whole set. The parameter  To  can  be
-- greater than the total number of  examples.  The  parameter  Features
-- specifies  the sequence of features. The nodes of the graph will test
-- the features in the order specified by the sequence. The order has  a
-- substantial  influence  on  the resulting graph. The list may contain
-- No_Feature  elements,  which  are ignored. The parameter Classes is a
-- feature  describing  the set of classes the graph should classify to.
-- It must be a valid handle otherwise Constraint_Error  is  propagated.
-- The  parameter  Threshold  specifies  the   truncation   level.   All
-- confidence  factors  below  Threshold are treated as 0. The parameter
-- Equivalence is the Level of separation  hypotheses  quality.  If  the
-- qualities differ less than Equivalence, they are assumed  same.  When
-- Equivalence  is  Confidence'Last, features  are not selected, so they
-- order of testing is not changed. The parameter Factory specifies  the
-- node  factory  used by default to create new nodes. Usually it is the
-- first node created.
--
-- Returns :
--
--    The built graph-scheme (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - Learning was aborted
--
   function Create
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle;
   function Create
            (  Lesson      : Lecture_Handle;
               Name        : String;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle;
--
-- Create -- Learn from a set of examples
--
--    Context  - The training data context
--    Features - The sequence of features to test
--    From     - The first example to learn from
--    To       - The last example to learn from
--
-- This function builds a  graph-scheme  from  the  fuzzy  training  set
-- Lesson or one associated with Context.
--
-- Returns :
--
--    The built graph-scheme (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - Learning was aborted
--
   function Create
            (  Context  : not null access Graph_Training_Data'Class;
               Features : Feature_Array;
               From     : Positive := 1;
               To       : Positive := Positive'Last
            )  return Classifier_Handle;
--
-- Estimate -- Overrides Fuzzy.Classifier...
--
   overriding
   function Estimate
            (  Scheme     : Graph_Scheme_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set;
--
-- Finalize -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Finalize (Scheme : in out Graph_Scheme_Object);
--
-- Get_Class -- Overrides Object.Arhived...
--
   overriding
   function Get_Class (Scheme : Graph_Scheme_Object) return String;
--
-- Get_Classes -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Classes (Scheme : Graph_Scheme_Object)
      return Feature_Handle;
--
-- Get_Examples -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Get_Examples
             (  Scheme : Graph_Scheme_Object;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Features -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Features (Scheme : Graph_Scheme_Object)
      return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Graph -- Get graph
--
--    Classifier - Object or a handle to it
--    Image      - Of the graph to get
--
-- This function returns the root node of the  graph  deduced  from  the
-- image  specified by the parameter Image. The result can be an invalid
-- handle.
--
-- Returns :
--
--    A handle to the root node of the graph
--
-- Exceptions :
--
--    Constraint_Error - Classifier is invalid or not a graph-scheme
--
   function Get_Graph
            (  Classifier : Classifier_Handle;
               Image      : Image_Type
            )  return Node_Handle;
   function Get_Graph
            (  Classifier : Classifier_Object'Class;
               Image      : Image_Type
            )  return Node_Handle;
--
-- Get_Number_Of_Nodes -- Get number of nodes
--
--    Classifier - Object or a handle to it
--
-- Returns :
--
--    The number the nodes in the graph-scheme
--
-- Exceptions :
--
--    Constraint_Error - Classifier is invalid or not a graph-scheme
--
   function Get_Number_Of_Nodes (Classifier : Classifier_Handle)
      return Natural;
   function Get_Number_Of_Nodes (Classifier : Classifier_Object'Class)
      return Natural;
--
-- Get_Referents -- Overrides Object.Arhived...
--
   overriding
   procedure Get_Referents
             (  Scheme : Graph_Scheme_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Get_Training_Set_From -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_From (Classifier : Graph_Scheme_Object)
      return Positive;
--
-- Get_Training_Set_From -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Length (Classifier : Graph_Scheme_Object)
      return Natural;
--
-- Get_Training_Set_Name -- Overrides Fuzzy.Classifier...
--
   overriding
   function Get_Training_Set_Name (Classifier : Graph_Scheme_Object)
      return String;
--
-- Learn -- Overrides Fuzzy.Classifier...
--
   overriding
   procedure Learn
             (  Classifier : in out Graph_Scheme_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             );
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Scheme : Graph_Scheme_Object) return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Scheme : in out Graph_Scheme_Object);
--
-- Set_Graph -- Set a graph
--
--    Classifier - Object or a handle to it
--    Node       - The root node (a handle to)
--    Image      - Of the graph to get
--
-- This  procedure  changes a root node of a graph-scheme. The parameter
-- Node is a handle to the root node. Image is the graph of Scheme to be
-- set. Constraint_Error  is  propagated  if  the  operation  cannot  be
-- performed because the the new root  node  is  incompatible  with  the
-- graph-scheme.
--
-- Exceptions :
--
--    Constraint_Error - Classifier is invalid or  not  a  graph-scheme;
--                       wrong node
--
   procedure Set_Graph
             (  Classifier : in out Classifier_Handle;
                Node       : Node_Handle;
                Image      : Image_Type
             );
   procedure Set_Graph
             (  Classifier : in out Classifier_Object'Class;
                Node       : Node_Handle;
                Image      : Image_Type
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Scheme      : Graph_Scheme_Object
             );
private
   pragma Inline (Get_Class);
   pragma Inline (Get_Classes);
   pragma Inline (Get_Graph);
   pragma Inline (Is_Modified);
   pragma Inline (Reset_Modified);
   pragma Inline (Set_Graph);

end Fuzzy.Graph.Scheme;
