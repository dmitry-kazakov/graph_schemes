--                                                                    --
--  package Parsers.FCL.Compiler    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2005       --
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

with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;

with Fuzzy.Feature.Handle.Container;
with Ada.Finalization;

package Parsers.FCL.Compiler is
--
-- Rules_Tables -- Tables of rule sets
--
   type Rules_Block is record
      Lesson   : Lecture_Handle;
      Location : Parsers.Multiline_Source.Location;
   end record;

   package Rules_Tables is new Tables (Rules_Block);
   package Rules_Lists is
      new Rules_Tables.UTF8_Names (Blanks => Blank_Characters);
--
-- Program -- The FCL program
--
--    Name   - The program name
--    Input  - The set of input variables
--    Output - The set of output variables
--    Rules  - The list of rules blocks
--
   type Program is
      new Ada.Finalization.Limited_Controlled with
   record
      Name   : Unbounded_String;
      Input  : Container.Set;
      Output : Container.Set;
      Rules  : Rules_Lists.Dictionary;
   end record;
--
-- Compile -- An FCL program
--
--    Compiler - The FCL program object (being compiled)
--    Code     - The source code
--
-- This procedure compiler Code and stores the result of the translation
-- into fields of Compiler which are erased before it starts.
--
-- Exceptions :
--
--    Syntax_Error - Translation error
--    I/O exceptions are propagated from operations on Code
--
   procedure Compile
             (  Compiler : in out Program'Class;
                Code     : in out Parsers.Multiline_Source.Source'Class
             );
--
-- Create_Rules -- Rules block creation
--
--    Compiler - The FCL program object
--    Name     - Of the rule block
--
-- This  function  is  called  to create a new rules block. It returns a
-- handle to the training set where the rules of the block are stored as
-- training examples. The default implementation creates a memory-mapped
-- training  set. When overridden an implementation is allowed to return
-- handle  to  any  training  set. If the set is not empty, the examples
-- generated out of the rule block are appended to it.
--
-- Returns :
--
--    A handle to a training set
--
   function Create_Rules
            (  Compiler : Program;
               Name     : String
            )  return Lecture_Handle;
--
-- Created_Feature -- Feature creation notification
--
--    Compiler - The FCL program object
--    Feature  - A handle to
--
-- This  procedure  is  called  when  Compiler  has  created Feature. An
-- implementation may use it to put  the  feature  into  the  persistent
-- storage. The default implementation does nothing.
--
   procedure Created_Feature
             (  Compiler : in out Program;
                Feature  : in out Feature_Handle
             );

end Parsers.FCL.Compiler;
