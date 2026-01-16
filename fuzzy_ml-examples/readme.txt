This  directory  contains  the  example  programs   described   in   the
documentation. To build these examples use  the  project  file  or  else
gnatmake: 

gnatmake -Pexample1.gpr
gnatmake -Pexample2.gpr
gnatmake -Pexample3.gpr
gprbuild -Pexample4.gpr                         -- ODBC variant
gprbuild -Pexample4.gpr example4_sqlite.adb     -- SQLite variant
