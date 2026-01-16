This  directory  contains  a  self-test programs. To build then with the
GNAT Ada compiler use project files.

1. Tests of the fuzzy machine learning framework:

   > gprbuild -Pfuzzy_ml-test.gpr

   Without gprbuild it can be built using gnatmake:

   > gcc -c ../sqlite-sources/sqlite3.c
   > gnatmake -Pfuzzy_ml-test.gpr -largs sqlite3.o

   The result file is test_graph_schemes[.exe]

2. Tests of the fuzzy machine learning widgets:

   > gprbuild -Pfuzzy_ml-gtk-test.gpr

   Without gprbuild:

   > gcc -c ../sqlite-sources/sqlite3.c
   > gnatmake -Pfuzzy_ml-gtk-test.gpr -largs sqlite3.o

   The result file is test_graph_schemes_gtk_widgets[.exe]

