
# Author: Benjamin Cooper
# Builds a group of tools to find cyclomatic complexity for programs in various languages
# This will be replaced by a better version once finished (probably).

#################################
#          Definitions          #
#################################

CC = ocamlc
CPARSE = ../lang_c/parsing
CPPPARSE = ../lang_cpp/parsing
JAVAPARSE = ../lang_java/parsing
GENERIC = ../h_program-lang
COMMON = ../commons
GLOBALS = ../globals
GRAPH = ../../analysis_tools


INCLUDEDIRS=../commons \
            ../h_program-lang ../lang_cpp/parsing ../h_version-control ../lang_c/parsing

INCLUDES=$(INCLUDEDIRS:%=-I %)

OCAMLCFLAGS=-g -annot

OCAMLC=$(CC) $(OCAMLCFLAGS) $(INCLUDES)

PROGS=c_cyclo.byte java_cyclo.byte c_parse_tester.byte

SYSLIBS= str.cma unix.cma nums.cma bigarray.cma graph.cma

LIBS= ../commons/lib.cma \
      ../globals/lib.cma \
      ../h_version-control/lib.cma \
      ../h_program-lang/lib.cma \
      ../lang_cpp/parsing/lib.cma \
      ../lang_c/parsing/lib.cma


#################################
#          Compilation          #
#################################


all:: $(PROGS) clean1

clean1::
	rm -f c_cyclo
	rm -f c_parse_tester

%.byte: %.cmo
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $(LIBS) $^

clean::
	rm -f *.opt *.byte


c_cyclo: c_cyclo.ml
	ocamlc -g -o c_cyclo \
         -I ../commons -I ../lang_c/parsing -I ../h_program-lang -I ../globals \
          str.cma unix.cma nums.cma bigarray.cma graph.cma \
	  ../commons/lib.cma ../globals/lib.cma ../h_program-lang/lib.cma ../lang_cpp/parsing/lib.cma \
          ../lang_c/parsing/lib.cma graph.cmo \
          c_cyclo.ml 

c_parse_tester: c_parse_tester.ml
	ocamlc -g -o c_parse_tester \
         -I ../commons -I ../lang_c/parsing -I ../h_program-lang -I ../globals \
          str.cma unix.cma nums.cma bigarray.cma \
	  ../commons/lib.cma ../globals/lib.cma ../h_program-lang/lib.cma ../lang_cpp/parsing/lib.cma \
          ../lang_c/parsing/lib.cma \
          c_parse_tester.ml 

#java_cyclo: java_cyclo.ml
#	$(CC) -g -o java_cyclo \
#	 -I $(COMMON) -I $(JAVAPARSE) \
#	  graph.cmo \
#	  java_cyclo.ml

