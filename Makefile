all: mc-test

LIBGEOM=../libgeom
MARCHING=.

INCLUDES=-I$(LIBGEOM) -I$(MARCHING)
LIBS=-L$(LIBGEOM)/release -lgeom -L$(MARCHING)/build -lmarching

CXXFLAGS=-std=c++20 -Wall -Wno-unused-but-set-variable -pedantic -O3 $(INCLUDES)

mc-test: mc-test.o
	$(CXX) -o $@ $^ $(LIBS)
