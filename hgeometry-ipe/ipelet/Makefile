ALL: libarith.so lualibhelper.so

libarith.so: LibArith.hs
	stack ghc -- $^ -o $@ -shared -fPIC -dynamic -lHSrts-ghc8.6.3

lualibhelper.so: libarith.so libarithhelper.c
	stack ghc -- libarithhelper.c -no-hs-main -o $@ -shared -fPIC -dynamic \
		-L. -larith -I/usr/include/lua5.3

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.so
	rm -f *_stub.h
