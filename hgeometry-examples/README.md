Some Demo/Example applications that use HGeometry.

For the ones that use the WASM backend to render some GUI;

compile them using the build-wasm.sh script in the main dir. Run this
script from the root of the repository; the script takes a parameter,
namely the  name of the (WASM) demo to compile.

This produces a file 'pub/NameOfTheDemo.wasm'.

Run a webserver in the pub directory (e.g. using python -m
http.server)

and then go to i.e. http://localhost:8000?demo=NameOftheDemo
to view the demo.
