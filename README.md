# Universal Keyboard Designer

This is a tool used for creating 3D printable cases. 

I wanted something similar to the Dactyl but that only allows for curvature in the x and y axes. My tool can take any 3D equation and the keyplate will take the equation's shape.

## How to use it

There is the some small config at the top of the src/optcase/core.clj file.

The main config will be at the bottom in the writingArrayFunctions and readingArrayFunctions functions. 
The writingArrayFunctions modifies the array with functions such as:
- moveonXYZ
- alignkeys
- angleKey


Take a look at the hf40 branch for an example of a fully modified design.


### Generating a Design

(from Adereth's Dactyl)

**Setting up the Clojure environment**
* [Install the Clojure runtime](https://clojure.org)
* [Install the Leiningen project manager](http://leiningen.org/)
* [Install OpenSCAD](http://www.openscad.org/)

**Generating the design**
* Run `lein repl`
* Load the file `(load-file "src/optcase/core.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files

**Tips**
* [Some other ways to evaluate the clojure design file](http://stackoverflow.com/a/28213489)
* [Example designing with clojure](http://adereth.github.io/blog/2014/04/09/3d-printing-with-clojure/)
