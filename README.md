# Universal Keyboard Designer

This is a tool used for creating 3D printable keyplates and cases.

[Pictures of 3D models](https://imgur.com/a/YmXi8)

I wanted something similar to the [Dactyl](https://github.com/adereth/dactyl-keyboard/) but his program only allows for curvature in the x and y axes, and deviations from his keyboard shape is dealt with as an exception in his key-place function.

Each key is described by a dictionary which contains its metadata. Such as
* position in the matrix
* position in space (coordinates)
* if the key exists (useful if you only want a few keys in a column)
* keycap size (useful for visualizing)

The design process is to start off with a 2D matrix of dictionaries. Then functions are applied to the dictionaries. Then the 3D shapes are derived from the dictionaries.

### How to use it

There is the some small config at the top of the src/optcase/core.clj file. This is for things like matrix width and height, the spacing between the keys, size of keycaps, thickness of the plate.

The main config is at the bottom in the writingArrayFunctions and readingArrayFunctions functions.
The writingArrayFunctions modifies the array with functions such as:
- moveonXYZ (can be for individual keys, specific columns or rows)
- alignkeys (useful for "pinning" a key to another one)
- angleKey (changes the directional vector of the key)
- curveit  (takes any 3D equation and the keyboard will take that shape)
- rotateKey (spins a key around the z-axis)
- changeNonExistentKeys (sets the existence of the keys to true/false)

Once the editing of the array is done, the final array gets passed onto to the readingArrayFunctions.
This is for things like:
* makeconnectors (creates the keyplate or the base of the case)
* showkeycaps (good for seeing if the keycaps won't touch)
* showconnectors (shows the vectors describing the position for each key)
* showsquareatkey (good for debugging)
* putupapost (puts a post in a corner of a specific key to allow joining custom shapes (e.g. screen mounts) to the keyboard)


### Setting Clojure up (from dactyl by adereth)

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

### Need help?
Feel free to open an issue with what you are trying to accomplish.
