# mmhyph

mmhyph is a reimplementation of the hyphenation algorithm from the
[libhyphen](https://github.com/hunspell/hyphen) library
that is intended to reduce the in-memory footprint of loaded
hyphenation dictionaries, especially when the same dictionary
may be in use by multiple processes.

To reduce memory footprint, mmhyph uses hyphenation dictionaries that are
"precompiled" into a flat, position-independent binary format that is used
directly by the runtime hyphenation functions.
Therefore, dictionaries do not have to be parsed into a dynamic structure in memory;
the files can simply be mmap'd into the address space and immediately used.
In addition, a compiled dictionary mapped into a shared-memory block
can be made available to multiple processes for no added physical memory cost.

One deliberate simplification compared to libhyphen
is that mmhyph only accepts UTF-8 text and hyphenation dictionaries;
legacy non-Unicode encodings are not supported.

mmhyph has been created primarily for use by Gecko, replacing the use of libhyphen,
and so its features (and limitations) are based on this use case.
However, it is hoped that it will also be more generally useful.

## Functionality

Currently, mmhyph supports only "standard" hyphenation, where spelling does not
change around the hyphenation position. At present this is the only kind of
hyphenation supported in Gecko.

The compiled hyphenation dictionary format includes provision for replacement
strings and indexes, as used by libhyphen to support non-standard hyphenations
(e.g. German "Schiffahrt" -> "Schiff-fahrt"), but the `find_hyphen_values` function
will panic!() with a "not yet implemented" message if it encounters such a case.
(None of the hyphenation dictionaries shipping with Firefox includes such rules.)

## Licensing

Please see the file named
[LICENSE](https://github.com/jfkthame/mmhyph/blob/master/LICENSE),
or the [MPL 2.0](https://www.mozilla.org/en-US/MPL/2.0/) license online.

## Documentation

TODO

## C and C++ bindings

See the `mmhyph.h` header for C/C++ APIs that can be used to load hyphenation files
and to locate valid hyphenation positions in a word.

## Sample programs

See main.rs for a simple example program.

## Compiled dictionaries

A tool to compile dictionary files (`*.dic`) as used by libhyphen
into mmhyph's binary format is not yet included.

## Release Notes

### 0.1.0

* Initial release.
