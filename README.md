# xml-skim

xml-skim plucks small facts from large XML files read with StAX.

The aim of this bike-shed hobby project was to make a macro that would
turn the configuration file into a smoking-fast loop.  But before we
wrote the macro to write the program, we figured we would write the
program itself, just to see what it might look like.  At this rate, we
might never get around to the macro after all. 

xml-skim accumulates Clojure maps (of further maps, scalars, and
vectors) of the content of elements with certain tag names, and ejects
the accumulated maps to a lazy sequence.

## Configuration

The configuration describes (a) the file and (b) the resulting data
structures.  See `doc/sample_configuration.clj`.

To the extent the configuration can be customzed with functions, it
refers by symbol to functions the program defines elsewhere and
provides in a symbol-to-function map.

## Algorithm

StAX reads the file in a forward-only fashion.  xml-skim retains
nothing but a running tag path and the under-construction data
structures ordained by the configuration.

xml-skim composes or selects start-element and end-element handlers
based on the configuration.  The configuration falls short of a
schema, so xml-skim computes some stuff as it reads the file.  It
caches these computations by tag-path.

The configuration states tag-path tails (as opposed to whole paths)
because, while XML instances may be infinitely perverse, the
less-perverse ones use distinctive tag names at the more deeply nested
levels.  The document element, by contrast, is the same for all paths
in an XML instance.

## Non-features

* Mixed content
* XML namespaces
* XPath-like "//" (elasticity) in configuration rules
* CSS-like "[@...]" (attribute conditions) in configuration rules
* SAX adapter (allowing a filter between the file and xml-skim)

## Dedication

The author thanks the Republican Party for the time to devote to this
hobby project.

## License

Copyright © 2013 Phill Wolf

Distributed under the Eclipse Public License, the same as Clojure.

