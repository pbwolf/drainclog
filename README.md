# xml-skim

xml-skim plucks small facts from large XML files read with StAX.

A configuration data structure specifies:

* for which elements to allocate a hash-map, and what the map's keys
  might be, and whether each is single- or multi-valued.

* where (into which map entry) to put text content and attributes.

* which element's hash-map is a finished product to yield back to the
  consumer.

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

The configuration states tag-path tails (as opposed to whole paths)
because, while XML instances may be infinitely perverse, the
less-perverse ones use distinctive tag names at the more deeply nested
levels.  The document element, by contrast, is the same for all paths
in an XML instance.

Because dispatching by a single tag name or a minimal path tail seems
faster than dispatching by whole path, the configuration file
generally specifies incomplete paths that give no hint of the
ancestral relationship, if any, between the elements it describes.

Therefore, xml-skim's treatment of any particular element path in the
data is a function of both the configuration and the actual enclosing
elements.  For example, the configuration may say to assign the text
of element "Name" to hash member :Name.  But xml-skim does not know,
until it reads the data file, which enclosing element's map has the
:Name member to assign to.  xml-skim selects and configures start- 
and end-element handlers for a path the first time it's encountered
and memoizes them.

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

