# xml-skim

xml-skim transforms a StAX event stream to a lazy sequence of objects.

A configuration structure relates the XML structure (which is not
preconfigured, but rather simply endured at run time) with an object
structure.

xml-skim is a stream reader that has no overall document awareness.
It processes events from the stream.  When an element starts, xml-skim
allocates an object for it if the configuration says to, and pushes
the object on a stack.  When the element ends, xml-skim pops its
now-complete object from the stack and ejects it to the lazy sequence
or assigns it to a property of an enclosing object.  When there is
character content or an attribute that the configuration says to
capture, xml-skim assigns it to the nearest enclosing object that has
a property by the right name.

Leiningen reference:

	[com.hoofdust/xml-skim "0.3"]


## XML parsing configuration

See the annotated `doc/sample_configuration.clj`.

A small XML file to parse, configuration, and expected output are in
`dev-resources`.  The test cases illustrate applying the configuration
to the XML file.


## Why?

It is the Clojure way to "decomplect" a hard problem like parsing XML
into usable data structures.  For example, `clojure.xml` parses XML
into an elements-and-attributes tree, and you can transform that tree
into whatever you like by whatever means.  

Perhaps if your data do not fit comfortably in memory (necessitating
streaming) or if you do not like spending time building one tree only
to tear it down into another, something like xml-skim might be useful.

xml-skim's configuration states tag-path tails (as opposed to whole
paths) because, while XML instances may be infinitely perverse, the
less-perverse ones use distinctive tag names at or near the more
deeply nested levels.  At the opposite extreme, the document element
is the same for all paths in an XML instance.


## Things xml-skim does not do

* Mixed content
* XML namespaces
* Full XPath in configuration rules
* SAX adapter (allowing a filter between the file and xml-skim)


## Dedication

The author thanks the Republican Party for the time to devote to this
hobby project.


## License

Copyright © 2013, 2014 Phill Wolf

Distributed under the Eclipse Public License 1.0.
