# xml-skim

xml-skim wrings a lazy sequence of just-the-facts Clojure data
structures (maps!) from an XML stream, by plucking information
selectively from Java StAX events.

xml-skim is particularly meant for use with large-ish data files
containing a series of independent sections, stuffed mostly with
irrelevant clutter but with some arbitrarily-deeply nested elements
that are worthy of harvest.  These elements are identifiable by their
tag name alone, or by a "path" of tag names leading down from a
distinguishing ancestor element.

xml-skim is an alternative to sectioning an XML document until it fits
in memory, then parsing it with `clojure.xml`, then reworking the
XML data representation into data structures that make sense for
processing.

Unlike clojure.xml, xml-skim yields no data by default.  Only when 
the configuration says to make data does it gather data.

## Configuration

The configuration describes (a) the file and (b) the resulting data
structures.  See `doc/sample_declaration.clj`.

The configuration is pure data.  It could be serialized as EDN, read
from a resource, or constructed dynamically.  The configuration refers
via symbol to optional functions (particularly, to "complete"
structures populated in xml-skim's one-size-fits-all way).  The
program supplies a map of symbol to actual function.

## Algorithm

xml-skim reads the file forward-only.  It retains nothing but a
running tag path and the under-construction data structures ordained
by the configuration.

xml-skim composes StAX event loops and start-element and end-element
handlers from the declaration.

We use the tag-path *tail* because the *leaf* element is often a
sufficient basis for selecting a processing rule, and when it's not,
often the leaf and its immediate parent suffice.  

## Non-features

* Mixed content
* XML namespaces
* XPath-like "//" (elasticity)
* CSS-like "[@...]" (attribute conditions)
* SAX adapter (allowing a filter between the file and xml-skim)

## License

Copyright © 2013 Phill Wolf

Distributed under the Eclipse Public License, the same as Clojure.

