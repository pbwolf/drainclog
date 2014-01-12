
;; The configuration is a vector of rules. 



;; Here is a sample rule.  It is a hash map of :path and optionally
;; :create, :atts, :text-value, and :prune.

{
 ;; :path is how to recognize an element. The *longest* configured
 ;; path that matches an element is the one that applies to the
 ;; element.  That is: if there are 3 rules: grand/parent/tag,
 ;; parent/tag, and tag, then grand/parent/tag will be selected for
 ;; great/grand/parent/tag and all other actual paths that end with
 ;; grand/parent/tag.  (Does not recognize "/" at the beginning
 ;; signifying the document root.)

 :path "tag" 

 ;; Each rule then specifies 
 ;;
 ;;   :create - if this element should be made into a map (structure
 ;;             of harvested data)
 ;;
 ;;   :atts - if this path has attributes that should be assigned to
 ;;             members of a map created for this element or an
 ;;             enclosing element
 ;;
 ;;   :text-value - if this path has text content (not mixed content!)
 ;;             that should be assigned to a member of a map created
 ;;             for this element or an enclosing element
 
 ;; If a Clojure map should be made from the element, the :create
 ;; member specifies the properties (map keys) the resulting object
 ;; may have, and optionally a :complete-by function (by symbol)
 ;; to filter the object on its way to being assigned or ejected.
 :create
 {
  :props ;; The Clojure map may have the following members:
  {
   :some-prop {:multi true} ;; some-prop will hold a vector of things.
   :another-prop {} ;; another-prop will hold one thing.
   }
  
  ;; When the element closes, the preliminary result is the accrued
  ;; map, if it has any populated members, otherwise nil.

  ;; If the accrued map needs refinement, use 
  :complete-by 'fn-symbol
  ;; to indicate indirectly a function of the preliminary result.  The
  ;; actual result is the function's value.  The parser's caller
  ;; supplies a map of symbol-to-actual-function.

  ;; What to do with a non-nil completed result:
  ;; either assign it to a property (of the nearest enclosing element
  ;; that declares the property)
  :assign :whatever-prop 
  ;; or eject it to the sequence of results: 
  :eject true
  }
 
 ;; If the element has significant attributes, assign them to properties:
 ;; The recipient is the nearest element that created a map with such a member.
 :atts {
        "xml-attr-name" {:assign :another-prop}} 
 
 ;; If the element has signficant text content, assign it to a property.
 ;; (Text of child elements is ignored.)
 :text-value {:assign :still-another-prop}
}

;; Alternatively:  a "prune" rule makes xml-skim ignore the matched
;; element and all of its content, to infinite depth.
;; It might save time by using an event loop with fewer conditions.
{ 
 :path "tag"
 :prune true
}

