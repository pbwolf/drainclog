
;; The configuration is a vector of rules. Here is a sample rule:

{
 ;; How to recognize an element:
 ;; (The first rule that matches an element is the one that applies.)
 :path "tag" ;; or "parent/tag", "grand/parent/tag", etc.

 ;; If a Clojure map should be made from the element:
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
  ;; to indicate indirectly a function of the preliminary result.
  ;; The actual result is the function's value.
  ;; The parser's caller supplies a map of symbol-to-actual-function.

  ;; What to do with the completed result:
  ;; either assign it to a property (of the nearest enclosing element
  ;; that declares the property)
  :assign :whatever-prop 
  ;; or eject it (to the sequence of results): 
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

