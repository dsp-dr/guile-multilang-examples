(define-module (dom-interaction)
  #:use-module (hoot ffi)
  #:export (create-button set-text-content add-event-listener))

(define-foreign document
  "document" 
  (ref extern))

(define-foreign create-element
  "createElement"
  (-> (ref string) (ref extern))
  (js-invoke document))

(define-foreign append-child
  "appendChild"
  (-> (ref extern) (ref extern))
  (js-invoke (js-global-ref "document.body")))

(define (set-text-content element text)
  "Set the textContent property of a DOM element."
  ((js-method element "textContent") text))

(define-foreign add-event-listener
  "addEventListener"
  (-> (ref extern) (ref string) (ref extern) (ref null)))

(define (create-button text onclick-handler)
  (let ((button (create-element "button")))
    (set-text-content button text)
    (add-event-listener button "click" onclick-handler)
    (append-child button)
    button))

(define (create-counter)
  (let ((count 0)
        (display-elem (create-element "div")))
    (define (update-display)
      (set-text-content display-elem 
                        (string-append "Count: " (number->string count))))
    
    (define (increment)
      (set! count (+ count 1))
      (update-display))
    
    (define (decrement)
      (set! count (- count 1))
      (update-display))
    
    (update-display)
    (append-child display-elem)
    (create-button "+" increment)
    (create-button "-" decrement)
    display-elem))

(define (main)
  (display "Creating interactive DOM elements...\n")
  (create-counter))

(when (eq? (current-module) (resolve-module '(dom-interaction)))
  (main))