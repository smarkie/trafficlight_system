#lang racket

;; Importing libraries that are required
;; for Main to work
(require "AsipMain.rkt")
(require "AMAN.rkt")

;; Input settings
;; We will only be using one pin which is inputPin=2
(define p 0)
(define inputPin 13)
(define curInput LOW)
(define oldInput LOW)

(connect)
(set-pin-mode! 2 OUTPUT_MODE)
(set-pin-mode! 3 3)
(set-pin-mode! 4 3)
(set-pin-mode! 5 3)
(set-pin-mode! 6 3)
(set-pin-mode! 7 3)
(set-pin-mode! 8 3)
(set-pin-mode! 9 3)
(set-pin-mode! 10 3)
(set-pin-mode! 11 3)
(set-pin-mode! 12 3)
(set-pin-mode! 13 INPUT_PULLUP_MODE)

(define pushed 0)

(define co 0)
(define changeLoop (λ ()
                     (cond
                       (( not (equal? pushed 1)) (c 13 LOW)))

                     (set! co (+ co 1))

                     (cond
                       ((equal? co 5)
                        (c 11 HIGH)
                        (c 10 LOW)
                        )
                       ((equal? co 7)
                        (c 11 LOW)
                        (c 13 HIGH)
                        (c 0 LOW))
                       ((equal? co 9)
                        (c 11 HIGH)
                        )
                       ((equal? co 11)
                        (c 13 LOW)
                        (c 11 LOW)
                        (c 10 HIGH)
                        (set! pushed 0)
                        (set! co 0)
                        (printTest)
                        )
                       )

                     (sleep 1)
                     (changeLoop)
                     )
  )                     

(define printTest (λ ()
                    (cond
                      ((equal? (digital-read inputPin) HIGH)
                       (c 3 LOW))
                      (#t
                       (cond
                         ((equal? pushed 1)
                          (set! pushed 1)
                          (c 3 HIGH)
                          (changeLoop)))
                         )
                      )
                    (sleep 0.01)
                    (printTest)
                    )
  )

(define i 0)
(define newLoopR (λ ()
                   (cond
                     ((equal? i 1)
                      (c 2 HIGH)
                      )
                     (#t
                      (cond
                        ((equal? (digital-read 13) 0)
                         (set! i 1)
                         )
                        )
                   
                         (sleep 0.5)
                         (newLoopR)
                      )
                     )
                   )
  )
(define loopR (λ ()
                (cond
                  ((not ( equal? i 0))
                   (display ""))
                  )
                (display (digital-read 13))
                (cond
                  ((equal? (digital-read 13) 0)
                   (set! i 1)
                   )
                  )
                (sleep 0.5)
                (loopR)
                )
  )

;; Main loop that will be called in order for the
;; code to work
(define counter 0)
(define setup (λ ()
                (c 6 1)
                (c 7 1)
                (c 2 1)
                )
  )

(define run (λ()
              (cond
                         ((equal? pushed 0)
                          (set! pushed 1)
                          (c 3 HIGH)
                          (changeLoop)))
                         )
  )

(define main (λ ()
               (cond
                      ((equal? (digital-read inputPin) HIGH)
                       (c 13 LOW))
                      (#t
                       (cond
                         ((equal? pushed 0)
                          (set! pushed 1)
                          (c 19 HIGH)
                          (changeLoop)))
                         )
                      )
                    ;;(c 13 (digital-read inputPin))
                    (sleep 0.01)
                    (set! counter (+ counter 1))
                    (display (string-append "\n" (number->string counter) "\n"))
                    (main)
               )
  )