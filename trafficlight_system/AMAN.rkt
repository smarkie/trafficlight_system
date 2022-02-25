#lang racket

;; Arduino Manager Library
;; Controlling the GPIO using AsipMain.rkt
;; Patrick Falcon M00668092
;; Ana Catarina Cardoso M00634184
;; Smith Rajesh D'Britto M00689896
;; Rojaht Sipan M00643413

;; gregor library needs to be imported before the code is compiled in order
;; to allow for timings to work correctly
(require gregor)
(require gregor/time)
(require "AsipMain.rkt")
(require json)

(define warnings 0)

(cond
  ((not(directory-exists? "sim/"))
   (display (string-append "\nWARNING: 'sim/' cannot be found within the directory.\n "))
   (set! warnings (+ warnings 1))
   )
  )


;; Simple version check to avoid using the wrong version
;; of Racket. Causes errors within the AsipMain.rkt file
(cond
  ((string-contains? (version) "7.")
   (raise "Please use version 6.X!")
   )
  )

(provide
 ports
 set-ports
 connect
 c
 turnOff
 turnOn
 testAll
 pushed
 disconnect
 inverse
 in
 out
 h
 setTimes
 l
 physInput?
 get-amber
 get-green
 set-simulation
 reload-sim
 get-value
 hour
 time
 red-time
 green-time
 redAmber-time
 amber-time
 is-sim?)

;; List of all ports that are being used
;; Example: '(1 2 3 4)
(define ports '())
(define inputs '())

;; Light state
;; 1 = 13,11,10
;; 2 = ...
;; 3 = ...
(define lightState '(1 2 3))

;; Simplified INPUT_PULLUP_MODE and OUTPUT_MODE
(define in INPUT_PULLUP_MODE)
(define out OUTPUT_MODE)
(define h HIGH)
(define l LOW)

;; Gets the inverse of each of the arguments
;; that should be used
(define inverse (λ (v1)
                  (cond
                    ((equal? v1 HIGH) LOW)
                    ((equal? v1 LOW) HIGH)
                    ((equal? v1 OUTPUT_MODE) INPUT_PULLUP_MODE)
                    ((equal? v1 INPUT_PULLUP_MODE) OUTPUT_MODE)
                    ((equal? v1 h) l)
                    ((equal? v1 l) h)
                    ((equal? v1 in) out)
                    ((equal? v1 out) in)
                    ((equal? v1 0) 1)
                    ((equal? v1 1) 0)
                    )
                  )
  
  )

;; Needs to be called at the end of the project in order to cut off any
;; lingering connection to the library
(define disconnect (λ ()
                     (close-asip)
                     (display (string-append "\n[ - ] Disconnected from ASIP [ - ]\n ")))
  )

;; Pushed is for user input
(define pushed 0)

;; To replace the digital write
;; Makes it easier to type
(define c (λ (v1 v2)
            (digital-write v1 v2)
            )
  )

(define testAll (λ ()
                  (display (string-append "\n[ - ] Turning on all available ports [ - ]\n "))
                  (turnOn)
                  (sleep 0.25)
                  (turnOff)
                  (display (string-append "\n[ - ] Test completed [ - ]\n "))
                  )
  )

;; Simulation and JSON management
(define simulation 0)
(define simFile "null.json")
(define load-file null)
(define reload-sim (λ ()
                     (set-simulation simFile)
                     (display (string-append "\nSimulation reloaded\n "))
                     )
  )

;; set-simulation is the file loader for the JSON file. Simulations can be found
;; in the 'sim/' directory that is located within the code
(define set-simulation (λ (v1)
                         (set! simulation 1)
                         (set! simFile v1)
                         (set! load-file (call-with-input-file simFile read-json))
                         )
  )

;; get-value is a simple nested JSON parser. ' needs to be used in order to grab
;; a variable that has been defined in the method
(define get-value (λ (v1 v2)
                    (cond
                      ((equal? load-file null)
                       (raise "no simulation loaded")
                       )
                      )
                 (hash-ref (hash-ref load-file v1) v2)
                 )
  )

;; Checking to see if the load-file variable is assigned to anything
;; this can now be used inside a condition to see whether we are loading a file to change the variable
(define is-sim?(λ ()
                (cond
                  ((equal? load-file null) #f)
                  (#t #t)
                  )
                )
  )

;; Puts power to high for all registered output ports
(define turnOn (λ ()
                 (for ([i ports])
                   (c i HIGH)
                   )
                 )
  )

;; Foundations for light changes
;; will handle all other changes too
(define rt 0)
(define at 0)
(define gt 0)
(define st 0)
(define changeState (λ (v1)

                      (cond
                        ((equal? v1 13)
                         (set! rt 13)
                         (set! at 11)
                         (set! gt 10)
                         (set! st (list-ref lightState 0))
                        )
                      )

                      ;; basic change

                      (cond
                        ;; Green Light
                        ((equal? st 1)
                         (sleep 2)
                         (c gt l)
                         (c at h)
                         (sleep 3)
                         (c rt h)
                         (c at l)
                         (sleep 2)
                         (c at h)
                         (sleep 1)
                         (c at l)
                         (c rt l)
                         (c gt h)
                         )
                        ;; Red light
                        ((equal? st 2)
                        )
                      )
  )
  )

;; Test for physical input (AKA button)
(define physInput? (λ()
                    (cond
                      ((equal? pushed 1) #t)
                      (#t #f))
                    )
  )

;; Puts power to low for all registered output ports
(define turnOff (λ ()
                  (for ([i ports])
                    (c i LOW)
                    )
                  )
  )

;; Setting output ports before we can connect
(define set-ports (λ (v1 v2)
                    (for ([i v1])
                      (set-pin-mode! i v2)
                      )
                    (set! ports v1)
                    )
  )

;; Call this method first in order to create the connection
;; between the Asip libraries as well as the AMAN library
;; Declares variable
(define connect (λ ()
                  (display (string-append "\nPorts: "))
                  (display (number->string (length ports)))
                  (display (string-append "\n----------\nLIGHTS READY"))
                  (setTimes)
                  (open-asip)
                  )
  )

;; Push button test
(define pushButton (λ ()
                     (cond
                       ((equal? pushed 1) "BUTTON ALREADY PUSHED")
                       (#t
                        (set! pushed 1)
                        (display (string-append "\n[BUTTON PRESSED]\n"))
                        ports
                        (sleep 2)
                        (display (string-append "\n[BUTTON RELEASED]\n"))
                        (set! pushed 0))
                       )
                     )
  )

;; Calculates which GPIO is being used for which light
(define s 0)
(define lighti (λ (v1)
                 (display (string-append "\nRED:"))
                 (display (string-append (number->string v1)))
                 (display (string-append "\n"))
                 (display (string-append "AMBER:"))
                 (display (string-append (number->string (- v1 2))))
                 (display (string-append "\nGREEN:"))
                 (display (string-append (number->string (- v1 3))))
                 )
  )

(define get-amber (λ (v1)
                    (- v1 2)
                    )
  )

(define get-green (λ (v1)
                    (- v1 3)
                    )
  )

;; Timing handlers, these are responsible for changing traffic lights
;; we are using a substring in order to get the first two numbers that are outputted
;; using the gregor library which is referenced above
(define time (time->iso8601 (current-time)))
(define hour (string->number (string-replace (substring time 0 3) ":" ".")))
(define red-time 0)
(define amber-time 0)
(define green-time 0)
(define redAmber-time 0)

(define setTimes (λ ()
                   (cond
                     ((is-sim?)
                      (set! hour (get-value 'simulation 'time))
                      (display (string-append "\nSim. Name: "))
                      (display (string-append (get-value 'simulation 'name)))
                      (display (string-append "\nTime set to: "))
                      (display (string-append (number->string(get-value 'simulation 'time))))
                      (display (string-append "\n "))
                      )
                     )
                   (cond
                     ((equal? hour 25.0)
                      (set! red-time 5)
                      (set! green-time 5)
                      (set! redAmber-time 5)
                      (set! amber-time 5)
                      )
                     ((equal? hour 17.0)
                      (set! red-time 20)
                      (set! redAmber-time 3)
                      (set! green-time 40)
                      (set! amber-time 3)
                      )
                     ((equal? hour 18.0)
                      (set! red-time 20)
                      (set! redAmber-time 3)
                      (set! green-time 40)
                      (set! amber-time 3)
                      )
                     ((equal? hour 19.0)
                      (set! red-time 20)
                      (set! redAmber-time 3)
                      (set! green-time 40)
                      (set! amber-time 3)
                      )
                     (#t
                      (set! red-time 30)
                      (set! redAmber-time 3)
                      (set! green-time 30)
                      (set! amber-time 3)
                      )
                   )
                   )
  )

(cond
  ((equal? hour 12.0)
   (set! red-time 20)
   (set! redAmber-time 10)
   (set! green-time 15)
   (set! amber-time 10)
   )
  ((equal? hour 19.0)
   (set! red-time 20)
   (set! redAmber-time 10)
   (set! green-time 15)
   (set! amber-time 10)
   )
  )

;; Must stay at end in order to assure that there were no errors with the syntax of the code
(display (string-append "\nFile compiled successfully"))

(cond
  ((> warnings 0)
   (display (string-append "\nWarnings found:"))
   (display (string-append (number->string warnings)))
   )
  )

(display (string-append "\n "))
 