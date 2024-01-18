
(setf prove:*enable-colors* nil)
(defpackage #:lqn-tests (:use #:cl #:prove) (:export #:run-tests))
(in-package #:lqn-tests)

(defun -run-tests (files)
  (labels ((rel (f) (mapcar (lambda (p) (asdf:system-relative-pathname
                                          "lqn/tests" p))
                            f)))
    (loop with fails = 0
          for f in (rel files)
          do (format t "~&~%starting tests in: ~a~%" (lqn:str! f))
             (unless (prove:run f :reporter :fiveam)
                     (incf fails))
             (format t "~&done: ~a~%" (lqn:str! f))
          finally (return (unless (< fails 1) (uiop:quit 7))))))

(defun run-tests ()
  (-run-tests '(#P"test/test-lqn.lisp"))
  (-run-tests '(#P"test/test-lqn-2.lisp")))

(defun strip (s)
  (lqn::repl s (lqn:str! #\Newline) ""))
(defmacro is-str (a b) `(is (strip ,a) (strip ,b)))

(defvar *test-data-fn* (lqn::internal-path-string "test/sample.json"))
(defvar *test-data-2-fn* (lqn::internal-path-string "test/sample2.json"))
(defvar *test-data-raw*
        #(((:_ID . "65679d23d38d711eaf999e89") (:INDEX . 0)
           (:THINGS . #(((:ID . 0) (:NAME . "Chris") (:EXTRA . "extra99"))))
           (:MSG . "this is a message") (:FAVE . "strawberry"))
          ((:_ID . "65679d23fe33bc4c240675c0") (:INDEX . 1)
           (:THINGS
            . #(((:ID . 10) (:NAME . "Winters") (:EXTRA . "extra1"))
                ((:ID . 11) (:NAME . "Haii") (:EXTRA . "extra2"))
                ((:ID . 12) (:NAME . "Klein"))))
           (:MSG . "Hello, undefined! You have 1 unread messages.")
           (:FAVE . "strawberry"))
          ((:_ID . "65679d235b4143d2932ea17a")
           (:THINGS
            . #(((:ID . 31) (:NAME . "Star")) ((:ID . 32) (:NAME . "Ball"))))
           (:MSG . "Hello, undefined! You have 5 unread messages.")
           (:FAVE . "blueberry"))))
(defvar *test-data-2-raw*
  '((:CREDIT . "Mega Corp.") (:CREDIT_URL . "http://fax.megacorp")
    (:DISCLAIMER_URL) (:COPYRIGHT_URL . "http://fax.megacorp/about/terms.asp")
    (:IMAGE (:URL . "http://fax.megacorp/images/Logo.jpg")
            (:TITLE . "Mega Corp")
            (:LINK . "http://fax.megacorp/yyyyyyyyy"))
    (:SUGGESTED_PICKUP . "15 minutes after the hour")
    (:SUGGESTED_PICKUP_PERIOD . "60") (:DEWPOINT_C . -22.2d0) (:DEWPOINT_F)
    (:DEWPOINT_STRING . "-8.0 F (-22.2 C)") (:HEAT_INDEX_C . -20.6d0)
    (:HEAT_INDEX_F . -5.0d0) (:HEAT_INDEX_STRING . "-5.0 F (-20.6 C)")
    (:OBSERVATION_TIME . "Last Updated on Dec 5 2023, 9:37 pm CET")
    (:CURRENT_OBSERVATION (:STATION_NAME . "Gulhuset") (:OBSERVATION_AGE . 42)
     (:DEWPOINT_DAY_HIGH_F . "-7") (:DEWPOINT_DAY_HIGH_TIME . "8:47pm")
     (:DEWPOINT_DAY_LOW_F . -8.0d0) (:WINDCHILL_MONTH_LOW_F . -9)
     (:WINDCHILL_YEAR_LOW_F . -9))
    (:TIME_TO_GENERATE . 0.012046d0)))

