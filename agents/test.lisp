;;; File: agents/test.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(deftest agents
  "Test agents in the vacuum and wumpus worlds." 
  "Here is how to create and print a vacuum world"
  ((setq vw (make-vacuum-world)))
  ((print-environment-map vw))
  "Here's one way to run it"
  ((run-eval-environment vw))
  "We can also run random vacuum worlds with this:"
  ((run-vacuum))
  "There are optional args to control execution and printing:"
  ((run-vacuum :display t :max-steps 10))
  "VACUUM-TRIALS compares agents on random worlds. This takes a few seconds."
  ((vacuum-trials))
  "Now for the wumpus world"
  ((print-environment-map (make-wumpus-world)))
  ((run-wumpus))
  )
