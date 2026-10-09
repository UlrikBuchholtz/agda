;; -*- lexical-binding: t -*-
;;; cross-check.el --- Do the two name-index clients agree?

;;; Commentary:

;; The name index's format is public and its matching is a
;; specification, because two clients read it: `Agda.Interaction.
;; NameIndex', which answers the agent interface's `findNames', and
;; `agda2-name-index.el', which answers `agda2-find-name' without an
;; Agda process at all.  Prose cannot hold those in step; this can.
;;
;; It builds an index over the same fixture library the agent suite
;; uses, asks the same questions of both clients, and compares every
;; field of every hit -- the ranks, the order they came in, and the
;; rendered types.
;;
;; Run it as
;;
;;   emacs --batch -Q -L src/data/emacs-mode -l test/emacs-mode/cross-check.el
;;
;; from the top of the tree, or as `make emacs-mode-test'.  AGDA_BIN
;; says which Agda to use, and AGDA_CLIENT which `agda-client', whose
;; answers are the reference.  The Makefile sets both, and a client it
;; names that is not there is a failure.  Run by hand without
;; AGDA_CLIENT, the client is looked for beside Agda and on the PATH,
;; and the fixture queries are skipped -- and said to be skipped --
;; when there is none.

;;; Code:

(require 'agda2-name-index)
(require 'cl-lib)

(defvar cross-check-fixture
  (expand-file-name "test/Agent/NameIndexLib")
  "The library both clients are asked about.")

(defvar cross-check-queries
  ;; (PATTERN . PLIST), where the plist is passed to both clients.
  '(("is-basis" :in "Lib")
    ("basis")
    ("is")
    ("Bool" :in "Lib" :kind "DataName")
    ("true" :in "Lib" :kind "con")
    ("is-*-sub" :in "Lib" :glob t)
    ("*basis*" :glob t)
    ("basis-of" :in "Lib")
    ("basis" :in "Lib" :limit 2)
    ("is-basis" :in "Lib" :implicit t)
    ("no-such-name-anywhere"))
  "The questions both clients are asked.
Between them they reach every rank, the module, kind and glob
filters, the cut a limit makes, both renderings of a type, and the
empty answer.")

(defvar cross-check-failures 0)

(defvar cross-check-skipped nil
  "Non-nil when the fixture queries could not be run.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The ranks, by example
;;
;; The fixture library below exercises the plumbing -- that both clients
;; read the same fields out of the same file and order them the same way
;; -- but it is five modules of ordinary names, and so reaches neither
;; an underscore, nor a name outside ASCII, nor the literal star that is
;; the reason globbing is opt-in.  This is the table that does: it is
;; `prop_ranks' and `prop_globRanks' of
;; `test/Internal/Interaction/NameIndex.hs', which pin the same
;; specification at the other end, and it needs no Agda to run.

(defvar cross-check-rank-table
  '((nil "is-basis"    "is-basis"       1)
    (nil "is-basis"    "is-basis-for"   2)
    (nil "basis"       "is-basis-for"   3)
    (nil "basi"        "is-basis-for"   4)
    (nil "is_basis"    "is_basis"       1)
    (nil "basis"       "is_basis_for"   3)
    (nil "basis"       "is-basisₛ-for"  4)
    (nil "Basis"       "is-basis"       nil)
    (nil "sisab"       "is-basis"       nil)
    (nil "ℤ*-assoc"    "ℤ*-assoc"       1)
    (nil "ℤ*-assoc"    "ℤ*-assoc₀"      2)
    (nil "*"           "ℤ*-assoc"       4)
    (t   "is-*-for"    "is-basis-for"   1)
    (t   "is-*"        "is-basis-for"   1)
    (t   "*-for"       "is-basis-for"   1)
    (t   "is-?asis"    "is-basis"       1)
    (t   "is-?asis"    "is-basis-for"   2)
    (t   "basi?"       "is-basis-for"   3)
    (t   "asi?"        "is-basis-for"   4)
    (t   "is-*-for"    "is-basis"       nil)
    ;; A literal star is written with a backslash, which is the whole
    ;; reason globbing is opt-in: Agda names contain stars.
    (t   "ℤ\\*-*"      "ℤ*-assoc"       1)
    (t   "ℤ*-*"        "ℤ-assoc"        1))
  "Rows of (GLOB PATTERN NAME RANK), RANK being nil for no match.")

(defun cross-check-ranks ()
  "Check the ranking against the table the specification names."
  (let ((wrong 0))
    (pcase-dolist (`(,glob ,pattern ,name ,rank) cross-check-rank-table)
      (let ((got (agda2-name-index--rank
                  (agda2-name-index--compile pattern glob) name)))
        (unless (equal got rank)
          (cl-incf wrong)
          (message "FAILED  %s %S against %S: rank %S, expected %S"
                   (if glob "glob" "literal") pattern name got rank))))
    ;; Every rank implies the fourth, which is what makes it sound for a
    ;; query to skip a line of the index that does not contain the
    ;; pattern at all.
    (dolist (pattern '("basis" "is" "-" "ℤ"))
      (dolist (name '("is-basis" "is_basis_for" "ℤ*-assoc" "unrelated"))
        (let ((matched (and (agda2-name-index--rank
                             (agda2-name-index--compile pattern nil) name)
                            t))
              (occurs (and (string-search pattern name) t)))
          (unless (eq matched occurs)
            (cl-incf wrong)
            (message "FAILED  %S in %S: matched %S, occurs %S"
                     pattern name matched occurs)))))
    (if (zerop wrong)
        (message "ok      the ranks, %d rows" (length cross-check-rank-table))
      (cl-incf cross-check-failures wrong))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Running the two clients

(defun cross-check-run (program args &optional directory)
  "Run PROGRAM with ARGS in DIRECTORY.  Returns (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory (or directory default-directory)))
      (cons (apply #'call-process program nil t nil args)
            (buffer-string)))))

(defun cross-check-client-run (agent args directory)
  "Run AGENT with ARGS against the daemon for DIRECTORY.
Every call has to name the same Agda and the same root, since the
daemon's socket is keyed by both: a call that did not would start a
daemon of its own."
  (let ((process-environment
         (append (list (concat "AGDA_CLIENT_BIN=" (getenv "CROSS_CHECK_AGDA"))
                       (concat "AGDA_CLIENT_ROOT=" directory))
                 process-environment)))
    (cross-check-run agent args directory)))

(defun cross-check-agent (agent directory args)
  "Ask AGENT, running in DIRECTORY, the question in ARGS.
Returns the hits of the protocol answer."
  (let* ((result (cross-check-client-run agent (append args '("--json"))
                                         directory)))
    (unless (zerop (car result))
      (error "agda-client %s failed: %s" (string-join args " ") (cdr result)))
    (with-temp-buffer
      (insert (cdr result))
      (goto-char (point-min))
      (let ((answer (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil)))
        (when (plist-get answer :error)
          (error "agda-client %s: %s" (string-join args " ")
                 (plist-get (plist-get answer :error) :message)))
        (plist-get (plist-get answer :result) :hits)))))

(defun cross-check-agent-args (query)
  "The `agda-client find' arguments for QUERY."
  (let ((pattern (car query)) (options (cdr query)))
    (append (list "find" pattern)
            (when (plist-get options :in) (list "--in" (plist-get options :in)))
            (when (plist-get options :kind)
              (list "--kind" (plist-get options :kind)))
            (when (plist-get options :glob) (list "--glob"))
            (when (plist-get options :implicit) (list "--show-implicit"))
            (list "--limit" (number-to-string
                             (or (plist-get options :limit) 100))))))

(defun cross-check-emacs (index query)
  "Answer QUERY from INDEX with the Emacs client."
  (let* ((options (cdr query))
         (agda2-name-index-show-implicit (plist-get options :implicit)))
    (car (agda2-name-index-query
          index (car query)
          :glob (plist-get options :glob)
          :module (plist-get options :in)
          :kinds (when (plist-get options :kind) (list (plist-get options :kind)))
          :limit (or (plist-get options :limit) 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Comparing the answers
;;
;; Both sides are reduced to the same list of strings: every field of
;; the protocol's hit, in the order the hits came in.  Comparing the
;; order as well as the contents is the point -- the ranking is what a
;; second implementation is most likely to get subtly wrong.

(defun cross-check-agent-hit (hit)
  "The canonical form of a hit as `agda-client' reports it."
  (format "%s %s %s %s %s %s:%s,%s uses=%s params=%S : %s"
          (plist-get hit :rank) (plist-get hit :match)
          (plist-get hit :module) (or (plist-get hit :qualifier) "-")
          (plist-get hit :name) (plist-get hit :kind)
          (plist-get hit :line) (plist-get hit :column)
          (plist-get hit :uses) (plist-get hit :params)
          (plist-get hit :type)))

(defun cross-check-emacs-hit (hit implicit)
  "The canonical form of a hit as the Emacs client reports it."
  (format "%s %s %s %s %s %s:%s,%s uses=%s params=%S : %s"
          (plist-get hit :rank) (plist-get hit :match)
          (plist-get hit :module) (or (plist-get hit :qualifier) "-")
          (plist-get hit :name) (plist-get hit :kind)
          (plist-get hit :line) (plist-get hit :col)
          (plist-get hit :uses) (plist-get hit :params)
          (agda2-name-index--type hit implicit)))

(defun cross-check-compare (query theirs ours)
  "Report on QUERY, whose answers were THEIRS and OURS."
  (let ((label (format "%s%s" (car query)
                       (if (cdr query) (format " %S" (cdr query)) ""))))
    (if (equal theirs ours)
        (message "ok      %-40s %d hit%s" label (length ours)
                 (if (= (length ours) 1) "" "s"))
      (cl-incf cross-check-failures)
      (message "FAILED  %s" label)
      (let ((n (max (length theirs) (length ours))))
        (dotimes (i n)
          (let ((a (nth i theirs)) (b (nth i ours)))
            (unless (equal a b)
              (message "  agda-client: %s" (or a "(nothing)"))
              (message "  emacs     : %s" (or b "(nothing)")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The test

(defun cross-check-index-location (agda directory)
  "Where DIRECTORY's index is, according to AGDA."
  (let ((result (cross-check-run agda '("--name-index-status") directory)))
    (unless (zerop (car result))
      (error "agda --name-index-status failed: %s" (cdr result)))
    (with-temp-buffer
      (insert (cdr result))
      (goto-char (point-min))
      (plist-get (json-parse-buffer :object-type 'plist :array-type 'list
                                    :null-object nil :false-object nil)
                 :location))))

(defun cross-check-main ()
  "Check the ranks, then ask both clients about the fixture library."
  (cross-check-ranks)
  (cross-check-fixture-queries)
  (if (zerop cross-check-failures)
      (message (if cross-check-skipped
                   "\nThe ranks agree; the fixture queries were skipped."
                 "\nThe two name-index clients agree."))
    (message "\n%d check%s failed." cross-check-failures
             (if (= cross-check-failures 1) "" "s"))
    (kill-emacs 1)))

(defun cross-check-fixture-queries ()
  "Build an index over the fixture and ask both clients about it."
  (let* ((agda (or (getenv "AGDA_BIN") "agda"))
         (agda (or (executable-find agda)
                   (and (file-executable-p agda) (expand-file-name agda))))
         (given (getenv "AGDA_CLIENT"))
         (agent (cond
                 ((and given (not (string-empty-p given)))
                  (and (file-executable-p given) (expand-file-name given)))
                 (agda
                  (let ((sibling (expand-file-name
                                  "agda-client" (file-name-directory agda))))
                    (if (file-executable-p sibling)
                        sibling
                      (executable-find "agda-client")))))))
    (cond
     ((null agda)
      (setq cross-check-skipped t)
      (message "skipped: no Agda binary (set AGDA_BIN)"))
     ((and (null agent) given (not (string-empty-p given)))
      (cl-incf cross-check-failures)
      (message "FAILED  AGDA_CLIENT names %s, which is not an executable" given))
     ((null agent)
      (setq cross-check-skipped t)
      (message "skipped: no agda-client beside %s or on the PATH (set AGDA_CLIENT)"
               agda))
     ((not (file-directory-p cross-check-fixture))
      (message "skipped: no fixture at %s -- run from the top of the tree"
               cross-check-fixture))
     (t
      (setenv "CROSS_CHECK_AGDA" agda)
      (let ((directory (make-temp-file "agda-name-index-cross-check" t)))
        (unwind-protect
            (progn
              (copy-directory cross-check-fixture directory nil t t)
              (message "index: building over %s" cross-check-fixture)
              (let ((result (cross-check-run agda '("--build-name-index")
                                             directory)))
                (unless (zerop (car result))
                  (error "agda --build-name-index failed: %s" (cdr result))))
              (let ((index (cross-check-index-location agda directory)))
                (unless (and index (file-readable-p index))
                  (error "no index was built"))
                ;; The agent answers only about a session that has a
                ;; file loaded, so give it one.  Which file does not
                ;; matter: the index spans what the project could
                ;; import, not what this file does.
                (cross-check-agent agent directory '("load" "Lib/Main.agda"))
                (unwind-protect
                    (dolist (query cross-check-queries)
                      (cross-check-compare
                       query
                       (mapcar #'cross-check-agent-hit
                               (cross-check-agent
                                agent directory
                                (cross-check-agent-args query)))
                       (mapcar (lambda (hit)
                                 (cross-check-emacs-hit
                                  hit (plist-get (cdr query) :implicit)))
                               (cross-check-emacs index query))))
                  (cross-check-client-run agent '("stop") directory))))
          (delete-directory directory t)))))))

(cross-check-main)

;;; cross-check.el ends here
