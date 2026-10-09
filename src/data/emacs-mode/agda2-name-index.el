;; -*- lexical-binding: t -*-
;;; agda2-name-index.el --- Search a project-wide index of Agda names
;; SPDX-License-Identifier: MIT License

;;; Commentary:

;; `agda2-find-name' answers one question -- does this project already
;; define something called roughly this, and if so, what is its type and
;; where is it? -- over every module the project could write an `import'
;; for, rather than over the scope of the file being edited.
;;
;; It never talks to the Agda process.  The index is built ahead of time
;; by `agda --build-name-index', which renders every public type to
;; text, so answering a query is a string match over a file: it works
;; while Agda is busy checking something else, which is exactly when one
;; looks for a name one has not got.
;;
;; Agda is run for two things only, both of them out of band: once per
;; search to ask `--name-index-status' where the index is and what it
;; covers, and on demand to build the index.
;;
;; The matching and the ranking here are a second implementation of a
;; published specification -- see "How a pattern matches" in the manual,
;; and `Agda.Interaction.NameIndex' for the first.  They are held to it
;; by a cross-check that replays the same queries through both.
;;
;; Reading the index needs `json-parse-buffer', and so Emacs 27.1; the
;; rest of the Agda mode does not, so this file loads on an older Emacs
;; and only the search itself declines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Defined in agda2-mode.el, which requires this file rather than the
;; other way round: a name query needs nothing else the mode has.
(defvar agda2-program-name)
(defvar agda2-program-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User options

(defgroup agda2-name-index nil
  "Searching the names an Agda project could import."
  :group 'agda2)

(defcustom agda2-name-index-default-action 'insert
  "What `agda2-find-name' does with the hit that was chosen.
A prefix argument asks for an action instead of using this one."
  :type '(choice (const :tag "Insert the name at point" insert)
                 (const :tag "Jump to the definition" jump)
                 (const :tag "Insert an `open import' for its module" import))
  :group 'agda2-name-index)

(defcustom agda2-name-index-limit 500
  "How many hits `agda2-find-name' offers, or nil for all of them.
Hits are ranked before this cuts them, so the ones dropped are the
worst matches."
  :type '(choice (const :tag "All" nil) integer)
  :group 'agda2-name-index)

(defcustom agda2-name-index-show-implicit nil
  "Whether the types shown beside candidates have implicit arguments.
The index stores both renderings; the hidden one is what a list of
hits is meant to be read as, and the explicit one is what one needs
before writing an application.  This is the default a search starts
from: `agda2-name-index-toggle-implicit' switches between the two
while the candidates are showing.  The other rendering of the hit
that was chosen is always available through
\\[agda2-name-index-describe]."
  :type 'boolean
  :group 'agda2-name-index)

(defcustom agda2-name-index-type-width 100
  "How much of a type is shown beside a candidate, in characters.
A type in the index can run to a hundred kilobytes."
  :type 'integer
  :group 'agda2-name-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The published format

(defconst agda2-name-index-format-version 3
  "The version of the index file format this client reads.
It is a compatibility promise: see \"The index file\" in the manual.
An index of another version is refused rather than misread.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matching
;;
;; This is the specification, in the order results are ranked.  Nothing
;; below this heading knows about Agda, or reads a file: a query matches
;; a string, ranks it, and stops.

(defalias 'agda2-name-index--in
  (if (fboundp 'string-search)
      #'string-search
    (lambda (needle haystack)
      (string-match-p (regexp-quote needle) haystack)))
  "Does the first string occur in the second?
`string-search' where there is one, which is Emacs 28.1 onwards.")

(defun agda2-name-index--supported ()
  "Signal unless this Emacs can read an index file."
  (unless (fboundp 'json-parse-buffer)
    (user-error
     "Searching Agda names needs Emacs 27.1 or later; this is Emacs %s"
     emacs-version)))

(defun agda2-name-index--components (name)
  "The hyphen- or underscore-separated components of NAME.
`is-basis-for' has the components `is', `basis' and `for'."
  (seq-remove #'string-empty-p (split-string name "[-_]")))

(defun agda2-name-index--proper-prefixes (name)
  "Every proper prefix of NAME, the empty one included."
  (cl-loop for i from 0 below (length name) collect (substring name 0 i)))

(defun agda2-name-index--substrings (name)
  "Every non-empty substring of NAME."
  (cl-loop for i from 0 below (length name)
           nconc (cl-loop for j from (1+ i) to (length name)
                          collect (substring name i j))))

;; A compiled pattern is either (literal . STRING) or (glob . PIECES),
;; where a piece is a string, the symbol `one' (?) or the symbol `many'
;; (*).

(defun agda2-name-index--compile (pattern glob)
  "Compile PATTERN.  With GLOB, read `*' and `?' as wildcards."
  (if (not glob)
      (cons 'literal pattern)
    (cons 'glob (agda2-name-index--parse-glob pattern))))

(defun agda2-name-index--parse-glob (pattern)
  "Split PATTERN into literal strings and the symbols `one' and `many'.
A backslash escapes the character after it, which is how a literal
`*' is written -- and Agda names do contain stars, which is why
globbing is opt-in in the first place."
  (let ((pieces nil) (lit nil) (i 0) (n (length pattern)))
    (cl-flet ((flush ()
                (when lit
                  (push (apply #'string (nreverse lit)) pieces)
                  (setq lit nil))))
      (while (< i n)
        (let ((c (aref pattern i)))
          (cond
           ((eq c ?*) (flush) (push 'many pieces) (cl-incf i))
           ((eq c ??) (flush) (push 'one pieces) (cl-incf i))
           ((eq c ?\\)
            (if (< (1+ i) n)
                (progn (push (aref pattern (1+ i)) lit) (cl-incf i 2))
              (push ?\\ lit) (cl-incf i)))
           (t (push c lit) (cl-incf i)))))
      (flush))
    (nreverse pieces)))

(defun agda2-name-index--glob-match (pieces string)
  "Does the glob PIECES match the whole of STRING?"
  (cond
   ((null pieces) (string-empty-p string))
   ((stringp (car pieces))
    (and (string-prefix-p (car pieces) string)
         (agda2-name-index--glob-match
          (cdr pieces) (substring string (length (car pieces))))))
   ((eq (car pieces) 'one)
    (and (> (length string) 0)
         (agda2-name-index--glob-match (cdr pieces) (substring string 1))))
   ;; The only place that backtracks, and the pattern is a query
   ;; someone typed, so the pathological case is not worth guarding
   ;; against.
   ((eq (car pieces) 'many)
    (cl-loop for i from 0 to (length string)
             thereis (agda2-name-index--glob-match
                      (cdr pieces) (substring string i))))))

(defun agda2-name-index--rank (matcher name)
  "How well MATCHER matches NAME: 1 to 4, or nil if it does not.
1 the whole name, 2 a proper prefix of it, 3 a whole component of
it, 4 anywhere in it.  Every rank implies the fourth."
  (if (eq (car matcher) 'literal)
      (let ((p (cdr matcher)))
        (cond ((string= p name)                                    1)
              ((string-prefix-p p name)                            2)
              ((member p (agda2-name-index--components name))      3)
              ((agda2-name-index--in p name)                      4)))
    (let ((g (cdr matcher)))
      (cond
       ((agda2-name-index--glob-match g name) 1)
       ((cl-some (lambda (s) (agda2-name-index--glob-match g s))
                 (agda2-name-index--proper-prefixes name)) 2)
       ((cl-some (lambda (s) (agda2-name-index--glob-match g s))
                 (agda2-name-index--components name)) 3)
       ((cl-some (lambda (s) (agda2-name-index--glob-match g s))
                 (agda2-name-index--substrings name)) 4)))))

(defconst agda2-name-index--rank-names
  ["exact" "prefix" "component" "substring" "mentions"]
  "What each rank is called in the protocol, indexed from rank 1.")

(defun agda2-name-index--matches-kind (kinds kind)
  "Is KIND one of KINDS?  An empty KINDS asks for all of them.
The comparison ignores case and a trailing `name', so `FunName',
`Fun' and `fun' all name a function."
  (or (null kinds)
      (let ((norm (lambda (k)
                    (let ((l (downcase k)))
                      (if (string-suffix-p "name" l)
                          (substring l 0 -4)
                        l)))))
        (cl-member (funcall norm kind) kinds
                   :test (lambda (a b) (string= a (funcall norm b)))))))

(defun agda2-name-index--module-prefix-p (prefix module)
  "Does MODULE lie under PREFIX?
`Foo' covers `Foo' and `Foo.Bar', but not `Foobar'."
  (or (string= prefix module)
      (string-prefix-p (concat prefix ".") module)))

(defun agda2-name-index--needle (string)
  "The bytes STRING occupies inside a JSON string.
Escaping is per character, so the encoding of a substring is a
substring of the encoding -- which is what makes searching a raw
line of the index for it sound."
  (replace-regexp-in-string "[\\\"]" "\\\\\\&" string t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Querying the index file

(defun agda2-name-index--include-rank (paths source)
  "Where on the include PATHS the directory holding SOURCE comes.
Its position, or the length of PATHS for a file under none of them.
Include directories can nest, so the longest one holding the file
is the one that counts.  The index lists the paths with the
project's own first and Agda's builtin modules last, so this is
what puts the project's own `cong' before a builtin's."
  (let ((best nil) (best-length -1) (i 0))
    (dolist (p paths)
      (let ((dir (file-name-as-directory p)))
        (when (and (string-prefix-p dir source)
                   (> (length dir) best-length))
          (setq best i best-length (length dir))))
      (setq i (1+ i)))
    (or best (length paths))))

(defun agda2-name-index--hit-key (hit)
  "The sort key of HIT: the rank, then what breaks ties inside it:
popularity, then where the hit lives, then the name."
  (list (plist-get hit :rank)
        (- (or (plist-get hit :uses) 0))
        (or (plist-get hit :origin) 0)
        (plist-get hit :module)
        (or (plist-get hit :qualifier) "")
        (plist-get hit :name)
        (or (plist-get hit :line) 0)))

(defun agda2-name-index--key< (a b)
  "Is the sort key A before the sort key B?"
  (cl-loop for x in a for y in b
           do (cond ((and (numberp x) (< x y)) (cl-return t))
                    ((and (numberp x) (> x y)) (cl-return nil))
                    ((and (stringp x) (string< x y)) (cl-return t))
                    ((and (stringp x) (string< y x)) (cl-return nil)))
           finally return nil))

(defun agda2-name-index--parse-line ()
  "Parse the JSON object at point, leaving point after it."
  (json-parse-buffer :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun agda2-name-index--needles (matcher module)
  "What every line holding a hit for MATCHER under MODULE must contain."
  (delq nil
        (list (pcase (car matcher)
                ('literal (cdr matcher))
                ;; A glob's longest literal run occurs in every name it
                ;; matches; a glob of nothing but wildcards has none.
                ('glob (let ((literals (seq-filter #'stringp (cdr matcher))))
                         (when literals
                           (car (sort literals
                                      (lambda (a b)
                                        (> (length a) (length b)))))))))
              module)))

(cl-defun agda2-name-index-query (file pattern &key glob module kinds limit)
  "Answer PATTERN from the index in FILE, ranked.
GLOB reads `*' and `?' as wildcards, MODULE restricts to a
top-level module prefix, KINDS to those kinds of name, and LIMIT
cuts the ranked hits.  Returns (HITS . MATCHED), where MATCHED is
how many there were before the cut and each hit is a plist.

Most of the index is never decoded.  Every rank requires the
pattern to occur literally in the module's line, so a line that
does not contain it cannot hold a hit; the same goes for the
module prefix, which occurs in the name of every module it admits.
Both tests are a plain search for the pattern's JSON encoding."
  (agda2-name-index--supported)
  (let* ((matcher (agda2-name-index--compile pattern glob))
         (needles (mapcar #'agda2-name-index--needle
                          (agda2-name-index--needles matcher module)))
         (sources (make-hash-table :test #'equal))
         (unknown-origin 0)
         (hits nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; The header, for the manifest: a module's source file is
      ;; recorded there once rather than in each of its entries, and
      ;; with it where on the include path that file lives.
      (let ((header (agda2-name-index--parse-line)))
        (unless (equal (plist-get header :format) agda2-name-index-format-version)
          (error "The index in %s is of format version %s, not %d"
                 file (plist-get header :format)
                 agda2-name-index-format-version))
        (setq unknown-origin (length (plist-get header :includePaths)))
        (dolist (m (plist-get header :modules))
          (let ((source (plist-get m :source)))
            (puthash (plist-get m :module)
                     (cons source
                           (agda2-name-index--include-rank
                            (plist-get header :includePaths) source))
                     sources))))
      (forward-line 1)
      (while (not (eobp))
        (if (not (let ((eol (line-end-position)))
                   (cl-every (lambda (needle)
                               (save-excursion (search-forward needle eol t)))
                             needles)))
            (forward-line 1)
          (let* ((line (agda2-name-index--parse-line))
                 (m (plist-get line :module))
                 (params (plist-get line :params))
                 (home (gethash m sources)))
            (when (or (null module)
                      (agda2-name-index--module-prefix-p module m))
              (dolist (e (plist-get line :entries))
                (let ((rank (and (agda2-name-index--matches-kind
                                  kinds (plist-get e :kind))
                                 (agda2-name-index--rank
                                  matcher (plist-get e :name)))))
                  (when rank
                    (push (list :name (plist-get e :name)
                                :qualifier (plist-get e :qualifier)
                                :kind (plist-get e :kind)
                                :module m
                                :params params
                                :file (car home)
                                :origin (if home (cdr home) unknown-origin)
                                :line (plist-get e :line)
                                :col (plist-get e :col)
                                :type (plist-get e :type)
                                :type-implicit (plist-get e :typeImplicit)
                                :uses (plist-get e :uses)
                                :rank rank
                                :match (aref agda2-name-index--rank-names
                                             (1- rank)))
                          hits)))))
            (forward-line 1)))))
    (let* ((ranked (sort hits
                         (lambda (a b)
                           (agda2-name-index--key<
                            (agda2-name-index--hit-key a)
                            (agda2-name-index--hit-key b)))))
           (matched (length ranked)))
      (cons (if (and limit (> matched limit))
                (seq-take ranked limit)
              ranked)
            matched))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Asking Agda where the index is
;;
;; The only Agda process a search starts, and it is started before the
;; pattern is prompted for, so that the scan -- a stat and twenty-four
;; bytes per module, but a fifth of a second on a large library -- runs
;; while the pattern is being typed.

(defun agda2-name-index--program ()
  "The Agda executable to run."
  (if (boundp 'agda2-program-name) agda2-program-name "agda"))

(defun agda2-name-index--program-args ()
  "The arguments to pass it, beside the mode."
  (if (boundp 'agda2-program-args) agda2-program-args nil))

(defun agda2-name-index--directory ()
  "The directory to run Agda in: the current buffer's."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun agda2-name-index--status-start ()
  "Start `agda --name-index-status'.  Returns the process.
Its standard error goes to a buffer of its own, kept on the
process, so that a message Agda writes there cannot be mistaken
for the JSON it was asked for."
  (let* ((default-directory (agda2-name-index--directory))
         (out (generate-new-buffer " *agda2-name-index-status*"))
         (err (generate-new-buffer " *agda2-name-index-status-stderr*")))
    (condition-case error
        (let ((process
               (make-process
                :name "agda2-name-index-status"
                :buffer out
                :stderr err
                :noquery t
                :connection-type 'pipe
                :command (append (list (agda2-name-index--program))
                                 (agda2-name-index--program-args)
                                 (list "--name-index-status")))))
          (process-put process 'agda2-name-index-stderr err)
          process)
      (error (kill-buffer out)
             (kill-buffer err)
             (error "Could not run %s: %s"
                    (agda2-name-index--program)
                    (error-message-string error))))))

(defun agda2-name-index--status-abort (process)
  "Stop PROCESS and drop what it was writing to."
  (delete-process process)
  (agda2-name-index--status-clean process))

(defun agda2-name-index--status-clean (process)
  "Kill the buffers PROCESS was writing to."
  (let ((out (process-buffer process))
        (err (process-get process 'agda2-name-index-stderr)))
    (when (buffer-live-p out) (kill-buffer out))
    (when (buffer-live-p err) (kill-buffer err))))

(defun agda2-name-index--status-finish (process)
  "Wait for PROCESS and parse the coverage report it printed."
  (let ((out (process-buffer process))
        (err (process-get process 'agda2-name-index-stderr)))
    (unwind-protect
        (progn
          (while (process-live-p process)
            (accept-process-output process 0.2))
          (with-current-buffer out
            (goto-char (point-min))
            (let ((status (condition-case nil
                              (json-parse-buffer :object-type 'plist
                                                 :array-type 'list
                                                 :null-object nil
                                                 :false-object nil)
                            (error nil))))
              (unless status
                (error "%s --name-index-status failed: %s"
                       (agda2-name-index--program)
                       (string-trim
                        (concat (buffer-string)
                                (when (buffer-live-p err)
                                  (with-current-buffer err (buffer-string)))))))
              status)))
      (agda2-name-index--status-clean process))))

(defun agda2-name-index--check (status)
  "Signal unless STATUS describes an index this client can read."
  (let ((version (plist-get status :formatVersion))
        (location (plist-get status :location)))
    (cond
     ((null location)
      (user-error "%s"
                  (or (plist-get status :reason)
                      "this project has no .agda-lib, so it can have no index")))
     ((not (equal version agda2-name-index-format-version))
      (user-error
       "The index format is version %s and this Agda mode reads version %d%s"
       version agda2-name-index-format-version
       (if (and (numberp version) (> version agda2-name-index-format-version))
           "; update the Agda mode" "; update Agda")))
     ((null (plist-get status :builtAt))
      (user-error "No name index for this project yet: build one with %s"
                  (substitute-command-keys "\\[agda2-name-index-build]")))
     ((not (file-readable-p location))
      (user-error "The index file %s is not readable" location)))))

(defun agda2-name-index--coverage-note (status)
  "What STATUS says is missing from an answer, as a string, or nil.
`modified' is the count no build brings down: those modules have
been edited since they were last checked, so the index describes
text that is no longer there."
  (let* ((parts
          (delq nil
                (list (let ((n (plist-get status :stale)))
                        (when (and n (> n 0)) (format "%d stale" n)))
                      (let ((n (plist-get status :modified)))
                        (when (and n (> n 0)) (format "%d edited" n)))
                      (let ((n (plist-get status :unindexed)))
                        (when (and n (> n 0)) (format "%d unchecked" n)))))))
    (when parts (string-join parts ", "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Presenting the hits

(defun agda2-name-index--qualified (hit)
  "The name HIT is reached by after importing its module."
  (string-join (delq nil (list (plist-get hit :qualifier)
                               (plist-get hit :name)))
               "."))

(defun agda2-name-index--full (hit)
  "HIT's module, qualifier and name, which is what identifies it."
  (concat (plist-get hit :module) "." (agda2-name-index--qualified hit)))

(defun agda2-name-index--home (hit)
  "Where HIT lives: its module, and the qualifier below it if any.
What a candidate line shows beside the name, which it already has."
  (string-join (delq nil (list (plist-get hit :module)
                               (plist-get hit :qualifier)))
               "."))

(defun agda2-name-index--type (hit &optional implicit)
  "HIT's type, in whichever rendering is asked for."
  (or (plist-get hit (if (or implicit agda2-name-index-show-implicit)
                         :type-implicit
                       :type))
      ""))

(defun agda2-name-index--clip (string width)
  "STRING, cut to WIDTH characters with an ellipsis if it is longer."
  (if (<= (length string) width)
      string
    (concat (substring string 0 width) "…")))

(defun agda2-name-index--candidates (hits)
  "Candidate strings for HITS, and a table mapping them back.
Two definitions can share a name, so a repeated candidate is made
unique by a counter that is displayed as nothing: `equal' ignores
text properties, so the string that comes back out of the
minibuffer still finds its hit."
  (let ((table (make-hash-table :test #'equal))
        (candidates nil))
    (dolist (hit hits)
      (let ((candidate (plist-get hit :name))
            (n 0))
        (while (gethash candidate table)
          (cl-incf n)
          (setq candidate (concat (plist-get hit :name)
                                  (propertize (number-to-string n)
                                              'display ""))))
        (puthash candidate hit table)
        (push candidate candidates)))
    (cons (nreverse candidates) table)))

(defun agda2-name-index--affixation (table)
  "An affixation function showing the kind, module and type from TABLE.
The candidates are aligned on the width of the widest name, not of
the widest candidate: a candidate may carry a disambiguating
counter that is displayed as nothing."
  (lambda (candidates)
    (let ((width (seq-max
                  (cons 0 (mapcar (lambda (candidate)
                                    (let ((hit (gethash candidate table)))
                                      (if hit
                                          (string-width (plist-get hit :name))
                                        (string-width candidate))))
                                  candidates)))))
      (mapcar
       (lambda (candidate)
         (let ((hit (gethash candidate table)))
           (list candidate
                 ""
                 (if (not hit)
                     ""
                   (concat
                    (make-string (max 1 (- (+ width 2)
                                           (string-width
                                            (plist-get hit :name))))
                                 ?\s)
                    (propertize (format "%-10s"
                                        (agda2-name-index--short-kind
                                         (plist-get hit :kind)))
                                'face 'shadow)
                    (propertize (agda2-name-index--home hit) 'face 'shadow)
                    "  "
                    (propertize (agda2-name-index--clip
                                 (agda2-name-index--type hit)
                                 agda2-name-index-type-width)
                                'face 'italic))))))
       candidates))))

(defun agda2-name-index--short-kind (kind)
  "KIND without the `Name' Agda spells it with."
  (if (string-suffix-p "Name" kind) (substring kind 0 -4) kind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Acting on a hit

(defun agda2-name-index--insert (hit)
  "Insert the name of HIT at point."
  (insert (agda2-name-index--qualified hit))
  (message "%s : %s"
           (agda2-name-index--full hit)
           (agda2-name-index--clip (agda2-name-index--type hit)
                                   agda2-name-index-type-width)))

(defun agda2-name-index--jump (hit)
  "Visit the definition of HIT."
  (let ((file (plist-get hit :file)))
    (unless (and file (file-readable-p file))
      (user-error "The source of %s is not readable: %s"
                  (agda2-name-index--full hit) (or file "unknown")))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- (max 1 (or (plist-get hit :line) 1))))
    (forward-char (max 0 (1- (or (plist-get hit :col) 1))))))

(defun agda2-name-index--import (hit)
  "Insert an `open import' for the module HIT is defined in.
It goes after the last import of the file, or after the module
header if there is none, since that is where an import has to be;
point does not move."
  (let ((module (plist-get hit :module))
        (line (format "open import %s\n" (plist-get hit :module))))
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward
           (concat "^open import[ \t]+" (regexp-quote module) "[ \t]*$")
           nil t))
        (message "%s is already imported" module)
      (let ((where nil))
        (save-excursion
          (goto-char (agda2-name-index--import-point))
          ;; A file whose last line has no newline: keep the import on a
          ;; line of its own rather than gluing it to that line.
          (unless (bolp) (insert "\n"))
          (setq where (line-number-at-pos))
          (insert line))
        ;; Where, and in which buffer: the import goes where an import
        ;; has to go, which is usually not where point is, so an answer
        ;; that only echoed the line would look like nothing happened.
        (message "%s (line %d of %s)"
                 (string-trim-right line) where (buffer-name))))))

(defun agda2-name-index--import-point ()
  "Where an `open import' should go: after the last one, or the header.
Falls back to the beginning of the current line, so that the name
still lands somewhere the user can see it."
  (let ((here (line-beginning-position)))
    (save-excursion
      (goto-char (point-min))
      (let ((last-import nil))
        (while (re-search-forward "^\\(?:open[ \t]+\\)?import[ \t]" nil t)
          (setq last-import (line-beginning-position 2)))
        (cond
         (last-import)
         ;; Otherwise just after the module header, whose `where' may
         ;; be on a later line than its name.
         ((and (progn (goto-char (point-min))
                      (re-search-forward "^module\\_>" nil t))
               (re-search-forward "\\_<where\\_>" nil t))
          (line-beginning-position 2))
         (t here))))))

(defconst agda2-name-index--actions
  '(("insert the name" . insert)
    ("jump to the definition" . jump)
    ("insert an open import" . import))
  "The actions `agda2-find-name' offers under a prefix argument.")

(defun agda2-name-index--act (action hit buffer)
  "Do ACTION with HIT, editing BUFFER.
BUFFER is the buffer the search was started from, named rather
than assumed: an action that edits must not be able to land
anywhere else -- least of all in the minibuffer the candidate was
read in, which is what a key acting on a candidate reads it in.
Jumping edits nothing and visits a file of its own, so it is the
one action that does not want the buffer."
  (pcase action
    ('jump (agda2-name-index--jump hit))
    ((or 'insert 'import)
     (unless (buffer-live-p buffer)
       (user-error "The buffer this search started in is gone"))
     (with-current-buffer buffer
       (if (eq action 'insert)
           (agda2-name-index--insert hit)
         (agda2-name-index--import hit))))
    (_ (error "Unknown action: %S" action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The commands

(defvar agda2-name-index--history nil
  "Patterns `agda2-find-name' has been given.")

(defvar agda2-name-index--last nil
  "The hit `agda2-find-name' last acted on.")

(defun agda2-name-index--read-pattern ()
  "Read a pattern, defaulting to the name around point.
`PATTERN in MODULE' restricts the search to a module prefix, as
`agda-client find' spells it.  Returns (PATTERN . MODULE)."
  (let* ((default (thing-at-point 'symbol t))
         (input (read-string
                 (if default
                     (format "Find name (default %s): " default)
                   "Find name: ")
                 nil 'agda2-name-index--history default))
         (input (string-trim input)))
    (when (string-empty-p input)
      (user-error "No pattern given"))
    ;; An Agda name cannot contain a space, so this is unambiguous.
    (if (string-match "\\`\\(.+?\\) +in +\\([^ ]+\\)\\'" input)
        (cons (match-string 1 input) (match-string 2 input))
      (cons input nil))))

(defvar agda2-name-index--minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-i") #'agda2-name-index-toggle-implicit)
    (define-key map (kbd "M-.") #'agda2-name-index-jump)
    (define-key map (kbd "M-RET") #'agda2-name-index-import)
    map)
  "The keys `agda2-find-name' adds while it reads a candidate.
Composed with whatever map `completing-read' set up, rather than
replacing it.  \\<agda2-name-index--minibuffer-map>\\[agda2-name-index-toggle-implicit] switches how the types are rendered;
\\[agda2-name-index-jump] and \\[agda2-name-index-import] choose the candidate and act on it, whatever
`agda2-name-index-default-action' says.")

(defun agda2-name-index-toggle-implicit ()
  "Show or hide the implicit arguments in the types beside the candidates.
`agda2-find-name' binds this to \\<agda2-name-index--minibuffer-map>\\[agda2-name-index-toggle-implicit] while it reads a
candidate, and rebinds `agda2-name-index-show-implicit' around
that read, so the switch lasts for the search in progress only;
customise that option to change where a search starts.

Nothing is queried again: the index stores both renderings of
every type, so this only redisplays the candidates that are
already on screen."
  (interactive)
  (unless (minibufferp)
    (user-error "Not searching for a name"))
  (setq agda2-name-index-show-implicit (not agda2-name-index-show-implicit))
  ;; The affixation function reads the option when it renders, so the
  ;; completions have to be built again -- and the cached order with
  ;; them, since the strings it caches carry the old affixations.
  (when (fboundp 'completion--flush-all-sorted-completions)
    (completion--flush-all-sorted-completions))
  (minibuffer-completion-help)
  (minibuffer-message (if agda2-name-index-show-implicit
                          "Implicit arguments shown"
                        "Implicit arguments hidden")))

(defvar agda2-name-index--exit-action nil
  "The action asked for by the key that ended the candidate read.
`agda2-find-name' rebinds this around the read; nil means the read
ended without a key choosing an action of its own.")

(defun agda2-name-index--exit-with (action)
  "End the candidate read the way \\`RET' does, and do ACTION with the hit.
Choosing the candidate is left to `minibuffer-complete-and-exit',
so these keys select exactly what \\`RET' would have, and differ from it
only in what is then done."
  (unless (minibufferp)
    (user-error "Not searching for a name"))
  (setq agda2-name-index--exit-action action)
  (minibuffer-complete-and-exit))

(defun agda2-name-index-jump ()
  "Take this candidate and go to where it is defined.
`agda2-find-name' binds this to \\<agda2-name-index--minibuffer-map>\\[agda2-name-index-jump] while it reads a candidate,
whatever `agda2-name-index-default-action' is."
  (interactive)
  (agda2-name-index--exit-with 'jump))

(defun agda2-name-index-import ()
  "Take this candidate and insert an `open import' for its module.
`agda2-find-name' binds this to \\<agda2-name-index--minibuffer-map>\\[agda2-name-index-import] while it reads a candidate,
whatever `agda2-name-index-default-action' is.  The import goes
where an import has to go -- after the last one, or after the
module header -- and so usually not where point is; the echo area
says which line it went to."
  (interactive)
  (agda2-name-index--exit-with 'import))

;;;###autoload
(defun agda2-find-name (&optional arg)
  "Find a name anywhere this project could import from.

Prompts for a pattern, matches it against every public definition
in the project's name index, and offers the hits ranked: the whole
name first, then a prefix of it, then a whole hyphen- or
underscore-separated component of it, then anywhere in it.
`PATTERN in MODULE' restricts the search to a module prefix.

The hit that is chosen is acted on by
`agda2-name-index-default-action'; with a prefix ARG, the action
is asked for instead.  Whichever it is, the action is carried out
in the buffer the search was started from.

This never talks to the Agda process: the index has the types
already rendered, so it answers while Agda is busy checking
something else.  Agda is run once, out of band, to say where the
index is and what it covers -- and \\[agda2-name-index-build]
builds or refreshes it.

While the candidates are showing, \\<agda2-name-index--minibuffer-map>\\[agda2-name-index-toggle-implicit] shows or hides the
implicit arguments in the types beside them, for this search
only; `agda2-name-index-show-implicit' is where one starts.
\\[agda2-name-index-jump] and \\[agda2-name-index-import] take the candidate \\`RET' would have taken and
jump to it, or insert an `open import' for its module, instead of
doing the default action with it."
  (interactive "P")
  (agda2-name-index--supported)
  ;; Started first, so that the coverage scan runs while the pattern is
  ;; being typed.
  (let* ((origin (current-buffer))
         (process (agda2-name-index--status-start))
         (query (condition-case error
                    (agda2-name-index--read-pattern)
                  (quit (agda2-name-index--status-abort process)
                        (signal 'quit nil))
                  (error (agda2-name-index--status-abort process)
                         (signal (car error) (cdr error)))))
         (pattern (car query))
         (module (cdr query))
         (status (agda2-name-index--status-finish process)))
    (agda2-name-index--check status)
    (let* ((answer (agda2-name-index-query
                    (plist-get status :location) pattern
                    :module module
                    :kinds nil
                    :limit agda2-name-index-limit))
           (hits (car answer))
           (matched (cdr answer))
           (note (agda2-name-index--coverage-note status)))
      (when (null hits)
        (user-error "No name matches %s%s%s" pattern
                    (if module (format " in %s" module) "")
                    (if note (format " (%s)" note) "")))
      (let* (;; Rebound so that `agda2-name-index-toggle-implicit'
             ;; changes this search and not the next one.
             (agda2-name-index-show-implicit agda2-name-index-show-implicit)
             (agda2-name-index--exit-action nil)
             (pair (agda2-name-index--candidates hits))
             (candidates (car pair))
             (table (cdr pair))
             (chosen
              (minibuffer-with-setup-hook
                  (lambda ()
                    (use-local-map
                     (make-composed-keymap agda2-name-index--minibuffer-map
                                           (current-local-map))))
                (completing-read
                 (format "%s (%s%s%s): "
                         pattern
                         (if (= (length hits) matched)
                             (format "%d" matched)
                           (format "%d of %d" (length hits) matched))
                         (if (= matched 1) " hit" " hits")
                         (if note (format "; %s" note) ""))
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       `(metadata
                         (category . agda2-name)
                         (affixation-function
                          . ,(agda2-name-index--affixation table))
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))
                     (complete-with-action action candidates string predicate)))
                 nil t nil nil (car candidates))))
             (hit (or (gethash chosen table)
                      (user-error "No such hit: %s" chosen)))
             (action (cond
                      ;; A key that named an action has said what to do,
                      ;; so there is nothing left to ask about.
                      (agda2-name-index--exit-action)
                      (arg (cdr (assoc (completing-read
                                        (format "Do what with %s? "
                                                (agda2-name-index--full hit))
                                        (mapcar #'car agda2-name-index--actions)
                                        nil t)
                                       agda2-name-index--actions)))
                      (t agda2-name-index-default-action))))
        (setq agda2-name-index--last hit)
        (agda2-name-index--act action hit origin)))))

;;;###autoload
(defun agda2-name-index-describe ()
  "Show the hit `agda2-find-name' last acted on, in full.
Both renderings of its type, which is the one thing a list of
candidates has to abbreviate."
  (interactive)
  (unless agda2-name-index--last
    (user-error "No name has been found yet"))
  (let ((hit agda2-name-index--last))
    (with-help-window "*Agda name*"
      (princ (format "%s\n  %s : %s\n"
                     (agda2-name-index--full hit)
                     (agda2-name-index--qualified hit)
                     (plist-get hit :type)))
      (unless (equal (plist-get hit :type) (plist-get hit :type-implicit))
        (princ (format "\nwith implicit arguments\n  %s : %s\n"
                       (agda2-name-index--qualified hit)
                       (plist-get hit :type-implicit))))
      (when (plist-get hit :params)
        (princ (format "\nmodule parameters\n  %s\n" (plist-get hit :params))))
      (princ (format "\n%s, %d use%s\n%s:%d,%d\n"
                     (plist-get hit :kind)
                     (or (plist-get hit :uses) 0)
                     (if (= (or (plist-get hit :uses) 0) 1) "" "s")
                     (plist-get hit :file)
                     (or (plist-get hit :line) 0)
                     (or (plist-get hit :col) 0))))))

(defvar agda2-name-index--build-process nil
  "The build started by `agda2-name-index-build', if it is running.")

;;;###autoload
(defun agda2-name-index-build (&optional arg)
  "Build or refresh this project's name index.

A refresh re-renders the modules whose interfaces have moved since
the index was built and keeps the rest, which after an ordinary
edit takes a second or two.  With a prefix ARG the whole index is
built again, which is what to reach for after changing Agda
itself.  A first build of a large library takes about a minute.

The build runs out of band -- it is a separate Agda process, and
not the one the session is using, since loading a whole library
peaks at about a gigabyte."
  (interactive "P")
  (when (process-live-p agda2-name-index--build-process)
    (user-error "A name index build is already running"))
  (let* ((default-directory (agda2-name-index--directory))
         (mode (if arg "--build-name-index=full" "--build-name-index"))
         (buffer (get-buffer-create "*Agda name index*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "%s %s\nin %s\n\n" (agda2-name-index--program)
                      mode default-directory)))
    (setq agda2-name-index--build-process
          (make-process
           :name "agda2-name-index-build"
           :buffer buffer
           :noquery t
           :connection-type 'pipe
           :command (append (list (agda2-name-index--program))
                            (agda2-name-index--program-args)
                            (list mode))
           :filter #'agda2-name-index--build-filter
           :sentinel #'agda2-name-index--build-sentinel))
    (message "Building the name index%s..." (if arg " from scratch" ""))))

(defun agda2-name-index--build-filter (process string)
  "Collect STRING from PROCESS, and echo the line it ended with.
The builder prints one line per module, which is the only progress
report there is for something that can take a minute."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (point-max))
        (insert string))))
  (let ((lines (seq-remove #'string-empty-p (split-string string "\n"))))
    (when lines
      (let ((message-log-max nil))
        (message "Name index: %s" (car (last lines)))))))

(defun agda2-name-index--build-sentinel (process event)
  "Report how PROCESS finished; EVENT is why it stopped."
  (unless (process-live-p process)
    (let ((ok (and (eq (process-status process) 'exit)
                   (zerop (process-exit-status process)))))
      (if (not ok)
          (progn
            (when (buffer-live-p (process-buffer process))
              (display-buffer (process-buffer process)))
            (message "Building the name index failed: %s" (string-trim event)))
        (message "%s" (or (agda2-name-index--build-summary process)
                          "The name index is up to date"))))))

(defun agda2-name-index--build-summary (process)
  "The last non-empty line PROCESS printed, which is its summary."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((lines (seq-remove #'string-empty-p
                               (split-string (buffer-string) "\n"))))
        (car (last lines))))))

(provide 'agda2-name-index)
;;; agda2-name-index.el ends here
