.. _agent-interface:

***************
Agent interface
***************

.. versionadded:: 2.9.0

The agent interface is a frontend for driving an Agda proof session from
the terminal or from a program: a coding assistant, a script, or an
experiment harness.  It consists of two parts:

* :option:`--interaction-agent`, a JSON-lines request/response protocol
  spoken over Agda's standard input and output, and

* ``agda-client``, a one-shot command line client that keeps a
  per-project Agda session alive between invocations.

The existing :option:`--interaction` and :option:`--interaction-json`
frontends are unaffected.

.. note::

   Unlike the query verbs, the proof steps (``give``, ``elaborate``,
   ``refine``, ``intro``, ``case``, ``solve`` and ``auto``) **rewrite
   the file on disk**.  Keep your work under version control, as you
   would when letting an editor fill in goals for you.

.. _agent-interface-cli:

The ``agda-client`` command
===========================

Run ``agda-client`` from inside your project.  The first invocation
starts a background daemon holding one Agda session; later invocations
attach to it, so type checking is not repeated:

.. code-block:: console

  $ agda-client load Sorting.agda
  2 goals
  ?0 : Sorted (insert x xs)
  ?1 : x ≤ y

  $ agda-client goal 0
  Goal: Sorted (insert x xs)
  ———— Context ———————————————————————————————————————————————
  xs : List ℕ
  x  : ℕ

  $ agda-client check 0 'insert-sorted x xs'
  accepted
  Goal: Sorted (insert x xs)
  Elaborates to: insert-sorted x xs

The verbs are:

.. list-table::
   :widths: 30 70
   :header-rows: 1

   * - Command
     - Meaning
   * - ``load FILE [AGDA-ARG...]``
     - Type-check ``FILE`` and list its goals.  The ``AGDA-ARG``\ s are
       passed on to Agda, and reused by the reloads that follow.  With
       ``--profile``, also list the CPU time spent on each definition
       (see :ref:`agent-interface-profiling`).
   * - ``reload``
     - Type-check the session's file again; takes ``--profile`` too.
   * - ``goals``
     - List the goals with their types and ranges.
   * - ``goal GOAL``
     - Goal type, context, boundary and constraints.
   * - ``context GOAL``
     - Just the context of ``GOAL``.
   * - ``check GOAL EXPR``
     - Type-check ``EXPR`` against ``GOAL`` *without* filling it.
   * - ``infer [GOAL] EXPR``
     - Infer the type of ``EXPR``.
   * - ``normalise [GOAL] EXPR``
     - Normalise ``EXPR`` (aliases: ``normalize``, ``norm``).
   * - ``candidates GOAL EXPR...``
     - Type-check several expressions against ``GOAL``, none of them
       filling it.
   * - ``constraints``
     - List the unsolved constraints.
   * - ``helper GOAL EXPR``
     - The type a helper function ``f args`` would have to have.
   * - ``search TERM...``
     - Names in scope whose *type* mentions all of ``TERM``\ s (space
       separated identifiers, not a type).  A search over types, not
       over names.
   * - ``find PATTERN [in MOD]``
     - Names matching ``PATTERN`` anywhere the project can import from,
       ranked, with their types.  A search over names, not over types.
   * - ``index``
     - Build the index ``find`` reads, if anything has gone stale.
   * - ``module-contents [GOAL] M``
     - The names and submodules ``M`` exports, with their types.
   * - ``why [GOAL] NAME``
     - Why ``NAME`` is in scope and what it can denote.
   * - ``definition LINE:COL``
     - Where the name at that position is defined.
   * - ``give GOAL EXPR``
     - Fill ``GOAL`` with ``EXPR``.
   * - ``elaborate GOAL EXPR``
     - Fill ``GOAL`` with the elaborated form of ``EXPR``.
   * - ``refine GOAL [EXPR]``
     - Refine ``GOAL`` by ``EXPR``, leaving a goal per argument; with no
       ``EXPR``, introduce.
   * - ``intro GOAL``
     - Introduce the constructor or lambda ``GOAL`` asks for.
   * - ``case GOAL [VARS]``
     - Split ``GOAL``'s clause on the space separated ``VARS``; with no
       ``VARS``, split on the result.
   * - ``solve [GOAL]``
     - Write out goals the type checker has already solved.
   * - ``auto [GOAL] [ARGS]``
     - Proof search (Mimer).  ``ARGS`` is passed to it, e.g. ``-t 5s``,
       or ``-l`` to list candidates instead of filling the goal.
   * - ``status``
     - What the session has loaded.
   * - ``handshake``
     - Protocol and Agda version, methods, defaults.
   * - ``abort``
     - Interrupt the command the daemon is running.
   * - ``stop``
     - Shut the daemon down.

``GOAL`` is either an interaction id (``0``, ``1``, ...) or a
``LINE:COL`` position inside a goal.  The latter is stable across the
renumbering that follows an edit, which makes it convenient when the
file is being edited between commands.

The verbs from ``give`` down are the ones that change your file.  Each
of them applies the result to the source, reloads, and prints the goals
of the new revision:

.. code-block:: console

  $ agda-client give 0 'insert-sorted x xs'
  give ?0 = insert-sorted x xs
  1 goal
  ?0 : x ≤ y

Useful options: ``--json`` prints the raw protocol answer instead of the
rendered text, ``--show-implicit`` and ``--show-irrelevant`` print the
arguments Agda usually hides, ``-n``/``--normalise`` chooses how much the answer is
normalised, ``-t``/``--timeout`` bounds how long a request may run
(default: 120 seconds, 600 for ``load`` and the proof steps), and
``--no-timeout`` removes the bound.  ``--force`` makes ``give`` skip
Agda's safety checks and ``index`` rebuild even when nothing is stale,
and ``-p``/``--pattern-lambda`` makes ``refine`` and ``intro`` introduce
a pattern matching lambda.  ``find`` takes ``--in MODULE``,
``--glob``, ``--kind KIND`` (repeatable), ``--type NAMES`` and
``--limit N``.  ``--profile[=OPTION]`` makes ``load`` or ``reload``
profile the check, and ``--version`` prints the client's version.
``agda-client --help`` lists them all.

The exit status is 0 when the request was answered, 1 when it failed
(an Agda error, a timeout, an abort), and 2 when the client itself
failed: bad arguments, or no daemon to talk to.  An answer need not be
good news: a ``check`` whose expression is rejected exits 0, and says
``rejected`` followed by the type error.

The client's options may come anywhere on the command line, before the
verb or after it.  ``load`` is the exception that needs a way out: a
long flag it does not know, once the file is in, is one of that verb's
``AGDA-ARG``\ s and reaches Agda untouched, as in ``agda-client load
Sorting.agda --without-K``.  ``--profile`` is always the client's, so
that it profiles one load rather than every reload after it.  A ``--`` ends the client's options for
good, which is how a flag both of them know is aimed at Agda:
``agda-client load Sorting.agda -- --show-implicit``.

The environment variables ``AGDA_CLIENT_BIN`` (which Agda to run),
``AGDA_CLIENT_ARGS`` (extra arguments for it), ``AGDA_CLIENT_ROOT`` (the
project root, otherwise the innermost enclosing directory containing an
``.agda-lib`` file) and ``AGDA_CLIENT_IDLE`` (seconds before an unused
daemon exits, default four hours) configure the daemon.

The daemon listens on a Unix domain socket under ``$XDG_RUNTIME_DIR``
(or, where that is not set, under the user's cache directory), named
after the project root, the identity of the Agda binary and the
arguments it is started with, so that a rebuilt Agda, or a different
``AGDA_CLIENT_ARGS``, gets a fresh session rather than a version
mismatch or the arguments of an earlier daemon.  It
exits when Agda does, when a client says ``stop``, or after
``AGDA_CLIENT_IDLE`` seconds with nothing to do; the next command starts
a new one.  ``agda-client`` needs Unix domain sockets, and says so and
stops on a platform that has none.

Editing while a session is open
-------------------------------

The file on disk is the source of truth.  Edit it with whatever tools
you like; the next query notices that the contents changed, reloads the
file, and says so:

.. code-block:: console

  $ agda-client goals
  [file changed on disk: reloaded]
  1 goal
  ?0 : x ≤ y

A load that fails leaves the session with no goals, and a query then
reports the error the load stopped at.  It does so from memory for as
long as the file on disk is the one that failed — checking it again
could only fail the same way, at the same cost — and checks again as
soon as the file changes, or when told to with ``reload``.  The error's
``data`` carries ``"unchangedSinceLoad": true`` in that case.

``find`` and ``index`` need no loaded file: before anything is loaded
they work from the directory the session was started in, which is the
project root, so a lemma can be looked for before the file that will
use it exists.

Note that goals are renumbered by such a reload.  Every answer reports
the ``revision`` the session is at; a client that would rather be told
about a change than have it silently handled can pass
``--expect-revision N``, which fails with a ``staleDocument`` error
instead of reloading.

.. _agent-interface-protocol:

The protocol
============

``agda --interaction-agent`` reads one JSON object per line from
standard input and writes one JSON object per line to standard output.
No prompt is printed, and diagnostics go to standard error.

A request is::

  {"id": <any>, "method": <string>, "params": {...},
   "timeoutMs": <int>, "expectRevision": <int>}

where ``id`` is echoed back verbatim, and ``timeoutMs`` and
``expectRevision`` are optional.  Agda answers with zero or more event
lines::

  {"id": <any>, "event": "progress", "message": <string>}

followed by exactly one terminal line, either::

  {"id": <any>, "result": {...}}

or::

  {"id": <any>, "error": {"code": <string>, "message": <string>,
                          "range": {...}, "data": {...}}}

Before the first request Agda emits an unsolicited
``{"event": "ready", "handshake": {...}}`` line.  The handshake reports
the protocol version, the Agda version, the available methods, the
normalisation and compute modes, the per-method defaults, and the
position encoding.

Positions count Unicode code points; lines and columns count from 1,
offsets from 0, and ranges are half open.  Every result carries the
fields ``file``, ``revision``, ``checked``, ``resynced`` (the file had
changed on disk and was reloaded) and ``reloaded`` (a reload happened,
for whichever reason), alongside the method's own fields and a ``text``
field holding Agda's own rendering of the answer.

Timeouts and aborting
---------------------

A request carrying ``timeoutMs`` is aborted when the time expires,
using the same machinery as the Emacs mode's abort command, and answered
with a ``timeout`` error.  An ``abort`` request sent while another
request is running does the same immediately, and that includes a
``buildIndex`` in progress, whose builder process is terminated.  In
both cases the session survives and answers the next request correctly.
The ``abort`` request's own answer says in ``aborted`` whether there was
anything running to abort.

A timeout also says where it struck.  When it fired while a definition
was being checked, the message names that definition and the CPU time
spent on it, ``data.checking`` holds its qualified name, and
``data.profile`` holds the time billed to each definition up to then,
in the form described under :ref:`agent-interface-profiling`.  In text
mode ``agda-client`` prints the most expensive of them:

.. code-block:: console

  $ agda-client -t 5 load Profile.agda
  load: timeout
  timed out while checking Profile.spin, after 3798 ms of CPU time on it; the command was aborted and the session is usable
  Slowest so far (CPU time):
        3798 ms  Profile.spin
        1132 ms  Profile.slow
         104 ms  Miscellaneous
          12 ms  Profile.fib
          10 ms  Profile.even

.. _agent-interface-profiling:

Profiling
---------

``load`` and ``reload`` take a ``profile`` parameter: a profiling
option as Agda's ``--profile`` accepts it (``"definitions"``,
``"modules"``, ``"internal"``, …), a list of them, or ``true`` for
``"definitions"``.  The load then runs with those options, for this
load only, and checks the file from source even if its interface file
is current — Agda would otherwise reuse the interface, check nothing,
and report an empty profile.  Imported modules keep their interfaces;
pass ``--ignore-interfaces`` in ``args`` to profile them as well.

The result carries a ``profile`` field,

.. code-block:: json

  {"totalMs": 1238,
   "accounts": [{"account": "Profile.slow", "definition": "Profile.slow", "ms": 1102},
                {"account": "Miscellaneous", "ms": 72},
                {"account": "Profile.fib", "definition": "Profile.fib", "ms": 12},
                {"account": "Profile.even", "definition": "Profile.even", "ms": 10}]}

listing the accounts billed at least 10 ms, most expensive first, and
the table is appended to the ``text``.  A profiling option set some other
way — in the file's ``OPTIONS`` pragma, or among the session's ``args`` —
gets the same field.  On the command line:

.. code-block:: console

  $ agda-client load --profile Profile.agda
  1 goal
  ?0 : loop zero ≡ zero

  Profile (CPU time; accounts of 10 ms or more):
    Total             1238 ms
    Profile.slow      1102 ms
    Miscellaneous       72 ms
    Profile.fib         12 ms
    Profile.even        10 ms

(``Profile.agda`` is the fixture of the test suite's ``Profile`` test.
``Miscellaneous`` is time spent outside any definition: parsing, scope
checking, serialisation, and the like.)

Requests are executed one at a time, and one sent while another is
running waits its turn — with the exception of ``abort``, which takes
effect as soon as it is read.

Methods
-------

``load``, ``reload``, ``goals``, ``goal``, ``context``, ``check``,
``checkCandidates``, ``infer``, ``normalise``, ``constraints``,
``helperType``, ``search``, ``findNames``, ``buildIndex``,
``moduleContents``, ``whyInScope``, ``definitionAt``, ``give``,
``elaborate``, ``refine``, ``intro``, ``case``, ``solve``, ``auto``,
``handshake``, ``status``, ``abort`` and ``close``; they correspond to
the ``agda-client`` verbs above.
``check`` deserves a note: it type-checks an expression against a goal
*without* consuming the goal, and a rejected candidate is reported as a
result with ``"accepted": false`` and the type error, not as a failed
request.  ``checkCandidates`` does the same for a list of expressions
in a single request, which is how to try several ideas without a round
trip each; its ``timeoutMs`` applies to each candidate separately.

The methods that ask about names take a goal when the answer depends on
the scope one is in, and answer with respect to the top-level scope when
they do not: ``search`` (identifiers, *not* a type: it lists the names
whose *type* mentions all of them, so it will not find a name by its
spelling, and it sees only what the loaded file imports — ``findNames``
below is the query for either of those),
``moduleContents``, ``whyInScope``, and ``infer``/``normalise``.
``definitionAt`` is the exception: it reads the highlighting index built
by the last successful load, so it describes the file as it was then,
and it answers with the symbol at the position (its text, its kind,
Agda's tooltip note and its range) and the site the symbol is defined
at — a file, line and column, which may be in another module:

.. code-block:: console

  $ agda-client definition 24:19
  double (function)
  defined at /home/me/proj/Queries.agda:16,3

A position that carries no name is not an error: the answer has
``"symbol": null``.

Searching names across the project
----------------------------------

``findNames`` is the complement of ``search``: it matches *names*
rather than types, and it looks in every module the project could write
an ``import`` for — its own include paths plus the transitive
``depend:`` closure of its ``.agda-lib`` — rather than in the scope of
the loaded file.

.. code-block:: console

  $ agda-client find is-basis
  findNames: 16 hits for "is-basis"
    OrderedTypes.SupLattice-SmallBasis.is-basis : {𝓤 𝓣 𝓥 : Universe} ...
      RecName, exact, 21 uses -- source/OrderedTypes/SupLattice-SmallBasis.lagda:94,9
      params (pt : propositional-truncations-exist) (fe : Fun-Ext)
    Locales.Frame.is-basis-for : {𝓤 𝓥 𝓦 : Universe} (F : Frame pt fe ...
      FunName, prefix, 4 uses -- source/Locales/Frame.lagda:1317,1
      params (pt : propositional-truncations-exist) (fe : Fun-Ext)
    ...
  index:     built 2026-08-31 13:11:32 UTC
  modules:   1033, 1007 indexed, 0 stale, 1 unindexed, 25 unindexable

Its hits come from an index built beside the interface files by a
separate process, so until ``buildIndex`` (the ``index`` verb, or
:option:`--build-name-index`) has made one there are none.

.. _findnames-ranking:

How a pattern matches
~~~~~~~~~~~~~~~~~~~~~

The pattern is matched against the ``name`` alone — not against the
module, the qualifier or the type — and hits are ranked by *how* it
matched.  The rank is part of the answer, so a caller can tell an exact
hit from the fortieth substring match without re-deriving the ordering,
and it is specified here rather than left to the implementation, because
a client that reads the index file itself has to agree with it:

.. list-table::
   :widths: 8 20 72
   :header-rows: 1

   * - ``rank``
     - ``match``
     - The pattern is …
   * - 1
     - ``exact``
     - the whole name.
   * - 2
     - ``prefix``
     - a proper prefix of the name.
   * - 3
     - ``component``
     - a whole component of the name, where the components are what the
       name splits into at hyphens and underscores: ``is-basis-for`` has
       the components ``is``, ``basis`` and ``for``.
   * - 4
     - ``substring``
     - anywhere in the name.
   * - 5
     - ``mentions``
     - not in the name at all, but in the unqualified part of a name the
       *type* mentions.  Only reached when ``type`` is given, since on
       its own this rank would bury the name matches.

Matching is case-sensitive, and every rank implies rank 4: a pattern
that does not occur in the name does not match it.  Ties within a rank
break on ``uses`` — how many indexed definitions mention the hit — then
on where the hit lives — the project's own include directories before
those of the libraries it depends on, and Agda's builtin modules last,
which is the order of ``includePaths`` in the index header — and then
on the module and the name, so the order is total and stable.
Hits are ranked before ``limit`` cuts them, and ``matched`` says how
many there were before the cut.

The other parameters filter rather than rank.  ``module`` restricts the
search to a top-level module prefix, and scopes the coverage report with
it, so that a query about one corner of a library need not care that a
distant part of it has moved; ``Foo`` covers ``Foo`` and ``Foo.Bar`` but
not ``Foobar``.  ``kinds`` restricts to Agda's kinds of name
(``FunName``, ``DataName``, ``RecName``, ``ConName``, ``FldName``,
``AxiomName``, ``PrimName``, ``MacroName``, ``PatternSynName``,
``GeneralizeName``, …), compared ignoring case and a trailing ``name``,
so ``FunName``, ``Fun`` and ``fun`` all name a function.  ``type`` is
``search``'s predicate — space separated identifiers, each of which must
occur in some name the type mentions — applied across the whole project
rather than the current scope.  ``glob`` reads ``*`` and ``?`` in the
pattern as wildcards, matching whole names, prefixes, components and
substrings just as a literal does; it is off by default, because Agda
names contain ``*`` (``ℤ*-assoc``), and a backslash escapes a wildcard.
``showImplicit`` in the request envelope chooses which of the two stored
renderings of the type each hit carries.

Each hit is ``{name, qualifier?, kind, module, file, line, column,
params?, type, rank, match, uses}``.  It is reported fully — its module,
its qualifier and its name — because it is by definition *not* in the
caller's scope, and the next move is to import the module and write the
name.  ``type`` is rendered against ``params``, the enclosing module's
telescope, which is why both are given.

What the answer covered
~~~~~~~~~~~~~~~~~~~~~~~

Every answer carries the ``index`` object, whether or not there were
hits:

.. code-block:: json

  {"modules": 1033, "definitions": 23907, "indexed": 1007, "stale": 0,
   "unindexed": 1, "unindexable": 25, "removed": 0, "modified": 0,
   "builtAt": "2026-08-31T13:11:32.259180911Z"}

``modules`` is how many source modules an index would have to cover;
``indexed`` how many of them the index covers and whose interface has
not moved since; ``stale`` how many have been checked but do not match
what the index recorded, and so would be fixed by another build;
``unindexed`` how many have never been checked here, and so are not in
the index yet (a build type-checks them, which is what makes a first
build over an unchecked library expensive); ``unindexable`` how many the builder tried and could
not read, which another build would not change; and ``removed`` how many
the index has whose source has since gone away.  ``builtAt`` is ``null``
when there is no index — which is how a query over a library that has
never been checked says so, instead of looking like a library with
nothing in it.

``modified`` is the one count that no build can bring down: it is how
many modules have been *edited* since they were last checked.  The index
is built from interface files, so for such a module it describes the
text that was checked rather than the text on disk, and it says so
rather than letting an answer look current.  Agda re-checks the module
the next time it loads it, and the module then shows up as ``stale``,
which a build does fix.

Deciding all this needs no type checking and no decompression: an
interface file begins with an uncompressed prefix holding its hashes and
its format version, so the scan costs a stat and twenty-four bytes per
module.  ``modified`` costs a little more — the modification times say
which sources are worth hashing, and the source hash in that same prefix
decides — but only for the files that look newer than their interface,
which is normally none of them.  That is why every query can pay for it.

.. _the-name-index:

The index file
--------------

The index is built by :option:`--build-name-index`, run in the project
directory, or by ``agda-client index``, or by the ``buildIndex`` method,
which run that same mode in a process of its own and relay its output as
progress events.  That process gets the session's own command line, so
options such as :option:`--library-file` that decide which libraries
the project sees hold for the index as well.  It is never built
implicitly: the first build of a library of a thousand modules takes
about a minute when their interfaces are current, which is worth
spending but not worth a query spending on its own initiative.

.. code-block:: console

  $ cd ~/TypeTopology && agda --build-name-index
  Indexing 1033 of 1033 modules on the include path.
  ...
  Wrote /home/me/TypeTopology/_build/2.9.0/agda/name-index.jsonl:
    1032 modules, 23907 definitions. (26 could not be loaded.)

Every build after the first is a *refresh*: it re-renders the modules
whose interfaces have moved since the index was built and keeps the
rest of it verbatim.  Nothing has to be tracked for this to reach the
dependents of a change, because ``iFullHash`` combines a module's own
source hash with those of its imports, so a module whose dependency
changed has moved too.  On the same library, refreshing after an edit to
a module a hundred others import — and the reload that re-checks the
five on the path to the file being edited — takes a second and a half
rather than a minute:

.. code-block:: console

  $ agda --build-name-index
  Indexing 6 of 1033 modules on the include path. (1027 already indexed.)
  ...

What a refresh cannot keep is ``uses``, which counts across the whole
index; it is recomputed every time, from the ``mentions`` and ``qname``
of kept and re-rendered entries alike, so a refresh gives the numbers a
full build would.  ``--build-name-index=full`` re-renders everything,
which is what to reach for after changing Agda itself; an index written
by another Agda, or under another include path, is rebuilt in full
anyway.

``buildIndex`` and ``index`` do nothing when nothing is stale, unless
``force`` is given, which is how they ask for that full build.  A first
build takes long enough that the request wants a generous ``timeoutMs``
(``agda-client index`` allows half an hour, and ``--no-timeout`` removes
the bound).  ``buildIndex`` also takes ``module``, which scopes the
coverage report in its answer — not the build, which always covers
everything the include path reaches.

Refreshing is cheap enough that calling it whenever ``stale`` is nonzero
is the intended use.  It is still not done implicitly: a query costs a
quarter of a second and a refresh a second or two, so a query that
refreshed would be several times slower for a result the caller usually
does not need — the names that go stale in a session are the ones in the
file it has loaded, and those are in its own scope already, where
``search`` and ``moduleContents`` reach them.

:option:`--name-index-status` prints the ``index`` object described
above, and the path of the index file, as one line of JSON and exits.
It is how a client that reads the file itself — the point of publishing
the format — finds it, without re-deriving the build-directory layout:

.. code-block:: console

  $ cd ~/TypeTopology && agda --name-index-status
  {"location":"/home/me/TypeTopology/_build/2.9.0/agda/name-index.jsonl",
   "formatVersion":3,"modules":1033,"indexed":1007,"stale":0, ...}

Nothing at query time needs a type checker — the builder has already
rendered every type to text — so the file, and not the daemon, is the
product.  Its format is therefore public: a client written in another
language can read it directly, and every field is interpretable without
Agda.  The Emacs mode is such a client: :ref:`find-name` reads this
same file, with no Agda process in the query at all, which is why it
answers while the session is busy checking something else.  ``formatVersion`` in the ``index`` object of a ``findNames``
answer is the version described here, and a file of a different version
is treated as no index at all.

The file is `JSON Lines <https://jsonlines.org>`_, in UTF-8.  Its first
line is the header:

.. code-block:: json

  {"format": 3, "agdaVersion": "2.9.0",
   "builtAt": "2026-08-31T12:32:51.113744835Z",
   "includePaths": ["/home/me/TypeTopology/source", "..."],
   "definitions": 23907,
   "modules": [{"module": "UF.Base",
                "source": "/home/me/TypeTopology/source/UF/Base.lagda",
                "hash": "3f6a1c9e07b2d541", "definitions": 120}]}

``includePaths`` is the include path the index was built under, in the
order the ranking uses: the project's own directories first, then those
of the libraries it depends on, and Agda's primitive library last.  A
module's place in that order is that of the longest of these
directories holding its ``source``.

``modules`` is the *manifest*: one record per indexed module, holding
its source file, the number of entries the index has for it, and
``hash``, the interface's full hash as sixteen hexadecimal digits.  The
manifest lives in the header rather than being spread over the lines
below so that a client deciding whether the index is stale reads exactly
one line — about ninety kilobytes for a thousand-module library, against
the megabytes of entries it does not have to touch.  A module is stale
when the ``iFullHash`` in the twenty-four-byte prefix of its current
``.agdai`` differs from ``hash``.  The full hash rather than the source
hash, because a module's *rendered* types change when an import does.

A manifest record with a ``failed`` field is a module the builder could
not read — one binding a builtin the project has already bound, say —
recorded against the hash it choked on, with no entries and no line of
its own below.  Such a module is *unindexable* rather than stale: it is
accounted for, and another build would not change that.

Each following line holds one module's entries:

.. code-block:: json

  {"module": "OrderedTypes.SupLattice-SmallBasis",
   "params": "(pt : propositional-truncations-exist) (fe : Fun-Ext)",
   "entries": [
     {"name": "is-basis", "kind": "RecName", "line": 94, "col": 9,
      "qname": "OrderedTypes.SupLattice-SmallBasis.is-basis",
      "type": "{𝓤 𝓣 𝓥 : Universe} {B : 𝓥 ̇} (L : Sup-Lattice pt fe 𝓤 𝓣 𝓥) ...",
      "typeImplicit": "{𝓤 𝓣 𝓥 : Universe} {B : 𝓥 ̇} (L : Sup-Lattice pt fe ...",
      "mentions": ["OrderedTypes.SupLattice.Sup-Lattice", "..."],
      "uses": 21}]}

``params`` is the enclosing telescope of the top-level module, rendered
once rather than into each of the types below it, which all begin with
it; it is absent for a module that takes no parameters.  A definition
in a *parameterised submodule* keeps that submodule's own binders in its
type, since a caller has to supply them.

Per entry:

``name``
  The concrete name, as the defining module exports it.  This is what a
  query matches against.

``qualifier``
  How the name is reached inside its module when it is not reached
  directly — the record or submodule path one writes in front of
  ``name`` after importing ``module``.  Absent for a name defined in the
  top-level module itself, which is the common case.

``qname``
  The name as ``mentions`` spells it: fully qualified, and through the
  anonymous modules a caller does not write.  It is what joins the two,
  and so what makes ``uses`` computable from the file alone — by a
  refresh that re-renders only part of the index, or by a client that
  wants to know which definitions mention a given one.

``kind``
  Agda's ``KindOfName``: ``FunName``, ``DataName``, ``RecName``,
  ``ConName``, ``FldName``, ``AxiomName``, ``PrimName``, ``MacroName``,
  ``PatternSynName``, ``GeneralizeName`` and so on.

``line``, ``col``
  Where the definition is, in the module's ``source`` file, 1-based.
  Zero for a name with no binding site of its own, such as the
  constructor of a record declared without one.

``type``, ``typeImplicit``
  The type, rendered against ``params`` and on one line, with implicit
  arguments hidden and shown.  Both are rendered in the defining
  module's own scope, so they read as that module's source does; the
  fully qualified name of everything a type refers to is in
  ``mentions``.  A list of hits shows ``type``; ``typeImplicit`` is what
  one needs before writing an application, and it is the rendering that
  shows the universe levels an enclosing anonymous module generalised.
  Both are empty for a pattern synonym, which has no type in the
  signature.

``mentions``
  The fully qualified names occurring in the type.

``uses``
  How many indexed definitions mention this one: a popularity signal,
  and the tiebreaker between hits of equal rank.

The index holds only what the interfaces hold, which is a module's
*public* surface: private names are not in it, and neither is a module
that has never been checked.  ``findNames``'s ``index`` object is how a
caller finds out about the latter.

Method reference
----------------

Every result additionally carries ``file``, ``revision``, ``checked``,
``resynced``, ``reloaded`` and ``text``, and a parameter marked with a
question mark is optional.  ``goal`` is an interaction id or a
``{"line": L, "col": C}`` position inside a goal; ``normalise`` is one
of the modes the handshake lists, defaulting per method.

.. list-table::
   :widths: 22 33 45
   :header-rows: 1

   * - Method
     - Parameters
     - Result
   * - ``handshake``
     -
     - ``handshake``
   * - ``status``
     -
     - ``agdaVersion``, ``goals``
   * - ``load``
     - ``file``, ``args?``
     - ``goals``, ``hiddenGoals``, ``warnings``, ``errors``
   * - ``reload``, ``goals``
     - ``normalise?`` (``goals`` only)
     - as ``load``
   * - ``goal``
     - ``goal``, ``normalise?``
     - ``goal``, ``type``, ``context``, ``boundary``, ``constraints``
   * - ``context``
     - ``goal``, ``normalise?``
     - ``goal``, ``context``
   * - ``check``
     - ``goal``, ``expr``, ``normalise?``
     - ``accepted``, and either ``error`` or the fields of ``goal``
       with ``elaborated``
   * - ``checkCandidates``
     - ``goal``, ``exprs``, ``normalise?``
     - ``goal``, ``candidates`` (each ``expr``, ``accepted``, and
       ``elaborated`` or ``error``)
   * - ``infer``
     - ``goal?``, ``expr``, ``normalise?``
     - ``scope``, ``type``
   * - ``normalise``
     - ``goal?``, ``expr``, ``mode?``
     - ``scope``, ``mode``, ``result``
   * - ``constraints``
     - ``normalise?``
     - ``constraints``
   * - ``helperType``
     - ``goal``, ``expr``, ``normalise?``
     - ``goal``, ``signature``
   * - ``search``
     - ``names``, ``normalise?``
     - ``searched``, ``results`` (``name``, ``type``)
   * - ``findNames``
     - ``pattern``, ``module?``, ``glob?``, ``type?``, ``kinds?``,
       ``limit?``
     - ``hits``, ``matched``, ``index``
   * - ``buildIndex``
     - ``force?``
     - ``built``, ``forced``, ``exitCode``, ``index``
   * - ``moduleContents``
     - ``goal?``, ``module``, ``normalise?``
     - ``scope``, ``module``, ``modules``, ``names``
   * - ``whyInScope``
     - ``goal?``, ``name``
     - ``scope``, ``name``, ``local``, ``definitions``, ``modules``
   * - ``definitionAt``
     - ``position``
     - ``position``, ``symbol``, ``site``
   * - ``give``
     - ``goal``, ``expr``, ``force?``
     - ``applied``, ``edits``, ``previousRevision``, ``goal``, and the
       fields of ``load`` for the new revision
   * - ``elaborate``
     - ``goal``, ``expr``, ``normalise?``
     - as ``give``
   * - ``refine``
     - ``goal``, ``expr?``, ``patternLambda?``
     - as ``give``
   * - ``intro``
     - ``goal``, ``patternLambda?``
     - as ``give``
   * - ``case``
     - ``goal``, ``vars?``
     - as ``give``, plus ``variant`` and ``clauses``
   * - ``solve``
     - ``goal?``, ``normalise?``
     - as ``give``
   * - ``auto``
     - ``goal?``, ``args?``, ``normalise?``
     - as ``give``, or ``applied: false`` with ``solutions`` when
       ``args`` asked for a list
   * - ``abort``, ``close``
     -
     - ``aborted`` / ``closed``

Display options
---------------

Any request may carry ``showImplicit`` or ``showIrrelevant``
(``--show-implicit``, ``--show-irrelevant``), which switch on Agda's
:option:`--show-implicit` and :option:`--show-irrelevant` for that
request alone; the session is unaffected, so query results do not depend
on the order they were asked in.  On a *mutating* method they change
what Agda prints, and therefore the text that is written to the file.

Mutations
---------

A mutating method is a small transaction.  Agda runs the semantic
operation, turns its result into a replacement for the goal's range,
checks that the range really does hold a hole and that the file has not
changed while the command was running, writes the new source through a
temporary file, and only then reloads.  If anything goes wrong before
the write — the expression does not type check, the goal does not
exist, proof search finds nothing — nothing is written at all.

The result reports ``"applied": true``, the ``edits`` that were made
(each with its ``range``, ``oldText`` and ``newText``),
``previousRevision``, and the goals of the new revision.  A method that
had nothing to do — no introduction form fits, proof search found
nothing, no goal was solved — fails with a ``notApplicable`` error
instead.

Two consequences are worth spelling out.  Goals are renumbered by the
reload, so the identifiers in the result are the ones to use next; and
if the edit happens to be one the file cannot survive, the result still
reports ``"applied": true`` with ``"checked": false`` and the error,
because the file really was changed.

Text that spans several lines is indented to the column of the goal, as
the Emacs mode indents what it inserts.  Line endings and a byte order
mark are preserved: only the goal's own range is rewritten.

Case splitting is the exception to "only the goal's own range": it
replaces the whole clause the goal is in, because that is what changes.
Agda reports the clause's range along with the new clauses, so no
scanning of the source is needed, and clauses the Emacs mode's
line-by-line replacement cannot handle work here — a clause spread over
several lines, or one clause of an inline ``λ { … ; … }``.  The clauses
of a ``λ { … }`` are written back separated by ``;``; function clauses,
and the clauses of a ``λ where``, go on lines of their own.  A ``where``
block is left standing, attached to the last of the new clauses.

Diagnostics
-----------

Errors and warnings are reported as objects rather than as prose::

  {"severity": "error",
   "code": "UnequalTypes",
   "message": "Sorting.agda:12.5-9: error: [UnequalTypes] …",
   "range": {...},
   "related": [{"message": "when checking that the expression …",
                "range": {...}}, ...]}

``code`` is Agda's own name for the error or the warning — the name in
the ``[Brackets]`` of an error message, or the one ``-W`` switches a
warning on and off by — so a client can recognise a kind of failure
without matching on English.  ``related`` is the chain of calls the type
checker was in, which is the trailing "when checking …" of the
rendering, each part with its own range.  A call Agda cannot print — a
few carry placeholders for fields they have no value for — keeps its
range and says in place of its message that it could not be printed;
the diagnostic itself is never lost with it.  ``message`` is Agda's
rendering, unchanged, and is what ``agda-client`` prints.

Diagnostics appear as the ``warnings`` and ``errors`` fields of any
result that lists goals, as the ``error`` field of a rejected ``check``,
and, for a failed request, in the error envelope: ``agdaCode`` and
``related`` beside the protocol's own ``code``.

Context entries are structured too::

  {"name": "n", "originalName": "n", "type": "ℕ", "value": null,
   "inScope": true,
   "modality": {"hiding": "visible", "relevance": "relevant",
                "quantity": "erased", "cohesion": "continuous"}}

The modality names are the protocol's own and are stable; Agda's
rendering of a modality is surface syntax and depends on how it was
written down.

.. _agent-interface-parity:

Parity with the Emacs mode
==========================

Every command in the Emacs mode's command table either has an agent
method or is listed here as an editor-only one.

.. list-table::
   :widths: 45 55
   :header-rows: 1

   * - Emacs command
     - Agent
   * - Load
     - ``load``, ``reload``
   * - Show goals / Show constraints
     - ``goals``, ``constraints``
   * - Goal type; Goal type and context
     - ``goal`` (type, context, boundary and constraints at once)
   * - Goal type, context and inferred type
     - ``goal`` together with ``infer GOAL EXPR``
   * - Goal type, context and checked type
     - ``check``, ``candidates``
   * - Context (environment)
     - ``context``
   * - Give; Elaborate and Give; Refine; Auto; Case
     - ``give``, ``elaborate``, ``refine``/``intro``, ``auto``, ``case``
   * - Solve constraints
     - ``solve``
   * - Helper function type
     - ``helper``
   * - Infer (deduce) type; Evaluate term to normal form
     - ``infer``, ``normalise``
   * - Search About; Module contents; Explain why a name is in scope
     - ``search``, ``module-contents``, ``why``
   * - Find a name the project can import; Build the name index
     - ``find``, ``index``.  Both frontends answer from the same index
       file rather than from each other: see :ref:`find-name`.
   * - Go to definition (``xref``)
     - ``definition``
   * - Toggle display of hidden/irrelevant arguments
     - ``--show-implicit``, ``--show-irrelevant``, per request rather
       than as session state
   * - Abort a command; Quit; Kill and restart Agda
     - ``abort``; ``close``/``agda-client stop``, which the next verb
       restarts
   * - Version; Switch to another version of Agda
     - ``handshake``, ``status``; the ``AGDA_CLIENT_BIN`` environment
       variable
   * - Compile; Run backend interaction
     - No method: a batch ``agda --compile`` in a shell needs nothing
       from the session, and backend hole commands are backend-specific
       editor extensions.
   * - Next goal, Previous goal, Remove goals and highlighting,
       Information about the character at point, Comment/uncomment,
       indentation
     - Editor-only: they act on a buffer and a cursor, not on the proof
       state.

Using it from a coding assistant
================================

The workflow an assistant needs to know is short: load once, then edit
the file with its own tools and query freely — the session notices the
change and reloads; use ``check`` or ``candidates`` to try an expression
before committing to it; use the proof steps for anything whose text
Agda computes (case splits, ``solve``, ``auto``), since transcribing
those by hand is where mistakes happen; and re-read the goal
identifiers from every answer, because a reload renumbers them.
Addressing a goal by ``LINE:COL`` avoids that renumbering entirely.

``doc/agent-workflow.md`` in the Agda repository says all of that in
the form an assistant can be handed directly.  It carries the front
matter a skill needs, so it can be copied or symlinked as one
(``~/.claude/skills/agda/SKILL.md`` for Claude Code), or its body can be
pasted into a proof project's agent instructions (``AGENTS.md``,
``CLAUDE.md``, or whatever your tool reads).

One thing to check in those project instructions: if they tell the
assistant to type-check with ``agda FILE``, replace that with
``agda-client load FILE``.  An always-loaded instruction to run ``agda``
outranks an optional skill, and an assistant that checks with ``agda``
gets the same errors but never sees a goal, so it is left guessing at
terms instead of asking.
