The agent interaction frontend
==============================

Design notes for `agda --interaction-agent` and the `agda-client` client.
The *user*-facing description is `doc/user-manual/tools/agent-interface.rst`
(protocol reference, method table, mutation and diagnostic semantics) and
`doc/agent-workflow.md` (the same thing in the form an assistant is handed);
this file is what a maintainer wants instead: why the frontend is shaped as
it is, what it maps onto, and where a new method has to be wired in.


Why a third frontend
--------------------

Agda already emits frontend-independent `Response` values, but the complete
interactive interface is split between Agda and Emacs, and the Emacs half is
substantial:

* Agda parses `IOTCM` commands, keeps the type-checking state, and emits
  `Response` values.
* `agda2-mode.el` owns the source buffer.  It identifies goal text,
  constructs ranges, applies give/refine/case-split results to the buffer,
  schedules follow-up commands, and reloads the file.
* `EmacsTop` serialises responses as Elisp forms which the mode evaluates.

A program that is not an editor therefore cannot use `--interaction` without
reimplementing `agda2-mode.el`'s edit logic, and cannot use
`--interaction-json` without prompt sniffing and response aggregation, since
neither frontend answers a request with exactly one terminal line.

The consumer this was built for is a coding assistant, which works in
discrete tool calls rather than in a long-lived REPL conversation, and which
already has good file editing and search tools.  What it does not have is a
way to see the proof state, and a way to apply the transformations whose edit
logic lives in Elisp cursor scans.  Three consequences ran through the whole
design:

* **One request, one terminal line.**  Every request carries an `id` and is
  answered by exactly one `result` or `error` line, so no output has to be
  parsed out of a stream.
* **The file on disk is the source of truth.**  The client edits the file
  with its own tools; a query notices the change and reloads, reporting
  `resynced`, rather than failing with a stale-state error the caller has to
  learn to recover from.  Conversely a mutating method rewrites the file
  itself and reloads, so the goal identifiers in its answer are the ones of
  the new revision.
* **Display options are per request, not session state.**  `showImplicit`
  and `showIrrelevant` are set for one request and restored, so results are
  not order-dependent.  This is a deliberate divergence from the Emacs
  mode's toggles.


Layers
------

```text
┌────────────────────────────────────────────────────────┐
│ agent instructions / skill        (workflow knowledge) │
│   doc/agent-workflow.md                                │
├────────────────────────────────────────────────────────┤
│ agda-client  (CLI client, an executable in Agda.cabal)  │
│   - discovers/starts one daemon per project root       │
│   - one subcommand per protocol method                 │
│   - renders text by default, --json for structure      │
├────────────────────────────────────────────────────────┤
│ agda --interaction-agent   (JSON-lines over stdio)     │
│   Agda.Interaction.Agent.Protocol   (wire format)      │
│   Agda.Interaction.Agent.Top        (request loop)     │
│   Agda.Interaction.Agent.Edit       (edit planner)     │
│   Agda.Interaction.Agent.Diagnostic (structured errors)│
└────────────────────────────────────────────────────────┘
```

The daemon is the frontend itself, run by the client behind a Unix socket.
The client owns the socket and spawns `agda --interaction-agent` on a pipe;
the Agda process stays a plain stdio program, so it remains directly drivable
by the test suite and by clients that are not `agda-client`.  One daemon per
project root, found through a socket file whose name hashes the root *and*
the identity of the Agda binary, so a rebuilt Agda gets a fresh daemon rather
than a version mismatch.

`agda-client` deliberately does not depend on the Agda library: it knows the
request/response envelope and passes payloads through, which keeps it cheap
to build and keeps the protocol honest about what a third-party client needs
to know.


Method → existing machinery
---------------------------

Almost nothing here is new type checking; the frontend is a different way of
addressing `Agda.Interaction.InteractionTop`.

| Method            | Backed by                                   | Notes |
|-------------------|---------------------------------------------|-------|
| load / reload     | `Cmd_load` + `Info_AllGoalsWarnings`        | returns revision + goals; `profile` adds `--profile=…` for one load and removes the main interface first |
| goals             | `Cmd_metas`                                 | |
| goal              | `Cmd_goal_type_context`                     | rendered by `EmacsTop.prettyGoalType`, shared with the Emacs frontend |
| context           | `Cmd_context`                               | full `ResponseContextEntry`: let values, modalities, in-scope flag |
| check             | `Cmd_goal_type_context_check`               | non-mutating: the goal survives acceptance and rejection alike |
| checkCandidates   | ditto, once per candidate                   | each check runs in a state of its own |
| infer             | `Cmd_infer` / `Cmd_infer_toplevel`          | |
| normalise         | `Cmd_compute` / `Cmd_compute_toplevel`      | |
| give              | `Cmd_give` + `Resp_GiveAction` → `Agent.Edit` | |
| elaborate         | `Cmd_elaborate_give`                        | |
| refine / intro    | `Cmd_refine_or_intro` / `Cmd_intro`         | |
| case              | `makeCase`, **plus the clause range**       | see below |
| solve             | `Cmd_solveOne` / `Cmd_solveAll`             | each solution is handed back to `Cmd_give` so it is parenthesised for its hole, as the Emacs mode does |
| auto              | `Cmd_autoOne` / `Cmd_autoAll` (Mimer)       | list-solutions mode is a result, not a failure |
| helperType        | `Cmd_helper_function`                       | |
| search            | `Cmd_search_about_toplevel`                 | searches *types*, in scope |
| findNames         | `Agda.Interaction.NameIndex`                | searches *names*, project-wide; see `notes/name-index.md` |
| buildIndex        | `agda --build-name-index` in a subprocess   | relayed as progress events |
| moduleContents    | `Cmd_show_module_contents[_toplevel]`       | |
| whyInScope        | `Cmd_why_in_scope[_toplevel]`               | |
| definitionAt      | `iHighlighting` of the visited module       | a targeted query, never the full highlighting stream |
| constraints       | `Cmd_constraints`                           | |
| abort / close     | the abort queue                             | plus a per-request `timeoutMs` |

Two things had to change outside the new modules:

* **`makeCase` now returns the range of the clause it split.**  The Emacs
  mode finds that text by scanning the buffer; a frontend that owns the file
  cannot, and the range is right there in the `A.SpineClause`.
  `Resp_MakeCase` carries it, and all three frontends were updated
  (`EmacsTop` ignores it, `JSONTop` reports it).
* **Four renderers moved into `EmacsTop`** — `prettyGoalType`,
  `prettyNamedType`, `prettyModuleContents`, `prettySearchAbout` — so that
  the agent frontend renders a goal, a module's contents and a type search
  exactly as `C-c C-,` and friends do, rather than growing a second layout.

Deliberately left out: session-level show/hide implicit toggles (see above),
compilation and backend holes, token highlighting.


Transactions
------------

`Agent.Edit` is where the frontend owning the file is paid for.  A mutation is
a small transaction:

1. snapshot the file *before* running the command;
2. plan one `TextEdit` per goal from the hole's range and the text Agda
   computed, checking that the range really does hold a hole — this is the
   expected-old-text check, and it is what turns a stale range into a clean
   error rather than a mangled file;
3. reject overlapping edits;
4. check the file has not changed while the command ran; if it has, write
   nothing and report `staleDocument` with the edits that were planned;
5. write atomically (sibling file, then rename), then reload.

Positions are Agda's: code-point offsets into the file *after* line endings
are normalised and a byte order mark stripped.  A snapshot keeps the raw text
beside the normalised one and translates between them, so editing one hole in
a CRLF file does not rewrite every line of it.


Touchpoints when a method is added
----------------------------------

Grep-able checklist, in the order a new method touches the tree:

1. `Agent/Protocol.hs`: `agentMethods`, and the `capabilities` and
   `defaults` blocks of `handshakeValue`.  Bump `agentProtocolVersion` only
   when the meaning of an existing field changes; adding a field or a method
   does not need one.
2. `Agent/Top.hs`: a case in `dispatch`.  If it mutates, `giveLike` or
   `mutationResult` does the transaction for you — do not write to the file
   anywhere else.
3. `src/agda-client/Main.hs`: a case in `buildRequest` (with its default
   timeout) and a line in `usage`, under "Queries" or under "Proof steps".
4. `test/Agent/`: a `.jsonl` script and its golden; `make agent-test`, with
   `AGDA_TESTS_OPTIONS=--accept` to regenerate.  A mutating method wants a
   `!source` after it and an `!unchanged` after the failure cases.
5. Docs: the verb table and the "Mutations" and "Diagnostics" sections of
   `doc/user-manual/tools/agent-interface.rst`, and the corresponding
   sentence in `CHANGELOG.md`.

The golden transcripts print a position as `LINE:COL@OFFSET` and a range as
`FILE START-END` rather than as nine lines of JSON apiece.  That is the
difference between evidence one can read in a diff and evidence one cannot;
keep it that way when adding to `test/Agent/Tests.hs`.


Things that bit, and are worth not re-learning
----------------------------------------------

* **The daemon must serve connections concurrently.**  `abort` is precisely
  the request that has to be *read* while another one is in flight, so the
  accept loop forks per connection; requests are routed by id and writes to
  Agda are serialised by a lock.  A request count keeps the idle timeout from
  walking out on a running command.
* **A forked thread cannot `exitWith`.**  When the Agda process goes away the
  router fills an MVar the main thread waits on; calling `exitWith` there
  killed the thread only, leaving a daemon that owned the socket and answered
  "the Agda process exited" for ever.
* **Unix domain sockets are not everywhere.**  `network` does not offer them
  on Windows.  The client checks `isUnixDomainSocketAvailable` and says so,
  rather than spawning a daemon, waiting a minute and blaming it.
* **The socket directory must be the user's.**  Its name is a hash of public
  information, so on a shared `/tmp` anybody who got there first could answer
  in the daemon's stead.  It lives under `$XDG_RUNTIME_DIR`, or the user's
  cache directory, `setFileMode`d to 0700 either way — `setPermissions` will
  not do, it leaves the group and other bits as it found them.
* **A current interface is reused for the main module too.**  So a
  load with `--profile` checks nothing and reports an empty profile
  unless the file changed.  A `profile` load removes the main module's
  interface file first, which is what touching the source would do;
  imports are left alone.
* **A timeout cannot ask afterwards where it was.**  The abort unwinds
  every `Bench.billTo` on the way out, so once the command is gone the
  benchmark's current account is the root again.  The timer thread takes
  a snapshot (`timingsSnapshot`) *before* it sets the abort flag, and
  that snapshot is what the error reports.  Definitions are billed
  even without profiling because `updateBenchmarkingStatus` turns on
  `BenchmarkSome isDefAccount` under `--interaction-agent`.  That costs
  two clock readings per definition, and `Bench.print` still goes by the
  profile options alone.
* **`-W` matters when building the manual.**  The default highlight language
  is Agda, so a shell transcript written as a bare `::` block fails to lex,
  which is a warning, which is an error.  Shell transcripts in
  `agent-interface.rst` are `.. code-block:: console`.  With Sphinx installed
  system-wide, `sphinx-build -M html . _build -E -W --keep-going` from
  `doc/user-manual` is the check CI runs; `make -C doc/user-manual html`
  wants a virtualenv and `pip install`.
