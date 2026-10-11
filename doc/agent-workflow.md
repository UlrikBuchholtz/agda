---
name: agda
description: Required for any edit to .agda/.lagda files. Do not run `agda FILE` to check your work; run `agda-client load FILE` instead, which gives the same errors and keeps a live session. Then work hole by hole: `goal` shows the type and context, `check` tries a term without writing it, and `case`/`refine`/`solve`/`auto` let Agda write what Agda computes. Read this before writing an Agda definition, after every type error, whenever a hole is open, and whenever you do not know what a name is or where it lives.
---

# Proving in Agda with `agda-client`

Working notes for driving an Agda proof session from a coding assistant
(or any program) through `agda-client`, the command line client for
[`agda --interaction-agent`](user-manual/tools/agent-interface.rst).

They are written to be handed to an assistant.  The front matter above
makes this file a skill as it stands: copy or symlink it to
`~/.claude/skills/agda/SKILL.md` (or wherever your tool keeps skills), or
paste the body into a proof project's agent instructions (`AGENTS.md`,
`CLAUDE.md`, ...).  If those instructions already tell the assistant to
check a file with `agda FILE`, replace that line: an always-loaded
instruction to run `agda` wins over an optional skill every time, and an
assistant that checks with `agda` never sees a goal.  Everything here is
also in the user manual, at greater length and with the protocol
underneath it.

`agda-client` keeps one type-checked Agda session alive per project, so
each command is a fast one-shot invocation.  Run it from inside the
project.  `agda-client --help` lists everything.

## Rules

1. Never check a file with `agda FILE` while editing it.  `agda-client load
   FILE` prints the same errors and keeps the session, so everything below
   is available afterwards.
2. Write the signature with `= ?` as the body, load, and only then write
   the proof: hole by hole, `check` before `give`, never a whole definition
   in one go.
3. A file that does not load has no goals.  When the error is in a term
   you wrote, replace that term by `?` and reload; the goals are back, and
   `goal N` shows what the term has to be.
4. After a second rejected attempt at the same hole, stop guessing: run
   `goal N`, then `infer` or `normalise` on the pieces, or `candidates` on
   the alternatives, before writing anything else.
5. A name you are not sure of: `find` first (the whole project), then
   `search` (by type, in scope) or `why` (what it resolves to).
6. Take goal numbers from the last answer, or address a goal as
   `LINE:COL`.

## The loop

```bash
agda-client load Sorting.agda    # type-check; prints the goals
agda-client goals                # ?0 : Sorted (insert x xs), ...
agda-client goal 0               # type + context + boundary + constraints
agda-client check 0 'insert-sorted x xs'   # does this fill the goal?
agda-client give 0 'insert-sorted x xs'    # yes: write it, reload, list new goals
```

Read the goals out of every answer: a reload **renumbers** them.  To
avoid that, address a goal by position instead of by number —
`agda-client goal 25:13` means "the goal at line 25, column 13".

## Edit the file yourself; the session follows

The file on disk is the source of truth.  Edit it with your normal
tools; the next command notices and reloads, printing
`[file changed on disk: reloaded]`.  There is no "sync" step and no
stale-state error to recover from.

## Try before you commit

`check GOAL EXPR` type-checks an expression against a goal *without*
filling it — a rejected candidate comes back with the real type error,
and the goal is still there.  `candidates GOAL EXPR...` does that for
several expressions in one call; prefer it to guessing.

## Let Agda write what Agda knows

These verbs rewrite the file and reload it, so use them instead of
typing out what Agda can compute:

- `case 0 'xs'` — split the goal's clause on `xs`; no argument splits on
  the result (copatterns).  Handles multi-line clauses and `λ { … }`.
- `refine 0 'f'` — apply `f`, leaving a goal per argument.
- `intro 0` — introduce the constructor or lambda the goal asks for.
- `solve` — write out goals the type checker already solved.
- `auto 0` — proof search (Mimer); `auto 0 -l` lists candidates instead.
- `give 0 'e'`, `elaborate 0 'e'` — fill a goal.

Nothing is written when a step fails, and the answer says exactly which
ranges were replaced.  Work on a clean git tree so a bad step is easy to
undo.

## Understanding what is around

```bash
agda-client infer 0 'f x'         # or without a goal: at the top level
agda-client normalise 0 'n + 0'
agda-client context 0
agda-client search '_≡_ ℕ'        # names whose TYPE mentions these
agda-client find insert-sorted    # names SPELLED like this, project-wide
agda-client module-contents Data.List
agda-client why 0 '_++_'          # why it is in scope, and what it can mean
agda-client definition 24:19      # where the name at that position is defined
agda-client constraints
agda-client --show-implicit goal 0
```

`search` is a search over *types*, not over names, and it sees only what
the loaded file imports.  It will not find a name by its spelling, and
it will not find anything in a module the file has not imported; `find`
is the query for either of those.

`find` is the complement: it matches the *name*, and it looks in every
module the project could write an `import` for, not just the ones the
loaded file imports.  That is what to reach for when you suspect the
library already has a lemma but do not know where.  Each hit comes with
its type, its module and its position, so the next move is to import
that module and write the name.

```bash
agda-client find is-basis in UF   # restrict to a top-level module prefix
agda-client --glob find 'is-*-for'
agda-client --kind RecName --limit 10 find basis
```

Hits are ranked: 1 the whole name, 2 a prefix of it, 3 a whole hyphen-
or underscore-separated component, 4 a substring of it, and — only with
`--type` — 5 a name its type mentions; ties break on how many indexed
definitions mention the hit, then on where it lives: the project's own
modules first, Agda's builtins last.

`find` answers from an index built beside the interface files, so it
finds nothing until `agda-client index` has built one.  The first build
costs about a minute on a thousand-module library; every one after that
re-renders only what has moved, so it costs a second or two.  Neither is
done implicitly, but every answer says what it covered: how many modules
an index would have to cover, how many are `stale` (checked, but the
index does not match — run `index` again, it is cheap), how many are
`unindexed` (never checked here — the answer there is to check the
library once) and how many are `modified` (edited since they were
checked, so the index describes them as they were, which no build
changes until Agda checks them again).  A module that has never been
checked is invisible to `find`, and so are private names, since the
index holds what the interfaces hold.

## Two errors that point somewhere else

Both of these report a problem at a place that looks fine, so they cost
a lot of time if you take the reported line at face value.

**Positional implicits count generalised binders.**  Inside an anonymous
module that generalises over universe levels, those levels come first
among the implicit arguments.  Writing `f {I} {J}` for what were meant
to be two type arguments binds two universes instead, and the error is
about a universe — `Universe is not a subtype of Set _𝓦` — somewhere
downstream.  The signature you wrote does not show the generalised
binders, so use named implicits, `f {I = I} {J = J}`, whenever the
enclosing module generalises.

**Implicit binders are not in scope in `where`.**  The implicit
arguments of a type signature cannot be referred to in the `where` block
of a clause.  Doing so is not a scope error, because the identifier
resolves to whatever else is in scope — `J` will happily resolve to the
identity eliminator — so what you get is a type error about something
you never mentioned.  `agda-client why 0 'J'` says which `J` you actually
got.

## When something hangs

Every request has a timeout (`-t SECS`, `--no-timeout` to remove it) and
`agda-client abort` interrupts the running one, and cancels any requests
queued behind it.  The session survives both.  `agda-client status`
answers at once even while Agda is busy, and then says what is running,
for how long, and which definition is being checked:

```
running:  load …/Slow.agda (12034 ms), checking Slow.test (11876 ms of CPU time)
```

`agda-client stop` shuts the daemon down and the next command starts a
fresh one.

A `load` that times out names the definition it was checking and lists
the slowest definitions so far:

```
load: timeout
timed out while checking Profile.spin, after 3798 ms of CPU time on it; …
Slowest so far (CPU time):
      3798 ms  Profile.spin
      1132 ms  Profile.slow
       104 ms  Miscellaneous
```

To measure every definition, use `agda-client load --profile FILE`
with a generous `-t`.  It checks the file from source even when it is
up to date, and appends a table of CPU time per definition.  Do not leave
the client for batch `agda --profile=…` to get this, because batch
`agda` checks nothing when the interface is current.  Look for definitions
that take tens of seconds.  The usual cures are `opaque` around a
construction whose unfolding conversion checking does not need, and
annotating implicit arguments that Agda has to solve by search (crisp
ones especially).

`agda-client` exits 1 when a request fails (an Agda error, a timeout),
but a rejected `check` is an answer and exits 0, so branch on the
`rejected` line, or on `accepted` in `--json`, not on the status.

## Reading the output

The rendered text is the default.  Add `--json` when you want the
structured answer: goal ids with ranges, the `edits` a proof step
applied, and diagnostics as `{severity, code, message, range, related}`
where `code` is Agda's own name for the error (`UnequalTypes`, …) —
match on that rather than on the English.
