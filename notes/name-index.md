A project-wide name index
=========================

Status
------

Implemented.  This is the design as built, with the record of what
building it corrected; see `notes/agent-interface.md` for the frontend
that `findNames` belongs to, and the user manual
(`doc/user-manual/tools/agent-interface.rst`, "Searching names across
the project") for the published format and the ranking.

The numbers under "Cost" are measurements against TypeTopology (~1000
modules, ~289K lines), not estimates.  Four things the design got wrong
and the implementation corrected, all of them worth knowing before
changing anything here:

* the *type* is rendered in the defining module's own scope, not fully
  qualified -- fully qualified it is unreadable, and 37 MB rather than
  4;
* the index is 28 MB, not 3, because both renderings of every type are
  stored;
* a module the builder cannot read is `unindexable`, not stale: no
  rebuild fixes it, and calling it stale makes the up-to-date answer
  unreachable for a project that shadows a builtin;
* the index tracks *interfaces*, so a source edited and not re-checked
  is a state of its own -- `modified` -- which no build fixes either.

The last two are changes to the file format, which is at version 3.

What it has to answer
---------------------

One question: *does this project already define something called
roughly this, and if so, what is its type and where is it?*  Not "what
fits this goal" -- that is Search About, which stays as it is.

The feature request this answers came out of a proof session driven
entirely through `agda-client` against TypeTopology: `search` returns the
names *in scope* whose *type* mentions every term, and both restrictions
bite in the one situation that comes up most, which is not "what can I
apply to this goal?" but "does this library already have the thing I am
about to define, and what is it called?".  Whatever the file already
imports is by construction the part of the library one already knows
about; what one needs to discover is in the modules one has not
imported, and those are invisible to Search About.

Concretely: match on the name with substring semantics, over every module the project could import rather than the
current scope, returning per hit the name, its kind, its type, its
defining module and its source position, ranked, filterable by module
prefix.

The type is not decoration.  Without it this is grep with better
tokenisation, and the session that prompted the feature request already
had grep.  The type is what settles `is-basis` against `is-basis-for`
without a round trip, and what lets the caller write the application
rather than discover its argument shape by trial.  So the design pays
for types, and the rest of it is arranged around that one cost.

What the index holds
--------------------

Per module:

    params       the enclosing module telescope, rendered once

Per public definition:

    name         the concrete name, as the defining module exports it
    kind         KindOfName: function, constructor, record field, ...
    module       the top-level module it is defined in
    position     file, line, column of the defining occurrence
    type         rendered against `params`, twice: implicits hidden
                 and shown
    mentions     the QNames occurring in the type
    uses         how many indexed definitions mention it

The hit itself is reported fully: its module, its qualifier and its
name, since it is by definition not in the caller's scope and the
caller's next move is to import the module and write the name.

The *type*, though, is rendered in the defining module's own scope --
`𝓤 ⊔ 𝓣 ⊔ (𝓥 ⁺) ̇` rather than
`(𝓤 Agda.Primitive.⊔ 𝓣) ... MLTT.Universes.̇`.  An earlier draft of this
design said fully qualified there too, on the same grounds; measuring it
showed that the same argument does not carry across, because what the
fully qualified rendering of a type buys -- knowing which module each
name in it comes from -- is what `mentions` already carries exactly, and
what it costs is that the type stops looking like the source it came
from.  `mentions` is the part that has to be fully qualified, and it
is.

Twice, because the two renderings answer different questions and the
index is built once.  The hidden form is what a list of twenty hits
should show; the explicit form is what you need before writing the
application, and it is exactly the form that would have prevented the
second finding of that same session -- positional implicits silently
binding the universe levels an enclosing anonymous module generalised.
The signature as written does not show those binders.  A rendering with
implicits shown does.  It roughly doubles the type text, which the split
below makes affordable.  Both being in the file also means a client can
offer either one without asking again, which is what makes the switch a
key rather than a query -- see "Two frontends".

The module telescope is stored once per module rather than repeated into
each type, and this is not a micro-optimisation.  TypeTopology
parameterises heavily, and asking the running frontend to render the
public names of its twenty densest modules produces types averaging
1317 characters, because every one of them carries the whole enclosing
telescope.  `Definitions.tsv` makes the same split -- its column 6 is
the type, averaging 49 characters, its column 7 the module parameters,
averaging 64 -- and that is the difference between an index of about
three megabytes and one of twenty-eight.  It also removes most of the
repeated rendering work, which is the dominant term in the build.

Entries are keyed on the defining `QName`, not on what a module's scope
exports.  A module's scope also carries the names it re-exports from
what it opens, and counting those inflates a dense module several times
over -- rendering the scope of `MGS.SIP` yields 945 names where 220 are
defined there.  Filtering to names whose `QName` module is the module
being indexed is what makes the count come out right.

`mentions` is Search About's own predicate -- "the type mentions all of
these" -- as a set of `QName`s, computed by a generic traversal rather
than by parsing the rendering.  It is worth carrying for two reasons: it
makes that query project-wide rather than scope-bounded, and one pass
over the finished index turns it into `uses`, the tiebreaker and
popularity signal that `Definitions.tsv` obtained by scraping HTML
reference anchors.

No source excerpt.  It was in an earlier draft of this design as a
stand-in for the type; with a real type it earns nothing that
`position` plus the existing `definition` verb does not.

What it spans
-------------

The project's own include paths plus the transitive `depend:` closure of
its `.agda-lib` -- that is, exactly what the project can write an
`import` for.

This needs no decision and no flag, because Agda has already computed
it.  `setLibraryPaths` calls `addDefaultLibraries`, which reads the
project's `.agda-lib`, and then `setLibraryIncludes`, which resolves the
named libraries through `libraryIncludePaths`; that walks a work list
adding each library's `_libDepends` until it closes, and unions their
`_libIncludes`.  The result is `optIncludePaths`, which becomes
`optAbsoluteIncludePaths` -- which every frontend already reads.

So the index spans the include path, full stop, and the definition of
"reachable" stays Agda's rather than becoming ours.

Where the data comes from
-------------------------

Everything except the rendering is in the `.agdai` file of the defining
module.

* `iScope` is `publicModules (iInsideScope i)`, computed by
  `constructIScope` on read, and `writeInterface` has already stripped
  private declarations with `withoutPrivates`.  So the interface's scope
  is exactly the public surface of the module: the right thing to index,
  and no filtering of our own.  Walking `nsNames` of its name spaces
  gives `Map C.Name (List1 AbstractName)`, and each `AbstractName` gives
  `anameName :: QName` and `anameKind :: KindOfName`.

* The position is `nameBindingSite` of that `QName`.  Ranges survive
  serialisation: `buildInterface` deliberately stopped killing binding
  sites, and `encode` replaces file paths in positions with module names
  for relocatability, so `decode` hands back a range with a real path
  resolved against the include path in force.

* The type is `defType` of the `Definition` that `_sigDefinitions` of
  `iSignature` maps the `QName` to.  Every public name has one:
  `eliminateDeadCode` roots its reachability at `publicModules scope`,
  so nothing exported is eliminated.

* Pattern synonyms are not in the signature; they are in `iPatternSyns`
  and have to be indexed from there, with kind `PatternSynName` and no
  type.

Why rendering forces the shape of the build
-------------------------------------------

`iSignature` holds only the module's own definitions.  A type in it
refers to `QName`s from imported modules that are not in that signature,
and reification looks definitions up -- for record projections, display
forms, argument hiding, not just for names.  Printing a module's types
against nothing but its own interface will, sooner or later, ask for a
definition that is not there.

So the builder cannot stream one interface at a time and discard it.  It
has to walk the include path in dependency order with the signature
accumulating, which is what Agda does anyway when it imports.  Four
consequences, and they are the whole reason this design looks the way it
does.

**It is a load, not a re-check.**  `getInterface` reads the `.agdai`
when it is current.  For a library that has been checked -- which is the
premise of the whole feature -- building the index costs the
decode-everything pass plus reification of every public type, not a
re-type-check.  Measured on TypeTopology, that load is 7.7 seconds --
not the hours that "check the whole library" suggests.

**It cannot happen inside an interactive session.**  That same load
peaks at 1.14 GB of residency and 2 GB in use, and an interactive
session's job -- the agent daemon's, agda2-mode's -- is to keep one
file's proof state responsive.  The builder is therefore a separate
process -- a new Agda mode, `agda --build-name-index`, which resolves
the include path, imports the closure, renders, writes the index file
and exits.  A session reads the finished file and never pays the
memory.  Making it a mode rather than an internal step also makes it
scriptable: a library can build its index in CI and ship it beside its
interfaces.

**A module that will not load has to leave no trace.**  A module that
checks with unsolved metas gets its definitions into the signature and
writes no interface, so Agda will not reuse the check: the next module
that imports it has it checked a second time.  In the one state the
builder checks everything in, that second check puts the same
definitions into the signature while they are still among the imports,
which is `Ambiguous name` -- an internal error, and the whole build lost
for one module.  So a module like that is rendered and then rolled back
out of the state again, which leaves the run as if it had not been
indexed yet: whoever imports it checks it once, fails on its warnings
exactly as under plain `agda`, and is recorded as unreadable like any
other module that will not load.  The builder also catches an
`Impossible` around each module, since an internal error is a Haskell
exception rather than a `TCErr` and would otherwise take the process
down -- and a project with one module Agda cannot make sense of is
precisely a project whose index has to keep working.

**Staleness is per module, refresh is per batch.**  Re-rendering one
changed module still needs its imports loaded, so an incremental refresh
is the same builder restricted to the stale set and its dependencies.
For a leaf module that is cheap; for a base module everything downstream
is stale anyway.  Either way it is one process invocation, not one per
module.

Deciding what is stale, without decoding
----------------------------------------

An interface file starts with an uncompressed prefix of
`ifacePrefixSize` = `2 * hashSize + ifaceVersionSize` = 24 bytes,
holding `(iSourceHash, iFullHash, currentInterfaceVersion)`, ahead of
the zlib-compressed body.  `deserializeInterface` already splits it off
-- and, today, throws the two hashes away.

So the staleness question is answered by reading 24 bytes per module.
Whatever answers a query stats the interface tree, reads each prefix,
compares `iFullHash` against what the index recorded, and knows exactly
which modules moved and whether it is worth invoking the builder at
all.  For a thousand-module library that is a few milliseconds before
any decompression happens.

`iFullHash` rather than `iSourceHash`: it is
`combineHashes (iSourceHash i : map snd (iImportedModules i))`, so it
changes when a dependency changes -- and a module's *rendered* types can
change when an import does, which is precisely what this index caches.
It costs nothing extra, since Agda has already rewritten that `.agdai`
for the same reason.

This wants one small addition to `Agda.TypeChecking.Serialise`, a
`readInterfacePrefix :: AbsolutePath -> IO (Maybe InterfacePrefix)` that
reads 24 bytes and deserialises them, factored out of what
`deserializeInterface` already does.

Enumerating from sources, not from the `_build` tree, keeps the coverage
question answerable: a source file with no interface, or one of a
foreign interface version, is a module the index cannot see, and the
query answer should say how many of those there are rather than
returning fewer hits in silence.  `toIFile` is the mapping to use --
Agda's own, so it handles both layouts, `_build/<version>/agda/` under a
project root and an `.agdai` beside the source without one.

The query
---------

    find PATTERN [in MODULE-PREFIX]

Matching, in the order results are ranked:

1. exact match on the whole name
2. prefix of the name
3. a whole hyphen- or underscore-separated component of the name
4. a substring anywhere in the name
5. a match against `mentions`, when `--type` is given

Substring is the default and globbing is opt-in behind `--glob`, which
disposes of the escaping problem: Agda names
contain `*` (`ℤ*-assoc`), and requiring an escape for the common case in
order to serve the rare one is the wrong trade.  With `--glob`, `*` and
`?` mean what they usually do and a literal `*` is escaped.

`in MODULE-PREFIX` filters on the top-level module name.  Against a
built index this is just a filter; its real work is on the *staleness*
scan, where it means a scoped query need not care that a distant part of
the library has moved.

Ties inside a rank break on `uses`, then on where the hit lives -- the
project's own include directories before its libraries', and Agda's
builtins last, which is the order the header's `includePaths` records --
so that the project's own `cong` is not listed after a cubical builtin's
helper of the same name.  Ranking must be stable and the
answer must carry the rank, so that a caller can tell an exact hit from
the fortieth substring match without re-deriving the ordering.

Protocol
--------

A new method, `findNames`, alongside `search` rather than replacing it,
and a capability flag `nameIndex`.  Adding a method needs no protocol
version bump -- `agentProtocolVersion` covers changes in meaning, not
additions.

Request: `pattern`, optional `module` prefix, `glob`, `type`, `kinds`
filter, `limit`, and `showImplicit` to choose between the two stored
renderings (the request envelope already carries that flag for other
methods).

Result: `hits`, each `{name, kind, module, file, line, column, type,
rank, uses}`; and an `index` object -- `{modules, definitions, stale,
unindexed, builtAt}` -- so the caller knows what the answer covered.
`stale` is the count of modules whose interface has moved since the
index was built; `unindexed` the count of source files with no usable
interface at all.  A query answered over a library that has never been
checked must say so rather than look like a library with nothing in it.

A second method, `buildIndex`, spawns the builder and streams its
progress through the existing progress events.  It is a method rather
than a side effect of `findNames` because it is the one operation here
that costs tens of seconds, and an agent should be the one deciding to
spend them.

The `notes.search` and `notes.findNames` strings in the handshake should
cross-reference each other: one searches types in scope, the other names
across everything importable.

Two frontends
-------------

The same index answers for Emacs, and it should, because the split that
makes this feature possible is not an agent-shaped split.  A name query
needs no Agda process at all: the builder renders every type to text,
and what a query does with that is match a string, rank, and print.
Nothing at query time reifies, resolves a scope, or consults a
signature.  So the index file is the product, and the agent daemon,
`agda-client` and an Emacs command are three peer clients of it, none of
them privileged.

The precedent is TypeTopologySearch, which is where the feature request
came from in the first place: `M-x typetopology-search`, bound to
`C-c C-g`, narrows a library-wide index as you type, in its own window,
and acts on the selection -- insert the name at point, jump to the
definition, insert `open import` for its module.  It reads
`Definitions.tsv` off the filesystem and never talks to Agda.  We can
search the same ground from interfaces rather than from scraped HTML,
which is the argument of "Rejected" below, and we can show a real type,
which it cannot.

Its three actions are the right three, and the Emacs client takes them
as they are: insert the name, jump to the definition, insert the `open
import`.  What it adds is that the choice is made three ways -- a
default, a prefix argument that asks, and, while the candidates are
showing, `M-.` and `M-RET`, which take the candidate RET would have
taken and jump to it, or import its module, instead of doing the
default with it.  The keys are the part worth having, because which
action one wants is a property of the hit rather than of the session:
the name one was about to write, the definition one wants to read
first, the module one has to import before either.  A default that has
to be changed in advance, or a prefix argument that asks every time,
both put that decision in the wrong place.

Two things came out of putting those keys in the minibuffer.  An
action that edits has to be told which buffer to edit: the candidate
is read in the minibuffer, so an `insert` at that point inserts into
the minibuffer and the file never changes.  Naming the buffer the
search started from makes that unreachable rather than merely
unlikely; jumping is the one action that does not want it, visiting a
file of its own.  And the toggle between the two renderings of a type
earns a key only because both are in the file: it redisplays
candidates that are already on screen rather than asking again.  That
is the second thing storing both renderings buys, after the one the
design argued for, and it is the cheaper of the two.

The `open import` is the action that has to say what it did.  It
inserts where an import has to go -- after the last one, or after the
module header -- which is usually not where point is, so an answer
that echoed only the line it had written was indistinguishable from
one that had written nothing.  It names the line and the buffer
instead.  This is a general shape for this client rather than one
action's detail: a search that acts on a file it is not showing has to
report the edit, since the user is looking at the candidate list.

Not through the interaction protocol, though.  A `Cmd_find_names`
reaching `EmacsTop` would share the matching code and inherit position
jumping from machinery that already exists, and it is still the wrong
shape.  agda2-mode serialises its commands, so such a search waits
behind whatever is being checked -- and looking for a name one has not
got is what one does *while* something is checking.  It would need a
loaded file, since the include path comes from the session.  And it
would pay for a round trip that buys nothing, the query having no
question to ask the type checker.  A one-shot `Cmd_find_names`
rendering into `*Agda Information*` stays available later if that
result buffer turns out to be wanted for its own sake; it is not the
interesting interface.

Three consequences.

**The format is public.**  It is what a client written in another
language reads, so it is documented in the user manual, its version is
a compatibility promise, and every field is interpretable without Agda:
a file, a line and a column rather than a range, a type as text, a
module as a string.  It is a commitment rather than an implementation
detail.  The same goes for
the ranking, which becomes specification rather than whatever the
Haskell happens to do, because a second implementation now has to agree
with it.  That second implementation is small -- substring matching and
five ranks over a name string, no Agda knowledge anywhere in it -- and
one fixture of queries and expected answers holds the two together
better than prose can.

**The client asks where the index is rather than deriving it.**
`nameIndexFile` walks up to the `.agda-lib` and appends
`_build/<version>/agda`; a client re-deriving that is a copy that goes
wrong the first time the layout moves.  Instead a status mode prints the
path along with the coverage report, and the client shells out to it
once per project and reads the file itself thereafter.  This is what
keeps the duplication bounded to matching: no path logic, no staleness
heuristic, no format archaeology.  The status mode is worth having
regardless -- CI wants the same answer, and the coverage scan already
computes it.

**Staleness is answered, not guessed.**  TypeTopologySearch compares
modification times, because a `.tsv` scraped out of HTML has nothing
better to compare.  We have the 24-byte prefixes, and the scan takes
about 160 ms on the largest library to hand: too slow to block a
keystroke, fast enough to run once when the search opens and update a
banner over results that are already on screen.

The naming follows.  The module is `Agda.Interaction.NameIndex` rather
than `Agda.Interaction.Agent.Index`, the file it writes is
`name-index.jsonl`, the locator is `nameIndexFile`,
and the modes are `--build-name-index` and `--name-index-status`: none
of that belongs to one frontend.  The agent protocol exposes the
`findNames` method and the `nameIndex` capability, while `agda-client`
exposes the `find` and `index` verbs.  Their integration tests live in
`test/Agent`.

Lifetime
--------

The index file lives under `_build/<version>/agda/` beside the
interfaces it is derived from, so that whatever invalidates those
invalidates it, it is ignored by the same rules, and it never appears in
the source tree.  It records its own format version, the include paths
it was built under, and per module the `iFullHash` it was built from.

A cold build is never implicit.  The first query against a project with
no index answers with `index.builtAt: null` and says how to build one --
`agda --build-name-index`, which each frontend wraps in a verb of its
own.  Tens of seconds is short enough to be worth spending and long
enough that a query should not spend it on its own initiative; an agent
that has learned a verb sometimes stalls stops using it, and so does a
person who has learned a key.

Staleness is different, because the scan that detects it costs 7.9 ms.
Every query pays that, and reports `stale`.  Whether a small stale set
should be refreshed implicitly is the one thing here I would rather
decide with the builder in hand than in advance: the refresh is the same
seconds-scale process, so the answer turns on how often a real session
knocks a handful of modules stale, which is a thing to observe rather
than predict.

`agda-client index` is a real verb either way, as TypeTopologySearch has
one -- with `--force` to rebuild from scratch, and `C-c C-u` for the
same thing from inside the Emacs search.

Cost
----

Measured, on the development that prompted the feature request
(TypeTopology, `_build/2.9.0`, this checkout's binary, warm cache):

    993 interface files                 122 MB
    994 source files (.lagda)           10.5 MB
    21,466 public definitions           across 894 modules

    staleness scan (993 prefixes)       7.9 ms
    load the whole library              7.7 s
      of which deserialisation          7.5 s
      max residency                     1.14 GB  (2051 MiB in use)
    render a public type                0.7 - 4.4 ms

So a cold build is the load plus the rendering: seconds, not the hours
that "check the whole library once" suggests, because for a library that
has been checked it is a load and `--profile=internal` bills essentially
all of it to `Deserialization`.  Rendering is the term with the spread.
A random sample of 25 modules renders at 0.7 ms per name; the twenty
densest render at 4.4 ms, and the difference is almost entirely the
enclosing telescope being reified into every name.  Against 21,466
definitions that is between fifteen seconds and a minute and a half, and
storing the telescope once per module moves it towards the bottom of
that range.  Call the cold build under a minute and the incremental one
proportional to what moved.

Two of these numbers settle design questions rather than merely sizing
them.  The 7.9 ms scan means a query can check staleness every time
without thinking about it.  The 1.14 GB residency means it still
must not do the build in process.

The definition count is not mine: it is `Definitions.tsv`, the index
TypeTopologySearch produces by scraping `agda --html`.  Comparing it
module by module against what the frontend renders from the interfaces
is the check that this design produces the same answers as the tool it
replaces -- `Various.Dedekind` 225 against 226, `UF.Base` 120 against
120, `UF.Equiv` 113 against 113, `NotionsOfDecidability.QuasiDecidable`
152 against 152.  Where the two disagree it is re-export inflation on
the interface side, which is the observation that entries must be keyed
on the defining `QName`.

These figures predate the builder: they come from driving
`moduleContents` over a loaded session, which reifies and pretty-prints
exactly what the builder does, and they do not include writing the index
out or the traversal that computes `mentions`, both of which are cheap
beside reification.  The built article costs about a minute cold on
TypeTopology and a second or two to refresh after an ordinary edit.

Touchpoints
-----------

    Agda/TypeChecking/Serialise.hs       readInterfacePrefix
    Agda/Interaction/FindFile.hs         source enumeration helper
    Agda/Interaction/NameIndex.hs        new: format, build, query
    Agda/Interaction/Agent/Protocol.hs   findNames, buildIndex, notes,
                                         nameIndex capability
    Agda/Interaction/Agent/Top.hs        the two method handlers
    Agda/Interaction/Options/Base.hs     --build-name-index,
                                         --name-index-status
    Agda/Main.hs                         the new modes
    src/agda-client/Main.hs              find and index verbs, help
    src/data/emacs-mode/agda2-name-index.el   new: the Emacs client
    src/data/emacs-mode/agda2-mode.el    require, C-c C-g
    Agda.cabal                           the new data file
    doc/user-manual/tools/agent-interface.rst
    doc/user-manual/tools/emacs-mode.rst
    doc/agent-workflow.md

Adding methods needs no protocol version bump; the `nameIndex`
capability is how a client discovers them.

Testing
-------

The agent suite drives `agda-client` over a `.jsonl` script and compares
a rendered transcript, so the index needs a fixture library rather than
a fixture file: a handful of modules under `test/Agent/`, with their own
`.agda-lib`, checked once by the test setup so that interfaces exist.
That fixture is what makes the interesting cases reachable -- a hit in a
module the loaded file does not import, which is the whole point; a
module with no interface, which must show up as `unindexed` rather than
as silence; a stale module, which must show up as `stale`; and a name
re-exported by a second module, which must be found once, under the
module that defines it.

Index building is too slow for a golden test to run it on every
invocation, so the fixture's index should be built once by the same
Makefile-driven setup that checks it, and registered in
`makefileDependentTests`.

What this does not do
---------------------

It does not see private names, because the interfaces do not carry
them.  For "what can I use from this library" that is correct.

It does not see a module that has never been checked.  `unindexed` is
how the caller finds out, and the answer for a library in that state is
to check it once.

It does not help with the file currently being edited, which is exactly
the file that does not type-check.  That file's names are in the
session's own scope already, so `search`, `moduleContents` and
`definition` cover it.

Rejected
--------

*Scraping `agda --html`*, as TypeTopologySearch's `agda-index.py` does.
Cheap, and the source of the `Definitions.tsv` that stood in for this
feature, but it needs a full HTML rendering of a library that
type-checks without holes -- and during development the interesting
library is the one that does not.  Interfaces stay valid for every
module except the one being edited.

*Routing the Emacs query through the interaction protocol.*  It would
share the matching code and reuse the existing jump-to-position
machinery, and it would make a search wait behind the type checker at
precisely the moment one wants to search.  See "Two frontends".

*Indexing only what the session has loaded.*  Incremental and never
stale, and it answers questions about the part of the library the agent
already knows, which is not the part it needs to search.

*Skipping the rendering to keep the build streamable.*  This was the
previous draft: index names, kinds, positions and mentioned `QName`s,
show the definition's source line in place of a type, and render
properly only for hits already in scope.  It builds in bounded memory
one interface at a time, which mattered while the build's cost was
unknown.  Now that it is known -- a load of seconds and a peak of about
a gigabyte, in a process that exits -- the memory it saves is not worth
what it gives up: a source line is a first line and Agda signatures are
not, and an agent that has to open the file to learn the argument shape
is back to doing the work outside the protocol.

Open questions
--------------

(Settled by the implementation: `mentions` holds fully qualified
`QName`s rendered to text, so the type query is a string match and no
scope resolution is needed at query time.  It is wrong across shadowing,
which is the price, and the qualifier being present is what lets a term
like `UF.` select by module.)

Whether `uses` should count occurrences in bodies as well as in types.
Bodies are where the library's actual usage lives, and the closure is
loaded anyway, so it is one more traversal over data already in memory
-- but it makes the count a measure of internal reuse rather than of
what the library exposes, and those rank differently.

Whether `find` should fall back to Search About when a name query
returns nothing.  A verb that silently changes what it is searching is
hard to reason about; a cross-reference in the empty answer is probably
enough.
