# Introduction

This document describes a set of standards for all code under the Plutus Use Cases
project. It also explains our reasoning for these choices, and acts as a living
document of our practices for current and future contributors to the project. We
intend for this document to evolve as our needs change, as well as act as a
single point of truth for standards.

# Motivation

The desired outcomes from the prescriptions in this document are as follows.

## Increase consistency

Inconsistency is worse than _any_ standard, as it requires us to track a large
amount of case-specific information. Software development is already a difficult
task due to the inherent complexities of the problems we seek to solve, as well
as the inherent complexities foisted upon us by _decades_ of bad historical
choices we have no control over. For newcomers to a project and old hands alike,
increased inconsistency translates to developmental friction, resulting in
wasted time, frustration and ultimately, worse outcomes for the code in
question.

To avoid putting ourselves into this boat, both currently and in the future, we
must strive to be _automatically consistent_. Similar things should look
similar; different things should look different; as much as possible, we must
pick some rules _and stick to them_; and this has to be clear, explicit and
well-motivated. This will ultimately benefit us, in both the short and the long
term. The standards described here, as well as this document itself, is written
with this foremost in mind.

## Limit non-local information

There is a limited amount of space in a developer's skull; we all have bad days,
and we forget things or make decisions that, perhaps, may not be ideal at the
time. Therefore, limiting cognitive load is good for us, as it reduces the
amount of trouble we can inflict due to said skull limitations. One of the worst
contributors to cognitive load (after inconsistency) is _non-local information_
- the requirement to have some understanding beyond the scope of the current
unit of work. That unit of work can be a data type, a module, or even a whole
project; in all cases, the more non-local information we require ourselves to
hold in our minds, the less space that leaves for actually doing the task at
hand, and the more errors we will introduce as a consequence.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided; as much locality as possible must be
present everywhere; needless duplication of effort or result must be avoided.
Thus, our work must be broken down into discrete, minimal, logical units, which
can be analyzed, worked on, reviewed and tested in as much isolation as
possible. This also applies to our external dependencies.

Thus, many of the decisions described here are oriented around limiting the
amount of non-local knowledge required at all levels of the codebase.
Additionally, we aim to avoid doing things 'just because we can' in a way that
would be difficult for other Haskellers to follow, regardless of skill level.

## Minimize impact of legacy

Haskell is a language that is older than some of the people currently writing
it; parts of its ecosystem are not exempt from it. With age comes legacy, and
much of it is based on historical decisions which we now know to be problematic
or wrong. We can't avoid our history, but we can minimize its impact on our
current work. 

Thus, we aim to codify good practices in this document _as seen today_. We also
try to avoid obvious 'sharp edges' by proscribing them away in a principled,
consistent and justifiable manner. 

## Automate away drudgery

As developers, we should use our tools to make ourselves as productive as
possible. There is no reason for us to do a task if a machine could do it for
us, especially when this task is something boring or repetitive. We love Haskell
as a language not least of all for its capability to abstract, to describe, and
to make fun what other languages make dull or impossible; likewise, our work
must do the same.

Many of the tool-related proscriptions and requirements in this document are
driven by a desire to remove boring, repetitive tasks that don't need a human to
perform. By removing the need for us to think about such things, we can focus on
those things which _do_ need a human; thus, we get more done, quicker.

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119].

# Tools

## Compiler warning settings

The following warnings MUST be enabled for all builds of any project, or any
project component:

* ``-Wall``
* ``-Wcompat``
* ``-Wincomplete-uni-patterns``
* ``-Wredundant-constraints``
* ``-Werror``

Additionally, ``-Wincomplete-record-updates`` SHOULD be enabled for all builds
of any project. The only exception is when this warning would be spuriously
triggered by ``record-dot-preprocessor``, which occurs for definitions like
this:

```haskell
data Foo = Bar {
   baz :: Int,
   quux :: String
   } | 
   Quux
```

Additionally, ``-Wredundant-constraints`` SHOULD be enabled for all builds of
any project. Exceptions are allowed when the additional constraints are designed
to ensure safety, rather than due to reliance on any method.

If a warning from this list is to be disabled, it MUST be disabled in the
narrowest possible scope; ideally, this SHOULD be a single module.

### Justification

These options are suggested by [Alexis King][alexis-king-options] - the
justifications for them can be found at the link. These fit well with our
motivations, and thus, should be used everywhere. The ``-Werror`` ensures that
warnings _cannot_ be ignored: this means that problems get fixed sooner.

The two permissible exceptions stem from limitations in the record-dot plugin
(for ``-Wincomplete-record-updates``) and from the way redundant constraints are
detected; basically, unless a type class method from a constraint is used within
the body of the definition, or is required by anything called in a transitive
manner, the constraint is deemed redundant. Mostly, this is accurate, but some
type-level safety constraints can be deemed redundant as a result of this
approach. In this case, a limited lowering (per module ideally) of those two
warnings is acceptable, as they represent workarounds to technical problems,
rather than issues with the warnings themselves.

## Linting

Every source file MUST be free of warnings as produced by [HLint][hlint], with
default settings.

### Justification

HLint automates away the detection of many common sources of boilerplate and
inefficiency. It also describes many useful refactors, which in many cases make
the code easier to read and understand. As this is fully automatic, it saves
effort on our part, and ensures consistency across the codebase without us
having to think about it.

## Code formatting

Every source file MUST be formatted according to [Fourmolu][fourmolu], with the
following settings (as per its settings file):

* ``indentation: 2``
* ``comma-style: leading``
* ``record-brace-space: true``
* ``indent-wheres: true``
* ``diff-friendly-import-export: true``
* ``respectful: true``
* ``haddock-style: multi-line``
* ``newlines-between-decls: 1``

Each source code line MUST be at most 100 characters wide, and SHOULD
be at most 80 characters wide.

### Justification

Consistency is the most important goal of readable codebases. Having a single
standard, automatically enforced, means that we can be sure that everything will
look similar, and not have to spend time or mind-space ensuring that our code
complies. Additionally, as Ormolu is opinionated, anyone familiar with its
layout will find our code familiar, which eases the learning curve.

Lines wider than 80 characters become difficult to read, especially when viewed
on a split screen. Sometimes, we can't avoid longer lines (especially with more
descriptive identifiers), but a line length of over 100 characters becomes
difficult to read even without a split screen. We don't _enforce_ a maximum of
80 characters for this exact reason; some judgment is allowed.

# Code practices

## Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as 
'JSON', 'API', etc) SHOULD be downcased; thus ``repairJson`` and
``fromHttpService`` are correct. Exceptions are allowed for external libraries
(Aeson's ``parseJSON`` for example).

### Justification

camelCase for non-type, non-data-constructor names is a long-standing convention
in Haskell (in fact, HLint checks for it); TitleCase for type names or data
constructors is _mandatory_. Obeying such conventions reduces cognitive load, as
it is common practice among the entire Haskell ecosystem. There is no particular
standard regarding acronym casing: examples of always upcasing exist (Aeson) as
well as examples of downcasing (``http-api-data``). One choice for consistency
(or as much as is possible) should be made however.

## Modules

All publically facing modules (namely, those which are not listed in
``other-modules`` in the Cabal file) MUST have explicit export lists.

All modules MUST use one of the following conventions for imports:

* ``import Foo (Baz, Bar, quux)``
* ``import qualified Foo as F``

Data types from qualified-imported modules SHOULD be imported unqualified by
themselves:

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as Vector
```

The main exception is if such an import would cause a name clash:

```haskell
-- no way to import both of these without clashing the Vector type name
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VStorable
```

The _sole_ exception is a 'hiding import' to replace part of the functionality
of ``Prelude``:

```haskell
-- replace the String-based readFile with a Text-based one
import Prelude hiding (readFile)
import Data.Text.IO (readFile)
```

Data constructors SHOULD be imported individually. For example, given the
following data type declaration:

```haskell
module Quux where

data Foo = Bar Int | Baz
```

Its corresponding import should be:

```haskell
import Quux (Foo, Bar, Baz)
```

For type class methods, the type class and its methods MUST be imported
as so:

```haskell
import Data.Aeson (FromJSON (fromJSON))
```

Qualified imports SHOULD use the entire module name (that is, the last component
of its hierarchical name) as the prefix. For example:

```haskell
import qualified Data.Vector as Vector
```

Exceptions are granted when:

* The import would cause a name clash anyway (such as different ``vector``
  modules); or
* We have to import a data type qualified as well.

Qualified imports of multiple modules MUST NOT be imported under the same name.
Thus, the following is wrong:

```haskell
import qualified Foo.Bar as Baz
import qualified Foo.Quux as Baz
```

### Justification

Explicit export lists are an immediate, clear and obvious indication of what
publically visible interface a module provides. It gives us stability guarantees
(namely, we know we can change things that aren't exported and not break
downstream code at compile time), and tells us where to go looking first when
inspecting or learning the module. Additionally, it means there is less chance
that implementation details 'leak' out of the module due to errors on the part
of developers, especially new developers.

One of the biggest challenges for modules which depend on other modules
(especially ones that come from the project, rather than an external library) is
knowing where a given identifier's definition can be found. Having explicit
imports of the form described helps make this search as straightforward as
possible. This also limits cognitive load when examining the sources (if we
don't import something, we don't need to care about it in general). Lastly,
being explicit avoids stealing too many useful names.

In general, type names occur far more often in code than function calls: we have
to use a type name every time we write a type signature, but it's unlikely we
use only one function that operates on said type. Thus, we want to reduce the
amount of extra noise needed to write a type name if possible. Additionally,
name clashes from function names are far more likely than name clashes from type
names: consider the number of types on which a ``size`` function makes sense.
Thus, importing type names unqualified, even if the rest of the module is
qualified, is good practice, and saves on a lot of prefixing.

## Plutus module import naming conventions

In addition to the general module import rules, we follow some conventions 
on how we import the Plutus API modules, allowing for some flexibility 
depending on the needs of a particular module.

Modules under the names `Plutus`, `Ledger` and `Plutus.V1.Ledger` SHOULD 
be imported qualified with their module name, as per the general module standards. 
An exception to this is `Plutus.V1.Ledger.Api`, where the `Ledger` name is preferred.

Some other exceptions to this are allowed where it may be more convenient to 
avoid longer qualified names.

For example:

```haskell
import Plutus.V1.Ledger.Slot qualified as Slot
import Plutus.V1.Ledger.Tx qualified as Tx
import Plutus.V1.Ledger.Api qualified as Ledger
import Ledger.Oracle qualified as Oracle
import Plutus.Contract qualified as Contract
```

In some cases it may be justified to use a shortened module name:

```haskell
import Plutus.V1.Ledger.AddressMap qualified as AddrMap
```

Modules under `PlutusTx` that are extensions to `PlutusTx.Prelude` MAY be 
imported unqualified when it is reasonable to do so. 

The `Plutus.V1.Ledger.Api` module SHOULD be avoided in favour of more 
specific modules where possible. For example, we should avoid:

```haskell
import Plutus.V1.Ledger.Api qualified as Ledger (ValidatorHash)
```

In favour of:

```haskell
import Plutus.V1.Ledger.Scripts qualified as Scripts (ValidatorHash)
```

### Justification

The Plutus API modules can be confusing, with numerous modules involved, many 
exporting the same items. Consistent qualified names help ease this problem, 
and decrease ambiguity about where imported items come from.

## LANGUAGE pragmata

The following pragmata MUST be enabled at project level (that is, in
``package.yaml``):

* ``BangPatterns``
* ``BinaryLiterals``
* ``ConstraintKinds``
* ``DataKinds``
* ``DeriveFunctor``
* ``DeriveGeneric``
* ``DeriveTraversable``
* ``DerivingStrategies``
* ``DuplicateRecordFields``
* ``EmptyCase``
* ``FlexibleContexts``
* ``FlexibleInstances``
* ``GADTs``
* ``GeneralizedNewtypeDeriving``
* ``HexFloatLiterals``
* ``InstanceSigs``
* ``ImportQualifiedPost``
* ``KindSignatures``
* ``LambdaCase``
* ``MultiParamTypeClasses``
* ``NoImplicitPrelude``
* ``NumericUnderscores``
* ``OverloadedStrings``
* ``StandaloneDeriving``
* ``TupleSections``
* ``TypeApplications``
* ``TypeOperators``
* ``TypeSynonymInstances``
* ``UndecidableInstances``

Any other LANGUAGE pragmata MUST be enabled per-file. All language pragmata MUST
be at the top of the source file, written as ``{-# LANGUAGE PragmaName #-}``.

Furthermore, the following pragmata MUST NOT be used, or enabled, anywhere:

* ``DeriveDataTypeable``
* ``DeriveFoldable``
* ``PartialTypeSignatures``
* ``PostfixOperators``

### Justification

``DataKinds``, ``DuplicateRecordFields``, ``GADTs``, ``TypeApplications``,
``TypeSynonymInstances`` and ``UndecidableInstances`` are needed globally to use
the GHC plugin from ``record-dot-preprocessor``. While some of these extensions
are undesirable to use globally, we end up needing them anyway, so we can't
really avoid this.

``BangPatterns`` are a much more convenient way to force evaluation than
repeatedly using `seq`. Furthemore, they're not confusing, and are considered
ubiquitous enough for ``GHC2021``. Having them on by default simplifies a lot of
performance tuning work, and they don't really need signposting.

``BinaryLiterals``, ``HexFloatLiterals`` and ``NumericUnderscores`` all simulate
features that are found in many other programming languages, and that are
extremely convenient in a range of settings, ranging from dealing with large
numbers to bit-twiddling. If anything, it is more surprising and annoying when
these _aren't_ enabled, and should really be part of Haskell syntax anyway.
Enabling this project-wide actually encourages better practice and readability.

The kind ``Constraint`` is not in Haskell2010, and thus, isn't recognized by
default. While working with constraints as first-class objects isn't needed
often, this extension effectively exists because Haskell2010 lacks exotic kinds
altogether. Since we require explicit kind signatures (and foralls) for all type
variables, this needs to be enabled as well. There is no harm in enabling this
globally, as other rich kinds (such as ``Symbol`` or ``Nat``) don't require an
extension for their use, and this doesn't change any behaviour (``Constraint``
exists whether you enable this extension or not, as do 'exotic kinds' in
general).

``DerivingStrategies`` is good practice (and in fact, is mandated by this
document); it avoids ambiguities between ``GeneralizedNewtypeDeriving`` and
``DeriveAnyClass``, allows considerable boilerplate savings through use of
``DerivingVia``, and makes the intention of the derivation clear on immediate
reading, reducing the amount of non-local information about derivation
priorities that we have to retain. ``DeriveFunctor`` and
``GeneralizedNewtypeDeriving`` are both obvious and useful extensions to the
auto-derivation systems available in GHC. Both of these have only one correct
derivation (the former given by [parametricity
guarantees][functor-parametricity], the latter by the fact that a newtype only
wraps a single value). As there is no chance of unexpected behaviour by these,
no possible behaviour variation, and that they're key to supporting both the
``stock`` and ``newtype`` deriving stratgies, having these on by default removes
considerable tedium and line noise from our code. A good example are newtype
wrappers around monadic stacks:

```haskell
newtype FooM a = FooM (ReaderT Int (StateT Text IO) a)
  deriving newtype (
    Functor, 
    Applicative, 
    Monad, 
    MonadReader Int, 
    MonadState Text, 
    MonadIO
    )
```

Deriving ``Traversable`` is a little tricky. While ``Traversable`` is lawful
(though not to the degree ``Functor`` is, permitting multiple implementations in
many cases), deriving it is complicated by issues of role assignation for
higher-kinded type variables and the fact that you can't ``coerce`` through a
``Functor``. These are arguably implementation issues, but repairing this
situation requires cardinal changes to ``Functor``, which is unlikely to ever
happen. Even newtype or via derivations of ``Traversable`` are mostly
impossible; thus, we must have special support from GHC, which
``DeriveTraversable`` enables. This is a very historically-motivated
inconsistency, and should really not exist at all. While this only papers over
the problem (as even with this extension on, only stock derivations become
possible), it at least means that it can be done at all. Having it enabled
globally makes this inconsistency slightly less visible, and is completely safe.

While GHC ``Generic``s are far from problem-free, many parts of the Haskell
ecosystem require ``Generic``, either as such (c.f. ``beam-core``) or for
convenience (c.f ``aeson``, ``hashable``). Additionally, several core parts of
Plutus (including ``ToSchema``) are driven by ``Generic``. The derivation is
trivial in most cases, and having to enable an extension for it is quite
annoying. Since no direct harm is done by doing this, and use of ``Generic`` is
already signposted clearly (and is mostly invisible), having this on globally
poses no problems.

``EmptyCase`` not being on by default is an inconsistency of Haskell 2010, as
the report allows us to define an empty data type, but without this extension,
we cannot exhaustively pattern match on it. This should be the default behaviour
for reasons of symmetry.

``FlexibleContexts`` and ``FlexibleInstances`` paper over a major deficiency of
Haskell2010, which in general isn't well-motivated. There is no real reason to
restrict type arguments to variables in either type class instances or type
signatures: the reasons for this choice in Haskell2010 are entirely for the
convenience of the implementation. It produces no ambiguities, and in many ways,
the fact this _isn't_ the default is more surprising than anything.
Additionally, many core libraries rely on one, or both, of these extensions
being enabled (``mtl`` is the most obvious example, but there are many others).
Thus, even for popularity and compatibility reasons, these should be on by
default.

``InstanceSigs`` are harmless by default, and introduce no complications. Their
not being default is strange. ``ImportQualifiedPost`` is already a convention 
of this project, and helps with formatting of imports. 

``KindSignatures`` become extremely useful in any setting where 'exotic kinds'
(meaning, anything which isn't `Type` or `Type -> Type` or similar) are
commonplace; much like type signatures clarify expectations and serve as active
documentation (even where GHC can infer them), explicit kind signatures serve
the same purpose 'one level up'. When combined with the requirement to provide
explicit foralls for type variables defined in this document, they simplify the
usage of 'exotic kinds' and provide additional help from both the type checker
and the code. Since this project is Plutus-based, we use 'exotic kinds'
extensively, especially in row-polymorphic records; thus, in our case, this is
especially important. This also serves as justification for
`ScopedTypeVariables`, as well as ironing out a weird behaviour where in cases
such as 

```haskell
foo :: a -> b
foo = bar . baz
   where
      bar :: String -> b
      bar = ...
      baz :: a -> String
      baz = ...
```

cause GHC to produce _fresh_ type variables in each ``where``-bind. This is
confusing and makes little sense - if the user wanted a fresh variable, they
would name it that way. What's worse is that the type checker emits an error
that makes little sense (except to those who have learned to look for this
error), creating even more confusion, especially in cases where the type
variable is constrained:

```haskell
foo :: (Monoid m) => m -> String
foo = bar . baz
   where
      baz :: m -> Int
      baz = ... -- this has no idea that m is a Monoid, since m is fresh!
```

``LambdaCase`` reduces a lot of code in the common case of analysis of sum
types. Without it, we are forced to either write a dummy ``case`` argument:

```haskell
foo s = case s of
-- rest of code here
```

Or alternatively, we need multiple heads:

```haskell
foo Bar = -- rest of code
foo (Baz x y) = -- rest of code
-- etc
```

``LambdaCase`` is shorter than both of these, and avoids us having to bind
variables, only to pattern match them away immediately. It is convenient, clear
from context, and really should be part of the language to begin with.

``MultiParamTypeClasses`` are required for a large number of standard Haskell
libraries, including ``mtl`` and ``vector``, and in many situations. Almost any
project of non-trivial size must have this extension enabled somewhere, and if
the code makes significant use of ``mtl``-style monad transformers or defines
anything non-trivial for ``vector``, it must use it. Additionally, it arguably
lifts a purely implementation-driven decision of the Haskell 2010 language, much
like ``FlexibleContexts`` and ``FlexibleInstances``. Lastly, although it can
introduce ambiguity into type checking, it only applies when we want to define
our own multi-parameter type classes, which is rarely necessary. Enabling it
globally is thus safe and convenient.

Based on the recommendations of this document (driven by the needs of the
project and the fact it's cardinally connected with Plutus),
``NoImplicitPrelude`` is required to allow us to default to the Plutus prelude
instead of the one from ``base``.

``OverloadedStrings`` deals with the problem that ``String`` is a suboptimal
choice of string representation for basically _any_ problem, with the general
recommendation being to use ``Text`` instead. It is not, however, without its
problems: 

* ``ByteString``s are treated as ASCII strings by their ``IsString`` instance;
* Overly polymorphic behaviour of many functions (especially in the presence of
  type classes) forces extra type signatures;

These are usually caused not by the extension itself, but by other libraries and
their implementations of either ``IsString`` or overly polymorphic use of type
classes without appropriate laws (Aeson's ``KeyValue`` is a particularly
egregious offender here). The convenience of this extension in the presence of
literals, and the fact that our use cases mostly covers ``Text``, makes it worth
using by default.

``StandaloneDeriving`` is mostly needed for GADTs, or situations where complex
type-level computations drive type class instances, requiring users to specify
constraints manually. This can pose some difficulties syntactically (such as
with deriving strategies), but isn't a problem in and of itself, as it doesn't
really change how the language works. Having this enabled globally is not
problematic.

``TupleSections`` smooths out an oddity in the syntax of Haskell 2010 regarding
partial application of tuple constructors. Given a function like ``foo :: Int -> String ->
Bar``, we accept it as natural that we can write ``foo 10`` to get a function of
type ``String -> Bar``. However, by default, this logic doesn't apply to tuple
constructors. As special cases are annoying to keep track of, and in this case,
serve no purpose, as well as being clear from their consistent use, this should
also be enabled by default; it's not clear why it isn't already.

``TypeOperators`` is practically a necessity when dealing with type-level
programming seriously. Much how infix data constructors are extremely useful
(and sometimes clearer than their prefix forms), infix _type_ constructors serve
a similar functionality. Additionally, Plutus relies on operators at the type
level significantly - for example, it's not really possible to define a
row-polymorphic record or variant without them. Having to enable this almost
everywhere is a needless chore, and having type constructors behaving
differently to data constructors here is a needless source of inconsistency.

We exclude ``DeriveDataTypeable``, as ``Data`` is a strictly-worse legacy
version of ``Generic``, and ``Typeable`` no longer needs deriving for anything
anyway. The only reason to derive either of these is for compatibility with
legacy libraries, which we don't have any of, and the number of which shrinks
every year. If we're using this extension at all, it's probably a mistake.

``Foldable`` is possibly the most widely-used lawless type class. Its only laws
are about self-consistency (such as agreement between ``foldMap`` and
``foldr``), but unlike something like ``Functor``, ``Foldable`` doesn't have any
laws specifying its behaviour outside of 'it compiles'. As a result, even if we
accept its usefulness (a debatable position in itself), there are large numbers
of possible implementations that could be deemed 'valid'. The approach taken by
``DeriveFoldable`` is _one_ such approach, but this requires knowing its
derivation algorithm, and may well not be something you need. Unlike a
``Functor`` derivation (whose meaning is obvious), a ``Foldable`` one is
anything but, and requires referencing a lot of non-local information to
determine how it will behave (especially for the 'richer' ``Foldable``, with
many additional methods). If you need a ``Foldable`` instance, you will either
newtype or via-derive it (which doesn't need this extension anyway), or you'll
write your own (which _also_ doesn't need this extension). Enabling this
encourages bad practices, is confusing, and ultimately doesn't really benefit
anything.

``PartialTypeSignatures`` is a misfeature. Allowing leaving in type holes (to be
filled by GHC's inference algorithm) is an anti-pattern for the same reason that
not providing top-level signatures: while it's possible (mostly) for GHC to
infer signatures, we lose considerable clarity and active documentation by doing
so, in return for (quite minor) convenience. While the use of typed holes during
development is a good practice, they should not remain in final code. Given that
Plutus projects require us to do some fairly advanced type-level programming
(where inference often _fails_), this extension can often provide totally
incorrect results due to GHC's 'best-effort' attempts at type checking. There is
no reason to leave behind typed holes instead of filling them in, and we
shouldn't encourage this.

``PostfixOperators`` are arguably a misfeature. Infix operators already require
a range of special cases to support properly (what symbols create an infix
operator, importing them at the value and type level, etc), which postfix
operators make _worse_. Furthermore, they are seldom, if ever, used, and
typically aren't worth the trouble. Haskell is not Forth, none of our
dependencies rely on postfix operators, and defining our own creates more
problems than it solves.

## ``record-dot-preprocessor``

The GHC plugin from ``record-dot-preprocessor`` SHOULD be enabled globally. 

### Justification

Haskell records are documentedly and justifiably subpar: the [original issue for
the record dot preprocessor extension][rdp-issue] provides a good summary of the
reasons. While a range of extensions (including ``DuplicateRecordFields``,
``DisambiguateRecordFields``, ``NamedFieldPuns``, and many others) have been
proposed, and accepted, to mitigate the situation, the reality is that, even
with them in place, use of records in Haskell is considerably more difficult,
and less flexible, than in any other language in widespread use today. The
proposal described in the previous link provides a solution which is familiar to
users of most other languages, and addresses the fundamental issue that makes
Haskell records so awkward.

While the proposal for the record dot syntax that this preprocessor enables is
coming, it's not available in the current version of Haskell used by Plutus (and
thus, transitively, by us). Additionally, the earliest this will be available is
GHC 9.2, and given that our dependencies must support this version too, it'll be
considerable time before we can get its benefits. The preprocessor gives us
these benefits immediately, at some dependency cost. While it's not a perfect
process, as it involves enabling several questionable extensions, and can
require disabling an important warning, it significantly reduces issues with
record use, making it worthwhile. Additionally, when GHC 9.2 becomes usable, we
can upgrade to it seamlessly.

## Prelude

The ``PlutusTx.Prelude`` MUST be used. A 'hiding import' to remove functionality
we want to replace SHOULD be used when necessary. If functionality from the
``Prelude`` in ``base`` is needed, it SHOULD be imported qualified. Other
preludes MUST NOT be used.

### Justification

As this is primarily a Plutus project, we are in some ways limited by what
Plutus requires (and provides). Especially for on-chain code, the Plutus prelude
is the one we need to use, and therefore, its use should be as friction-free as
possible. As many modules may contain a mix of off-chain and on-chain code, we
also want to make impendance mismatches as limited as possible.

By the very nature of this project, we can assume a familiarity (or at least,
the goal of such) with Plutus stuff. Additionally, _every_ Haskell developer is
familiar with the ``Prelude`` from ``base``. Thus, any replacements of the
Plutus prelude functionality with the ``base`` prelude should be clearly
indicated locally.

Haskell is a 30-year-old language, and the ``Prelude`` is one of its biggest
sources of legacy. A lot of its defaults are questionable at best, and often
need replacing. As a consequence of this, a range of 'better ``Prelude``s' have
been written, with a range of opinions: while there is a common core, a large
number of decisions are opinionated in ways more appropriate to the authors of
said alternatives and their needs than those of other users of said
alternatives. This means that, when a non-``base`` ``Prelude`` is in scope, it
often requires familiarity with its specific decisions, in addition to whatever
cognitive load the current module and its other imports impose. Given that we
already use an alternative prelude (in tandem with the one from ``base``),
additional alternatives present an unnecessary cognitive load. Lastly, the 
dependency footprint of many alternative ``Prelude``s is _highly_ non-trivial; 
it isn't clear if we need all of this in our dependency tree.

For all of the above reasons, the best choice is 'default to Plutus, with local
replacements from `base`'.

## Versioning

A project MUST use the [PVP][pvp]. Two, and only two, version numbers MUST be
used: a major version and a minor version.

### Justification

The [Package Versioning Policy][pvp] is the conventional Haskell versioning
scheme, adopted by most packages on Hackage. It is clearly described, and even
automatically verifiable by use of tools like [``policeman``][policeman]. Thus,
adopting it is both in line with community standards (making it easier to
remember), and simplifies cases such as Hackage publication or open-sourcing in
general.

Two version numbers (major and minor) is the minimum allowed by the PVP,
indicating compilation-breaking and compilation-non-breaking changes
respectively. As parsimony is best, and more granularity than this isn't
generally necessary, adopting this model is the right decision.

## Documentation

Every publically-exported definition MUST have a Haddock comment, detailing its
purpose. If a definition is a function, it SHOULD also have examples of use
using [Bird tracks][bird-tracks]. The Haddock for a publically-exported
definition SHOULD also provide an explanation of any caveats, complexities of
its use, or common issues a user is likely to encounter. 

If the code project is a library, these Haddock comments SHOULD carry an
[``@since``][haddock-since] annotation, stating what version of the library they
were introduced in, or the last version where their functionality or type
signature changed.

For type classes, their laws MUST be documented using a Haddock comment.

### Justification

Code reading is a difficult task, especially when the 'why' rather than the
'how' of the code needs to be deduced. A good solution to this is documentation,
especially when this documentation specifies common issues, provides examples of
use, and generally states the rationale behind the definition.

For libraries, it is often important to inform users what changed in a given
version, especially where 'major bumps' are concerned. While this would ideally
be addressed with accurate changelogging, it can be difficult to give proper
context. ``@since`` annotations provide a granular means to indicate the last
time a definition changed considerably, allowing someone to quickly determine
whether a version change affects something they are concerned with.

As stated elsewhere in the document, type classes having laws is critical to our
ability to use equational reasoning, as well as a clear indication of what
instances are and aren't permissible. These laws need to be clearly stated, as
this assists both those seeking to understand the purpose of the type class, and
also the expected behaviour of its instances.

## Other

Lists SHOULD NOT be field values of types; this extends to ``String``s. Instead,
``Vector``s (``Text``s) SHOULD be used, unless a more appropriate structure exists. 
On-chain code, due to a lack of alternatives, is one place lists can be used as
field values of types.

Partial functions MUST NOT be defined. Partial functions SHOULD NOT be used
except to ensure that another function is total (and the type system cannot be
used to prove it). 

Derivations MUST use an explicit [strategy][deriving-strategies]. Thus, the 
following is wrong:

```haskell
newtype Foo = Foo (Bar Int)
    deriving (Eq, Show, Generic, FromJSON, ToJSON, Data, Typeable)
```

Instead, write it like this:

```haskell
newtype Foo = Foo (Bar Int)
    deriving stock (Generic, Data, Typeable)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON, ToJSON)
```

Deriving via SHOULD be preferred to newtype derivation, especially where the
underlying type representation could change significantly.

``type`` SHOULD NOT be used. The only acceptable case is abbreviation of large
type-level computations. In particular, using ``type`` to create an abstraction
boundary MUST NOT be done.

Type variables MUST have an explicit ``forall`` scoping it, and all type
variables MUST have kind signatures explicitly provided. Thus, the following is
wrong:

```haskell
data Foo a = Bar | Baz [a]

quux :: (Monoid m) => [m] -> m -> m
```

Instead, write it like this:

```haskell
data Foo (a :: Type) = Bar | Baz [a]

quux :: forall (m :: Type) . (Monoid m) => [m] -> m -> m
```

`where`-bindings MUST have type signatures.

### Justification

Haskell lists are a large example of the legacy of the language: they (in the
form of singly linked lists) have played an important role in the development of
functional programming (and for some 'functional' languages, continue to do so).
However, from the perspective of data structures, they are suboptimal except for
_extremely_ specific use cases. In almost any situation involving data (rather
than control flow), an alternative, better structure exists. Although it is both
acceptable and efficient to use lists within functions (due to GHC's extensive
fusion optimizations), from the point of view of field values, they are a poor
choice from both an efficiency perspective, both in theory _and_ in practice.
For almost all cases where you would want a list field value, a ``Vector`` field
value is more appropriate, and in almost all others, some other structure (such
as a ``Map``) is even better. We make a named exception for on-chain code, as no
alternatives presently exist.

Partial functions are runtime bombs waiting to explode. The number of times the
'impossible' happened, especially in production code, is significant in our
experience, and most partiality is easily solvable. Allowing the compiler to
support our efforts, rather than being blind to them, will help us write more
clear, more robust, and more informative code. Partiality is also an example of
legacy, and it is legacy of _considerable_ weight. Sometimes, we do need an
'escape hatch' due to the impossibility of explaining what we want to the
compiler; this should be the _exception_, not the rule.

Derivations are one of the most useful features of GHC, and extend the
capabilities of Haskell 2010 considerably. However, with great power comes great
ambiguity, especially when ``GeneralizedNewtypeDeriving`` is in use. While there
_is_ an unambiguous choice if no strategy is given, it becomes hard to remember.
This is especially dire when ``GeneralizedNewtypeDeriving`` combines with
``DeriveAnyClass`` on a newtype. Explicit strategies give more precise control
over this, and document the resulting behaviour locally. This reduces the number
of things we need to remember, and allows more precise control when we need it.
Lastly, in combination with ``DerivingVia``, considerable boilerplate can be
saved; in this case, explicit strategies are _mandatory_.

The only exception to the principle above is newtype deriving, which can
occasionally cause unexpected problems; if we use a newtype derivation, and
change the underlying type, we get no warning. Since this can affect the effect
of some type classes drastically, it would be good to have the compiler check
our consistency.

``type`` is generally a terrible idea in Haskell. You don't create an
abstraction boundary with it (any operations on the 'underlying type' still work
over it), and compiler output becomes _very_ inconsistent (sometimes showing the
``type`` definition, sometimes the underlying type). If your goal is to create
an abstraction boundary with its own operations, ``newtype`` is both cost-free
and clearer; if that is _not_ your goal, just use the type you'd otherwise
rename, since it's equivalent semantically. The only reasonable use of ``type``
is to hide complex type-level computations, which would otherwise be too long.
Even this is somewhat questionable, but the questionability comes from the
type-level computation being hidden, not ``type`` as such.

Type-level programming is mandated in many places by Plutus (including, but not
limited to, row-polymorphic records and variants from `Data.Row`). This often
requires use of ``TypeApplications``, which essentially makes not only the type
variables, but their _order_, part of the API of any definition that uses them.
While there is an algorithm determining this precisely, something that is
harmless at the value level (such as re-ordering constraints) could potentially
serve as an API break. Additionally, this algorithm is a huge source of
non-local information, and in the presence of a large number of type variables,
of different kinds, can easily become confusing. Having explicit foralls
quantifying all type variables makes it clear what the order for these type
variables is for ``TypeApplications``, and also allows us to choose it
optimally for our API, rather than relying on what the algorithm would produce.
This is significantly more convenient, and means less non-local information and
confusion.

Additionally, type-level programming requires significant use of 'exotic kinds',
which in our case include ``Constraint -> Type`` and ``Row Type``, to name but a
few. While GHC can (mostly) infer kind signatures, much the same way as we
explicitly annotate type signatures as a form of active documentation (and to
assist the type checker when using type holes), explicitly annotating _kind_
signatures allows us to be clear to the users where exotic kinds are expected,
as well as ensuring that we don't make any errors ourselves. This, together with
explicit foralls, essentially bring the same practices to the kind level as the
Haskell community already considers to be good at the type level.

`where` bindings are quite common in idiomatic Haskell, and quite often contain
non-trivial logic. They're also a common refactoring, and 'hole-driven
development' tool, where you create a hole to be filled with a `where`-bound
definition. Even in these cases, having an explicit signature on
`where`-bindings helps: during development, you can use typed holes inside the
`where`-binding with useful information (absent a signature, you'll get
nothing), and it makes the code much easier to understand, especially if the
`where`-binding is complex. It's also advantageous when 'promoting'
`where`-binds to full top-level definitions, as the signature is already there.
Since we need to do considerable type-level programming as part of Plutus, this
becomes even more important, as GHC's type inference algorithm can often fail in
those cases on `where`-bindings, which will sometimes fail to derive, giving a
very strange error message, which would need a signature to solve anyway. By
making this practice proactive, we are decreasing confusion, as well as
increasing readability. While in theory, this standard should extend to
`let`-bindings as well, these are much rarer, and can be given signatures with
`::` if `ScopedTypeVariables` is on (which it is for us by default) if needed.

# Design practices

## Parse, don't validate

[Boolean blindness][boolean-blindness] SHOULD NOT be used in the design of any
function or API. Returning more meaningful data SHOULD be the preferred choice.
The general principle of ['parse, don't validate'][parse-dont-validate] SHOULD
guide design and implementation.

### Justification

The [description of boolean blindness][boolean-blindness] gives specific reasons why it is a poor
design choice; additionally, it runs counter to the principle of ['parse, don't
validate][parse-dont-validate]. While sometimes unavoidable, in many cases, it's
possible to give back a more meaningful response than 'yes' or 'no, and we
should endeavour to do this. Designs that avoid boolean blindness are more
flexible, less bug-prone, and allow the type checker to assist us when writing.
This, in turn, reduces cognitive load, improves our ability to refactor, and
means fewer bugs from things the compiler _could_ have checked if a function
_wasn't_ boolean-blind.

## No multi-parameter type-classes without functional dependencies

Any multi-parameter type class MUST have a functional dependency restricting its
relation to a one-to-many at most. In cases of true many-to-many relationships,
type classes MUST NOT be used as a solution to the problem.

### Justification

Multi-parameter type classes allow us to express more complex relationships
among types; single-parameter type classes effectively permit us to 'subset'
``Hask`` only. However, multi-parameter type classes make type inference
_extremely_ flakey, as the global coherence condition can often lead to the
compiler being unable to determine what instance is sought even if all the type
parameters are concrete, due to anyone being able to add a new instance at any
time. This is largely caused by multi-parameter type classes defaulting to
effectively representing arbitrary many-to-many relations.

When we do not _have_ arbitrary many-to-many relations, multi-parameter type
classes are useful and convenient. We can indicate this using functional
dependencies, which inform the type checker that our relationship is not
arbitrarily many-to-many, but rather many-to-one or even one-to-one. This is a
standard practice in many libraries (``mtl`` being the most ubiquitous example),
and allows us the benefits of multi-parameter type classes without making type
checking confusing and difficult.

In general, many-to-many relationships pose difficult design choices, for which
type classes are _not_ the correct solution. If a functional dependency _cannot_
be provided for a type class, it suggests that the current design relies
inherently on a many-to-many relation, and should be either rethought to
eliminate it, or be dealt with using a more appropriate means.

## Type classes must have laws

Any type class not imported from an external dependency MUST have laws. These
laws MUST be documented in a Haddock comment on the type class definition, and
all instances MUST follow these laws.

### Justification

Type classes are a powerful feature of Haskell, but can also be its most
confusing. As they allow arbitrary ad-hoc polymorphism, and are globally
visible, it is important that we limit the confusion this can produce.
Additionally, type classes without laws inhibit equational reasoning, which is
one of Haskell's biggest strengths, _especially_ in the presence of what amounts
to arbitrary ad-hoc polymorphism.

Additionally, type classes with laws allow the construction of _provably_
correct abstractions above them. This is also a common feature in Haskell,
ranging from profunctor optics to folds. If we define our own type classes, we
want to be able to abstract above them with _total_ certainty of correctness.
Lawless type classes make this difficult to do: compare the number of
abstractions built on `Functor` or `Traversable` as opposed to `Foldable`.

Thus, type classes having laws provides both ease of understanding and
additional flexibility.

[pvp]: https://pvp.haskell.org/
[policeman]: https://hackage.haskell.org/package/policeman
[haddock-since]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#since
[bird-tracks]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#code-blocks
[hedgehog-classes]: http://hackage.haskell.org/package/hedgehog-classes
[hspec-hedgehog]: http://hackage.haskell.org/package/hspec-hedgehog
[property-based-testing]: https://dl.acm.org/doi/abs/10.1145/1988042.1988046
[hedgehog]: http://hackage.haskell.org/package/hedgehog
[deriving-strategies]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies
[functor-parametricity]: https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
[alexis-king-options]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build
[hlint]: http://hackage.haskell.org/package/hlint
[fourmolu]: http://hackage.haskell.org/package/fourmolu
[rfc-2119]: https://tools.ietf.org/html/rfc2119
[boolean-blindness]: http://dev.stephendiehl.com/hask/#boolean-blindness
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[hspec]: http://hackage.haskell.org/package/hspec
[rdp]: https://hackage.haskell.org/package/record-dot-preprocessor
[rdp-issue]: https://github.com/ghc-proposals/ghc-proposals/pull/282
