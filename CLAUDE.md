# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Haskell implementation of **Sparcl**, a language for partially-invertible computation with linear (multiplicity-annotated) types (Matsuda & Wang, ICFP 2020). The deliverable is an interpreter/REPL; there is no compiler backend. Sparcl source files use the `.sparcl` extension.

## Commands

Toolchain: GHC 9.6.7 + cabal (installed via ghcup). The `stack-*.yaml` files are for older GHCs and are not the primary build.

```sh
cabal build                         # build library + executable
cabal run                           # start REPL (prompt: "Sparcl>")
cabal run sparcl-exe -- FILE.sparcl # start REPL and :load FILE
cabal run sparcl-exe -- -v2 FILE    # verbosity 0-3 (debug output of typing/desugaring)
cabal test                          # hspec suite (test/Spec.hs)
cabal test --test-options='--match "Fib.sparcl"'   # run a subset of tests by name
cabal bench                         # typing benchmark (bench/Bench.hs)
cabal repl lib:sparcl               # GHCi on the library, e.g. to call parseModule directly
```

REPL commands: `:load FILE`, `:reload`, `:type EXP`, `:verbosity [0-3]`, `:help`, `:quit`. There is no batch mode; passing a file just runs `:load`.

Formatting: `fourmolu.yaml` (2-space indent, leading commas/arrows) is the formatter config. Compiled with `-Wall`; many modules already have warnings, so don't treat existing ones as regressions.

## Tests

`test/Spec.hs` is data-driven rather than per-module:
- Every file under `./Examples` must load (parse → rename → typecheck → desugar) without a static error. `recursivelyListDirectories` is meant to recurse, but it calls `doesDirectoryExist` on the bare entry name instead of `dir </> entry`. So subdirectories are not recursed into: any subdirectory placed in `Examples/` is itself "loaded" as a file and fails with `openFile: inappropriate type`. Keep `Examples/` flat; work-in-progress examples that should not be tested live outside it (e.g. `Examples-Class/`). A multi-module example still fits a flat directory: a dotted module name is a path relative to the search path (the repository root in the tests, the current directory in the REPL), so `module Examples.ClassModA` is found as `Examples/ClassModA.sparcl` (see `Examples/ClassMod{A,B,C}.sparcl`).
- Every file under `./Nonexamples_Illtyped` must raise a `StaticException`.
- `./TestCases/RuntimeError*.sparcl` must raise a `RunTimeException`.
- A few examples (`Fib`, `ArithmeticCoding`, `LZ77`, `AddLSB`) are evaluated and specific top-level bindings compared (e.g. `n0` vs `n0'`). The type-class examples (`Examples/Class*.sparcl`, and `t6`/`t7` of `HigherRankLetCase`) write the expected value `tK'` of each test binding `tK`, and `checkExpected` compares them; the list of files and names is in `test/Spec.hs`.

So adding a `.sparcl` file directly in `Examples/` automatically adds a test that expects it to typecheck. Tests run from the repo root (paths are relative).

## Architecture

Pipeline, driven by `Language.Sparcl.Module` (`readModule` / `interpDecls`):

1. **Parse** — `Surface/Parser/{Exp,Id,Helper}.hs` (megaparsec) → `Surface.Syntax` AST at pass `'Parsing`. The grammar is *not* indentation sensitive; blocks are closed with `end` (`case ... end`, `where ... end`). Keywords are listed in `Surface/Parser/Id.hs` (`keyWords`) — a new keyword must be added there, or it will be parsed as an identifier.
2. **Rename** — `Renaming.hs` resolves names/operators/fixities → pass `'Renaming`.
3. **Type check** — `Typing/Typing.hs` (bidirectional, `Expected = Check | Infer`) in the `TC` monad (`Typing/TCMonad.hs`), producing pass `'TypeCheck`. Multiplicity constraints (`MSub`, `TyEq` in `Typing/Type.hs`) are gathered and checked with the small SAT solver in `Algorithm/SAT.hs`; `Typing/Constraint.hs` simplifies them. Supports higher-rank polymorphism via signatures (see `Examples/Poly.sparcl`).
4. **Desugar** — `Desugar.hs` turns the typed surface AST into `Core.Syntax` (pattern elaboration, `revdo`, etc.).
5. **Evaluate** — `Eval.hs` / `Value.hs`. Values are HOAS-style (`Value -> Eval Value`); invertible computations are residuals with forward/backward functions. Forward evaluation ignores linearity.

The surface AST is indexed by `Pass` (`Pass.hs`) and uses type families (`XId p`, `XTId p`, …) so one datatype serves all three stages.

Built-in primitives (arithmetic, `eqInt`/`leInt`/…, `Un`, `Many`) are defined in `baseModuleInfo` in `Module.hs` (type table, value table, constructor table, operator fixities together). `runtime/` is a separate, currently unused package (`sparcl-runtime`, commented out in `cabal.project`).

`notes/Syntax.md` documents pattern elaboration and syntax design discussions.

## Ongoing work: type classes (branch `type-class`)

- Parser and surface AST support `class ... extends ... where ... end` / `instance [x :] ... where ... end` (`DClass`, `DInst` in `Surface.Syntax`; `classDecl`, `instanceDecl` in `Parser/Exp.hs`) and the `{{ }}` forms in types, expressions and patterns. Renaming handles them (`renameTopDecls` returns classes and instances as extra tuple lists). Type checking, desugaring and evaluation were implemented in stages (see "Implementation plan" and PROGRESS in the notes): class and instance declarations are registered and checked (class/instance tables `tcClassEnv`/`tcInstEnv`, `mcClassTable`/`mcInstTable`; `checkClassTypes` for `{{ }}` contents and class arities); instance abstractions and applications are inserted (`checkTy` entry, `alignPatsTy` slots, `checkSpine`), omitted instance arguments become goals solved by instance search (`Typing/Instance.hs`, `solveGoals`) that fill `InstHole`s; default methods and instance bodies are checked (`checkSigDef`, `instMethodType`) and returned by `inferTopDecls` as `TypedDefault` / `TypedInstance` (`TCMonad.hs`). Desugaring (`desugarTopDecls`) builds dictionaries as values of a constructor `<dict C>` (superclass dictionaries, then methods), methods and superclass projections as `case` projections, defaults as `<default m>` bindings, and instances as functions of their instance arguments returning a self-referential dictionary; the generated names are `System` names made in `Name.hs` (`dictName`, `superProjName`, `defaultName`). Superclasses may be cyclic, so dictionaries can be cyclic values: `rnf` forces dictionary fields only to WHNF and dictionaries print as `<dict C>` (`Value.hs`). A module's value table keeps imported instance / projection / default values regardless of import and export lists (`isImplicitValueName` in `Module.hs`). Inserted instance patterns share one variable across the clauses of a definition (`Inserted d c` in `alignPatsTy`), so that `desugarAlts` can still group clauses with `rev` patterns. The implementation plan of the notes is complete (stages 0–6); higher-kinded classes and the items under "TODO / known issues" in the notes remain. Examples are `Examples/Class*.sparcl` (evaluated by the tests) and error examples `Nonexamples_Illtyped/Class*.sparcl`.
- Design direction has changed: instances are to be passed as **implicit instance arguments (Agda/Lean style)**, not as Haskell-style constraints (`Eq a => ...`). They are written with Agda-style double braces: `sig elem : {{Eq a}} -> a -> List a -> Bool`. Instance names are optional (`instance eqNat : Eq Nat where ... end`), instances with arguments are written `instance eqList : {{Eq a}} -> Eq (List a) where ... end`, and superclasses are fields declared with `class Ord a extends Eq a where ... end` (so `{{Ord a}}` also provides `Eq a`). Instance-argument arrows are always unrestricted (Omega). Instances can be passed explicitly (`elem {{eqNat}} x xs`) and bound by name in definitions (`def f {{eq}} x = ...`), but not named inside types. In the surface AST, `{{C}} -> t` is `TCon nameTyInstArr [C, t]` (no dedicated constructor; see `isTyInstArr`), explicit passing is `App e (InstArg e')` where the argument of `App` has type `Arg p = ExplicitArg (LExp p) | InstArg (LExp p)` (so `{{e}}` can only be an argument; elsewhere it is a parse error with a dedicated message, `misplacedInstArg` in `simpleExpr`), and binding is the pattern `PInst`. Pass-indexed fields that are `Void` in some passes: `Op (XOp p) ...` (`Void` after type checking, since the type checker outputs operator applications as `App`s) and `InstHole (XHole p) (IORef (Maybe (LExp p)))` (a dictionary hole made only by the type checker; `XHole` is `()` after type checking and `Void` before); like `XDecls`/`XHDecls` these tag fields are lazy and the impossible cases are handled by `absurd`. Type-level names (class names, class parameters) use `XTId`, not `XId`: after type checking `XId` carries a type, `XTId` does not. `Examples-Class/` now holds only `Functor.sparcl`, which does not parse, since type-variable application `f a` is not supported by the type parser (higher-kinded classes are future work).
- Design notes: `notes/extend-type-class_2026.txt`.
