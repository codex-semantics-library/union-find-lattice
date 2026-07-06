# Union-Find Lattice

<!-- LTeX: language=en-US -->

[![Latest Version](https://img.shields.io/github/v/release/codex-semantics-library/union-find-lattice)](https://github.com/codex-semantics-library/union-find-lattice/releases)
[![OCaml Version](https://img.shields.io/badge/OCaml-4.14_--_5.5-blue?logo=ocaml&logoColor=white)](https://github.com/codex-semantics-library/union-find-lattice/blob/main/dune-project)
[![GitHub License](https://img.shields.io/github/license/codex-semantics-library/union-find-lattice)](https://github.com/codex-semantics-library/union-find-lattice/blob/main/LICENSE)
[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/codex-semantics-library/union-find-lattice/ocaml.yml)](https://github.com/codex-semantics-library/union-find-lattice/actions/workflows/ocaml.yml)
[![Documentation](https://img.shields.io/website?url=https%3A%2F%2Fcodex.top%2Fapi%2Funion-find-lattice%2F&up_message=online&down_message=offline&label=documentation)](https://codex.top/api/union-find-lattice/)

This package centers around the `union-find-lattice` library, whose root module is
`Union_Find_Lattice`. It extends the classic union-find structures with:

- A persistent `union: t -> node -> node -> t`
  which returns a new copy instead of modifying the source
- Lattice operations:
  - `join: t -> t -> t`, which
       is the union-find containing only the equalities that are true in both arguments;
  - `meet: t -> t -> t`, which
       is the union-find containing the equalities that are true in either argument;
  - `incl: t -> t -> bool`, which
       checks if all equalities from the left argument hold in the right argument;

These lattice operations and the algorithms implementing the are described by
Lesbre and Lemerre, *A Lattice of Union-Finds*, SAS 2026, specifically,
the difference-based lattice operations using union-by-rank.

This library was originally written by Dorian Lesbre and Matthieu Lemerre.
Copyright (C) 2026 CEA (Commissariat à l'énergie atomique et aux énergies
alternatives). It is provided here under a [LGPL v2.1 license](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html).

**Contents:**
<!-- TOC -->
- [Data-structures](#data-structures)
- [Variants](#variants)
- [Example usage](#example-usage)
  - [Installation](#installation)
  - [Example](#example)
- [Other included libraries](#other-included-libraries)
<!-- /TOC -->


## Data-structures

Each variant proposes four different data-structures to represent the union-find lattice:

- Patricia trees use functional maps
  and thus pay logarithmic cost for `find`
  and `union`, but are truly
  immutable and are thus the most flexible and easy to use.
- Persistent arrays are faster when
  repeatedly accessing one version, but pay a rerooting cost when switching (i.e.
  calling any function) on another version.
  - They are also limited to lattice operations between versions deriving from
     the same common ancestor.
  - As they are not truly persistent, they **do not support unsynchronized access**.

- `PersistentArrayNCA` is a variant
  of persistent array that adds a version tag to the arrays. It allows
  `join` to build the result on
  the nearest common ancestor, rather than one of the arguments. This takes a bit more memory
  but should result in shorter version chains when stacking joins on top of joins.
- `ArrayWithCopy` uses a standard
  union-find implementation based on a single mutable array. It is **NOT persistent**,
  but provides an explicit `copy`
  function. It uses linear lattice operations rather than the difference based one,
  which can be faster when the difference (i.e. the number of changes between both versions)
  is comparable to the number of nodes.


## Variants

In addition to the `Union_Find_Lattice.Classic` implementations, we provide several variants

- `Union_Find_Lattice.Valued` extend union-find by attaching a `Value`
  to each equivalence class. These values also have a lattice structure.
  As an example, if union-find is representing a set of equalities between terms,
  these values can represent the set of possible values for each term equivalence class.
- `Union_Find_Lattice.Labeled` extend union-find by attaching a `relation`
  to each link between a node and its parents. These relations have a group structure.
  They allow representing more complex relations than equality, for instance,
  equality up to a constant.
  See [Lesbre et al, *Relational Abstractions based on Labeled Union-Find*, PLDI 2024](https://dl.acm.org/doi/10.1145/3729298)
  for an in-depth description of labeled union-find.
- `Union_Find_Lattice.Polymorphic` is the same as `Union_Find_Lattice.Labeled`,
  but replaces the monomorphic types `node` and `relation` by polymorphic ones
  (`'a node` and `('a, 'b) relation`
- `Union_Find_Lattice.LabeledValued` and `Union_Find_Lattice.PolymorphicValued`
  combine values and labels.


## Example usage

### Installation

To use the library, download the package with [opam](https://opam.ocaml.org/):

```bash
opam install union-find-lattice
```

Alternatively, clone the repository on [github](https://github.com/codex-semantics-library/union-find-lattice),
install dependencies and build locally:

```bash
git clone git@github.com:codex-semantics-library/union-find-lattice.git
cd union-find-lattice
opan install . --deps-only
dune build -p union-find-lattice
dune install -p union-find-lattice
# To build tests and benchmarks
opan install . --deps-only --with-test --with-dev-setup
dune build
# To build documentation
opam install . --deps-only --with-doc
dune build @doc
```

Next add the library as a dependency in your `dune` files:

```dune
(executable ; or library
  ...
  (libraries union-find-lattice ...)
)
```

### Example

Here is a minimal example of a union-find join with integer nodes

```ocaml
module UF = Union_Find_Lattice.Classic.PatriciaTree
  (Union_Find_Lattice.DefaultConfig)
  (struct
    include Int
    let to_int x = x
    let pretty = Format.pp_print_int
  end)

let root = UF.make 0 (* the number passed to make only matters for arrays *)

let left = UF.union (UF.union root 0 1) 1 3 (* one class: {0,1,3} *)
let right = UF.union (UF.union root 0 2) 2 3 (* one class: {0,2,3} *)

let join = UF.join left right (* one class: {0,3} *)
let meet = UF.meet left right (* one class: {0,1,2,3} *)
```

```ocaml
# UF.check_related join 0 3;;
- : bool = true

# UF.check_related join 0 1;;
- : bool = false

# UF.check_related meet 1 2 (* meet creates new equalities by transitivity *);;
- : bool = true

# UF.incl meet left && UF.incl meet right && UF.incl left join && UF.incl right join
  (* join is the least upper bound, meet the greatest lower bound *);;
- : bool = true
```

## Other included libraries

This package also includes the following libraries:

- `union-find-lattice.persistent-array`, with root module `PersistentArray`, the persistent array
  structure used. It is very close to the one described by
  [Conchon and Filliâtre, *A Persistent Union-Find Data Structure*, ML 2007](https://dl.acm.org/doi/10.1145/1292535.1292541),
  with the following differences:
  - we added our difference operator: `PersistentArray.S.diff` and `PersistentArray.S.diff_key`;
  - we added a variant, `PersistentArray.Versioned`, which add a version tag to arrays,
      allowing `diff` to return the nearest common ancestor;
  - we made our arrays extendable, similar to dynamic arrays/vectors.
     See `PersistentArray.S.append` and `PersistentArray.S.extend`

- `union-find-lattice.utils`, with root module `Utils`. Undocumented internal utilities
