DepTrack
========

DepTrack is a concise DSL to express dependencies between objects.

One goal of DepTrack is to leverage the Haskell type system to reap most of the
benefits:
* write your dependencies using a concise DSL, with no accidental complexity
  (e.g., type inference)
* catch typos early, not when generating a 1000-nodes graph from a set of YAML
  files

# Motivation

The concept of dependencies arise naturally when building and operating
systems. Unfortunately, tracking dependencies between components of a system is
one of these easy but daunting task.

There exist systems such as build-systems, package managers, and configuration
management systems to express dependencies between components. Theses systems
focus on particular business requirements and have an opinionated idea of what
is a dependency. These systems live aside your business model, you may not want
to attach an action to check for dependencies or build the missing ones.

We wanted to design something as good as a Graph editor. The dot language from
Graphviz is a good start but unfortunately a bit alien to our day-to-day code:
the dot language is a perfect target for outputing graphs but not a great input
if you want to attach behaviors to dependencies. Therefore, we built DepTrack,
a library to track dependencies (which you tailor to your business
requirements) between objects (which can be represent files, software
components, pieces of hardware or an heterogeneous mix of these).

# Installation

This project is cabalized but not yet published on hackage. You can install it
from source like any other cabal project.

# Design

DepTrack uses an applicative functor to carry out a side-effect (recording
dependency links) while computing a value (an object you may be interested in).
Sometimes, the only interesting result is the dependency graph from the
side-effect.

The type of dependency to track is a parameter and may depend on your business
requirements. For instance, dependencies could be servers that must be running
for your application to operate. In this example, we probably want to attach a
"check" function to each dependency. This way we get a troubleshooting plan
when things go wrong. In other situation, you may be interested in the
dependency graph for documentation purposes only.

## Implementation

The applicative functor is implemented using the Ap free-applicative from the
`free` package. 

Dependencies of type `a` are encoded in a `Data.Tree (Maybe a)`. The children
of a node are the node's direct dependencies.

The `evalTree` function evaluates a tree where node may appear multiple times.
For instance, if A and B both depends on C, you will see two copies of the node
C in the branch for A dependencies and in the branch for B dependencies.

Performance are not the most critical goal for DepTrack and DepTrack may be
overly lazy. If you run into situations where DepTrack performance are poor,
please file a bug report (GitHub issue).

## Known limitations

* DepTrack cannot directly express "alternate paths" (e.g. "A" depends on "B OR
  C").  A possible workaround is to enrich the dependency type.
* The `evalTree` computation must terminate (especially, we cannot express a
  lazily-computed infinite list of dependencies.)
* DepTrack cannot encode loops. You should not have loops in your design,
  however we may still want to let people represent them. DepTrack needs some
  primitive to stop iterating on a dependency when an already-explored node
  arrives.

# Usage

The intended workflow for DepTrack is as follows:

```
a) describe what is a dependency (e.g., hardware/software/an action to check if present/alive)
b) write `data` definitions for a small component or a combination of components
c) wrap constructors with the `pure` and `nest` functions
d) print the graph of the component
e) goto b
```

See the example in the example/Car.hs for a result. This example takes a
fictional example of a "Car" which has a brand, a type of engine, and four
wheels. Therefore, we want to provide a nice DSL to build Cars and track their
dependencies with DepTrack. Existing "combinators" from Data.Traversable and
Control.Applicative let you write type-checked helpers.

We plan to record a video to explain how we leverage DepTrack and the Haskell
type system to iteratively grow an accurate representations of a real-world
dependency tracking problem.

# TODO
* implement designs for Monad, Alternative, and MonadPlus versions
* generate a Data.Graph from the Data.Tree
