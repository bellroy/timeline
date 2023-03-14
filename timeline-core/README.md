# timeline

## Motivation

The world is always changing, and often we want to manage the changes of data
using computers. Below are some concrete examples:

- Employee data such as compensation, city, tax rule, time-off, etc.
- Prices of products. A product could have different prices on Amazon and EBay,
  and in different currencies.

Timeline data is often implemented by attaching extra fields to your business
object, denoting the start and end time of each interval. However, only
representing and storing the data is not sufficient, we need to run operations
on timeline data, like extracting a single data point at some specific time,
merging multiple timelines together, etc.

If you have a similar use case and don't want to reinvent the wheel, this
library is for you.

## Package Organization

- `timeline` essential types and functions
- `timeline-tests` unit tests
- `timeline-hedgehog` hedgehog generators for timeline types

## Getting Started

The core type is `Timeline a`, refer to
[Haddock](https://hackage.haskell.org/package/timeline-0.0.1.0/docs/Data-Timeline.html)
for its usage.

## Contribution
Bellroy actively maintains this project. Feel free to submit issues and
pull requests!

The code is formatted with [`ormolu`](https://hackage.haskell.org/package/ormolu)

If you use Nix:
- `nix develop` enter a shell with all necessary tools
- `nix build` build and run tests on all GHC versions we support
- Use `nix flake show` to view a full list of outputs
