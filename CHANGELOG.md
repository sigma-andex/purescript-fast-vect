# Changelog

## Unreleased
- Add new instances for `Vect` and `Matrix`:
	- `Bind`
	- `Monad`
	- `Traversable1`
	- `Distributive`
	- `Semigroup`
	- `Monoid`
	- `Semiring`
	- `Ring`
	- `CommutativeRing`
- Add `last` function for `Vect`

## [v1.0.0]
- Add support for Matrices and `mapWithTerm` & `generate` for `Vect` (#16 by @yukikurage)
- Add Foldable1 instance (#13 by @JamieBallingall)
- Clean up code and readme (#15 & #12 by @JamieBallingall)

## [v0.7.0]
- Add read & write optimised sparse vectors (#10 by @mikesol)

## [v0.6.0]
- Add `indexModulo`, `reifyVect`. Add additional typeclass instances. Simplify `head`. Format code. Upgrade to latest package-set. (#9 by @mikesol)

## [v0.5.0]

- Update to Purescript v0.15.0
- Use typelevel ints instead of symbols
