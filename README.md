 [![Build Status](https://travis-ci.org/typelift/Valence.svg?branch=master)](https://travis-ci.org/typelift/Valence)

# Valence

Abstract Binding Trees for designing programming languages in Swift.

The implementation is heavily inspired by Jon Sterling et al.'s 
[ABT library](https://github.com/RedPRL/sml-typed-abts) that powers, among other
things, the [JonPRL](https://github.com/RedPRL/sml-redprl) language.

# Introduction

One way of thinking about an ABT is that it is an AST that is aware of binding
structure.  Another would be that it is a tree representation of binding-aware
abstract syntactic structure.

In addition to the standard structural and organizational advantanges, ABTs 
manage the ins and outs of keeping track of variable bindings and scopes through
a system of arities [à la Martin
Löf](http://www.cse.chalmers.se/research/group/logic/book/book.pdf).  This
particular implementation also handles the checking of symbol sorts
automatically and hence allows for multiple kinds of syntax to coexist in
the binding structure.

# Working With ABTs

We'll begin by defining the binding structure of a simple calculator language.  The
first thing to define are the `Sort`s of terms that can be constructed.  Here, because
we don't make a distinction between, say, types and terms, we only have one sort: `TERM`s.

```swift
import Valence

enum CalculatorSort : Sort {
	case TERM
	
	var description : String {
		return "term"
	}
	
	static func ==(l : CalculatorSort, r : CalculatorSort) -> Bool {
		return true
	}
}
```

Next we'll define the set of operators available to the calculator.  Operators
are tagged with a sort for organizational purposes and can calculate an arity
per operation that describes its binding structure.

```swift
enum CalculatorOperator : Operator {
	typealias OperatorSort = CalculatorSort

	case Num(Int)
	case Plus
	case Times
	case Let

	// An `Arity` specifies an operator of sort `S` with 
	// arguments of valences.
	//
	// A `Valence` is an expression of sort `S` that binds 
	// symbols and variables around an operator.
	var arity : Arity<CalculatorSort> {
		switch self {
		// Numbers
		case .Num(_):
			// Constants are operators that take no arguments.
			return Arity([], .TERM)
		// `+` has two arguments.
		case .Plus:
			return Arity([
				// The LHS
				Valence([], [], .TERM),
				// The RHS
				Valence([], [], .TERM),
			], .TERM)
		// `*` has two arguments.
		case .Times:
			return Arity([
				// The LHS
				Valence([], [], .TERM),
				// The RHS
				Valence([], [], .TERM),
			], .TERM)
		// `let` binds a single argument 
		case .Let:
			return Arity([
				Valence([], [.TERM], .TERM),
			], .TERM)
		}
	}

	var description : String {
		switch self {
		case let .Num(i):
			return "\(i)"
		case .Plus:
			return "plus"
		case .Times:
			return "times"
		case .Let:
			return "let"
		}
	}
	
	static func ==(l : CalculatorOperator, r : CalculatorOperator) -> Bool {
		switch (l, r) {
		case let (.Num(i), .Num(j)):
			return i == j
		case (.Plus, .Plus):
			return true
		case (.Times, .Times):
			return true
		case (.Let, .Let):
			return true
		default:
			return false
		}
	}
}
```

And we're done!

```swift
typealias CalculatorTerm = AbstractBindingTree<Named, Named, Named, CalculatorOperator, ()>
```

# System Requirements

Valence supports OS X 10.9+, iOS 8.0+, tvOS 9.0+, and watchOS 2.0+.

# License

Valence is released under an MIT license.

