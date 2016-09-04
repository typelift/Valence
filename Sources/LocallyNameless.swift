//
//  LocallyNameless.swift
//  Valence
//
//  Created by Robert Widmann on 4/7/16.
//  Copyright © 2016 CodaFi. All rights reserved.
//

/// The type of names of terms in a locally nameless representation.
///
/// Two names in a term are considered equal when they are the same name. In
/// patterns, however, any two names are equal if they occur in the same place 
/// within the pattern. This induces alpha equivalence on terms in general.
///
/// Names may either be free or bound. Free names may be extracted from patterns
/// using `getFree`. Bound names cannot be.
public enum LocallyNameless<A> : Hashable, CustomStringConvertible {
	case Free(A)
	case Bound(Coordinate)
	
	public var description : String {
		switch self {
		case let .Free(v):
			return "\(v)"
		case let .Bound(c):
			return "\(c)"
		}
	}

	public var hashValue : Int {
		return self.description.hashValue
	}

	public func map<B>(_ f : (A) -> B) -> LocallyNameless<B> {
		switch self {
		case let .Free(v):
			return .Free(f(v))
		case let .Bound(c):
			return .Bound(c)
		}
	}
	
	public static func pure(_ v : A) -> LocallyNameless<A> {
		return .Free(v)
	}
	
	public func flatMap<B>(_ f : (A) -> LocallyNameless<B>) -> LocallyNameless<B> {
		switch self {
		case let .Free(v):
			return f(v)
		case let .Bound(c):
			return .Bound(c)
		}
	}
	
	public var getFree : A {
		switch self {
		case let .Free(v):
			return v
		case .Bound(_):
			fatalError("Unexpectedly found bound variable.")
		}
	}
}

public func == <A>(l : LocallyNameless<A>, r : LocallyNameless<A>) -> Bool {
	switch (l, r) {
	case let (.Bound(i), .Bound(j)):
		return i == j
	default:
		return false
	}
}

public struct Coordinate : Equatable {
	private let coord : (Int, Int)
	
	public init() { self.coord = (0, 0) }
	public init(_ c : (Int, Int)) { self.coord = c }
	
	public var shiftRight : Coordinate {
		let (i, j) = self.coord
		return Coordinate((i, j + 1))
	}
	
	public var shiftDown : Coordinate {
		let (i, j) = self.coord
		return Coordinate((i + 1, j))
	}

	public static func == (l : Coordinate, r : Coordinate) -> Bool {
		return l.coord == r.coord
	}
}
