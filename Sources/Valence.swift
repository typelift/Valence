//
//  Valence.swift
//  Valence
//
//  Created by Robert Widmann on 9/3/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// A `Valence` is an expression of sort `S` that binds symbols and variables
/// around an operator.
public struct Valence<S : Sort> : Equatable, CustomStringConvertible {
	let symSorts : [S]
	let varSorts : [S]
	let sort : S

	public init(_ symSorts : [S], _ varSorts : [S], _ sort : S) {
		self.symSorts = symSorts
		self.varSorts = varSorts
		self.sort = sort
	}

	public var description : String {
		return "{\(self.symSorts)}{\(self.varSorts)}"
	}
}

public func == <S : Sort>(l : Valence<S>, r : Valence<S>) -> Bool {
	return l.symSorts == r.symSorts
		&& l.varSorts == r.varSorts
		&& l.sort == r.sort
}
