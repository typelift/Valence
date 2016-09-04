//
//  Arity.swift
//  Valence
//
//  Created by Robert Widmann on 9/3/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// An `Arity` specifies an operator of sort `S` with arguments of valences.
public struct Arity<S : Sort> : Equatable, CustomStringConvertible {
	let sort : S
	let valences : [Valence<S>]

	public init(_ valences : [Valence<S>], _ sort : S) {
		self.valences = valences
		self.sort = sort
	}

	public var description : String {
		return "(\(self.valences))"
	}
}

public func == <S : Sort>(l : Arity<S>, r : Arity<S>) -> Bool {
	return l.valences == r.valences
		&& l.sort == r.sort
}
