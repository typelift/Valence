//
//  Operator.swift
//  Valence
//
//  Created by Robert Widmann on 9/3/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// An `Operator` is the principle kind of data carried in an ABT.  They delimit
/// the kinds of operations supported by a particular language or grammar and
/// capture the binding and scope of variables relevant to each operation.
///
/// An Operator can also be thought of as a local map from an operation kind to
/// an *arity* for that operation that generalizes the natural-number arity of
/// ASTs to structures capturing the valence of arguments.
public protocol Operator : Equatable, CustomStringConvertible {
	associatedtype OperatorSort : Sort
	associatedtype OperatorIndex = LocallyNameless<Named>

	var arity : Arity<OperatorSort> { get }
	var support : [(Self.OperatorIndex, Self.OperatorSort)] { get }
}

extension Operator {
	public var support : [(Self.OperatorIndex, Self.OperatorSort)] {
		return []
	}
}
