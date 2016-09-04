//
//  Views.swift
//  Valence
//
//  Created by Robert Widmann on 9/3/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// A pattern-match-enabled view of an annotated `AbstractSyntaxTree`.
public enum View<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>
	where O.I == LocallyNameless<S>
{
	/// A variable binding.
	case Variable(V)
	/// An operator applied to a series of bindings.
	case Apply(O, [BindingView<V, S, M, O, A>])
	/// A metavariable applied to some series of symbols and terms.
	case MetaApply(M, [(S, O.S)], [AbstractBindingTree<V, S, M, O, A>])
}

/// A pattern-match-enabled view of a term with access to a sequence of bound 
/// symbols and variables and the body (rest of the syntax tree).
public enum BindingView<V : Symbol, S : Symbol, M : Symbol, O : Operator, A> : CustomStringConvertible
	where O.I == LocallyNameless<S>
{
	case Binding([S], [V], AbstractBindingTree<V, S, M, O, A>)

	/// Modifies a binding by applying a function to its body.
	public func map(_ f : (AbstractBindingTree<V, S, M, O, A>) -> AbstractBindingTree<V, S, M, O, A>) -> BindingView<V, S, M, O, A> {
		switch self {
		case let .Binding(s, v, m):
			return .Binding(s, v, f(m))
		}
	}

	/// The sequence of bound symbols captured in this abstraction.
	public var symbols : [S] {
		switch self {
		case let .Binding(s, _, _):
			return s
		}
	}

	/// The sequence of bound variables captured in this abstraction.
	public var variables : [V] {
		switch self {
		case let .Binding(_, v, _):
			return v
		}
	}

	/// The body of the abstraction.
	public var body : AbstractBindingTree<V, S, M, O, A> {
		switch self {
		case let .Binding(_, _, abt):
			return abt
		}
	}

	public var description : String {
		switch self {
		case let .Binding(us, xs, m):
			return us.map({ $0.description }).joined(separator: ",")
				+ xs.map({ $0.description }).joined(separator: ",")
				+ ".\(m.description)"
		}
	}
}
