//
//  Calculator.swift
//  Valence
//
//  Created by Robert Widmann on 8/30/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

import Valence
	
fileprivate enum CalculatorSort : Sort {
	case TERM
	
	var description : String {
		return "term"
	}
	
	static func ==(l : CalculatorSort, r : CalculatorSort) -> Bool {
		return true
	}
}

fileprivate enum CalculatorOperator : Operator {
	typealias OperatorSort = CalculatorSort

	case Num(Int)
	case Var(String)
	case Plus
	case Times
	case Cat
	case Len
	case Let
	
	var arity : Arity<CalculatorSort> {
		switch self {
		case .Num(_):
			return Arity([], .TERM)
		case .Var(_):
			return Arity([], .TERM)
		case .Plus:
			return Arity([
				Valence([], [], .TERM),
				Valence([], [], .TERM),
			], .TERM)
		case .Times:
			return Arity([
				Valence([], [], .TERM),
				Valence([], [], .TERM),
			], .TERM)
		case .Cat:
			return Arity([
				Valence([], [], .TERM),
				Valence([], [], .TERM),
			], .TERM)
		case .Len:
			return Arity([
				Valence([], [], .TERM),
			], .TERM)
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
		case let .Var(s):
			return s
		case .Plus:
			return "plus"
		case .Times:
			return "times"
		case .Cat:
			return "cat"
		case .Len:
			return "len"
		case .Let:
			return "let"
		}
	}
	
	static func ==(l : CalculatorOperator, r : CalculatorOperator) -> Bool {
		switch (l, r) {
		case let (.Num(i), .Num(j)):
			return i == j
		case let (.Var(s), .Var(t)):
			return s == t
		case (.Plus, .Plus):
			return true
		case (.Times, .Times):
			return true
		case (.Cat, .Cat):
			return true
		case (.Len, .Len):
			return true
		case (.Let, .Let):
			return true
		default:
			return false
		}
	}
}

fileprivate typealias CalculatorTerm = AbstractBindingTree<Named, Named, Named, CalculatorOperator, ()>

fileprivate func eval(_ c : CalculatorTerm) -> CalculatorTerm {
	var ct = OperationalSemantics.Closure(c, [:])
	while case let .STEP(c1) = ct.step {
		ct = c1
	}
	return ct.term
}

fileprivate indirect enum OperationalSemantics {
	struct Closure {
		let term : CalculatorTerm
		let env  : Dictionary<CalculatorTerm.ABTVariable, Closure>
		
		init(_ term : CalculatorTerm, _ env : Dictionary<CalculatorTerm.ABTVariable, Closure>) {
			self.term = term
			self.env = env
		}
	
		var viewBinding : (CalculatorOperator, [CalculatorTerm.ABTBView]) {
			switch self.term.toView {
			case let .Apply(theta, es):
				return (theta, es)
			default:
				fatalError("Expected operator")
			}
		}
		
		var subterms : [CalculatorTerm.ABTBView] {
			return self.viewBinding.1
		}
		
		func withArgument(_ idx : Int, _ f : (CalculatorTerm.ABTView) -> OperationalSemantics) -> OperationalSemantics {
			let (theta, es) = self.viewBinding
			guard case let .Binding(us, xs, n) = es[idx] else {
				fatalError("Wat?")
			}
			
			switch Closure(n, self.env).step {
			case .VAL:
				let v = n.toView
				switch v {
				case .Apply(_, _):
					return f(v)
				default:
					fatalError("Expected operator")
				}
			case let .STEP(c2):
				var a2 = es
				a2[idx] = .Binding(us, xs, c2.term)
				return .STEP(Closure(CalculatorTerm(checking: .Apply(theta, a2), self.term.sort), c2.env))
			}
		}
		
		func withArguments(_ idx1 : Int, _ idx2 : Int, _ f : (CalculatorTerm.ABTView, CalculatorTerm.ABTView) -> OperationalSemantics) -> OperationalSemantics {
			let (theta, es) = self.viewBinding
			guard case let .Binding(us1, xs1, n1) = es[idx1] else {
				fatalError("Wat?")
			}
			
			guard case let .Binding(us2, xs2, n2) = es[idx2] else {
				fatalError("Wat?")
			}
			
			switch (Closure(n1, self.env).step, Closure(n2, self.env).step) {
			case (.VAL, .VAL):
				let (v1, v2) = (n1.toView, n2.toView)
				switch (v1, v2) {
				case (.Apply(_, _), .Apply(_, _)):
					return f(v1, v2)
				default:
					fatalError("Expected operator")
				}
			case let (.STEP(c2), .STEP(c3)):
				var args = es
				args[idx1] = .Binding(us1, xs1, c2.term)
				args[idx2] = .Binding(us2, xs2, c2.term)
				return .STEP(Closure(CalculatorTerm(checking: .Apply(theta, args), self.term.sort), c2.env.merge(c3.env)))
			default:
				fatalError()
			}
		}
		
		var step : OperationalSemantics {
			switch self.term.toView {
			case let .Variable(x):
				return .STEP(self.env[x]!)
			case .MetaApply(_, _, _):
				fatalError("Unexpected Meta")
			case let .Apply(theta, aa):
				switch theta {
				case .Num(_):
					return .VAL
				case .Var(_):
					return .VAL
				case .Plus:
					return withArguments(0, 1) { l, r in
						switch (l, r) {
						case let (.Apply(.Num(i), _), .Apply(.Num(j), _)):
							return .STEP(Closure(CalculatorTerm(checking: .Apply(.Num(i + j), []), .TERM), self.env))
						default:
							fatalError()
						}
					}
				case .Times:
					return withArguments(0, 1) { l, r in
						switch (l, r) {
						case let (.Apply(.Num(i), _), .Apply(.Num(j), _)):
							return .STEP(Closure(CalculatorTerm(checking: .Apply(.Num(i * j), []), .TERM), self.env))
						default:
							fatalError()
						}
					}
				case .Cat:
					return withArguments(0, 1) { l, r in
						switch (l, r) {
						case let (.Apply(.Var(i), _), .Apply(.Var(j), _)):
							return .STEP(Closure(CalculatorTerm(checking: .Apply(.Var(i + j), []), .TERM), self.env))
						default:
							fatalError()
						}
					}
				case .Len:
					return withArgument(0) { l in
						switch l {
						case let .Apply(.Var(i), _):
							return .STEP(Closure(CalculatorTerm(checking: .Apply(.Num(i.characters.count), []), .TERM), self.env))
						default:
							fatalError()
						}
					}
				case .Let:
					guard case let .Binding(_, xs, mx) = aa.first! else {
						fatalError("Wat?")
					}
					guard case let .Binding(_, _, n) = self.subterms[1] else {
						fatalError("Wat?")
					}
					return .STEP(Closure(mx, self.env.insert(xs.first!, v: Closure(n, self.env))))
				}
			}
		}
	}
		
	case VAL // Canonical forms
	case STEP(Closure) // One step of evaluation
}

import XCTest

class CalculatorTests : XCTestCase {
	func testThings() {
		let expr : CalculatorTerm.ABTView = .Apply(.Plus, [
			.Binding([], [], CalculatorTerm(applying: .Num(1),  to:  [])),
			.Binding([], [], CalculatorTerm(applying: .Num(2),  to:  [])),
		])
		let ev = eval(CalculatorTerm(checking: expr, .TERM))
		print(ev)
	}
}

