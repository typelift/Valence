//
//  Naming.swift
//  Valence
//
//  Created by Robert Widmann on 4/2/16.
//  Copyright © 2016 CodaFi. All rights reserved.
//

/*
/// Nameless variables ["de Bruijn indices"]
public struct Nameless : Symbol {
	private let unVariable : UInt
	
	public init() {
		self.unVariable = Nameless.freshVariable()
	}
	
	private init(copying : UInt) {
		self.unVariable = copying
	}
	
	public var fresh : Nameless {
		return Nameless()
	}
	
	public var copy : Nameless {
		return Nameless(copying: self.unVariable)
	}
	
	public var hashValue : Int {
		return Int(self.unVariable)
	}
	
	public var description : String {
		return String(self.unVariable)
	}
	
	static var _varPool : UInt = 0
	
	static func freshVariable() -> UInt {
		_varPool += 1;
		return _varPool
	}
}

public func <=(lhs : Nameless, rhs : Nameless) -> Bool {
	return lhs.unVariable <= rhs.unVariable
}

public func >=(lhs : Nameless, rhs : Nameless) -> Bool {
	return lhs.unVariable >= rhs.unVariable
}

public func >(lhs : Nameless, rhs : Nameless) -> Bool {
	return lhs.unVariable > rhs.unVariable
}

public func <(lhs : Nameless, rhs : Nameless) -> Bool {
	return lhs.unVariable < rhs.unVariable
}

public func ==(lhs : Nameless, rhs : Nameless) -> Bool {
	return lhs.unVariable == rhs.unVariable
}*/

/// Named variables; Reference equality is needed because strings have value semantics.
public struct Named : Symbol {
	private let unVariable : String
	public typealias StringLiteralType = String
	
	public init(unicodeScalarLiteral value: UnicodeScalar) {
		self.unVariable = String(value)
	}
	
	public init(extendedGraphemeClusterLiteral value: String) {
		self.unVariable = value
	}
	
	public init(stringLiteral value: String) {
		self.unVariable = value
	}
	
	public init(_ copying : String) {
		self.unVariable = copying
	}
	
	public var fresh : Named {
		return Named(self.unVariable + "'")
	}
	
	public init<A>(fresh ctx : [Named: A], _ s : String) { 
		self.unVariable = s
	}

	public var hashValue : Int {
		return self.unVariable.hashValue
	}
	
	public var description : String {
		return self.unVariable
	}

	public static func <(lhs : Named, rhs : Named) -> Bool {
		return lhs.unVariable < rhs.unVariable
	}

	public static func ==(lhs : Named, rhs : Named) -> Bool {
		return lhs.unVariable == rhs.unVariable
	}
}

