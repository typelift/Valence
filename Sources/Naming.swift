//
//  Naming.swift
//  Valence
//
//  Created by Robert Widmann on 4/2/16.
//  Copyright © 2016 CodaFi. All rights reserved.
//

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

