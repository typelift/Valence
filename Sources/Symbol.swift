//
//  Symbol.swift
//  Valence
//
//  Created by Robert Widmann on 9/3/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// A variable is a fixed term.  ABTs require variables have a "freshness 
/// guarantee".  That is, newly created variables are wholly distinct from 
/// previously created variables.
public protocol Symbol : Equatable, Comparable, Hashable, CustomStringConvertible, ExpressibleByStringLiteral {
	init<A>(fresh ctx : [Self: A], _ s : String)
	init(_ named : String)
}
