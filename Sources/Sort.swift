//
//  Sort.swift
//  Valence
//
//  Created by Robert Widmann on 9/4/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

/// A `Sort` is a classifier for related syntax.  In ASTs a `Sort` acts as a
/// discriminator for e.g. types, values, expressions, contstants.  Because this
/// library supports multi-sorted ABTs, those different kinds of syntax can
/// coexist in the same ABT.
public protocol Sort : Equatable, CustomStringConvertible { }
