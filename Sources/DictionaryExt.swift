//
//  DictionaryExt.swift
//  Valence
//
//  Created by Robert Widmann on 4/2/16.
//  Copyright © 2016 CodaFi. All rights reserved.
//

extension Dictionary {
	/// Initialize a Dictionary from a list of Key-Value pairs.
	public init<S : Sequence>(_ seq : S)
		where S.Iterator.Element == Element
	{
		self.init()
		for (k, v) in seq {
			self[k] = v
		}
	}

	/// MARK: Query

	/// Returns whether the given key exists in the receiver.
	public func isMember(_ k : Key) -> Bool {
		return self[k] != nil
	}

	/// Returns whether the given key does not exist in the receiver.
	public func notMember(_ k : Key) -> Bool {
		return !self.isMember(k)
	}

	/// Looks up a value in the receiver.  If one is not found the default is used.
	public func lookup(_ k : Key, def : Value) -> Value {
		return self[k] ?? def
	}

	/// Returns a copy of the receiver with the given key associated with the given value.
	public func insert(_ k : Key, v : Value) -> [Key: Value] {
		var d = self
		d[k] = v
		return d
	}

	/// Returns a copy of the receiver with the given key disassociated from its value.
	public func remove(_ k : Key) -> [Key: Value] {
		var d = self
		d[k] = nil
		return d
	}

	/// MARK: Map

	/// Maps a function over all values in the receiver.
	public func map<Value2>(_ f : (Value) -> Value2) -> [Key: Value2] {
		return self.mapWithKey { (_, x) -> Value2 in
			return f(x)
		}
	}

	/// Maps a function over all keys and values in the receiver.
	public func mapWithKey<Value2>(_ f : (Key, Value) -> Value2) -> [Key: Value2] {
		var d = [Key: Value2]()
		self.forEach { (k, v) in
			d[k] = f(k, v)
		}
		return d
	}

	public func merge(_ right: [Key: Value]) -> [Key: Value] {
		var leftc = self
		for (k, v) in right {
			leftc.updateValue(v, forKey: k)
		}
		return leftc
	}
}

func mapEq<A, B, C>(_ f : (A, B) -> C, _ ass : [A], _ bss : [B]) -> [C] {
	return zip(ass, bss).map(f)
}


