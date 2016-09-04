//
//  Detritus.swift
//  Valence
//
//  Created by Robert Widmann on 8/16/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

enum Array1View<T> { case E(T) }
enum Array2View<T> { case E(T, T) }

extension Array {
	var match1 : Array1View<Element> {
		precondition(self.count == 1)
		return .E(self[0])
	}
	
	var match2 : Array2View<Element> {
		precondition(self.count == 2)
		return .E(self[0], self[1])
	}
}
