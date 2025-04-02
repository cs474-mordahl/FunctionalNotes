package edu.uic.cs474.spring25.inclass.functional
package monoid

import semigroup.*

trait CommutativeMonoid[T] extends Monoid[T], CommutativeSemigroup[T]
