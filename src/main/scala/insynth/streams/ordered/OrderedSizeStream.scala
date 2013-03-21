package insynth.streams.ordered

import insynth.streams.{ Streamable, Valuable }

trait OrderedSizeStreamable[T] extends Streamable[T] with Valuable[Int]