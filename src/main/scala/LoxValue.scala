opaque type LoxWrapper[A] = A
type LoxPrimitive = LoxWrapper[Boolean | String | Double]

object LoxWrapper:
  def apply[A](value: A): LoxWrapper[A] = value
  extension (d: LoxWrapper[Double]) def -(other: LoxWrapper[Double]) = d - other

// Convenience trait for marking valid runtime lox values
trait LoxRoot

// A type encompassing any valid lox value (prevents inadvertent insertion of raw compiler types into the runtime environment)
type LoxValue = LoxPrimitive | LoxWrapper[Any] | LoxRoot
