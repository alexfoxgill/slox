// A LoxValue is anything that can be referenced in a Lox program
type LoxValue = LoxWrapper[Any] | LoxRoot

// Opaque wrapper around non-Lox values
opaque type LoxWrapper[A] = A
type LoxPrimitive = LoxWrapper[Boolean | String | Double]

object LoxWrapper:
  def apply[A](value: A): LoxWrapper[A] = value

// Convenience trait for marking valid runtime lox values
trait LoxRoot
