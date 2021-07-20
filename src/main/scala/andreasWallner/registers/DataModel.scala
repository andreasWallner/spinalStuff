package andreasWallner.registers

import spinal.core._

sealed trait AccessType

object AccessType {
  case object RO extends AccessType //- W: no effect, R: no effect
  case object RW extends AccessType //- W: as-is, R: no effect
  case object RC extends AccessType //- W: no effect, R: clears all bits
  case object RS extends AccessType //- W: no effect, R: sets all bits
  case object WRC extends AccessType //- W: as-is, R: clears all bits
  case object WRS extends AccessType //- W: as-is, R: sets all bits
  case object WC extends AccessType //- W: clears all bits, R: no effect
  case object WS extends AccessType //- W: sets all bits, R: no effect
  case object WSRC extends AccessType //- W: sets all bits, R: clears all bits
  case object WCRS extends AccessType //- W: clears all bits, R: sets all bits
  case object W1C extends AccessType //- W: 1/0 clears/no effect on matching bit, R: no effect
  case object W1S extends AccessType //- W: 1/0 sets/no effect on matching bit, R: no effect
  case object W1T extends AccessType //- W: 1/0 toggles/no effect on matching bit, R: no effect
  case object W0C extends AccessType //- W: 1/0 no effect on/clears matching bit, R: no effect
  case object W0S extends AccessType //- W: 1/0 no effect on/sets matching bit, R: no effect
  case object W0T extends AccessType //- W: 1/0 no effect on/toggles matching bit, R: no effect
  case object W1SRC extends AccessType //- W: 1/0 sets/no effect on matching bit, R: clears all bits
  case object W1CRS extends AccessType //- W: 1/0 clears/no effect on matching bit, R: sets all bits
  case object W0SRC extends AccessType //- W: 1/0 no effect on/sets matching bit, R: clears all bits
  case object W0CRS extends AccessType //- W: 1/0 no effect on/clears matching bit, R: sets all bits
  case object WO extends AccessType //- W: as-is, R: error
  case object WOC extends AccessType //- W: clears all bits, R: error
  case object WOS extends AccessType //- W: sets all bits, R: error
  case object W1 extends AccessType //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
  case object WO1 extends AccessType //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
  case object NA extends AccessType // -W: reserved, R: reserved
  case object W1P extends AccessType // -W: 1/0 pulse/no effect on matching bit, R: no effect
  case object W0P extends AccessType // -W: 0/1 pulse/no effect on matching bit, R: no effect
}

class Section(val max: Int, val min: Int) {
  override def toString(): String = {
    if (this.max == this.min) {
      s"[${this.min}]"
    } else {
      s"[${this.max}:${this.min}]"
    }
  }
}

object Section {
  def apply(x: Range): Section = new Section(x.max, x.min)
  implicit def tans(x: Range) = Section(x)
}

object Value {
  def apply(value: Long, name: String, doc: String) = new Value(value, name, Some(doc))
}
case class Value(value: Long, name: String, doc: Option[String]=None) {}

case class Field(
    name: String,
    hardbit: Data, // HardType?
    section: Range,
    accType: AccessType,
    resetValue: Long,
    readError: Boolean,
    doc: String,
    values: List[Value]=List()
) {
  def tailBitPos = section.max
}

case class Register(
    name: String,
    address: Long,
    doc: String,
    fields: List[Field]
)

trait BusRegisterModule {
  def registers: List[Register]
}


