import java.util.UUID

import cats.Show
import doobie.Meta

package object model {

  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)

  implicit val show: Show[BigDecimal] = (value: BigDecimal) => value.toString()

}
