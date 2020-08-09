import java.util.UUID

import doobie.Meta

package object model {

  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)

}
