package andreasWallner

package object util {
  import Timer._

  implicit class IterablePimper[A](it: Iterable[A]) {
    def zipWithIsLast = it.iterableFactory.from(new ZipWithIsLast(it))
    def zipWithIsFirst = it.iterableFactory.from(new ZipWithIsFirst(it))
    def zipWithIsFirstLast = it.iterableFactory.from(new ZipWithIsFirstLast(it))
  }
}
