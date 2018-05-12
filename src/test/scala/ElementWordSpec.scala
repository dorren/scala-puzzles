import org.scalatest.{FlatSpec, Matchers}

class ElementWordSpec extends FlatSpec with Matchers{
  val emptySln = Seq.empty

  "it" should "solve for emtpy string" in {
    val actual = ElementWord.solve("")
    assert(actual == Seq(Seq.empty))
  }

  "it" should "solve one letter" in {
    assert(ElementWord.solve("a") == emptySln)
    assert(ElementWord.solve("b") == List(List("(B) Boron")))
  }

  "it" should "solve more letters" in {
    assert(ElementWord.solve("bank")   == List(List("(Ba) Barium", "(N) Nitrogen", "(K) Potassium")))
    assert(ElementWord.solve("bat")    == List(List("(B) Boron", "(At) Astatine")))
    assert(ElementWord.solve("Ryan")   == emptySln)
    assert(ElementWord.solve("Dorren") == emptySln)
    assert(ElementWord.solve("Larry")  == emptySln)
    assert(ElementWord.solve("Varun")  == List(List("(V) Vanadium", "(Ar) Argon", "(U) Uranium", "(N) Nitrogen")))
    assert(ElementWord.solve("Chris")  == emptySln)
    assert(ElementWord.solve("Dinesh") == emptySln)
    assert(ElementWord.solve("Bill")   == emptySln)
    assert(ElementWord.solve("Dan")    == emptySln)
    assert(ElementWord.solve("Tobi")   == emptySln)
    assert(ElementWord.solve("Matt")   == emptySln)
    assert(ElementWord.solve("Ville")  == emptySln)
    assert(ElementWord.solve("Kanchi") == emptySln)
    assert(ElementWord.solve("Steph")  == List(List("(S) Sulfur", "(Te) Tellurium", "(P) Phosphorus", "(H) Hydrogen")))
    assert(ElementWord.solve("Rob")    == emptySln)
    assert(ElementWord.solve("Noam")   == List(List("(N) Nitrogen", "(O) Oxygen", "(Am) Americium"),
                                               List("(No) Nobelium", "(Am) Americium")))
  }
}
