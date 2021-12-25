package tagless

final class FunctionKSpec extends munit.FunSuite:

  test("option to list") {
    val optionToList: Option ~> List = [A] => (opt: Option[A]) => opt.toList

    assert(optionToList(Some(10)) == List(10))
    assert(optionToList(Some("test")) == List("test"))
  }

  test("list to option") {
    val listToOption: List ~> Option = [A] => (list: List[A]) => list.headOption

    assert(listToOption(List(10)) == Option(10))
    assert(listToOption(List("test")) == Option("test"))
  }
