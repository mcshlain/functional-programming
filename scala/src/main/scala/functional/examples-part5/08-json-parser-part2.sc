
import functional.part5.parserCombinatorApplicativeWithLazyOr._
import functional.part5.jsonParser._


// It appears that worksheets don't work with circular definitions so we move the
// definition to a proper scala object and just use it here

primitiveValue.run(Location("true"))
primitiveValue.run(Location("-13.7"))
primitiveValue.run(Location("\"abc\""))


array.run(Location("[]"))
array.run(Location("[true]"))
array.run(Location("[true,12.6,\"word\"]"))
array.run(Location("[true,[12.6,14.2],{\"a\":[],\"b\":true},\"word\"]"))


objectP.run(Location("{\"name\":\"john\",\"age\":29,\"address\":{\"street\":\"Hertzel\",\"no\":12}}"))
