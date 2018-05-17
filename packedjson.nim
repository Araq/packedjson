#
#
#            Nim's Runtime Library
#        (c) Copyright 2018 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Packedjson is an alternative JSON tree implementation that takes up far
## less space than Nim's stdlib JSON implementation. The space savings can be as much
## as 80%. It can be faster or much slower than the stdlib's JSON, depending on the
## workload.

import parsejson, parseutils, streams
from unicode import toUTF8, Rune

type
  JsonNodeKind* = enum ## possible JSON node types
    JNull,
    JBool,
    JInt,
    JFloat,
    JString,
    JObject,
    JArray

# We use the following mapping which doesn't bite with the parLe, parRi, space
# tokens:
const
  parLe = '\0'
  parRi = '\1'
  space = '\2'

  opcodeNull = '0'
  opcodeBool = 'b'
  opcodeInt = '+'
  opcodeFloat = '.'
  opcodeString = '"' # note: merged with the string data's quotes to save space.
  opcodeObject = '{'
  opcodeArray = '['

type
  JsonNode* = object
    a, b: int
    t: ref string

proc newJString*(s: string): JsonNode =
  ## Creates a new `JString JsonNode`.
  doAssert s.len > 0 and s[0] == '"'
  new(result.t)
  result.t[] = s
  result.a = 0
  result.b = high(result.t[])

proc newJInt*(n: BiggestInt): JsonNode =
  ## Creates a new `JInt JsonNode`.
  new(result.t)
  result.t[] = $opcodeInt & $n
  result.a = 0
  result.b = high(result.t[])

proc newJFloat*(n: float): JsonNode =
  ## Creates a new `JFloat JsonNode`.
  new(result.t)
  result.t[] = $opcodeFloat & $n # XXX Ensure no precision is lost here
  result.a = 0
  result.b = high(result.t[])

proc newJBool*(b: bool): JsonNode =
  ## Creates a new `JBool JsonNode`.
  new(result.t)
  result.t[] = $opcodeBool & (if b: 't' else: 'f')
  result.a = 0
  result.b = high(result.t[])

proc newJNull*(): JsonNode =
  ## Creates a new `JNull JsonNode`.
  new(result.t)
  result.t[] = $opcodeNull
  result.a = 0
  result.b = high(result.t[])

proc newJObject*(): JsonNode =
  ## Creates a new `JObject JsonNode`
  new(result.t)
  result.t[] = opcodeObject & "\0\1"
  result.a = 0
  result.b = high(result.t[])

proc newJArray*(): JsonNode =
  ## Creates a new `JArray JsonNode`
  new(result.t)
  result.t[] = opcodeArray & "\0\1"
  result.a = 0
  result.b = high(result.t[])

when false:
  proc `%`*(s: string): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JString JsonNode`.
    new(result)
    result.kind = JString
    result.str = s

  proc `%`*(n: BiggestInt): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JInt JsonNode`.
    new(result)
    result.kind = JInt
    result.num  = n

  proc `%`*(n: float): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JFloat JsonNode`.
    new(result)
    result.kind = JFloat
    result.fnum  = n

  proc `%`*(b: bool): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JBool JsonNode`.
    new(result)
    result.kind = JBool
    result.bval = b

  proc `%`*(keyVals: openArray[tuple[key: string, val: JsonNode]]): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JObject JsonNode`
    if keyvals.len == 0: return newJArray()
    result = newJObject()
    for key, val in items(keyVals): result.fields[key] = val

  template `%`*(j: JsonNode): JsonNode = j

  proc `%`*[T](elements: openArray[T]): JsonNode =
    ## Generic constructor for JSON data. Creates a new `JArray JsonNode`
    result = newJArray()
    for elem in elements: result.add(%elem)


proc hasNext*(x: JsonNode; pos: int): bool =
  assert pos >= x.a
  result = pos < x.b

proc nextChild*(x: JsonNode; pos: int): JsonNode =
  var i = pos
  var nested = 0
  while i < x.b:
    case x.t[i]
    of parLe: inc nested
    of parRi: dec nested
    of space:
      if nested == 0:
        return JsonNode(a: pos, b: i-1, t: x.t)
    else: discard
    inc i
  doAssert false, "no next child"

proc nextPair(x: JsonNode; pos: int): array[2, JsonNode] =
  var start = pos
  var i = pos
  var nested = 0
  var r = 0
  while i < x.b:
    case x.t[i]
    of parLe: inc nested
    of parRi: dec nested
    of space:
      if nested == 0:
        result[r] = JsonNode(a: start, b: i-1, t: x.t)
        start = i+1
        if r == 1: return
        inc r
    else: discard
    inc i
  doAssert false, "no next pair"


proc kind*(x: JsonNode): JsonNodeKind =
  case x.t[x.a]
  of opcodeNull: result = JNull
  of opcodeBool: result = JBool
  of opcodeInt: result = JInt
  of opcodeFloat: result = JFloat
  of opcodeString: result = JString
  of opcodeObject: result = JObject
  of opcodeArray: result = JArray
  else:
    doAssert(false, "unknown node kind " & $ord(x.t[x.a]))

iterator items*(x: JsonNode): JsonNode =
  ## Iterator for the items of `x`. `x` has to be a JArray.
  assert x.kind == JArray
  var pos = x.a+2
  while hasNext(x, pos):
    let c = nextChild(x, pos)
    pos = c.b+2 # also skip the separator/terminator
    yield c

proc len*(n: JsonNode): int =
  ## If `n` is a `JArray`, it returns the number of elements.
  ## If `n` is a `JObject`, it returns the number of pairs.
  ## Else it returns 0.
  if n.kind notin {JArray, JObject}: return 0
  var i = n.a+2
  var nested = 0
  result = 0
  while i < n.b:
    case n.t[i]
    of parLe: inc nested
    of parRi: dec nested
    of space:
      if nested == 0: inc result
    else: discard
    inc i
  # divide by two because we counted the pairs wrongly:
  if n.kind == JObject: result = result shr 1

when false:
  proc rawAdd(parent: var JsonNode; child: JsonNode) =
    let tail = substr(parent.t[], parent.b)
    let pa = parent.b
    let ca = child.a
    let L = child.b - child.a + 1
    setLen(parent.t[], parent.t[].len + L + 1)
    for i in 0 ..< L:
      parent.t[][pa + i] = child.t[][ca + i]
    parent.t[][pa + L] = space
    let pa2 = pa + L + 1
    for i in 0 ..< tail.len:
      parent.t[][pa2 + i] = tail[i]
    inc parent.b, L + 1

proc rawAdd(parent: var JsonNode; child: string; a, b: int) =
  let tail = substr(parent.t[], parent.b)
  let pa = parent.b
  let L = b - a + 1
  setLen(parent.t[], parent.t[].len + L + 1)
  for i in 0 ..< L:
    parent.t[][pa + i] = child[a + i]
  parent.t[][pa + L] = space
  let pa2 = pa + L + 1
  for i in 0 ..< tail.len:
    parent.t[][pa2 + i] = tail[i]
  inc parent.b, L + 1

proc add*(parent: var JsonNode; child: JsonNode) =
  doAssert parent.kind == JArray, "parent is not a JArray"
  rawAdd(parent, child.t[], child.a, child.b)


proc getStr*(n: JsonNode, default: string = ""): string =
  ## Retrieves the string value of a `JString JsonNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``JString``.
  if n.kind != JString: return default

  result = ""
  var pos = n.a+1 # skip the quote
  while pos < n.b:
    if n.t[pos] == '\\':
      case n.t[pos]
      of '\\', '"', '\'', '/':
        add(result, n.t[pos])
        inc(pos, 2)
      of 'b':
        add(result, '\b')
        inc(pos, 2)
      of 'f':
        add(result, '\f')
        inc(pos, 2)
      of 'n':
        add(result, '\L')
        inc(pos, 2)
      of 'r':
        add(result, '\C')
        inc(pos, 2)
      of 't':
        add(result, '\t')
        inc(pos, 2)
      of 'u':
        inc(pos, 2)
        var pos2 = pos
        var r = parseEscapedUTF16(n.t[], pos)
        if r < 0:
          discard
        # Deal with surrogates
        elif (r and 0xfc00) == 0xd800:
          if n.t[pos] != '\\' or n.t[pos+1] != 'u':
            discard
          else:
            inc(pos, 2)
            var s = parseEscapedUTF16(n.t[], pos)
            if (s and 0xfc00) == 0xdc00 and s > 0:
              r = 0x10000 + (((r - 0xd800) shl 10) or (s - 0xdc00))
              add(result, toUTF8(Rune(r)))
        else:
          add(result, toUTF8(Rune(r)))
      else:
        # don't bother with the error
        add(result, n.t[pos])
        inc(pos)
    else:
      add(result, n.t[pos])
      inc(pos)

iterator pairs*(x: JsonNode): tuple[key: string, val: JsonNode] =
  ## Iterator for the child elements of `x`. `x` has to be a JObject.
  assert x.kind == JObject
  var pos = x.a+2
  while hasNext(x, pos):
    let c = nextPair(x, pos)
    pos = c[1].b+2 # also skip the separator/terminator
    yield (c[0].getStr, c[1])

proc getInt*(n: JsonNode, default: int = 0): int =
  ## Retrieves the int value of a `JInt JsonNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``JInt``, or if ``n`` is nil.
  if n.kind != JInt: return default
  doAssert parseInt(n.t[], result, n.a+1) == n.b-n.a

proc getBiggestInt*(n: JsonNode, default: BiggestInt = 0): BiggestInt =
  ## Retrieves the BiggestInt value of a `JInt JsonNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``JInt``, or if ``n`` is nil.
  if n.kind != JInt: return default
  doAssert parseBiggestInt(n.t[], result, n.a+1) == n.b-n.a

proc getFloat*(n: JsonNode, default: float = 0.0): float =
  ## Retrieves the float value of a `JFloat JsonNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``JFloat`` or ``JInt``, or if ``n`` is nil.
  case n.kind
  of JFloat, JInt:
    doAssert parseFloat(n.t[], result, n.a+1) == n.b-n.a
  else:
    result = default

proc getBool*(n: JsonNode, default: bool = false): bool =
  ## Retrieves the bool value of a `JBool JsonNode`.
  ##
  ## Returns ``default`` if ``n`` is not a ``JBool``, or if ``n`` is nil.
  if n.kind == JBool: result = n.t[n.a+1] == 't'
  else: result = default

when false:
  proc getFields*(n: JsonNode,
      default = initOrderedTable[string, JsonNode](4)):
          OrderedTable[string, JsonNode] =
    ## Retrieves the key, value pairs of a `JObject JsonNode`.
    ##
    ## Returns ``default`` if ``n`` is not a ``JObject``, or if ``n`` is nil.
    if n.kind != JObject: return default
    else: return n.fields

  proc getElems*(n: JsonNode, default: seq[JsonNode] = @[]): seq[JsonNode] =
    ## Retrieves the array of a `JArray JsonNode`.
    ##
    ## Returns ``default`` if ``n`` is not a ``JArray``, or if ``n`` is nil.
    if n.kind != JArray: return default
    else: return n.elems

  proc add*(father, child: JsonNode) =
    ## Adds `child` to a JArray node `father`.
    assert father.kind == JArray
    father.elems.add(child)


proc escapeJson*(s: string; result: var string) =
  ## Converts a string `s` to its JSON representation.
  ## Appends to ``result``.
  result.add("\"")
  for c in s:
    case c
    of '\L': result.add("\\n")
    of '\b': result.add("\\b")
    of '\f': result.add("\\f")
    of '\t': result.add("\\t")
    of '\r': result.add("\\r")
    of '"': result.add("\\\"")
    of '\\': result.add("\\\\")
    of parLe, parRi, space:
      result.add("\\")
      result.add(ord(c))
    else: result.add(c)
  result.add("\"")

proc escapeJson*(s: string): string =
  ## Converts a string `s` to its JSON representation.
  result = newStringOfCap(s.len + s.len shr 3)
  escapeJson(s, result)

proc indent(s: var string, i: int) =
  for _ in 1..i: s.add ' '

proc newIndent(curr, indent: int, ml: bool): int =
  if ml: return curr + indent
  else: return indent

proc nl(s: var string, ml: bool) =
  s.add(if ml: '\L' else: ' ')

proc toPretty(result: var string, node: JsonNode, indent = 2, ml = true,
              lstArr = false, currIndent = 0) =
  case node.kind
  of JObject:
    if lstArr: result.indent(currIndent) # Indentation
    if node.hasNext(node.a+2):
      result.add("{")
      result.nl(ml) # New line
      var i = 0
      for key, val in pairs(node):
        if i > 0:
          result.add(",")
          result.nl(ml) # New Line
        inc i
        # Need to indent more than {
        result.indent(newIndent(currIndent, indent, ml))
        escapeJson(key, result)
        result.add(": ")
        toPretty(result, val, indent, ml, false,
                 newIndent(currIndent, indent, ml))
      result.nl(ml)
      result.indent(currIndent) # indent the same as {
      result.add("}")
    else:
      result.add("{}")
  of JString, JInt, JFloat:
    if lstArr: result.indent(currIndent)
    # XXX This can be optimized still:
    for i in node.a+ord(node.kind != JString)..node.b:
      result.add node.t[][i]
  of JBool:
    if lstArr: result.indent(currIndent)
    result.add(if node.getBool: "true" else: "false")
  of JArray:
    if lstArr: result.indent(currIndent)
    if node.hasNext(node.a+2):
      result.add("[")
      result.nl(ml)
      var i = 0
      for x in items(node):
        if i > 0:
          result.add(",")
          result.nl(ml) # New Line
        toPretty(result, x, indent, ml,
            true, newIndent(currIndent, indent, ml))
        inc i
      result.nl(ml)
      result.indent(currIndent)
      result.add("]")
    else: result.add("[]")
  of JNull:
    if lstArr: result.indent(currIndent)
    result.add("null")

proc pretty*(node: JsonNode, indent = 2): string =
  ## Returns a JSON Representation of `node`, with indentation and
  ## on multiple lines.
  result = ""
  toPretty(result, node, indent)

proc toUgly*(result: var string, node: JsonNode) =
  ## Converts `node` to its JSON Representation, without
  ## regard for human readability. Meant to improve ``$`` string
  ## conversion performance.
  ##
  ## JSON representation is stored in the passed `result`
  ##
  ## This provides higher efficiency than the ``pretty`` procedure as it
  ## does **not** attempt to format the resulting JSON to make it human readable.
  var comma = false
  case node.kind:
  of JArray:
    result.add "["
    for child in node:
      if comma: result.add ","
      else: comma = true
      result.toUgly child
    result.add "]"
  of JObject:
    result.add "{"
    for key, value in pairs(node):
      if comma: result.add ","
      else: comma = true
      key.escapeJson(result)
      result.add ":"
      result.toUgly value
    result.add "}"
  of JString, JInt, JFloat:
    # XXX This can be optimized still:
    for i in node.a+ord(node.kind != JString)..node.b:
      result.add node.t[][i]
  of JBool:
    result.add(if node.getBool: "true" else: "false")
  of JNull:
    result.add "null"

proc `$`*(node: JsonNode): string =
  ## Converts `node` to its JSON Representation on one line.
  result = newStringOfCap(node.len shl 1)
  toUgly(result, node)

proc add*(obj: var JsonNode, key: string, val: JsonNode) =
  ## Sets a field from a `JObject`. **Warning**: It is currently not checked
  ## but assumed that the object does not yet have a field named `key`.
  assert obj.kind == JObject
  let k = escapeJson(key)
  rawAdd(obj, k, 0, high(k))
  rawAdd(obj, val.t[], val.a, val.b)
  when false:
    # XXX assert that the key does not exist yet
    obj.fields[key] = val

    var pos = obj.a+2
    while hasNext(x, pos):
      let c = nextPair(x, pos)

      pos = c[1].b+2 # also skip the separator/terminator

proc `[]`*(node: JsonNode, name: string): JsonNode =
  ## Gets a field from a `JObject`.
  ## If the value at `name` does not exist, raises KeyError.
  assert(node.kind == JObject)
  for k, v in pairs(node):
    if k == name: return v
  raise newException(KeyError, "key not found in object: " & name)

proc `[]`*(node: JsonNode, index: int): JsonNode =
  ## Gets the node at `index` in an Array. Result is undefined if `index`
  ## is out of bounds, but as long as array bound checks are enabled it will
  ## result in an exception.
  assert(node.kind == JArray)
  var i = index
  for x in items(node):
    if i == 0: return x
    dec i
  raise newException(IndexError, "index out of bounds")

proc contains*(node: JsonNode, key: string): bool =
  ## Checks if `key` exists in `node`.
  assert(node.kind == JObject)
  for k, v in pairs(node):
    if k == key: return true
  result = false

proc hasKey*(node: JsonNode, key: string): bool =
  ## Checks if `key` exists in `node`.
  assert(node.kind == JObject)
  result = node.contains(key)

proc `{}`*(node: JsonNode, keys: varargs[string]): JsonNode =
  ## Traverses the node and gets the given value. If any of the
  ## keys do not exist, returns ``JNull``. Also returns ``JNull`` if one of the
  ## intermediate data structures is not an object.
  result = node
  for kk in keys:
    if result.kind != JObject: return newJNull()
    block searchLoop:
      for k, v in pairs(result):
        if k == kk:
          result = v
          break searchLoop
      return newJNull()

proc `{}`*(node: JsonNode, indexes: varargs[int]): JsonNode =
  ## Traverses the node and gets the given value. If any of the
  ## indexes do not exist, returns ``JNull``. Also returns ``JNull`` if one of the
  ## intermediate data structures is not an array.
  result = node
  for j in indexes:
    if result.kind != JArray: return newJNull()
    block searchLoop:
      var i = j
      for x in items(result):
        if i == 0:
          result = x
          break searchLoop
        dec i
      return newJNull()

proc getOrDefault*(node: JsonNode, key: string): JsonNode =
  ## Gets a field from a `node`. If `node` is nil or not an object or
  ## value at `key` does not exist, returns JNull
  for k, v in pairs(node):
    if k == key: return v
  result = newJNull()

proc parseJson(p: var JsonParser; buf: var string) =
  ## Parses JSON from a JSON Parser `p`. We break the abstraction here
  ## and construct the low level representation directly for speed.
  case p.tok
  of tkString:
    doAssert p.a.len > 0 and p.a[0] == '"'
    buf.add(p.a)
    discard getTok(p)
  of tkInt:
    buf.add opcodeInt
    buf.add p.a
    discard getTok(p)
  of tkFloat:
    buf.add opcodeFloat
    buf.add p.a
    discard getTok(p)
  of tkTrue:
    buf.add opcodeBool
    buf.add 't'
    discard getTok(p)
  of tkFalse:
    buf.add opcodeBool
    buf.add 'f'
    discard getTok(p)
  of tkNull:
    buf.add opcodeNull
    discard getTok(p)
  of tkCurlyLe:
    buf.add opcodeObject
    buf.add parLe
    discard getTok(p)
    while p.tok != tkCurlyRi:
      if p.tok != tkString:
        raiseParseErr(p, "string literal as key")
      doAssert p.a.len > 0 and p.a[0] == '"'
      buf.add p.a
      buf.add space
      discard getTok(p)
      eat(p, tkColon)
      parseJson(p, buf)
      buf.add space
      if p.tok != tkComma: break
      discard getTok(p)
    eat(p, tkCurlyRi)
    buf.add parRi
  of tkBracketLe:
    buf.add opcodeArray
    buf.add parLe
    discard getTok(p)
    while p.tok != tkBracketRi:
      parseJson(p, buf)
      buf.add space
      if p.tok != tkComma: break
      discard getTok(p)
    eat(p, tkBracketRi)
    buf.add parRi
  of tkError, tkCurlyRi, tkBracketRi, tkColon, tkComma, tkEof:
    raiseParseErr(p, "{")

proc parseJson*(s: Stream, filename: string = ""): JsonNode =
  ## Parses from a stream `s` into a `JsonNode`. `filename` is only needed
  ## for nice error messages.
  ## If `s` contains extra data, it will raise `JsonParsingError`.
  var p: JsonParser
  p.open(s, filename, rawStringLiterals = true)
  new result.t
  result.t[] = newStringOfCap(64)
  try:
    discard getTok(p) # read first token
    p.parseJson(result.t[])
    result.a = 0
    result.b = high(result.t[])
    eat(p, tkEof) # check if there is no extra data
  finally:
    p.close()

proc parseJson*(buffer: string): JsonNode =
  ## Parses JSON from `buffer`.
  ## If `buffer` contains extra data, it will raise `JsonParsingError`.
  result = parseJson(newStringStream(buffer), "input")

proc parseFile*(filename: string): JsonNode =
  ## Parses `file` into a `JsonNode`.
  ## If `file` contains extra data, it will raise `JsonParsingError`.
  var stream = newFileStream(filename, fmRead)
  if stream == nil:
    raise newException(IOError, "cannot read from file: " & filename)
  result = parseJson(stream, filename)

when isMainModule:
  let testJson = parseJson"""{ "a": [1, 2, 3, 4], "b": "asd", "c": "\ud83c\udf83", "d": "\u00E6"}"""
  echo testJson
  echo testJson{"d"}

  echo testJson{"a"}[3]
