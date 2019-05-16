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

#[A ``JsonNode`` that is added to another ``JsonNode`` gets copied:

.. code-block:: nim
    var x = newJObject()
    var arr = newJArray()
    arr.add x
    x["field"] = %"value"
    assert $arr == "[{}]"

These semantics also imply that code like ``myobj["field"]["nested"] = %4``
needs instead be written as ``myobj["field", "nested"] = %4`` so that the
changes are end up in the tree.
]##

import parsejson, parseutils, streams, strutils, macros
from unicode import toUTF8, Rune

import std / varints

type
  JsonNodeKind* = enum 
    JNull,
    JBool,
    JInt,
    JFloat,
    JString,
    JObject,
    JArray

# An atom is always prefixed with its kind and for JString, JFloat, JInt also
# with a length information. The length information is either part of the first byte
# or a variable length integer. An atom is everything
# that is not an object or an array. Since we don't know the elements in an array
# or object upfront during parsing, these are not stored with any length
# information. Instead the 'opcodeEnd' marker is produced. This means that during
# traversal we have to be careful of the nesting and always jump over the payload
# of an atom. However, this means we can save key names unescaped which speeds up
# most operations.

const
  opcodeBits = 3

  opcodeNull = ord JNull
  opcodeBool = ord JBool
  opcodeFalse = opcodeBool
  opcodeTrue = opcodeBool or 0b0000_1000
  opcodeInt = ord JInt
  opcodeFloat = ord JFloat
  opcodeString = ord JString
  opcodeObject = ord JObject
  opcodeArray = ord JArray
  opcodeEnd* = 7

  opcodeMask = 0b111

proc storeAtom(buf: var seq[byte]; k: JsonNodeKind; data: string) =
  if data.len < 0b1_1111:
    # we can store the length in the upper bits of the kind field.
    buf.add byte(data.len shl 3) or byte(k)
  else:
    # we need to store the kind field and the length separately:
    buf.add 0b1111_1000u8 or byte(k)
    # ensure we have the space:
    let start = buf.len
    buf.setLen start + maxVarIntLen
    let realVlen = writeVu64(toOpenArray(buf, start, start + maxVarIntLen - 1), uint64 data.len)
    buf.setLen start + realVlen
  for i in 0..data.high:
    buf.add byte(data[i])

proc beginContainer(buf: var seq[byte]; k: JsonNodeKind) =
  buf.add byte(k)

proc endContainer(buf: var seq[byte]) = buf.add byte(opcodeEnd)

type
  JsonNode* = ref object
    k: JsonNodeKind
    a, b*: int
    t*: ref seq[byte]

proc kind*(x: JsonNode): JsonNodeKind = x.k

proc newJNull*(): JsonNode =
  new result
  result.k = JNull

template newBody(kind, x) =
  new result
  new result.t
  storeAtom(result.t[], kind, x)
  result.a = 0
  result.b = high(result.t[])
  result.k = kind

proc newJString*(s: string): JsonNode =
  newBody JString, s

proc newJInt*(n: BiggestInt): JsonNode =
  newBody JInt, $n

proc newJFloat*(n: float): JsonNode =
  newBody JFloat, formatFloat(n)

proc newJBool*(b: bool): JsonNode =
  new result
  new result.t
  result.k = JBool
  result.t[] = @[if b: byte(opcodeTrue) else: byte(opcodeFalse)]
  result.a = 0
  result.b = result.t[].high

proc newJObject*(): JsonNode =
  new result
  new result.t
  result.k = JObject
  result.t[] = @[byte opcodeObject,  opcodeEnd]
  result.a = 0
  result.b = result.t[].high

proc newJArray*(): JsonNode =
  new result
  new result.t
  result.k = JArray
  result.t[] = @[byte opcodeArray, byte opcodeEnd]
  result.a = 0
  result.b = result.t[].high


proc `%`*[T](elements: openArray[T]): JsonNode =
  result = newJArray()
  for elem in elements: result.add(%elem)

proc `%`*(o: object): JsonNode =
  result = newJObject()
  for k, v in o.fieldPairs: result[k] = %v

proc `%`*(o: ref object): JsonNode =
  if o.isNil:
    result = newJNull()
  else:
    result = %(o[])

proc `%`*(o: enum): JsonNode =
  result = %($o)
proc `%`*(s: string): JsonNode =
  newJString(s)

proc `%`*(n: BiggestInt): JsonNode =
  newJInt(n)

proc `%`*(n: float): JsonNode =
  result = newJFloat(n)

proc `%`*(b: bool): JsonNode =
  result = newJBool(b)

template `%`*(j: JsonNode): JsonNode = j

proc rawPut*(obj: JsonNode, oldval: JsonNode, key: string, val: JsonNode): int =
  let oldlen = oldval.b - oldval.a + 1
  let newlen = val.b - val.a + 1
  result = newlen - oldlen
  if result == 0:
    for i in 0 ..< newlen:
      obj.t[][oldval.a + i] = (if val.k == JNull: byte opcodeNull else: val.t[][i])
  else:
    let oldfull = obj.t[].len
    if newlen > oldlen:
      setLen(obj.t[], oldfull+result)
      for i in countdown(oldfull+result-1, oldval.a+newlen): 
        shallowCopy(obj.t[][i], obj.t[][i-result])
    else:
      for i in countup(oldval.a+newlen, oldfull+result-1): 
        shallowCopy(obj.t[][i], obj.t[][i-result])
      setLen(obj.t[], oldfull+result)
    for i in 0 ..< newlen:
      obj.t[][oldval.a + i] = (if val.k == JNull: byte opcodeNull else: val.t[][i])

proc rawAdd*(obj: JsonNode; child: seq[byte]; a, b: int) =
  let pa = obj.b
  let L = b - a + 1
  let oldfull = obj.t[].len
  setLen(obj.t[], oldfull+L)
  for i in countdown(oldfull+L-1, pa+L):
    shallowCopy(obj.t[][i], obj.t[][i-L])
  copyMem( obj.t[][pa].addr, child[a].unsafeAddr, L)
  inc obj.b, L

proc rawAddWithNull*(parent: JsonNode; child: JsonNode) =
  if child.k == JNull:
    let pa = parent.b
    let oldLen = parent.t[].len
    setLen(parent.t[], oldLen + 1)
    for i in pa .. oldLen-1:
      parent.t[][1 + i] = parent.t[][i]
    parent.t[][pa] = byte opcodeNull
    inc parent.b, 1
  else:
    rawAdd(parent, child.t[], child.a, child.b)

proc add*(parent:JsonNode; child: JsonNode) =
  rawAddWithNull(parent, child)

proc extractSlice*(x: seq[byte]; pos: int): (int, int) =
  if (x[pos] and 0b1111_1000u8) != 0b1111_1000u8:
    result = (pos + 1, int(x[pos]) shr 3)
  else:
    var varint: uint64
    let varintLen = readVu64(toOpenArray(x, pos+1, min(pos + 1 + maxVarIntLen, x.high)), varint)
    result = (pos + 1 + varintLen, int(varint))

proc extractLen*(x: seq[byte]; pos: int): int =
  if (x[pos] and 0b1111_1000u8) != 0b1111_1000u8:
    result = int(x[pos]) shr 3
  else:
    var varint: uint64
    let varintLen = readVu64(toOpenArray(x, pos+1, min(pos + 1 + maxVarIntLen, x.high)), varint)
    result = int(varint) + varintLen

proc skip(x: seq[byte]; start: int; elements: var int): int =
  var nested = 0
  var pos = start
  while true:
    let k = x[pos] and opcodeMask
    var nextPos = pos + 1
    case k
    of opcodeNull, opcodeBool:
      if nested == 0: inc elements
    of opcodeInt, opcodeFloat, opcodeString:
      let L = extractLen(x, pos)
      nextPos = pos + 1 + L
      if nested == 0: inc elements
    of opcodeObject, opcodeArray:
      if nested == 0: inc elements
      inc nested
    of opcodeEnd:
      if nested == 0: return nextPos
      dec nested
    else: discard
    pos = nextPos

proc rawGet*(x: JsonNode; name: string): JsonNode =
  var pos = x.a + 1
  var dummy: int
  while pos <= x.b:
    let k2 = x.t[][pos] and opcodeMask
    if k2 == opcodeEnd or k2 != opcodeString: break
    let (start, L) = extractSlice(x.t[], pos)
    var isMatch = name.len == L
    if isMatch:
      for i in 0 ..< L:
        if name[i] != char(x.t[][start+i]):
          isMatch = false
          break
    pos = start + L

    let k = x.t[][pos] and opcodeMask
    var nextPos = pos + 1
    case k
    of opcodeNull, opcodeBool: discard
    of opcodeInt, opcodeFloat, opcodeString:
      let L = extractLen(x.t[], pos)
      nextPos = pos + 1 + L
    of opcodeObject, opcodeArray:
      nextPos = skip(x.t[], pos+1, dummy)
    of opcodeEnd: return newJNull()
    else: discard
    if isMatch:
      return JsonNode(k: JsonNodeKind(k), a: pos, b: nextPos-1, t: x.t)
    pos = nextPos
  new result
  new result.t
  result.a = -1

proc `[]`*(x: JsonNode; name: string): JsonNode =
  rawGet(x, name)

iterator items*(x: JsonNode): JsonNode =
  if x.kind != JArray: discard
  var pos = x.a+1
  var dummy: int
  while pos <= x.b:
    let k = x.t[][pos] and opcodeMask
    var nextPos = pos + 1
    case k
    of opcodeNull, opcodeBool: discard
    of opcodeInt, opcodeFloat, opcodeString:
      let L = extractLen(x.t[], pos)
      nextPos = pos + 1 + L
    of opcodeObject, opcodeArray:
      nextPos = skip(x.t[], pos+1, dummy)
    of opcodeEnd: break
    else: discard
    yield JsonNode(k: JsonNodeKind(k), a: pos, b: nextPos-1, t: x.t)
    pos = nextPos

iterator pairs*(x: JsonNode): (string, JsonNode) =
  if x.kind != JObject: discard
  var pos = x.a+1
  var dummy: int
  var key: string
  while pos <= x.b:
    let k2 = x.t[][pos] and opcodeMask
    if k2 == opcodeEnd or k2 != opcodeString: break
    let (start, L) = extractSlice(x.t[], pos)
    key.setLen L
    for i in 0 ..< L: 
      key[i] = char(x.t[][start+i])
    pos = start + L

    let k = x.t[][pos] and opcodeMask
    var nextPos = pos + 1
    case k
    of opcodeNull, opcodeBool: discard
    of opcodeInt, opcodeFloat, opcodeString:
      let L = extractLen(x.t[], pos)
      nextPos = pos + 1 + L
    of opcodeObject, opcodeArray:
      nextPos = skip(x.t[], pos+1, dummy)
    of opcodeEnd: break
    else: discard
    yield (key, JsonNode(k: JsonNodeKind(k), a: pos, b: nextPos-1, t: x.t))
    pos = nextPos

template escape(result, c) =
  case c
  of '\L': result.add("\\n")
  of '\b': result.add("\\b")
  of '\f': result.add("\\f")
  of '\t': result.add("\\t")
  of '\r': result.add("\\r")
  of '"': result.add("\\\"")
  of '\\': result.add("\\\\")
  else: result.add(c)

proc escapeJson*(s: string; result: var string) =
  result.add("\"")
  for c in s: escape(result, c)
  result.add("\"")

proc escapeJson*(s: string): string =
  result = newStringOfCap(s.len + s.len shr 3)
  escapeJson(s, result)

proc emitAtom(result: var string, n: JsonNode) =
  let (start, L) = extractSlice(n.t[], n.a)
  if n.k == JString: result.add("\"")
  for i in 0 ..< L:
    let c = char(n.t[][start+i])
    escape(result, c)
  if n.k == JString: result.add("\"")

proc getBool*(n: JsonNode, default: bool = false): bool =
  if n.kind == JBool: result = (n.t[][n.a] shr opcodeBits) == 1
  else: result = default

proc toUgly*(result: var string, node: JsonNode) =
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
    for key, value in node.pairs:
      if comma: result.add ","
      else: comma = true
      key.escapeJson(result)
      result.add ":"
      result.toUgly value
    result.add "}"
  of JString, JInt, JFloat:
    emitAtom(result, node)
  of JBool:
    result.add(if node.getBool: "true" else: "false")
  of JNull:
    result.add "null"

proc `$`*(node: JsonNode): string =
  toUgly(result, node)

proc len*(n: JsonNode): int =
  if n.k notin {JArray, JObject}: return 0
  discard skip(n.t[], n.a+1, result)
  if n.k == JObject: result = result shr 1

proc add*(obj: JsonNode, key: string, val: JsonNode) =
  if obj.kind != JObject: discard
  let k = newJstring(key)
  rawAdd(obj, k.t[], k.a, k.b)
  rawAddWithNull(obj, val)
  when false:
    discard "XXX assert that the key does not exist yet"

proc `%`*(keyVals: openArray[tuple[key: string, val: JsonNode]]): JsonNode =
  if keyvals.len == 0: return newJArray()
  result = newJObject()
  for key, val in items(keyVals): result.add key, val

proc getFloat*(n: JsonNode, default: float = 0.0): float =
  case n.kind
  of JFloat, JInt:
    let (start, L) = extractSlice(n.t[], n.a)
    if parseFloat(cast[string](n.t[]), result, start) != L:
      # little hack ahead: If parsing failed because of following control bytes,
      # patch the control byte, do the parsing and patch the control byte back:
      let old = n.t[][start+L]
      n.t[][start+L] = 0
      doAssert parseFloat(cast[string](n.t[]), result, start) == L
      n.t[][start+L] = old
  else:
    result = default

proc `+`*(a,b: JsonNode):JsonNode = 
    %(a.getFloat + b.getFloat)

proc contains*(node: JsonNode, key: string): bool =
  if node.kind != JObject: return false
  let x = rawGet(node, key)
  result = x.a >= 0

proc hasKey*(node: JsonNode, key: string): bool =
  if node.kind != JObject: return false
  let x = node[key]
  result = x.a >= 0

proc myParseInt(s: seq[byte]; first, last: int): BiggestInt =
  var i = first
  var isNegative = false
  if i < last:
    case chr(s[i])
    of '+': inc(i)
    of '-':
      isNegative = true
      inc(i)
    else: discard
  if i <= last and chr(s[i]) in {'0'..'9'}:
    var res = 0u64
    while i <= last and chr(s[i]) in {'0'..'9'}:
      let c = uint64(ord(s[i]) - ord('0'))
      if res <= (0xFFFF_FFFF_FFFF_FFFFu64 - c) div 10:
        res = res * 10u64 + c
      inc(i)
    if isNegative:
      if res >= uint64(high(BiggestInt))+1:
        result = low(BiggestInt)
      else:
        result = -BiggestInt(res)
    else:
      if res >= uint64(high(BiggestInt)):
        result = high(BiggestInt)
      else:
        result = BiggestInt(res)

proc getInt*(n: JsonNode, default: int = 0): int =
  if n.kind != JInt: return default
  let (start, L) = extractSlice(n.t[], n.a)
  result = int(myParseInt(n.t[], start, start + L - 1))

proc `[]=`*(obj: JsonNode, key: string, val: JsonNode) =
  let oldval = obj[key]
  if oldval.a < 0:
    add(obj, key, val)
  else:
    let diff = rawPut(obj, oldval, key, val)
    inc obj.b, diff

macro `[]=`*(obj: JsonNode, keys: varargs[typed], val: JsonNode): untyped =
  ## keys can be strings or integers for the navigation.
  result = newStmtList()
  template t0(obj, key) {.dirty.} =
    var oldval = obj[key]

  template ti(key) {.dirty.} =
    oldval = oldval[key]

  template tput(obj, finalkey, val) =
    let diff = rawPut(obj, oldval, finalkey, val)
    inc obj.b, diff

  result.add getAst(t0(obj, keys[0]))
  for i in 1..<len(keys):
    result.add getAst(ti(keys[i]))
  result.add getAst(tput(obj, keys[len(keys)-1], val))
  result = newBlockStmt(result)

proc rawDelete(x: JsonNode, key: string) =
  var pos = x.a+1
  var dummy: int
  while pos <= x.b:
    let k2 = x.t[][pos] and opcodeMask
    if k2 == opcodeEnd or k2 != opcodeString: break
    let begin = pos
    let (start, L) = extractSlice(x.t[], pos)
    var isMatch = key.len == L
    if isMatch:
      for i in 0 ..< L:
        if key[i] != char(x.t[][start+i]):
          isMatch = false
          break
    pos = start + L

    let k = x.t[][pos] and opcodeMask
    var nextPos = pos + 1
    case k
    of opcodeNull, opcodeBool: discard
    of opcodeInt, opcodeFloat, opcodeString:
      let L = extractLen(x.t[], pos)
      nextPos = pos + 1 + L
    of opcodeObject, opcodeArray:
      nextPos = skip(x.t[], pos+1, dummy)
    of opcodeEnd: discard
    else: discard
    if isMatch:
      let diff = nextPos - begin
      let oldfull = x.t[].len
      for i in countup(begin, oldfull-diff-1): 
        shallowCopy(x.t[][i], x.t[][i+diff])
      setLen(x.t[], oldfull-diff)
      dec x.b, diff
      return
    pos = nextPos
  raise newException(KeyError, "key not in object: " & key)

proc delete*(x:JsonNode, key: string) =
  rawDelete(x, key)

proc toJson(x: NimNode): NimNode {.compiletime.} =
  case x.kind
  of nnkBracket: # array
    if x.len == 0: return newCall(bindSym"newJArray")
    result = newNimNode(nnkBracket)
    for i in 0 ..< x.len:
      result.add(toJson(x[i]))
    result = newCall(bindSym"%", result)
  of nnkTableConstr: # object
    if x.len == 0: return newCall(bindSym"newJObject")
    result = newNimNode(nnkStmtListExpr)
    var res = gensym(nskVar, "cons")
    result.add newVarStmt(res, newCall(bindSym"newJObject"))
    for i in 0 ..< x.len:
      x[i].expectKind nnkExprColonExpr
      result.add newCall(bindSym"[]=", res, x[i][0], toJson(x[i][1]))
    result.add res
  of nnkCurly: # empty object
    x.expectLen(0)
    result = newCall(bindSym"newJObject")
  of nnkNilLit:
    result = newCall(bindSym"newJNull")
  of nnkPar:
    if x.len == 1: result = toJson(x[0])
    else: result = newCall(bindSym"%", x)
  else:
    result = newCall(bindSym"%", x)

macro `%*`*(x: untyped): untyped =
  ## Convert an expression to a JsonNode directly, without having to specify
  ## `%` for every element.
  result = toJson(x)

proc getStr*(n: JsonNode, default: string = ""): string =
  if n.kind != JString: return default
  let (start, L) = extractSlice(n.t[], n.a)
  result = newString(L)
  for i in 0 ..< L:
    result[i] = char(n.t[start+i])
  # copyMem(result[0].addr, n.t[][start].addr, L)

proc getBiggestInt*(n: JsonNode, default: BiggestInt = 0): BiggestInt =
  if n.kind != JInt: return default
  let (start, L) = extractSlice(n.t[], n.a)
  result = myParseInt(n.t[], start, start + L - 1)

proc isEmpty(n: JsonNode): bool =
  assert n.kind in {JArray, JObject}
  result = n.t[][n.a+1] == opcodeEnd

proc indent(s: var string, i: int) =
  for _ in 1..i: s.add ' '

proc newIndent(curr, indent: int, ml: bool): int =
  if ml: return curr + indent
  else: return indent

proc nl(s: var string, ml: bool) =
  s.add(if ml: '\L' else: ' ')

proc toPretty(result: var string, n: JsonNode, indent = 2, ml = true,
              lstArr = false, currIndent = 0) =
  case n.kind
  of JObject:
    if lstArr: result.indent(currIndent) # Indentation
    if not n.isEmpty:
      result.add("{")
      result.nl(ml) # New line
      var i = 0
      for key, val in pairs(n):
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
    emitAtom(result, n)
  of JBool:
    if lstArr: result.indent(currIndent)
    result.add(if n.getBool: "true" else: "false")
  of JArray:
    if lstArr: result.indent(currIndent)
    if not n.isEmpty:
      result.add("[")
      result.nl(ml)
      var i = 0
      for x in items(n):
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

proc `[]`*(node: JsonNode, index: int): JsonNode =
  ## Gets the node at `index` in an Array. Result is undefined if `index`
  ## is out of bounds, but as long as array bound checks are enabled it will
  ## result in an exception.
  if node.kind != JArray: discard
  var i = index
  for x in items(node):
    if i == 0: return x
    dec i
  raise newException(IndexError, "index out of bounds")

proc `{}`*(node: JsonNode, indexes: varargs[int]): JsonNode =
  result = node
  for j in indexes:
    if result.k != JArray: return newJArray()
    block searchLoop:
      var i = j
      for x in items(result):
        if i == 0:
          result = x
          break searchLoop
        dec i
      return newJArray()

proc `{}`*(node: JsonNode, keys: varargs[string]): JsonNode =
  result = node
  for kk in keys:
    if result.k != JObject: return newJObject()
    block searchLoop:
      for k, v in result.pairs:
        if k == kk:
          result = v
          break searchLoop
      return newJObject()

proc `{}=`*(node: JsonNode, keys: varargs[string], value: JsonNode) =
  if keys[0] notin node:
    node[keys[0]] = newJObject()
  for i in 1..keys.high:
    if i == keys.high:
      node{keys[0..i-1]}[keys[i]] =  value
      node.t[].add opcodeEnd
    else:
      node{keys[0..i-1]}[keys[i]] = newJObject()
  node.b = node.t[].high - 1

proc getOrDefault*(node: JsonNode, key: string): JsonNode =
  for k, v in pairs(node):
    if k == key: return v
  result = newJNull()

proc `==`*(x, y: JsonNode): bool =
  # Equality for two JsonNodes. Note that the order in field
  # declarations is also part of the equality requirement as
  # everything else would be too costly to implement.
  if x.k != y.k: return false
  if x.k == JNull: return true
  if x.b - x.a != y.b - y.a: return false
  for i in 0 ..< x.b - x.a + 1:
    if x.t[][x.a + i] != y.t[][i + y.a]: return false
  return true

proc parseJson(p: var JsonParser; buf: var seq[byte]) =
  ## Parses JSON from a JSON Parser `p`. We break the abstraction here
  ## and construct the low level representation directly for speed.
  case p.tok
  of tkString:
    storeAtom(buf, JString, p.a)
    discard getTok(p)
  of tkInt:
    storeAtom(buf, JInt, p.a)
    discard getTok(p)
  of tkFloat:
    storeAtom(buf, JFloat, p.a)
    discard getTok(p)
  of tkTrue:
    buf.add opcodeTrue
    discard getTok(p)
  of tkFalse:
    buf.add opcodeFalse
    discard getTok(p)
  of tkNull,tkError, tkCurlyRi, tkBracketRi, tkColon, tkComma, tkEof:
    buf.add opcodeNull
    discard getTok(p)
  of tkCurlyLe:
    beginContainer(buf, JObject)
    discard getTok(p)
    while p.tok != tkCurlyRi:
      if p.tok != tkString:
        raiseParseErr(p, "string literal as key")
      storeAtom(buf, JString, p.a)
      discard getTok(p)
      eat(p, tkColon)
      parseJson(p, buf)
      if p.tok != tkComma: break
      discard getTok(p)
    eat(p, tkCurlyRi)
    endContainer(buf)
  of tkBracketLe:
    beginContainer(buf, JArray)
    discard getTok(p)
    while p.tok != tkBracketRi:
      parseJson(p, buf)
      if p.tok != tkComma: break
      discard getTok(p)
    eat(p, tkBracketRi)
    endContainer(buf)
  # of tkError, tkCurlyRi, tkBracketRi, tkColon, tkComma, tkEof:
    # raiseParseErr(p, "{")

proc parseJson*(s: Stream, filename: string = ""): JsonNode =
  var p: JsonParser
  p.open(s, filename)
  new result
  new result.t
  result.t[] = newSeqOfCap[byte](64)
  try:
    discard getTok(p) # read first token
    p.parseJson(result.t[])
    result.a = 0
    result.b = high(result.t[])
    result.k = JsonNodeKind(result.t[][0] and opcodeMask)
    eat(p, tkEof) # check if there is no extra data
  finally:
    p.close()

proc parseJson*(buffer: string): JsonNode =
  result = parseJson(newStringStream(buffer), "input")

proc parseFile*(filename: string): JsonNode =
  var stream = newFileStream(filename, fmRead)
  if stream == nil:
    raise newException(IOError, "cannot read from file: " & filename)
  result = parseJson(stream, filename)

when isMainModule:
  var cost = newJObject()
  cost.t[].add opcodeEnd
  var rows = [["AWS","compute","monthly"],["AWS","network","monthly"],
              ["Alibaba","compute","monthly"],["Alibaba","network","monthly"],
              ["Tencent","compute","monthly"],["Tencent","network","monthly"]]

  for row in rows:
      cost{row[0..2]} = %(cost{row[0..2]}.getFloat + 1.0)
  cost["AWS"].add "yearly", %""
  echo cost
  echo cost["AWS"]["compute"]["monthly"].getFloat
  echo cost["AWS"]["yearly"].getStr
  template test(a, b) =
    let x = a
    if x != b:
      echo "test failed ", astToStr(a), ":"
      echo "got:",x
      echo "should be:",b

  let empty = parseJson ""
  echo empty
  let testJson = parseJson"""{ "a": [1, 2, 3, 4], "b": "asd", "c": "\ud83c\udf83", "d": "\u00E6"}"""
  test $testJson{"a"}[3], "4"

  var moreStuff = %*{"abc": 3, "more": 6.6, "null": nil}
  test $moreStuff, """{"abc":3,"more":6.600000000000000,"null":null}"""
  
  moreStuff["more"] = %"foo bar"
  test $moreStuff, """{"abc":3,"more":"foo bar","null":null}"""

  moreStuff["more"] = %"a"
  moreStuff["null"] = %678

  test $moreStuff, """{"abc":3,"more":"a","null":678}"""

  moreStuff.delete "more"
  test $moreStuff, """{"abc":3,"null":678}"""

  moreStuff{"keyOne", "keyTwo", "keyThree"} = %*{"abc": 3, "more": 6.6, "null": nil}
  test $moreStuff, """{"abc":3,"null":678,"keyOne":{"keyTwo":{"keyThree":{"abc":3,"more":6.600000000000000,"null":null}}}}"""

  moreStuff["alias"] = newJObject()
  moreStuff.delete "keyOne"

  test $moreStuff, """{"abc":3,"null":678,"alias":{}}"""
  moreStuff{"keyOne"} = %*{"keyTwo": 3}

  moreStuff{"keyOne", "keyTwo", "keyThree"} = %*{"abc": 3, "more": 6.6, "null": nil}
  block:
    var x = newJObject()
    var arr = newJArray()
    arr.add x
    x["field"] = %"value"
    assert $arr == "[{}]"

  block:
    var x = newJObject()
    x["field"] = %"value"
    var arr = newJArray()
    arr.add x
    assert arr == %*[{"field":"value"}]

  block:
    var testJson = parseJson"""{ "a": [1, 2, {"key": [4, 5]}, 4]}"""
    testJson["a", 2, "key"] = %10
    test $testJson, """{"a":[1,2,{"key":10},4]}"""

  block:
    var mjson = %*{"properties":{"subnet":"a","securitygroup":"b"}}
    mjson["properties","subnet"] = %""
    mjson["properties","securitygroup"] = %""
    test $mjson, """{"properties":{"subnet":"","securitygroup":""}}"""

  block:
    var msg = %*{
      "itemId":25,
      "cityId":15,
      "less": low(BiggestInt),
      "more": high(BiggestInt),
      "million": 1_000_000
    }
    var itemId = msg["itemId"].getInt
    var cityId = msg["cityId"].getInt
    assert itemId == 25
    assert cityId == 15
    doAssert msg["less"].getBiggestInt == low(BiggestInt)
    doAssert msg["more"].getBiggestInt == high(BiggestInt)
    doAssert msg["million"].getBiggestInt == 1_000_000
