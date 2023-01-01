import ".." / packedjson, macros, strutils, options, tables
# -- Json deserialiser. --

template verifyJsonKind(node: JsonNode, kinds: set[JsonNodeKind], ast: string) =
  if node.kind == JNull:
    raise newException(KeyError, "key not found: " & ast)
  elif node.kind notin kinds:
    let msg = "Incorrect JSON kind. Wanted '$1' in '$2' but got '$3'." % [
      $kinds,
      ast,
      $node.kind
    ]
    raise newException(JsonKindError, msg)

macro isRefSkipDistinct*(arg: typed): untyped =
  ## internal only, do not use
  var impl = getTypeImpl(arg)
  if impl.kind == nnkBracketExpr and impl[0].eqIdent("typeDesc"):
    impl = getTypeImpl(impl[1])
  while impl.kind == nnkDistinctTy:
    impl = getTypeImpl(impl[0])
  result = newLit(impl.kind == nnkRefTy)

# The following forward declarations don't work in older versions of Nim

# forward declare all initFromJson

proc initFromJson(dst: var string; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson(dst: var bool; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson(dst: var JsonNode; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T: SomeInteger](dst: var T; jsonNode: JsonNode, jsonPath: var string)
proc initFromJson[T: SomeFloat](dst: var T; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T: enum](dst: var T; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T](dst: var seq[T]; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[S, T](dst: var array[S, T]; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T](dst: var Table[string, T]; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T](dst: var OrderedTable[string, T]; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T](dst: var ref T; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T](dst: var Option[T]; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T: distinct](dst: var T; jsonNode: JsonNode; jsonPath: var string)
proc initFromJson[T: object|tuple](dst: var T; jsonNode: JsonNode; jsonPath: var string)

# initFromJson definitions

proc initFromJson(dst: var string; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JString, JNull}, jsonPath)
  # since strings don't have a nil state anymore, this mapping of
  # JNull to the default string is questionable. `none(string)` and
  # `some("")` have the same potentional json value `JNull`.
  dst = jsonNode.getStr

proc initFromJson(dst: var bool; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JBool}, jsonPath)
  dst = jsonNode.getBool

proc initFromJson(dst: var JsonNode; jsonNode: JsonNode; jsonPath: var string) =
  if jsonNode.kind == JNull:
    raise newException(KeyError, "key not found: " & jsonPath)
  dst = jsonNode.copy

proc initFromJson[T: SomeInteger](dst: var T; jsonNode: JsonNode, jsonPath: var string) =
  verifyJsonKind(jsonNode, {JInt}, jsonPath)
  dst = T(jsonNode.getInt)

proc initFromJson[T: SomeFloat](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JInt, JFloat}, jsonPath)
  dst = T(jsonNode.getFloat)

proc initFromJson[T: enum](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JString}, jsonPath)
  dst = parseEnum[T](jsonNode.getStr)

proc initFromJson[T](dst: var seq[T]; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JArray}, jsonPath)
  dst.setLen jsonNode.len
  let orignalJsonPathLen = jsonPath.len
  for i in 0 ..< jsonNode.len:
    jsonPath.add '['
    jsonPath.addInt i
    jsonPath.add ']'
    initFromJson(dst[i], jsonNode[i], jsonPath)
    jsonPath.setLen orignalJsonPathLen

proc initFromJson[S,T](dst: var array[S,T]; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JArray}, jsonPath)
  let originalJsonPathLen = jsonPath.len
  for i in 0 ..< jsonNode.len:
    jsonPath.add '['
    jsonPath.addInt i
    jsonPath.add ']'
    initFromJson(dst[i], jsonNode[i], jsonPath)
    jsonPath.setLen originalJsonPathLen

proc initFromJson[T](dst: var Table[string,T]; jsonNode: JsonNode; jsonPath: var string) =
  dst = initTable[string, T]()
  verifyJsonKind(jsonNode, {JObject}, jsonPath)
  let originalJsonPathLen = jsonPath.len
  for key, value in jsonNode.pairs:
    jsonPath.add '.'
    jsonPath.add key
    initFromJson(mgetOrPut(dst, key, default(T)), jsonNode[key], jsonPath)
    jsonPath.setLen originalJsonPathLen

proc initFromJson[T](dst: var OrderedTable[string,T]; jsonNode: JsonNode; jsonPath: var string) =
  dst = initOrderedTable[string,T]()
  verifyJsonKind(jsonNode, {JObject}, jsonPath)
  let originalJsonPathLen = jsonPath.len
  for key, value in jsonNode.pairs:
    jsonPath.add '.'
    jsonPath.add key
    initFromJson(mgetOrPut(dst, key, default(T)), jsonNode[key], jsonPath)
    jsonPath.setLen originalJsonPathLen

proc initFromJson[T](dst: var ref T; jsonNode: JsonNode; jsonPath: var string) =
  verifyJsonKind(jsonNode, {JObject, JNull}, jsonPath)
  if jsonNode.kind == JNull:
    dst = nil
  else:
    dst = new(T)
    initFromJson(dst[], jsonNode, jsonPath)

proc initFromJson[T](dst: var Option[T]; jsonNode: JsonNode; jsonPath: var string) =
  if jsonNode.kind != JNull:
    when T is ref:
      dst = some(new(T))
    else:
      dst = some(default(T))
    initFromJson(dst.get, jsonNode, jsonPath)

macro assignDistinctImpl[T: distinct](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  let typInst = getTypeInst(dst)
  let typImpl = getTypeImpl(dst)
  let baseTyp = typImpl[0]

  result = quote do:
    when nimvm:
      # workaround #12282
      var tmp: `baseTyp`
      initFromJson( tmp, `jsonNode`, `jsonPath`)
      `dst` = `typInst`(tmp)
    else:
      initFromJson( `baseTyp`(`dst`), `jsonNode`, `jsonPath`)

proc initFromJson[T: distinct](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  assignDistinctImpl(dst, jsonNode, jsonPath)

proc detectIncompatibleType(typeExpr, lineinfoNode: NimNode): void =
  if typeExpr.kind == nnkTupleConstr:
    error("Use a named tuple instead of: " & typeExpr.repr, lineinfoNode)

proc foldObjectBody(dst, typeNode, tmpSym, jsonNode, jsonPath, originalJsonPathLen: NimNode) =
  case typeNode.kind
  of nnkEmpty:
    discard
  of nnkRecList, nnkTupleTy:
    for it in typeNode:
      foldObjectBody(dst, it, tmpSym, jsonNode, jsonPath, originalJsonPathLen)

  of nnkIdentDefs:
    typeNode.expectLen 3
    let fieldSym = typeNode[0]
    let fieldNameLit = newLit(fieldSym.strVal)
    let fieldPathLit = newLit("." & fieldSym.strVal)
    let fieldType = typeNode[1]

    # Detecting incompatiple tuple types in `assignObjectImpl` only
    # would be much cleaner, but the ast for tuple types does not
    # contain usable type information.
    detectIncompatibleType(fieldType, fieldSym)

    dst.add quote do:
      jsonPath.add `fieldPathLit`
      when nimvm:
        when isRefSkipDistinct(`tmpSym`.`fieldSym`):
          # workaround #12489
          var tmp: `fieldType`
          initFromJson(tmp, getOrDefault(`jsonNode`,`fieldNameLit`), `jsonPath`)
          `tmpSym`.`fieldSym` = tmp
        else:
          initFromJson(`tmpSym`.`fieldSym`, getOrDefault(`jsonNode`,`fieldNameLit`), `jsonPath`)
      else:
        initFromJson(`tmpSym`.`fieldSym`, getOrDefault(`jsonNode`,`fieldNameLit`), `jsonPath`)
      jsonPath.setLen `originalJsonPathLen`

  of nnkRecCase:
    let kindSym = typeNode[0][0]
    let kindNameLit = newLit(kindSym.strVal)
    let kindPathLit = newLit("." & kindSym.strVal)
    let kindType = typeNode[0][1]
    let kindOffsetLit = newLit(uint(getOffset(kindSym)))
    dst.add quote do:
      var kindTmp: `kindType`
      jsonPath.add `kindPathLit`
      initFromJson(kindTmp, `jsonNode`[`kindNameLit`], `jsonPath`)
      jsonPath.setLen `originalJsonPathLen`
      when defined js:
        `tmpSym`.`kindSym` = kindTmp
      else:
        when nimvm:
          `tmpSym`.`kindSym` = kindTmp
        else:
          # fuck it, assign kind field anyway
          ((cast[ptr `kindType`](cast[uint](`tmpSym`.addr) + `kindOffsetLit`))[]) = kindTmp
    dst.add nnkCaseStmt.newTree(nnkDotExpr.newTree(tmpSym, kindSym))
    for i in 1 ..< typeNode.len:
      foldObjectBody(dst, typeNode[i], tmpSym, jsonNode, jsonPath, originalJsonPathLen)

  of nnkOfBranch, nnkElse:
    let ofBranch = newNimNode(typeNode.kind)
    for i in 0 ..< typeNode.len-1:
      ofBranch.add copyNimTree(typeNode[i])
    let dstInner = newNimNode(nnkStmtListExpr)
    foldObjectBody(dstInner, typeNode[^1], tmpSym, jsonNode, jsonPath, originalJsonPathLen)
    # resOuter now contains the inner stmtList
    ofBranch.add dstInner
    dst[^1].expectKind nnkCaseStmt
    dst[^1].add ofBranch

  of nnkObjectTy:
    typeNode[0].expectKind nnkEmpty
    typeNode[1].expectKind {nnkEmpty, nnkOfInherit}
    if typeNode[1].kind == nnkOfInherit:
      let base = typeNode[1][0]
      var impl = getTypeImpl(base)
      while impl.kind in {nnkRefTy, nnkPtrTy}:
        impl = getTypeImpl(impl[0])
      foldObjectBody(dst, impl, tmpSym, jsonNode, jsonPath, originalJsonPathLen)
    let body = typeNode[2]
    foldObjectBody(dst, body, tmpSym, jsonNode, jsonPath, originalJsonPathLen)

  else:
    error("unhandled kind: " & $typeNode.kind, typeNode)

macro assignObjectImpl[T](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  let typeSym = getTypeInst(dst)
  let originalJsonPathLen = genSym(nskLet, "originalJsonPathLen")
  result = newStmtList()
  result.add quote do:
    let `originalJsonPathLen` = len(`jsonPath`)
  if typeSym.kind in {nnkTupleTy, nnkTupleConstr}:
    # both, `dst` and `typeSym` don't have good lineinfo. But nothing
    # else is available here.
    detectIncompatibleType(typeSym, dst)
    foldObjectBody(result, typeSym, dst, jsonNode, jsonPath, originalJsonPathLen)
  else:
    foldObjectBody(result, typeSym.getTypeImpl, dst, jsonNode, jsonPath, originalJsonPathLen)

proc initFromJson[T: object|tuple](dst: var T; jsonNode: JsonNode; jsonPath: var string) =
  assignObjectImpl(dst, jsonNode, jsonPath)

proc to*[T](node: JsonNode, t: typedesc[T]): T =
  ## `Unmarshals`:idx: the specified node into the object type specified.
  ##
  ## Known limitations:
  ##
  ##   * Heterogeneous arrays are not supported.
  ##   * Sets in object variants are not supported.
  ##   * Not nil annotations are not supported.
  ##
  var jsonPath = ""
  initFromJson(result, node, jsonPath)

when isMainModule:
  type
    ContentNodeKind = enum
      P, Br, Text
    ContentNode = object
      case kind: ContentNodeKind
      of P: pChildren: seq[ContentNode]
      of Br: nil
      of Text: textStr: string
  block:
    let mynode = ContentNode(kind: P, pChildren: @[
      ContentNode(kind: Text, textStr: "mychild"),
      ContentNode(kind: Br)
    ])

    doAssert $mynode == """(kind: P, pChildren: @[(kind: Text, textStr: "mychild"), (kind: Br)])"""
    let jsonNode = %*mynode
    doAssert $jsonNode["pChildren"].to(seq[JsonNode]) == """@[{"kind":"Text","textStr":"mychild"}, {"kind":"Br"}]"""
    doAssert $jsonNode == """{"kind":"P","pChildren":[{"kind":"Text","textStr":"mychild"},{"kind":"Br"}]}"""
    doAssert $jsonNode.to(ContentNode) == """(kind: P, pChildren: @[(kind: Text, textStr: "mychild"), (kind: Br)])"""
