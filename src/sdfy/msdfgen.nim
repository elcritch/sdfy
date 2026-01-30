import std/algorithm
import std/math
import std/strutils
import std/unicode

import pixie
import pixie/blends
import vmath

const
  msdfCubicSearchStarts = 4
  msdfCubicSearchSteps = 4
  edgeLengthPrecision = 4
  msdfCornerDotEpsilon = 0.000001
  deconvergeOvershoot = 1.11111111111111111


type
  EdgeColor* = uint8

const
  ecBlack* = 0'u8
  ecRed* = 1'u8
  ecGreen* = 2'u8
  ecYellow* = 3'u8
  ecBlue* = 4'u8
  ecMagenta* = 5'u8
  ecCyan* = 6'u8
  ecWhite* = 7'u8


type
  SignedDistance = object
    distance: float64
    dot: float64

  EdgeSegmentKind = enum
    esLine
    esQuadratic
    esCubic

  EdgeSegment = ref object
    color: EdgeColor
    kind: EdgeSegmentKind
    p: array[4, DVec2]

  Contour = ref object
    edges: seq[EdgeSegment]

  ShapeBounds* = object
    l*: float64
    b*: float64
    r*: float64
    t*: float64

  Shape = ref object
    contours: seq[Contour]

  Range = object
    lower: float64
    upper: float64

  DistanceMapping = object
    scale: float64
    translate: float64

  Projection = object
    scale: DVec2
    translate: DVec2

  MultiDistance = object
    r: float64
    g: float64
    b: float64

  MultiAndTrueDistance = object
    r: float64
    g: float64
    b: float64
    a: float64

  PerpSelectorBase = object
    minTrueDistance: SignedDistance
    minNegativePerp: float64
    minPositivePerp: float64
    nearEdge: EdgeSegment
    nearEdgeParam: float64

  MultiDistanceSelector = object
    p: DVec2
    r: PerpSelectorBase
    g: PerpSelectorBase
    b: PerpSelectorBase

  MsdfGlyphResult* = object
    image*: Image
    scale*: float64
    translate*: DVec2
    bounds*: ShapeBounds
    range*: float64


proc normalizeVec(v: DVec2; allowZero = false): DVec2 {.inline.} =
  let len = length(v)
  if len != 0:
    v / len
  else:
    if allowZero:
      dvec2(0, 0)
    else:
      dvec2(0, 1)

proc orthonormal(v: DVec2; polarity = true; allowZero = false): DVec2 {.inline.} =
  let len = length(v)
  if len != 0:
    if polarity:
      dvec2(-v.y / len, v.x / len)
    else:
      dvec2(v.y / len, -v.x / len)
  else:
    if allowZero:
      dvec2(0, 0)
    else:
      if polarity:
        dvec2(0, 1)
      else:
        dvec2(0, -1)

proc orthogonal(v: DVec2; polarity = true): DVec2 {.inline.} =
  if polarity:
    dvec2(-v.y, v.x)
  else:
    dvec2(v.y, -v.x)

proc cross(a, b: DVec2): float64 {.inline.} =
  a.x * b.y - a.y * b.x

proc median(a, b, c: float64): float64 {.inline.} =
  max(min(a, b), min(max(a, b), c))

proc sign(n: float64): int {.inline.} =
  (0.0 < n).int - (n < 0.0).int

proc nonZeroSign(n: float64): int {.inline.} =
  if n > 0.0: 1 else: -1

proc clamp01(n: float64): float64 {.inline.} =
  if n < 0.0:
    0.0
  elif n > 1.0:
    1.0
  else:
    n

proc clamp(n, a, b: float64): float64 {.inline.} =
  if n < a:
    a
  elif n > b:
    b
  else:
    n

proc clampInt(n, a, b: int): int {.inline.} =
  if n < a:
    a
  elif n > b:
    b
  else:
    n

proc lerp(a, b, t: float64): float64 {.inline.} =
  a + (b - a) * t


proc sdLess(a, b: SignedDistance): bool {.inline.} =
  (abs(a.distance) < abs(b.distance)) or
    (abs(a.distance) == abs(b.distance) and a.dot < b.dot)

proc sdGreater(a, b: SignedDistance): bool {.inline.} =
  (abs(a.distance) > abs(b.distance)) or
    (abs(a.distance) == abs(b.distance) and a.dot > b.dot)


proc initSignedDistance(): SignedDistance {.inline.} =
  SignedDistance(distance: -1.0e300, dot: 0)


proc newLine(p0, p1: DVec2; color: EdgeColor = ecWhite): EdgeSegment =
  EdgeSegment(kind: esLine, color: color, p: [p0, p1, dvec2(0, 0), dvec2(0, 0)])

proc newQuadratic(p0, p1, p2: DVec2; color: EdgeColor = ecWhite): EdgeSegment =
  if cross(p1 - p0, p2 - p1) == 0:
    return newLine(p0, p2, color)
  EdgeSegment(kind: esQuadratic, color: color, p: [p0, p1, p2, dvec2(0, 0)])

proc newCubic(p0, p1, p2, p3: DVec2; color: EdgeColor = ecWhite): EdgeSegment =
  let p12 = p2 - p1
  if cross(p1 - p0, p12) == 0 and cross(p12, p3 - p2) == 0:
    return newLine(p0, p3, color)
  let quadCtrl = mix(p1, p0, -0.5)
  if quadCtrl == mix(p2, p3, -0.5):
    return newQuadratic(p0, quadCtrl, p3, color)
  EdgeSegment(kind: esCubic, color: color, p: [p0, p1, p2, p3])

proc convertQuadraticToCubic(edge: EdgeSegment) =
  if edge.kind != esQuadratic:
    return
  let p0 = edge.p[0]
  let p1 = edge.p[1]
  let p2 = edge.p[2]
  edge.kind = esCubic
  edge.p[0] = p0
  edge.p[1] = mix(p0, p1, 2.0 / 3.0)
  edge.p[2] = mix(p1, p2, 1.0 / 3.0)
  edge.p[3] = p2

proc point(edge: EdgeSegment; t: float64): DVec2 {.inline.} =
  case edge.kind
  of esLine:
    mix(edge.p[0], edge.p[1], t)
  of esQuadratic:
    mix(mix(edge.p[0], edge.p[1], t), mix(edge.p[1], edge.p[2], t), t)
  of esCubic:
    let p12 = mix(edge.p[1], edge.p[2], t)
    mix(mix(mix(edge.p[0], edge.p[1], t), p12, t),
        mix(p12, mix(edge.p[2], edge.p[3], t), t), t)

proc direction(edge: EdgeSegment; t: float64): DVec2 {.inline.} =
  case edge.kind
  of esLine:
    edge.p[1] - edge.p[0]
  of esQuadratic:
    let tangent = mix(edge.p[1] - edge.p[0], edge.p[2] - edge.p[1], t)
    if tangent == dvec2(0, 0):
      edge.p[2] - edge.p[0]
    else:
      tangent
  of esCubic:
    let tangent = mix(mix(edge.p[1] - edge.p[0], edge.p[2] - edge.p[1], t),
                      mix(edge.p[2] - edge.p[1], edge.p[3] - edge.p[2], t), t)
    if tangent == dvec2(0, 0):
      if t == 0:
        edge.p[2] - edge.p[0]
      else:
        edge.p[3] - edge.p[1]
    else:
      tangent

proc solveQuadratic(outRoots: var openArray[float64], a, b, c: float64): int =
  if a == 0 or abs(b) > 1e12 * abs(a):
    if b == 0:
      if c == 0:
        return -1
      return 0
    outRoots[0] = -c / b
    return 1
  let dscr = b * b - 4 * a * c
  if dscr > 0:
    let root = sqrt(dscr)
    outRoots[0] = (-b + root) / (2 * a)
    outRoots[1] = (-b - root) / (2 * a)
    return 2
  elif dscr == 0:
    outRoots[0] = -b / (2 * a)
    return 1
  0

proc solveCubicNormed(outRoots: var array[3, float64], a, b, c: float64): int =
  let a2 = a * a
  var q = (a2 - 3 * b) / 9
  var r = (a * (2 * a2 - 9 * b) + 27 * c) / 54
  let r2 = r * r
  let q3 = q * q * q
  let aThird = a / 3
  if r2 < q3:
    var t = r / sqrt(q3)
    t = clamp(t, -1.0, 1.0)
    t = arccos(t)
    q = -2 * sqrt(q)
    outRoots[0] = q * cos(t / 3) - aThird
    outRoots[1] = q * cos((t + 2 * PI) / 3) - aThird
    outRoots[2] = q * cos((t - 2 * PI) / 3) - aThird
    return 3
  var u = (if r < 0: 1.0 else: -1.0) * pow(abs(r) + sqrt(r2 - q3), 1.0 / 3.0)
  let v = if u == 0: 0.0 else: q / u
  outRoots[0] = (u + v) - aThird
  if u == v or abs(u - v) < 1e-12 * abs(u + v):
    outRoots[1] = -0.5 * (u + v) - aThird
    return 2
  1

proc solveCubic(outRoots: var array[3, float64], a, b, c, d: float64): int =
  if a != 0:
    let bn = b / a
    if abs(bn) < 1e6:
      return solveCubicNormed(outRoots, bn, c / a, d / a)
  solveQuadratic(outRoots, b, c, d)

proc scanlineIntersections(edge: EdgeSegment; x: var array[3, float64]; dy: var array[3, int]; y: float64): int =
  case edge.kind
  of esLine:
    if (y >= edge.p[0].y and y < edge.p[1].y) or (y >= edge.p[1].y and y < edge.p[0].y):
      let param = (y - edge.p[0].y) / (edge.p[1].y - edge.p[0].y)
      x[0] = mix(edge.p[0].x, edge.p[1].x, param)
      dy[0] = sign(edge.p[1].y - edge.p[0].y)
      return 1
    return 0
  of esQuadratic:
    var total = 0
    var nextDy = if y > edge.p[0].y: 1 else: -1
    x[total] = edge.p[0].x
    if edge.p[0].y == y:
      if edge.p[0].y < edge.p[1].y or (edge.p[0].y == edge.p[1].y and edge.p[0].y < edge.p[2].y):
        dy[total] = 1
        inc total
      else:
        nextDy = 1
    let ab = edge.p[1] - edge.p[0]
    let br = edge.p[2] - edge.p[1] - ab
    var t: array[2, float64]
    var solutions = solveQuadratic(t, br.y, 2 * ab.y, edge.p[0].y - y)
    if solutions >= 2 and t[0] > t[1]:
      let tmp = t[0]
      t[0] = t[1]
      t[1] = tmp
    if solutions < 0:
      solutions = 0
    for i in 0 ..< solutions:
      if total >= 2:
        break
      if t[i] >= 0 and t[i] <= 1:
        x[total] = edge.p[0].x + 2 * t[i] * ab.x + t[i] * t[i] * br.x
        if nextDy.float64 * (ab.y + t[i] * br.y) >= 0:
          dy[total] = nextDy
          inc total
          nextDy = -nextDy
    if edge.p[2].y == y:
      if nextDy > 0 and total > 0:
        dec total
        nextDy = -1
      if (edge.p[2].y < edge.p[1].y or (edge.p[2].y == edge.p[1].y and edge.p[2].y < edge.p[0].y)) and total < 2:
        x[total] = edge.p[2].x
        if nextDy < 0:
          dy[total] = -1
          inc total
          nextDy = 1
    if nextDy != (if y >= edge.p[2].y: 1 else: -1):
      if total > 0:
        dec total
      else:
        if abs(edge.p[2].y - y) < abs(edge.p[0].y - y):
          x[total] = edge.p[2].x
        dy[total] = nextDy
        inc total
    total
  of esCubic:
    var total = 0
    var nextDy = if y > edge.p[0].y: 1 else: -1
    x[total] = edge.p[0].x
    if edge.p[0].y == y:
      if edge.p[0].y < edge.p[1].y or (edge.p[0].y == edge.p[1].y and
          (edge.p[0].y < edge.p[2].y or (edge.p[0].y == edge.p[2].y and edge.p[0].y < edge.p[3].y))):
        dy[total] = 1
        inc total
      else:
        nextDy = 1
    let ab = edge.p[1] - edge.p[0]
    let br = edge.p[2] - edge.p[1] - ab
    let asv = (edge.p[3] - edge.p[2]) - (edge.p[2] - edge.p[1]) - br
    var t: array[3, float64]
    var solutions = solveCubic(t, asv.y, 3 * br.y, 3 * ab.y, edge.p[0].y - y)
    if solutions < 0:
      solutions = 0
    if solutions >= 2 and t[0] > t[1]:
      let tmp = t[0]
      t[0] = t[1]
      t[1] = tmp
    if solutions >= 3 and t[1] > t[2]:
      let tmp = t[1]
      t[1] = t[2]
      t[2] = tmp
      if t[0] > t[1]:
        let tmp2 = t[0]
        t[0] = t[1]
        t[1] = tmp2
    for i in 0 ..< solutions:
      if total >= 3:
        break
      if t[i] >= 0 and t[i] <= 1:
        x[total] = edge.p[0].x + 3 * t[i] * ab.x + 3 * t[i] * t[i] * br.x + t[i] * t[i] * t[i] * asv.x
        if nextDy.float64 * (ab.y + 2 * t[i] * br.y + t[i] * t[i] * asv.y) >= 0:
          dy[total] = nextDy
          inc total
          nextDy = -nextDy
    if edge.p[3].y == y:
      if nextDy > 0 and total > 0:
        dec total
        nextDy = -1
      if (edge.p[3].y < edge.p[2].y or (edge.p[3].y == edge.p[2].y and
          (edge.p[3].y < edge.p[1].y or (edge.p[3].y == edge.p[1].y and edge.p[3].y < edge.p[0].y)))) and total < 3:
        x[total] = edge.p[3].x
        if nextDy < 0:
          dy[total] = -1
          inc total
          nextDy = 1
    if nextDy != (if y >= edge.p[3].y: 1 else: -1):
      if total > 0:
        dec total
      else:
        if abs(edge.p[3].y - y) < abs(edge.p[0].y - y):
          x[total] = edge.p[3].x
        dy[total] = nextDy
        inc total
    total

proc signedDistance(edge: EdgeSegment; origin: DVec2; param: var float64): SignedDistance =
  case edge.kind
  of esLine:
    let aq = origin - edge.p[0]
    let ab = edge.p[1] - edge.p[0]
    param = dot(aq, ab) / dot(ab, ab)
    let eq = if param > 0.5: edge.p[1] - origin else: edge.p[0] - origin
    let endpointDistance = length(eq)
    if param > 0 and param < 1:
      let orthoDistance = dot(orthonormal(ab, false), aq)
      if abs(orthoDistance) < endpointDistance:
        return SignedDistance(distance: orthoDistance, dot: 0)
    SignedDistance(
      distance: nonZeroSign(cross(aq, ab)).float64 * endpointDistance,
      dot: abs(dot(normalizeVec(ab), normalizeVec(eq)))
    )
  of esQuadratic:
    let qa = edge.p[0] - origin
    let ab = edge.p[1] - edge.p[0]
    let br = edge.p[2] - edge.p[1] - ab
    let a = dot(br, br)
    let b = 3 * dot(ab, br)
    let c = 2 * dot(ab, ab) + dot(qa, br)
    let d = dot(qa, ab)
    var tRoots: array[3, float64]
    let solutions = solveCubic(tRoots, a, b, c, d)

    var epDir = direction(edge, 0)
    var minDistance = nonZeroSign(cross(epDir, qa)).float64 * length(qa)
    param = -dot(qa, epDir) / dot(epDir, epDir)

    let distB = length(edge.p[2] - origin)
    if distB < abs(minDistance):
      epDir = direction(edge, 1)
      minDistance = nonZeroSign(cross(epDir, edge.p[2] - origin)).float64 * distB
      param = dot(origin - edge.p[1], epDir) / dot(epDir, epDir)

    for i in 0 ..< solutions:
      let t = tRoots[i]
      if t > 0 and t < 1:
        let qe = qa + 2.0 * t * ab + t * t * br
        let dist = length(qe)
        if dist <= abs(minDistance):
          minDistance = nonZeroSign(cross(ab + t * br, qe)).float64 * dist
          param = t

    if param >= 0 and param <= 1:
      return SignedDistance(distance: minDistance, dot: 0)
    if param < 0.5:
      return SignedDistance(distance: minDistance, dot: abs(dot(normalizeVec(direction(edge, 0)), normalizeVec(qa))))
    SignedDistance(distance: minDistance, dot: abs(dot(normalizeVec(direction(edge, 1)), normalizeVec(edge.p[2] - origin))))
  of esCubic:
    let qa = edge.p[0] - origin
    let ab = edge.p[1] - edge.p[0]
    let br = edge.p[2] - edge.p[1] - ab
    let asv = (edge.p[3] - edge.p[2]) - (edge.p[2] - edge.p[1]) - br

    var epDir = direction(edge, 0)
    var minDistance = nonZeroSign(cross(epDir, qa)).float64 * length(qa)
    param = -dot(qa, epDir) / dot(epDir, epDir)

    let distB = length(edge.p[3] - origin)
    if distB < abs(minDistance):
      epDir = direction(edge, 1)
      minDistance = nonZeroSign(cross(epDir, edge.p[3] - origin)).float64 * distB
      param = dot(epDir - (edge.p[3] - origin), epDir) / dot(epDir, epDir)

    for i in 0 .. msdfCubicSearchStarts:
      var t = (1.0 / msdfCubicSearchStarts.float64) * i.float64
      var qe = qa + 3.0 * t * ab + 3.0 * t * t * br + t * t * t * asv
      var d1 = 3.0 * ab + 6.0 * t * br + 3.0 * t * t * asv
      var d2 = 6.0 * br + 6.0 * t * asv
      var improvedT = t - dot(qe, d1) / (dot(d1, d1) + dot(qe, d2))
      if improvedT > 0 and improvedT < 1:
        var remaining = msdfCubicSearchSteps
        while remaining > 0 and improvedT > 0 and improvedT < 1:
          t = improvedT
          qe = qa + 3.0 * t * ab + 3.0 * t * t * br + t * t * t * asv
          d1 = 3.0 * ab + 6.0 * t * br + 3.0 * t * t * asv
          dec remaining
          if remaining == 0:
            break
          d2 = 6.0 * br + 6.0 * t * asv
          improvedT = t - dot(qe, d1) / (dot(d1, d1) + dot(qe, d2))
        let dist = length(qe)
        if dist < abs(minDistance):
          minDistance = nonZeroSign(cross(d1, qe)).float64 * dist
          param = t

    if param >= 0 and param <= 1:
      return SignedDistance(distance: minDistance, dot: 0)
    if param < 0.5:
      return SignedDistance(distance: minDistance, dot: abs(dot(normalizeVec(direction(edge, 0)), normalizeVec(qa))))
    SignedDistance(distance: minDistance, dot: abs(dot(normalizeVec(direction(edge, 1)), normalizeVec(edge.p[3] - origin))))

proc distanceToPerpendicularDistance(edge: EdgeSegment; distance: var SignedDistance; origin: DVec2; param: float64) =
  if param < 0:
    let dir = normalizeVec(direction(edge, 0))
    let aq = origin - point(edge, 0)
    let ts = dot(aq, dir)
    if ts < 0:
      let perpendicular = cross(aq, dir)
      if abs(perpendicular) <= abs(distance.distance):
        distance.distance = perpendicular
        distance.dot = 0
  elif param > 1:
    let dir = normalizeVec(direction(edge, 1))
    let bq = origin - point(edge, 1)
    let ts = dot(bq, dir)
    if ts > 0:
      let perpendicular = cross(bq, dir)
      if abs(perpendicular) <= abs(distance.distance):
        distance.distance = perpendicular
        distance.dot = 0

proc deconvergeEdge(edge: EdgeSegment; param: int; vector: DVec2) =
  case edge.kind
  of esQuadratic:
    convertQuadraticToCubic(edge)
    if param == 0:
      edge.p[1] = edge.p[1] + length(edge.p[1] - edge.p[0]) * vector
    elif param == 1:
      edge.p[2] = edge.p[2] + length(edge.p[2] - edge.p[3]) * vector
  of esCubic:
    if param == 0:
      edge.p[1] = edge.p[1] + length(edge.p[1] - edge.p[0]) * vector
    elif param == 1:
      edge.p[2] = edge.p[2] + length(edge.p[2] - edge.p[3]) * vector
  else:
    discard

proc simplifyDegenerateCurve(controlPoints: var array[12, DVec2]; base: int; order: var int) =
  if order == 3 and
      ((controlPoints[base + 1] == controlPoints[base] or controlPoints[base + 1] == controlPoints[base + 3]) and
      (controlPoints[base + 2] == controlPoints[base] or controlPoints[base + 2] == controlPoints[base + 3])):
    controlPoints[base + 1] = controlPoints[base + 3]
    order = 1
  if order == 2 and
      (controlPoints[base + 1] == controlPoints[base] or controlPoints[base + 1] == controlPoints[base + 2]):
    controlPoints[base + 1] = controlPoints[base + 2]
    order = 1
  if order == 1 and controlPoints[base] == controlPoints[base + 1]:
    order = 0

proc convergentCurveOrdering(controlPoints: var array[12, DVec2]; cornerIdx, controlPointsBefore, controlPointsAfter: int): int =
  if not (controlPointsBefore > 0 and controlPointsAfter > 0):
    return 0

  var a1 = controlPoints[cornerIdx - 1] - controlPoints[cornerIdx]
  var b1 = controlPoints[cornerIdx + 1] - controlPoints[cornerIdx]
  var a2 = dvec2(0, 0)
  var b2 = dvec2(0, 0)
  var a3 = dvec2(0, 0)
  var b3 = dvec2(0, 0)

  if controlPointsBefore >= 2:
    a2 = controlPoints[cornerIdx - 2] - controlPoints[cornerIdx - 1] - a1
  if controlPointsAfter >= 2:
    b2 = controlPoints[cornerIdx + 2] - controlPoints[cornerIdx + 1] - b1
  if controlPointsBefore >= 3:
    a3 = controlPoints[cornerIdx - 3] - controlPoints[cornerIdx - 2] -
      (controlPoints[cornerIdx - 2] - controlPoints[cornerIdx - 1]) - a2
    a2 = a2 * 3.0
  if controlPointsAfter >= 3:
    b3 = controlPoints[cornerIdx + 3] - controlPoints[cornerIdx + 2] -
      (controlPoints[cornerIdx + 2] - controlPoints[cornerIdx + 1]) - b2
    b2 = b2 * 3.0

  a1 = a1 * controlPointsBefore.float64
  b1 = b1 * controlPointsAfter.float64

  let a1NonZero = a1.x != 0 or a1.y != 0
  let b1NonZero = b1.x != 0 or b1.y != 0

  if a1NonZero and b1NonZero:
    let aScale = length(a1)
    let bScale = length(b1)
    var d = aScale * cross(a1, b2) + bScale * cross(a2, b1)
    if d != 0:
      return sign(d)
    d = aScale * aScale * cross(a1, b3) +
      aScale * bScale * cross(a2, b2) +
      bScale * bScale * cross(a3, b1)
    if d != 0:
      return sign(d)
    d = aScale * cross(a2, b3) + bScale * cross(a3, b2)
    if d != 0:
      return sign(d)
    return sign(cross(a3, b3))

  var s = 1
  if a1NonZero:
    b1 = a1
    let tmpA2 = a2
    a2 = b2
    b2 = tmpA2
    let tmpA3 = a3
    a3 = b3
    b3 = tmpA3
    s = -1

  if b1NonZero:
    var d = cross(a3, b1)
    if d != 0:
      return s * sign(d)
    d = cross(a2, b2)
    if d != 0:
      return s * sign(d)
    d = cross(a3, b2)
    if d != 0:
      return s * sign(d)
    d = cross(a2, b3)
    if d != 0:
      return s * sign(d)
    return s * sign(cross(a3, b3))

  let d = sqrt(length(a2)) * cross(a2, b3) + sqrt(length(b2)) * cross(a3, b2)
  if d != 0:
    return sign(d)
  sign(cross(a3, b3))

proc edgeOrder(edge: EdgeSegment): int {.inline.} =
  case edge.kind
  of esLine:
    1
  of esQuadratic:
    2
  of esCubic:
    3

proc convergentCurveOrdering(a, b: EdgeSegment): int =
  let aOrder = edgeOrder(a)
  let bOrder = edgeOrder(b)
  if aOrder < 1 or aOrder > 3 or bOrder < 1 or bOrder > 3:
    return 0

  var controlPoints: array[12, DVec2]
  let cornerIdx = 4
  let tmpIdx = 8

  for i in 0 .. aOrder:
    controlPoints[tmpIdx + i] = a.p[i]
  for i in 0 .. bOrder:
    controlPoints[cornerIdx + i] = b.p[i]

  if controlPoints[tmpIdx + aOrder] != controlPoints[cornerIdx]:
    return 0

  var aOrderVar = aOrder
  var bOrderVar = bOrder
  simplifyDegenerateCurve(controlPoints, tmpIdx, aOrderVar)
  simplifyDegenerateCurve(controlPoints, cornerIdx, bOrderVar)
  for i in 0 ..< aOrderVar:
    controlPoints[cornerIdx + i - aOrderVar] = controlPoints[tmpIdx + i]

  convergentCurveOrdering(controlPoints, cornerIdx, aOrderVar, bOrderVar)

proc splitInThirds(edge: EdgeSegment; part0, part1, part2: var EdgeSegment) =
  case edge.kind
  of esLine:
    part0 = newLine(edge.p[0], point(edge, 1.0 / 3.0), edge.color)
    part1 = newLine(point(edge, 1.0 / 3.0), point(edge, 2.0 / 3.0), edge.color)
    part2 = newLine(point(edge, 2.0 / 3.0), edge.p[1], edge.color)
  of esQuadratic:
    part0 = newQuadratic(edge.p[0], mix(edge.p[0], edge.p[1], 1.0 / 3.0), point(edge, 1.0 / 3.0), edge.color)
    part1 = newQuadratic(
      point(edge, 1.0 / 3.0),
      mix(mix(edge.p[0], edge.p[1], 5.0 / 9.0), mix(edge.p[1], edge.p[2], 4.0 / 9.0), 0.5),
      point(edge, 2.0 / 3.0),
      edge.color
    )
    part2 = newQuadratic(point(edge, 2.0 / 3.0), mix(edge.p[1], edge.p[2], 2.0 / 3.0), edge.p[2], edge.color)
  of esCubic:
    part0 = newCubic(
      edge.p[0],
      (if edge.p[0] == edge.p[1]: edge.p[0] else: mix(edge.p[0], edge.p[1], 1.0 / 3.0)),
      mix(mix(edge.p[0], edge.p[1], 1.0 / 3.0), mix(edge.p[1], edge.p[2], 1.0 / 3.0), 1.0 / 3.0),
      point(edge, 1.0 / 3.0),
      edge.color
    )
    part1 = newCubic(
      point(edge, 1.0 / 3.0),
      mix(
        mix(mix(edge.p[0], edge.p[1], 1.0 / 3.0), mix(edge.p[1], edge.p[2], 1.0 / 3.0), 1.0 / 3.0),
        mix(mix(edge.p[1], edge.p[2], 1.0 / 3.0), mix(edge.p[2], edge.p[3], 1.0 / 3.0), 1.0 / 3.0),
        2.0 / 3.0
      ),
      mix(
        mix(mix(edge.p[0], edge.p[1], 2.0 / 3.0), mix(edge.p[1], edge.p[2], 2.0 / 3.0), 2.0 / 3.0),
        mix(mix(edge.p[1], edge.p[2], 2.0 / 3.0), mix(edge.p[2], edge.p[3], 2.0 / 3.0), 2.0 / 3.0),
        1.0 / 3.0
      ),
      point(edge, 2.0 / 3.0),
      edge.color
    )
    part2 = newCubic(
      point(edge, 2.0 / 3.0),
      mix(mix(edge.p[1], edge.p[2], 2.0 / 3.0), mix(edge.p[2], edge.p[3], 2.0 / 3.0), 2.0 / 3.0),
      (if edge.p[2] == edge.p[3]: edge.p[3] else: mix(edge.p[2], edge.p[3], 2.0 / 3.0)),
      edge.p[3],
      edge.color
    )

proc reverse(edge: EdgeSegment) =
  case edge.kind
  of esLine:
    let tmp = edge.p[0]
    edge.p[0] = edge.p[1]
    edge.p[1] = tmp
  of esQuadratic:
    let tmp = edge.p[0]
    edge.p[0] = edge.p[2]
    edge.p[2] = tmp
  of esCubic:
    let tmp = edge.p[0]
    edge.p[0] = edge.p[3]
    edge.p[3] = tmp
    let tmp2 = edge.p[1]
    edge.p[1] = edge.p[2]
    edge.p[2] = tmp2

proc boundPoint(p: DVec2; xMin, yMin, xMax, yMax: var float64) {.inline.} =
  if p.x < xMin: xMin = p.x
  if p.y < yMin: yMin = p.y
  if p.x > xMax: xMax = p.x
  if p.y > yMax: yMax = p.y

proc bound(edge: EdgeSegment; xMin, yMin, xMax, yMax: var float64) =
  case edge.kind
  of esLine:
    boundPoint(edge.p[0], xMin, yMin, xMax, yMax)
    boundPoint(edge.p[1], xMin, yMin, xMax, yMax)
  of esQuadratic:
    boundPoint(edge.p[0], xMin, yMin, xMax, yMax)
    boundPoint(edge.p[2], xMin, yMin, xMax, yMax)
    let bot = (edge.p[1] - edge.p[0]) - (edge.p[2] - edge.p[1])
    if bot.x != 0:
      let t = (edge.p[1].x - edge.p[0].x) / bot.x
      if t > 0 and t < 1:
        boundPoint(point(edge, t), xMin, yMin, xMax, yMax)
    if bot.y != 0:
      let t = (edge.p[1].y - edge.p[0].y) / bot.y
      if t > 0 and t < 1:
        boundPoint(point(edge, t), xMin, yMin, xMax, yMax)
  of esCubic:
    boundPoint(edge.p[0], xMin, yMin, xMax, yMax)
    boundPoint(edge.p[3], xMin, yMin, xMax, yMax)
    let a0 = edge.p[1] - edge.p[0]
    let a1 = 2.0 * (edge.p[2] - edge.p[1] - a0)
    let a2 = edge.p[3] - 3.0 * edge.p[2] + 3.0 * edge.p[1] - edge.p[0]
    var params: array[2, float64]
    var solutions = solveQuadratic(params, a2.x, a1.x, a0.x)
    for i in 0 ..< solutions:
      if params[i] > 0 and params[i] < 1:
        boundPoint(point(edge, params[i]), xMin, yMin, xMax, yMax)
    solutions = solveQuadratic(params, a2.y, a1.y, a0.y)
    for i in 0 ..< solutions:
      if params[i] > 0 and params[i] < 1:
        boundPoint(point(edge, params[i]), xMin, yMin, xMax, yMax)


proc newContour(): Contour =
  Contour(edges: @[])

proc addEdge(contour: Contour; edge: EdgeSegment) =
  contour.edges.add(edge)

proc contourWinding(contour: Contour): int =
  if contour.edges.len == 0:
    return 0

  proc shoelace(a, b: DVec2): float64 =
    (b.x - a.x) * (a.y + b.y)

  var total = 0.0
  if contour.edges.len == 1:
    let a = point(contour.edges[0], 0)
    let b = point(contour.edges[0], 1.0 / 3.0)
    let c = point(contour.edges[0], 2.0 / 3.0)
    total += shoelace(a, b)
    total += shoelace(b, c)
    total += shoelace(c, a)
  elif contour.edges.len == 2:
    let a = point(contour.edges[0], 0)
    let b = point(contour.edges[0], 0.5)
    let c = point(contour.edges[1], 0)
    let d = point(contour.edges[1], 0.5)
    total += shoelace(a, b)
    total += shoelace(b, c)
    total += shoelace(c, d)
    total += shoelace(d, a)
  else:
    var prev = point(contour.edges[^1], 0)
    for edge in contour.edges:
      let cur = point(edge, 0)
      total += shoelace(prev, cur)
      prev = cur
  sign(total)

proc reverse(contour: Contour) =
  let count = contour.edges.len
  for i in 0 ..< count div 2:
    let tmp = contour.edges[i]
    contour.edges[i] = contour.edges[count - i - 1]
    contour.edges[count - i - 1] = tmp
  for edge in contour.edges:
    reverse(edge)

proc newShape(): Shape =
  Shape(contours: @[])

proc normalize(shape: Shape) =
  for contour in shape.contours:
    if contour.edges.len == 1:
      var part0, part1, part2: EdgeSegment
      splitInThirds(contour.edges[0], part0, part1, part2)
      contour.edges = @[part0, part1, part2]
    elif contour.edges.len > 0:
      var prevEdge = contour.edges[^1]
      for edge in contour.edges:
        let prevDir = normalizeVec(direction(prevEdge, 1))
        let curDir = normalizeVec(direction(edge, 0))
        if dot(prevDir, curDir) < msdfCornerDotEpsilon - 1:
          let denom = msdfCornerDotEpsilon - 1
          let factor = deconvergeOvershoot *
            sqrt(1 - denom * denom) / denom
          var axis = normalizeVec(curDir - prevDir) * factor
          if convergentCurveOrdering(prevEdge, edge) < 0:
            axis = -axis
          deconvergeEdge(prevEdge, 1, orthogonal(axis, true))
          deconvergeEdge(edge, 0, orthogonal(axis, false))
        prevEdge = edge

proc orientContours(shape: Shape) =
  type Intersection = object
    x: float64
    direction: int
    contourIndex: int

  let ratio = 0.5 * (sqrt(5.0) - 1.0)
  var orientations = newSeq[int](shape.contours.len)
  var intersections: seq[Intersection] = @[]

  for i, contour in shape.contours:
    if orientations[i] != 0 or contour.edges.len == 0:
      continue
    var y0 = point(contour.edges[0], 0).y
    var y1 = y0
    for edge in contour.edges:
      if y0 != y1:
        break
      y1 = point(edge, 1).y
    for edge in contour.edges:
      if y0 != y1:
        break
      y1 = point(edge, ratio).y
    let y = mix(y0, y1, ratio)

    intersections.setLen(0)
    for j, other in shape.contours:
      for edge in other.edges:
        var xs: array[3, float64]
        var dys: array[3, int]
        let n = scanlineIntersections(edge, xs, dys, y)
        for k in 0 ..< n:
          intersections.add(Intersection(x: xs[k], direction: dys[k], contourIndex: j))

    if intersections.len > 0:
      intersections.sort(proc(a, b: Intersection): int = cmp(a.x, b.x))
      for j in 1 ..< intersections.len:
        if intersections[j].x == intersections[j - 1].x:
          intersections[j].direction = 0
          intersections[j - 1].direction = 0
      for j in 0 ..< intersections.len:
        if intersections[j].direction != 0:
          let parity = j and 1
          let dirPos = if intersections[j].direction > 0: 1 else: 0
          orientations[intersections[j].contourIndex] += 2 * (parity xor dirPos) - 1

  for i, contour in shape.contours:
    if orientations[i] < 0:
      reverse(contour)

proc shapeBounds(shape: Shape): ShapeBounds =
  var bounds = ShapeBounds(l: 1e300, b: 1e300, r: -1e300, t: -1e300)
  for contour in shape.contours:
    for edge in contour.edges:
      bound(edge, bounds.l, bounds.b, bounds.r, bounds.t)
  bounds


proc symmetricalTrichotomy(position, n: int): int =
  int(3 + 2.875 * position.float64 / (n - 1).float64 - 1.4375 + 0.5) - 3

proc isCorner(aDir, bDir: DVec2; crossThreshold: float64): bool =
  dot(aDir, bDir) <= 0 or abs(cross(aDir, bDir)) > crossThreshold

proc seedExtract2(seed: var uint64): int =
  let v = int(seed and 1)
  seed = seed shr 1
  v

proc seedExtract3(seed: var uint64): int =
  let v = int(seed mod 3)
  seed = seed div 3
  v

proc initColor(seed: var uint64): EdgeColor =
  let colors = [ecCyan, ecMagenta, ecYellow]
  colors[seedExtract3(seed)]

proc switchColor(color: var EdgeColor; seed: var uint64) =
  let shifted = (color shl (1 + seedExtract2(seed))).uint8
  color = ((shifted or (shifted shr 3)) and ecWhite)

proc switchColor(color: var EdgeColor; seed: var uint64; banned: EdgeColor) =
  let combined = color and banned
  if combined == ecRed or combined == ecGreen or combined == ecBlue:
    color = combined xor ecWhite
  else:
    switchColor(color, seed)

proc edgeColoringSimple(shape: Shape; angleThreshold: float64; seed: uint64 = 0) =
  var seedVar = seed
  let crossThreshold = sin(angleThreshold)
  var color = initColor(seedVar)
  var corners: seq[int]

  for contour in shape.contours:
    if contour.edges.len == 0:
      continue

    corners.setLen(0)
    var prevDir = normalizeVec(direction(contour.edges[^1], 1))
    var index = 0
    for edge in contour.edges:
      if isCorner(prevDir, normalizeVec(direction(edge, 0)), crossThreshold):
        corners.add(index)
      prevDir = normalizeVec(direction(edge, 1))
      inc index

    if corners.len == 0:
      switchColor(color, seedVar)
      for edge in contour.edges:
        edge.color = color
    elif corners.len == 1:
      var colors: array[3, EdgeColor]
      switchColor(color, seedVar)
      colors[0] = color
      colors[1] = ecWhite
      switchColor(color, seedVar)
      colors[2] = color
      let corner = corners[0]
      if contour.edges.len >= 3:
        let m = contour.edges.len
        for i in 0 ..< m:
          let idx = (corner + i) mod m
          contour.edges[idx].color = colors[1 + symmetricalTrichotomy(i, m)]
      elif contour.edges.len >= 1:
        var parts: array[7, EdgeSegment]
        splitInThirds(contour.edges[0], parts[0 + 3 * corner], parts[1 + 3 * corner], parts[2 + 3 * corner])
        if contour.edges.len >= 2:
          splitInThirds(contour.edges[1], parts[3 - 3 * corner], parts[4 - 3 * corner], parts[5 - 3 * corner])
          parts[0].color = colors[0]
          parts[1].color = colors[0]
          parts[2].color = colors[1]
          parts[3].color = colors[1]
          parts[4].color = colors[2]
          parts[5].color = colors[2]
        else:
          parts[0].color = colors[0]
          parts[1].color = colors[1]
          parts[2].color = colors[2]
        contour.edges.setLen(0)
        for part in parts:
          if part != nil:
            contour.edges.add(part)
    else:
      let cornerCount = corners.len
      var spline = 0
      let start = corners[0]
      let m = contour.edges.len
      switchColor(color, seedVar)
      let initialColor = color
      for i in 0 ..< m:
        let idx = (start + i) mod m
        if spline + 1 < cornerCount and corners[spline + 1] == idx:
          inc spline
          switchColor(color, seedVar, EdgeColor((spline == cornerCount - 1).uint8 * initialColor))
        contour.edges[idx].color = color


proc initRange(rangeWidth: float64): Range =
  Range(lower: -0.5 * rangeWidth, upper: 0.5 * rangeWidth)

proc initDistanceMapping(range: Range): DistanceMapping =
  DistanceMapping(
    scale: 1 / (range.upper - range.lower),
    translate: -range.lower
  )

proc mapDistance(mapping: DistanceMapping; distance: float64): float64 {.inline.} =
  mapping.scale * (distance + mapping.translate)


proc initProjection(scale: DVec2; translate: DVec2): Projection =
  Projection(scale: scale, translate: translate)

proc unproject(projection: Projection; p: DVec2): DVec2 {.inline.} =
  dvec2(p.x / projection.scale.x - projection.translate.x,
        p.y / projection.scale.y - projection.translate.y)


proc reset(selector: var PerpSelectorBase) =
  selector.minTrueDistance = initSignedDistance()
  selector.minNegativePerp = -abs(selector.minTrueDistance.distance)
  selector.minPositivePerp = abs(selector.minTrueDistance.distance)
  selector.nearEdge = nil
  selector.nearEdgeParam = 0

proc addEdgeTrueDistance(selector: var PerpSelectorBase; edge: EdgeSegment; distance: SignedDistance; param: float64) =
  if sdLess(distance, selector.minTrueDistance):
    selector.minTrueDistance = distance
    selector.nearEdge = edge
    selector.nearEdgeParam = param

proc addEdgePerpendicularDistance(selector: var PerpSelectorBase; distance: float64) =
  if distance <= 0 and distance > selector.minNegativePerp:
    selector.minNegativePerp = distance
  if distance >= 0 and distance < selector.minPositivePerp:
    selector.minPositivePerp = distance

proc merge(selector: var PerpSelectorBase; other: PerpSelectorBase) =
  if sdLess(other.minTrueDistance, selector.minTrueDistance):
    selector.minTrueDistance = other.minTrueDistance
    selector.nearEdge = other.nearEdge
    selector.nearEdgeParam = other.nearEdgeParam
  if other.minNegativePerp > selector.minNegativePerp:
    selector.minNegativePerp = other.minNegativePerp
  if other.minPositivePerp < selector.minPositivePerp:
    selector.minPositivePerp = other.minPositivePerp

proc computeDistance(selector: PerpSelectorBase; p: DVec2): float64 =
  var minDistance = if selector.minTrueDistance.distance < 0:
    selector.minNegativePerp
  else:
    selector.minPositivePerp
  if selector.nearEdge != nil:
    var distance = selector.minTrueDistance
    distanceToPerpendicularDistance(selector.nearEdge, distance, p, selector.nearEdgeParam)
    if abs(distance.distance) < abs(minDistance):
      minDistance = distance.distance
  minDistance

proc trueDistance(selector: PerpSelectorBase): SignedDistance =
  selector.minTrueDistance

proc getPerpendicularDistance(distance: var float64; ep: DVec2; edgeDir: DVec2): bool =
  let ts = dot(ep, edgeDir)
  if ts > 0:
    let perpendicular = cross(ep, edgeDir)
    if abs(perpendicular) < abs(distance):
      distance = perpendicular
      return true
  false

proc reset(selector: var MultiDistanceSelector; p: DVec2) =
  selector.p = p
  reset(selector.r)
  reset(selector.g)
  reset(selector.b)

proc addEdge(selector: var MultiDistanceSelector; prevEdge, edge, nextEdge: EdgeSegment) =
  var param = 0.0
  let distance = signedDistance(edge, selector.p, param)

  if (edge.color and ecRed) != 0:
    addEdgeTrueDistance(selector.r, edge, distance, param)
  if (edge.color and ecGreen) != 0:
    addEdgeTrueDistance(selector.g, edge, distance, param)
  if (edge.color and ecBlue) != 0:
    addEdgeTrueDistance(selector.b, edge, distance, param)

  let ap = selector.p - point(edge, 0)
  let bp = selector.p - point(edge, 1)
  let aDir = normalizeVec(direction(edge, 0), true)
  let bDir = normalizeVec(direction(edge, 1), true)
  let prevDir = normalizeVec(direction(prevEdge, 1), true)
  let nextDir = normalizeVec(direction(nextEdge, 0), true)
  let add = dot(ap, normalizeVec(prevDir + aDir, true))
  let bdd = -dot(bp, normalizeVec(bDir + nextDir, true))

  if add > 0:
    var pd = distance.distance
    if getPerpendicularDistance(pd, ap, -aDir):
      pd = -pd
      if (edge.color and ecRed) != 0:
        addEdgePerpendicularDistance(selector.r, pd)
      if (edge.color and ecGreen) != 0:
        addEdgePerpendicularDistance(selector.g, pd)
      if (edge.color and ecBlue) != 0:
        addEdgePerpendicularDistance(selector.b, pd)

  if bdd > 0:
    var pd = distance.distance
    if getPerpendicularDistance(pd, bp, bDir):
      if (edge.color and ecRed) != 0:
        addEdgePerpendicularDistance(selector.r, pd)
      if (edge.color and ecGreen) != 0:
        addEdgePerpendicularDistance(selector.g, pd)
      if (edge.color and ecBlue) != 0:
        addEdgePerpendicularDistance(selector.b, pd)

proc merge(selector: var MultiDistanceSelector; other: MultiDistanceSelector) =
  merge(selector.r, other.r)
  merge(selector.g, other.g)
  merge(selector.b, other.b)

proc distance(selector: MultiDistanceSelector): MultiDistance =
  MultiDistance(
    r: computeDistance(selector.r, selector.p),
    g: computeDistance(selector.g, selector.p),
    b: computeDistance(selector.b, selector.p)
  )

proc trueDistance(selector: MultiDistanceSelector): SignedDistance =
  var dist = trueDistance(selector.r)
  if sdLess(trueDistance(selector.g), dist):
    dist = trueDistance(selector.g)
  if sdLess(trueDistance(selector.b), dist):
    dist = trueDistance(selector.b)
  dist


proc resolveDistance(distance: MultiDistance): float64 {.inline.} =
  median(distance.r, distance.g, distance.b)

proc invertDistance(distance: MultiDistance): MultiDistance {.inline.} =
  MultiDistance(r: -distance.r, g: -distance.g, b: -distance.b)

proc resolveDistance(distance: MultiAndTrueDistance): float64 {.inline.} =
  median(distance.r, distance.g, distance.b)

proc invertDistance(distance: MultiAndTrueDistance): MultiAndTrueDistance {.inline.} =
  MultiAndTrueDistance(r: -distance.r, g: -distance.g, b: -distance.b, a: -distance.a)


proc distanceForShape(shape: Shape; p: DVec2; overlapSupport: bool): MultiDistance =
  if shape.contours.len == 0:
    return MultiDistance(r: 0, g: 0, b: 0)

  if not overlapSupport:
    var selector = MultiDistanceSelector()
    reset(selector, p)
    for contour in shape.contours:
      if contour.edges.len == 0:
        continue
      var prevEdge = if contour.edges.len >= 2: contour.edges[^2] else: contour.edges[0]
      var curEdge = contour.edges[^1]
      for edge in contour.edges:
        addEdge(selector, prevEdge, curEdge, edge)
        prevEdge = curEdge
        curEdge = edge
    return distance(selector)

  var windings = newSeq[int](shape.contours.len)
  for i, contour in shape.contours:
    windings[i] = contourWinding(contour)

  var edgeSelectors = newSeq[MultiDistanceSelector](shape.contours.len)
  for i, contour in shape.contours:
    reset(edgeSelectors[i], p)
    if contour.edges.len == 0:
      continue
    var prevEdge = if contour.edges.len >= 2: contour.edges[^2] else: contour.edges[0]
    var curEdge = contour.edges[^1]
    for edge in contour.edges:
      addEdge(edgeSelectors[i], prevEdge, curEdge, edge)
      prevEdge = curEdge
      curEdge = edge

  var shapeSelector = MultiDistanceSelector()
  var innerSelector = MultiDistanceSelector()
  var outerSelector = MultiDistanceSelector()
  reset(shapeSelector, p)
  reset(innerSelector, p)
  reset(outerSelector, p)

  for i in 0 ..< edgeSelectors.len:
    let edgeDistance = distance(edgeSelectors[i])
    merge(shapeSelector, edgeSelectors[i])
    if windings[i] > 0 and resolveDistance(edgeDistance) >= 0:
      merge(innerSelector, edgeSelectors[i])
    if windings[i] < 0 and resolveDistance(edgeDistance) <= 0:
      merge(outerSelector, edgeSelectors[i])

  let shapeDistance = distance(shapeSelector)
  var innerDistance = distance(innerSelector)
  var outerDistance = distance(outerSelector)
  let innerScalar = resolveDistance(innerDistance)
  let outerScalar = resolveDistance(outerDistance)

  var resultDistance = MultiDistance(r: -1.0e300, g: -1.0e300, b: -1.0e300)
  var winding = 0

  if innerScalar >= 0 and abs(innerScalar) <= abs(outerScalar):
    resultDistance = innerDistance
    winding = 1
    for i in 0 ..< edgeSelectors.len:
      if windings[i] > 0:
        let contourDistance = distance(edgeSelectors[i])
        if abs(resolveDistance(contourDistance)) < abs(outerScalar) and
            resolveDistance(contourDistance) > resolveDistance(resultDistance):
          resultDistance = contourDistance
  elif outerScalar <= 0 and abs(outerScalar) < abs(innerScalar):
    resultDistance = outerDistance
    winding = -1
    for i in 0 ..< edgeSelectors.len:
      if windings[i] < 0:
        let contourDistance = distance(edgeSelectors[i])
        if abs(resolveDistance(contourDistance)) < abs(innerScalar) and
            resolveDistance(contourDistance) < resolveDistance(resultDistance):
          resultDistance = contourDistance
  else:
    return shapeDistance

  for i in 0 ..< edgeSelectors.len:
    if windings[i] != winding:
      let contourDistance = distance(edgeSelectors[i])
      if resolveDistance(contourDistance) * resolveDistance(resultDistance) >= 0 and
          abs(resolveDistance(contourDistance)) < abs(resolveDistance(resultDistance)):
        resultDistance = contourDistance

  if resolveDistance(resultDistance) == resolveDistance(shapeDistance):
    resultDistance = shapeDistance

  resultDistance

proc trueDistanceForShape(shape: Shape; p: DVec2; overlapSupport: bool): float64 =
  if shape.contours.len == 0:
    return 0

  if not overlapSupport:
    var selector = MultiDistanceSelector()
    reset(selector, p)
    for contour in shape.contours:
      if contour.edges.len == 0:
        continue
      var prevEdge = if contour.edges.len >= 2: contour.edges[^2] else: contour.edges[0]
      var curEdge = contour.edges[^1]
      for edge in contour.edges:
        addEdge(selector, prevEdge, curEdge, edge)
        prevEdge = curEdge
        curEdge = edge
    return trueDistance(selector).distance

  var windings = newSeq[int](shape.contours.len)
  for i, contour in shape.contours:
    windings[i] = contourWinding(contour)

  var edgeSelectors = newSeq[MultiDistanceSelector](shape.contours.len)
  for i, contour in shape.contours:
    reset(edgeSelectors[i], p)
    if contour.edges.len == 0:
      continue
    var prevEdge = if contour.edges.len >= 2: contour.edges[^2] else: contour.edges[0]
    var curEdge = contour.edges[^1]
    for edge in contour.edges:
      addEdge(edgeSelectors[i], prevEdge, curEdge, edge)
      prevEdge = curEdge
      curEdge = edge

  var shapeSelector = MultiDistanceSelector()
  var innerSelector = MultiDistanceSelector()
  var outerSelector = MultiDistanceSelector()
  reset(shapeSelector, p)
  reset(innerSelector, p)
  reset(outerSelector, p)

  for i in 0 ..< edgeSelectors.len:
    let edgeDistance = distance(edgeSelectors[i])
    merge(shapeSelector, edgeSelectors[i])
    if windings[i] > 0 and resolveDistance(edgeDistance) >= 0:
      merge(innerSelector, edgeSelectors[i])
    if windings[i] < 0 and resolveDistance(edgeDistance) <= 0:
      merge(outerSelector, edgeSelectors[i])

  let shapeDistance = distance(shapeSelector)
  var innerDistance = distance(innerSelector)
  var outerDistance = distance(outerSelector)
  let innerScalar = resolveDistance(innerDistance)
  let outerScalar = resolveDistance(outerDistance)

  var resultDistance = MultiDistance(r: -1.0e300, g: -1.0e300, b: -1.0e300)
  var resultTrue = -1.0e300
  var winding = 0

  if innerScalar >= 0 and abs(innerScalar) <= abs(outerScalar):
    resultDistance = innerDistance
    resultTrue = trueDistance(innerSelector).distance
    winding = 1
    for i in 0 ..< edgeSelectors.len:
      if windings[i] > 0:
        let contourDistance = distance(edgeSelectors[i])
        if abs(resolveDistance(contourDistance)) < abs(outerScalar) and
            resolveDistance(contourDistance) > resolveDistance(resultDistance):
          resultDistance = contourDistance
          resultTrue = trueDistance(edgeSelectors[i]).distance
  elif outerScalar <= 0 and abs(outerScalar) < abs(innerScalar):
    resultDistance = outerDistance
    resultTrue = trueDistance(outerSelector).distance
    winding = -1
    for i in 0 ..< edgeSelectors.len:
      if windings[i] < 0:
        let contourDistance = distance(edgeSelectors[i])
        if abs(resolveDistance(contourDistance)) < abs(innerScalar) and
            resolveDistance(contourDistance) < resolveDistance(resultDistance):
          resultDistance = contourDistance
          resultTrue = trueDistance(edgeSelectors[i]).distance
  else:
    return trueDistance(shapeSelector).distance

  for i in 0 ..< edgeSelectors.len:
    if windings[i] != winding:
      let contourDistance = distance(edgeSelectors[i])
      if resolveDistance(contourDistance) * resolveDistance(resultDistance) >= 0 and
          abs(resolveDistance(contourDistance)) < abs(resolveDistance(resultDistance)):
        resultDistance = contourDistance
        resultTrue = trueDistance(edgeSelectors[i]).distance

  if resolveDistance(resultDistance) == resolveDistance(shapeDistance):
    resultDistance = shapeDistance
    resultTrue = trueDistance(shapeSelector).distance

  resultTrue

proc distanceForShapeMtsdf(shape: Shape; p: DVec2; overlapSupport: bool): MultiAndTrueDistance =
  let dist = distanceForShape(shape, p, overlapSupport)
  MultiAndTrueDistance(
    r: dist.r,
    g: dist.g,
    b: dist.b,
    a: trueDistanceForShape(shape, p, overlapSupport)
  )

proc isSpaceChar(c: char): bool {.inline.} =
  c == ' ' or c == '\t' or c == '\n' or c == '\r'

proc parseNumber(path: string; idx: var int): float64 {.raises: [PixieError].} =
  while idx < path.len and (isSpaceChar(path[idx]) or path[idx] == ','):
    inc idx
  let start = idx
  if idx < path.len and (path[idx] == '-' or path[idx] == '+'):
    inc idx
  while idx < path.len and path[idx].isDigit:
    inc idx
  if idx < path.len and path[idx] == '.':
    inc idx
    while idx < path.len and path[idx].isDigit:
      inc idx
  if idx < path.len and (path[idx] == 'e' or path[idx] == 'E'):
    inc idx
    if idx < path.len and (path[idx] == '-' or path[idx] == '+'):
      inc idx
    while idx < path.len and path[idx].isDigit:
      inc idx
  try:
    parseFloat(path[start ..< idx])
  except ValueError:
    raise newException(PixieError, "Invalid path number")

proc hasNumber(path: string; idx: int): bool =
  var i = idx
  while i < path.len and (isSpaceChar(path[i]) or path[i] == ','):
    inc i
  if i >= path.len:
    return false
  path[i].isDigit or path[i] == '-' or path[i] == '+' or path[i] == '.'

proc isCommandChar(c: char): bool =
  c in {'M', 'm', 'L', 'l', 'H', 'h', 'V', 'v', 'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'Z', 'z'}

proc shapeFromPath(path: Path): Shape {.raises: [PixieError].} =
  let pathStr = $path
  var shape = newShape()
  var contour = newContour()
  var idx = 0
  var current = dvec2(0, 0)
  var start = dvec2(0, 0)
  var lastCtrl = dvec2(0, 0)
  var lastCmd = '\0'

  proc finishContour(closeContour: bool) =
    if contour.edges.len > 0:
      if closeContour and current != start:
        contour.addEdge(newLine(current, start))
      shape.contours.add(contour)
    contour = newContour()

  while idx < pathStr.len:
    while idx < pathStr.len and isSpaceChar(pathStr[idx]):
      inc idx
    if idx >= pathStr.len:
      break
    var cmd = lastCmd
    if isCommandChar(pathStr[idx]):
      cmd = pathStr[idx]
      inc idx
    if cmd == '\0':
      break

    let isRel = cmd.isLowerAscii
    let upCmd = cmd.toUpperAscii

    case upCmd
    of 'M':
      if not hasNumber(pathStr, idx):
        continue
      finishContour(true)
      let x = parseNumber(pathStr, idx)
      let y = parseNumber(pathStr, idx)
      let move = dvec2(x, y)
      if isRel:
        current = current + move
      else:
        current = move
      start = current
      lastCmd = cmd
      while hasNumber(pathStr, idx):
        let nx = parseNumber(pathStr, idx)
        let ny = parseNumber(pathStr, idx)
        var next = dvec2(nx, ny)
        if isRel:
          next = next + current
        contour.addEdge(newLine(current, next))
        current = next
        lastCmd = 'L'
    of 'L':
      while hasNumber(pathStr, idx):
        let x = parseNumber(pathStr, idx)
        let y = parseNumber(pathStr, idx)
        var next = dvec2(x, y)
        if isRel:
          next = next + current
        contour.addEdge(newLine(current, next))
        current = next
      lastCmd = cmd
    of 'H':
      while hasNumber(pathStr, idx):
        let x = parseNumber(pathStr, idx)
        let nx = if isRel: current.x + x else: x
        let next = dvec2(nx, current.y)
        contour.addEdge(newLine(current, next))
        current = next
      lastCmd = cmd
    of 'V':
      while hasNumber(pathStr, idx):
        let y = parseNumber(pathStr, idx)
        let ny = if isRel: current.y + y else: y
        let next = dvec2(current.x, ny)
        contour.addEdge(newLine(current, next))
        current = next
      lastCmd = cmd
    of 'C':
      while hasNumber(pathStr, idx):
        let x1 = parseNumber(pathStr, idx)
        let y1 = parseNumber(pathStr, idx)
        let x2 = parseNumber(pathStr, idx)
        let y2 = parseNumber(pathStr, idx)
        let x = parseNumber(pathStr, idx)
        let y = parseNumber(pathStr, idx)
        var p1 = dvec2(x1, y1)
        var p2 = dvec2(x2, y2)
        var p3 = dvec2(x, y)
        if isRel:
          p1 = p1 + current
          p2 = p2 + current
          p3 = p3 + current
        contour.addEdge(newCubic(current, p1, p2, p3))
        current = p3
        lastCtrl = p2
      lastCmd = cmd
    of 'S':
      while hasNumber(pathStr, idx):
        let x2 = parseNumber(pathStr, idx)
        let y2 = parseNumber(pathStr, idx)
        let x = parseNumber(pathStr, idx)
        let y = parseNumber(pathStr, idx)
        var p2 = dvec2(x2, y2)
        var p3 = dvec2(x, y)
        if isRel:
          p2 = p2 + current
          p3 = p3 + current
        let p1 = if lastCmd in ['C', 'c', 'S', 's']:
          current * 2 - lastCtrl
        else:
          current
        contour.addEdge(newCubic(current, p1, p2, p3))
        current = p3
        lastCtrl = p2
      lastCmd = cmd
    of 'Q':
      while hasNumber(pathStr, idx):
        let x1 = parseNumber(pathStr, idx)
        let y1 = parseNumber(pathStr, idx)
        let x = parseNumber(pathStr, idx)
        let y = parseNumber(pathStr, idx)
        var p1 = dvec2(x1, y1)
        var p2 = dvec2(x, y)
        if isRel:
          p1 = p1 + current
          p2 = p2 + current
        contour.addEdge(newQuadratic(current, p1, p2))
        current = p2
        lastCtrl = p1
      lastCmd = cmd
    of 'T':
      while hasNumber(pathStr, idx):
        let x = parseNumber(pathStr, idx)
        let y = parseNumber(pathStr, idx)
        var p2 = dvec2(x, y)
        if isRel:
          p2 = p2 + current
        let p1 = if lastCmd in ['Q', 'q', 'T', 't']:
          current * 2 - lastCtrl
        else:
          current
        contour.addEdge(newQuadratic(current, p1, p2))
        current = p2
        lastCtrl = p1
      lastCmd = cmd
    of 'Z':
      if current != start:
        contour.addEdge(newLine(current, start))
      finishContour(false)
      current = start
      lastCmd = cmd
    else:
      lastCmd = cmd

  finishContour(true)
  shape


proc toByte(value: float64): uint8 =
  let clamped = clamp01(value)
  uint8(round(clamped * 255.0))

proc initDistanceMappingInverse(range: Range): DistanceMapping =
  let width = range.upper - range.lower
  let denom = if width == 0: 1.0 else: width
  DistanceMapping(
    scale: width,
    translate: range.lower / denom
  )

proc unpremultiply(value, alpha: float64): float64 {.inline.} =
  if alpha <= 0.0:
    0.0
  else:
    clamp01(value / alpha)

proc interpolateMsdf(sdf: Image; pos: DVec2): array[3, float64] =
  var px = clamp(pos.x, 0.0, sdf.width.float64)
  var py = clamp(pos.y, 0.0, sdf.height.float64)
  px -= 0.5
  py -= 0.5

  let lBase = floor(px)
  let bBase = floor(py)
  let lr = px - lBase
  let bt = py - bBase
  let l = clampInt(lBase.int, 0, sdf.width - 1)
  let b = clampInt(bBase.int, 0, sdf.height - 1)
  let r = clampInt(l + 1, 0, sdf.width - 1)
  let t = clampInt(b + 1, 0, sdf.height - 1)
  let width = sdf.width

  let inv = 1.0 / 255.0
  let lb = sdf.data[b * width + l]
  let rb = sdf.data[b * width + r]
  let lt = sdf.data[t * width + l]
  let rt = sdf.data[t * width + r]
  let lba = lb.a.float64 * inv
  let rba = rb.a.float64 * inv
  let lta = lt.a.float64 * inv
  let rta = rt.a.float64 * inv
  let lbr = unpremultiply(lb.r.float64 * inv, lba)
  let lbg = unpremultiply(lb.g.float64 * inv, lba)
  let lbb = unpremultiply(lb.b.float64 * inv, lba)
  let rbr = unpremultiply(rb.r.float64 * inv, rba)
  let rbg = unpremultiply(rb.g.float64 * inv, rba)
  let rbb = unpremultiply(rb.b.float64 * inv, rba)
  let ltr = unpremultiply(lt.r.float64 * inv, lta)
  let ltg = unpremultiply(lt.g.float64 * inv, lta)
  let ltb = unpremultiply(lt.b.float64 * inv, lta)
  let rtr = unpremultiply(rt.r.float64 * inv, rta)
  let rtg = unpremultiply(rt.g.float64 * inv, rta)
  let rtb = unpremultiply(rt.b.float64 * inv, rta)
  result[0] = lerp(
    lerp(lbr, rbr, lr),
    lerp(ltr, rtr, lr),
    bt
  )
  result[1] = lerp(
    lerp(lbg, rbg, lr),
    lerp(ltg, rtg, lr),
    bt
  )
  result[2] = lerp(
    lerp(lbb, rbb, lr),
    lerp(ltb, rtb, lr),
    bt
  )

proc sampleMsdfMedian(sdf: Image; x, y: float32): float64 {.inline.} =
  let x0 = x.floor.int
  let y0 = y.floor.int
  let x1 = x0 + 1
  let y1 = y0 + 1
  let xf = x - x.floor
  let yf = y - y.floor

  let inv = 1.0 / 255.0
  let c00 = sdf[x0, y0]
  let c10 = sdf[x1, y0]
  let c01 = sdf[x0, y1]
  let c11 = sdf[x1, y1]

  let a00 = c00.a.float64 * inv
  let a10 = c10.a.float64 * inv
  let a01 = c01.a.float64 * inv
  let a11 = c11.a.float64 * inv

  let r00 = unpremultiply(c00.r.float64 * inv, a00)
  let g00 = unpremultiply(c00.g.float64 * inv, a00)
  let b00 = unpremultiply(c00.b.float64 * inv, a00)
  let r10 = unpremultiply(c10.r.float64 * inv, a10)
  let g10 = unpremultiply(c10.g.float64 * inv, a10)
  let b10 = unpremultiply(c10.b.float64 * inv, a10)
  let r01 = unpremultiply(c01.r.float64 * inv, a01)
  let g01 = unpremultiply(c01.g.float64 * inv, a01)
  let b01 = unpremultiply(c01.b.float64 * inv, a01)
  let r11 = unpremultiply(c11.r.float64 * inv, a11)
  let g11 = unpremultiply(c11.g.float64 * inv, a11)
  let b11 = unpremultiply(c11.b.float64 * inv, a11)

  let r = lerp(lerp(r00, r10, xf), lerp(r01, r11, xf), yf)
  let g = lerp(lerp(g00, g10, xf), lerp(g01, g11, xf), yf)
  let b = lerp(lerp(b00, b10, xf), lerp(b01, b11, xf), yf)

  median(r, g, b)

proc renderMsdf*(sdf: Image; pxRange: float64; width, height: int; sdThreshold = 0.5): Image =
  let srcWidth = sdf.width
  let srcHeight = sdf.height
  let scale = dvec2(
    srcWidth.float64 / width.float64,
    srcHeight.float64 / height.float64
  )
  let output = newImage(width, height)
  let range = Range(lower: -0.5 * pxRange, upper: 0.5 * pxRange)

  if range.lower == range.upper:
    for y in 0 ..< height:
      for x in 0 ..< width:
        let sample = interpolateMsdf(sdf, dvec2(
          scale.x * (x.float64 + 0.5),
          scale.y * (y.float64 + 0.5)
        ))
        let sd = median(sample[0], sample[1], sample[2])
        let v = if sd >= sdThreshold: 1.0 else: 0.0
        let byte = toByte(v)
        output.data[y * width + x] = rgba(byte, byte, byte, 255).rgbx()
    return output

  let scaleFactor = (width + height).float64 / (srcWidth + srcHeight).float64
  let scaledRange = Range(
    lower: range.lower * scaleFactor,
    upper: range.upper * scaleFactor
  )
  let mapping = initDistanceMappingInverse(scaledRange)
  let sdBias = 0.5 - sdThreshold

  for y in 0 ..< height:
    for x in 0 ..< width:
      let sample = interpolateMsdf(sdf, dvec2(
        scale.x * (x.float64 + 0.5),
        scale.y * (y.float64 + 0.5)
      ))
      let sd = median(sample[0], sample[1], sample[2])
      let v = clamp01(mapDistance(mapping, sd + sdBias) + 0.5)
      let byte = toByte(v)
      output.data[y * width + x] = rgba(byte, byte, byte, 255).rgbx()

  output

proc renderMsdf*(sdf: Image; pxRange: float64; sdThreshold = 0.5): Image =
  renderMsdf(sdf, pxRange, sdf.width, sdf.height, sdThreshold)

proc renderMsdf*(glyph: MsdfGlyphResult; width, height: int; sdThreshold = 0.5): Image =
  let pxRange = glyph.range * glyph.scale
  renderMsdf(glyph.image, pxRange, width, height, sdThreshold)

proc renderMsdf*(glyph: MsdfGlyphResult; sdThreshold = 0.5): Image =
  renderMsdf(glyph, glyph.image.width, glyph.image.height, sdThreshold)

proc blitMsdfGlyph*(
  canvas: Image;
  msdf: Image;
  dx, dy: int;
  width, height: int;
  pxRange: float64;
  sdThreshold = 0.5;
  supersample = 1;
  fgColor = rgba(0, 0, 0, 255);
  bgColor = rgba(255, 255, 255, 255)
) =
  let ss = max(1, supersample)
  let invSamples = 1.0 / (ss * ss).float64
  let scaleX = msdf.width.float64 / width.float64
  let scaleY = msdf.height.float64 / height.float64
  let heightMax = msdf.height.float64 - 1.0
  let screenPxRange = max(
    1.0,
    pxRange * 0.5 * (
      width.float64 / msdf.width.float64 +
      height.float64 / msdf.height.float64
    )
  )
  let fg = fgColor.rgbx()
  let bg = bgColor.rgbx()

  for y in 0 ..< height:
    let destY = dy + y
    if destY < 0 or destY >= canvas.height:
      continue
    for x in 0 ..< width:
      let destX = dx + x
      if destX < 0 or destX >= canvas.width:
        continue
      var coverage = 0.0
      for sy in 0 ..< ss:
        let fy = (sy.float64 + 0.5) / ss.float64
        for sx in 0 ..< ss:
          let fx = (sx.float64 + 0.5) / ss.float64
          let baseX = clamp(scaleX * (x.float64 + fx), 0.0, msdf.width.float64)
          let baseY = clamp(scaleY * (y.float64 + fy), 0.0, msdf.height.float64)
          let sampleX = baseX - 0.5
          let sampleY = baseY - 0.5
          let sampleYFlip = heightMax - sampleY
          let sd = sampleMsdfMedian(msdf, sampleX.float32, sampleYFlip.float32)
          let screenPxDistance = screenPxRange * (sd - sdThreshold)
          let opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0)
          coverage += opacity
      let v = coverage * invSamples
      let src = mix(bg, fg, v.float32)
      let destIdx = destY * canvas.width + destX
      canvas.data[destIdx] = blendNormal(canvas.data[destIdx], src)

proc blitMsdfGlyph*(
  canvas: Image;
  msdf: Image;
  dx, dy: int;
  pxRange: float64;
  sdThreshold = 0.5;
  supersample = 1;
  fgColor = rgba(0, 0, 0, 255);
  bgColor = rgba(255, 255, 255, 255)
) =
  blitMsdfGlyph(canvas, msdf, dx, dy, msdf.width, msdf.height, pxRange, sdThreshold, supersample, fgColor, bgColor)

proc blitMsdfGlyph*(
  canvas: Image;
  glyph: MsdfGlyphResult;
  dx, dy: int;
  width, height: int;
  sdThreshold = 0.5;
  supersample = 1;
  fgColor = rgba(0, 0, 0, 255);
  bgColor = rgba(255, 255, 255, 255)
) =
  let pxRange = glyph.range * glyph.scale
  blitMsdfGlyph(canvas, glyph.image, dx, dy, width, height, pxRange, sdThreshold, supersample, fgColor, bgColor)

proc makeProjection(bounds: ShapeBounds; width, height: int; pxRange: float64): tuple[scale: float64, translate: DVec2] =
  var l = bounds.l
  var b = bounds.b
  var r = bounds.r
  var t = bounds.t

  if l >= r or b >= t:
    l = 0
    b = 0
    r = 1
    t = 1

  let dims = dvec2(r - l, t - b)
  let rangeLower = -0.5 * pxRange
  var frame = dvec2(width.float64, height.float64)
  frame = frame + dvec2(2 * rangeLower, 2 * rangeLower)

  let safeDims = if dims.x <= 0 or dims.y <= 0: dvec2(1, 1) else: dims
  if safeDims.x * frame.y < safeDims.y * frame.x:
    let scale = frame.y / safeDims.y
    var translate = dvec2(
      0.5 * (frame.x / frame.y * safeDims.y - safeDims.x) - l,
      -b
    )
    translate = translate - dvec2(rangeLower / scale, rangeLower / scale)
    return (scale, translate)
  else:
    let scale = frame.x / safeDims.x
    var translate = dvec2(
      -l,
      0.5 * (frame.y / frame.x * safeDims.x - safeDims.y) - b
    )
    translate = translate - dvec2(rangeLower / scale, rangeLower / scale)
    return (scale, translate)

proc generateMsdfFromShape(shape: Shape; width, height: int; scale, translate: DVec2; pxRange: float64; overlapSupport: bool): MsdfGlyphResult =
  let range = pxRange / min(scale.x, scale.y)
  let mapping = initDistanceMapping(initRange(range))
  let projection = initProjection(scale, translate)
  let image = newImage(width, height)

  for y in 0 ..< height:
    let py = height.float64 - (y.float64 + 0.5)
    for x in 0 ..< width:
      let p = unproject(projection, dvec2(x.float64 + 0.5, py))
      let dist = invertDistance(distanceForShape(shape, p, overlapSupport))
      let r = mapDistance(mapping, dist.r)
      let g = mapDistance(mapping, dist.g)
      let b = mapDistance(mapping, dist.b)
      let color = rgba(toByte(r), toByte(g), toByte(b), 255)
      image.data[y * width + x] = color.rgbx()

  MsdfGlyphResult(
    image: image,
    scale: scale.x,
    translate: translate,
    bounds: shapeBounds(shape),
    range: range
  )

proc generateMtsdfFromShape(shape: Shape; width, height: int; scale, translate: DVec2; pxRange: float64; overlapSupport: bool): MsdfGlyphResult =
  let range = pxRange / min(scale.x, scale.y)
  let mapping = initDistanceMapping(initRange(range))
  let projection = initProjection(scale, translate)
  let image = newImage(width, height)

  for y in 0 ..< height:
    let py = height.float64 - (y.float64 + 0.5)
    for x in 0 ..< width:
      let p = unproject(projection, dvec2(x.float64 + 0.5, py))
      let dist = invertDistance(distanceForShapeMtsdf(shape, p, overlapSupport))
      let r = mapDistance(mapping, dist.r)
      let g = mapDistance(mapping, dist.g)
      let b = mapDistance(mapping, dist.b)
      let a = mapDistance(mapping, dist.a)
      let color = rgba(toByte(r), toByte(g), toByte(b), toByte(a))
      image.data[y * width + x] = color.rgbx()

  MsdfGlyphResult(
    image: image,
    scale: scale.x,
    translate: translate,
    bounds: shapeBounds(shape),
    range: range
  )

proc generateMsdfPath*(path: Path; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  var shape = shapeFromPath(path)
  orientContours(shape)
  normalize(shape)
  let bounds = shapeBounds(shape)
  let probe = dvec2(
    bounds.l - (bounds.r - bounds.l) - 1.0,
    bounds.b - (bounds.t - bounds.b) - 1.0
  )
  if resolveDistance(distanceForShape(shape, probe, overlapSupport)) < 0:
    for contour in shape.contours:
      reverse(contour)
  edgeColoringSimple(shape, angleThreshold, seed)
  let (scale, translate) = makeProjection(bounds, width, height, pxRange)
  generateMsdfFromShape(shape, width, height, dvec2(scale, scale), translate, pxRange, overlapSupport)

proc generateMtsdfPath*(path: Path; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  var shape = shapeFromPath(path)
  orientContours(shape)
  normalize(shape)
  let bounds = shapeBounds(shape)
  let probe = dvec2(
    bounds.l - (bounds.r - bounds.l) - 1.0,
    bounds.b - (bounds.t - bounds.b) - 1.0
  )
  if resolveDistance(distanceForShape(shape, probe, overlapSupport)) < 0:
    for contour in shape.contours:
      reverse(contour)
  edgeColoringSimple(shape, angleThreshold, seed)
  let (scale, translate) = makeProjection(bounds, width, height, pxRange)
  generateMtsdfFromShape(shape, width, height, dvec2(scale, scale), translate, pxRange, overlapSupport)

proc generateMsdfGlyph*(typeface: Typeface; rune: Rune; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  let path = typeface.getGlyphPath(rune)
  generateMsdfPath(path, width, height, pxRange, angleThreshold, seed, overlapSupport)

proc generateMtsdfGlyph*(typeface: Typeface; rune: Rune; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  let path = typeface.getGlyphPath(rune)
  generateMtsdfPath(path, width, height, pxRange, angleThreshold, seed, overlapSupport)

proc generateMsdfGlyph*(font: Font; rune: Rune; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  generateMsdfGlyph(font.typeface, rune, width, height, pxRange, angleThreshold, seed, overlapSupport)

proc generateMtsdfGlyph*(font: Font; rune: Rune; width, height: int; pxRange: float64; angleThreshold = 3.0; seed: uint64 = 0; overlapSupport = true): MsdfGlyphResult {.raises: [PixieError].} =
  generateMtsdfGlyph(font.typeface, rune, width, height, pxRange, angleThreshold, seed, overlapSupport)
