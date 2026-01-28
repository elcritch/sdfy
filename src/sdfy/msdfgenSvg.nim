import std/math

import pixie
include pixie/fileformats/svg

proc normalizeSvgData(data: string): string =
  result = data.replace(",", " ")
  let key = "viewBox=\""
  let start = result.find(key)
  if start < 0:
    return
  let valueStart = start + key.len
  let valueEnd = result.find("\"", valueStart)
  if valueEnd < 0:
    return
  let viewBox = result[valueStart ..< valueEnd]
  let parts = viewBox.splitWhitespace()
  if parts.len != 4:
    return
  var updated = false
  var fixed = parts
  for i in 0 ..< fixed.len:
    if fixed[i].contains("."):
      var value: float64
      try:
        value = parseFloat(fixed[i])
      except ValueError:
        return
      fixed[i] = $int(round(value))
      updated = true
  if updated:
    let newViewBox = fixed.join(" ")
    result = result[0 ..< valueStart] & newViewBox & result[valueEnd .. ^1]

proc svgToPath*(svg: Svg): tuple[path: Path, elements: int] =
  let combined = newPath()
  var count = 0
  for (path, props) in svg.elements:
    if not props.display or props.opacity <= 0:
      continue
    let local = path.copy()
    local.transform(props.transform)
    combined.addPath(local)
    inc count
  (combined, count)

proc parseSvgPath*(
    svgData: string
): tuple[path: Path, elements: int] {.raises: [PixieError].} =
  let svg = parseSvg(normalizeSvgData(svgData))
  svgToPath(svg)

proc loadSvgPath*(
    svgPath: string
): tuple[path: Path, elements: int] {.raises: [PixieError].} =
  let data =
    try:
      readFile(svgPath)
    except IOError as err:
      raise newException(PixieError, "Failed to read SVG: " & err.msg)
  parseSvgPath(data)
