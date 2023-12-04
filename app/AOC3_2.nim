import strutils, sequtils, options, tables

proc isSymbol(matrix: seq[seq[char]], x, y: int): (bool, bool) =
  let gotSymbol = matrix[y][x] != '.' and not matrix[y][x].isDigit()
  let isGear = matrix[y][x] == '*'
  result = (gotSymbol, isGear)

proc checkSymbol(matrix: seq[seq[char]], x, y: int): (bool, Option[(int, int)]) =
  let hLength = matrix[y].len()
  let vLength = matrix.len()

  if x < hLength - 1:
    let (gotSymbol, isGear) = isSymbol(matrix, x + 1, y)
    result[0] = gotSymbol
    if isGear: result[1] = some((x + 1, y))
  if x > 0:
    let (gotSymbol, isGear) = isSymbol(matrix, x - 1, y)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x - 1, y))
  if y < vLength - 1:
    let (gotSymbol, isGear) = isSymbol(matrix, x, y + 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x, y + 1))
  if y > 0:
    let (gotSymbol, isGear) =  isSymbol(matrix, x, y - 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x, y - 1))
  if x < hLength - 1 and y < vLength - 1:
    let (gotSymbol, isGear) = isSymbol(matrix, x + 1, y + 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x + 1, y + 1))
  if x > 0 and y < vLength - 1:
    let (gotSymbol, isGear) = isSymbol(matrix, x - 1, y + 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x - 1, y + 1))
  if x < hLength - 1 and y > 0:
    let (gotSymbol, isGear) = isSymbol(matrix, x + 1, y - 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x + 1, y - 1))
  if x > 0 and y > 0: 
    let (gotSymbol, isGear) = isSymbol(matrix, x - 1, y - 1)
    result[0] = result[0] or gotSymbol
    if isGear: result[1] = some((x - 1, y - 1))

let matrix = stdin.readAll().split("\n").mapIt(cast[seq[char]](it))
var gears: OrderedTable[(int, int), seq[int]]

for y in 0 ..< matrix.len():
  var numberAcc = ""
  var adjuscent = false
  var gotGear : Option[(int, int)]

  for x in 0 ..< matrix[y].len():
    if matrix[y][x].isDigit():
      numberAcc.add(matrix[y][x])
      let (gotSymbol, isGear) = checkSymbol(matrix, x, y)
      adjuscent = adjuscent or gotSymbol
      if isGear.isSome(): gotGear = isGear
  
      if x == matrix[y].len() - 1 and adjuscent and  gotGear.isSome():
        if gears.hasKey(gotGear.get()):
          gears[gotGear.get()].add(numberAcc.parseInt())
        else:
          gears[gotGear.get()] = @[numberAcc.parseInt()]
    elif adjuscent and gotGear.isSome():
      if gears.hasKey(gotGear.get()):
        gears[gotGear.get()].add(numberAcc.parseInt())
      else:
        gears[gotGear.get()] = @[numberAcc.parseInt()]
      numberAcc = ""; adjuscent = false; gotGear = none((int, int))
    else:
      numberAcc = ""; adjuscent = false; gotGear = none((int, int))

var sum = 0
for gear in gears.values():
  if gear.len() == 2:
    sum += gear[0] * gear[1]

echo sum