import strutils, sequtils

proc isSymbol(matrix: seq[seq[char]], x, y: int): bool =
  matrix[y][x] != '.' and not matrix[y][x].isDigit()

proc checkSymbol(matrix: seq[seq[char]], x, y: int): bool =
  let hLength = matrix[y].len()
  let vLength = matrix.len()

  if x < hLength - 1: result = result or isSymbol(matrix, x + 1, y)
  if x > 0: result = result or isSymbol(matrix, x - 1, y)
  if y < vLength - 1: result = result or isSymbol(matrix, x, y + 1)
  if y > 0: result = result or isSymbol(matrix, x, y - 1)
  if x < hLength - 1 and y < vLength - 1: result = result or isSymbol(matrix, x + 1, y + 1)
  if x > 0 and y < vLength - 1: result = result or isSymbol(matrix, x - 1, y + 1)
  if x < hLength - 1 and y > 0: result = result or isSymbol(matrix, x + 1, y - 1)
  if x > 0 and y > 0: result = result or isSymbol(matrix, x - 1, y - 1)

let matrix = stdin.readAll().split("\n").mapIt(cast[seq[char]](it))
var numbers: seq[int]

for y in 0 ..< matrix.len():
  var numberAcc = ""
  var adjuscent = false

  for x in 0 ..< matrix[y].len():
    if matrix[y][x].isDigit():
      numberAcc.add(matrix[y][x])
      adjuscent = adjuscent or checkSymbol(matrix, x, y)
      if x == matrix[y].len() - 1 and adjuscent: numbers.add(numberAcc.parseInt())
    elif adjuscent:
      numbers.add(numberAcc.parseInt())
      numberAcc = ""; adjuscent = false
    else:
      numberAcc = ""; adjuscent = false

echo numbers.foldl(a + b)