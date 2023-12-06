import strutils, tables

proc parseMap(map: seq[string]): seq[(int, int, int)] =
  for line in map:
    let data = line.split(" ")
    result.add (data[0].parseInt(), data[1].parseInt(), data[2].parseInt())

proc findMapping(map: seq[(int, int, int)], seed: int): int =
  for m in map:
    let startRange = m[1]
    let endRange = startRange + m[2] - 1
    if seed >= startRange and seed <= endRange: return m[0] + (seed - startRange)
  return -1

var globalMap = initOrderedTable[string, seq[(int, int, int)]]()

let input = stdin.readAll().split("\n\n")

for m in input[1..input.len() - 1]:
  let data = m.split("\n")
  let name = data[0].split(" ")[0]
  globalMap[name] = parseMap(data[1..data.len() - 1])


var steps: seq[string] = @[]
for k in globalMap.keys: steps.add(k)

proc traverseMap(seed: int): int =
  result = seed
  for step in steps:
    var map = globalMap[step]
    let m = findMapping(map, result)
    if m != -1: result = m

var seedToLocation: seq[int] = @[]
let seedsData = input[0].split(" ")
let seeds = seedsData[1..seedsData.len() - 1]

for seed in seeds: seedToLocation.add(traverseMap(seed.parseInt()))
echo seedToLocation.min()