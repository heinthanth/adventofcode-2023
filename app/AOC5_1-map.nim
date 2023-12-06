import strutils, tables

proc parseMap(map: seq[string]): OrderedTable[int, int] =
  for line in map:
    let data = line.split(" ")
    let (dest, source, length) = (data[0].parseInt(), data[1].parseInt(), data[2].parseInt())
    echo "dest: " & $dest & ", source: " & $source & ", length: " & $length
    for i in 0 ..< length:
      result[source + i] = dest + i

var globalMap = initOrderedTable[string, OrderedTable[int, int]]()

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
    if map.hasKey(result): result = map[result]

var seedToLocation: seq[int] = @[]
let seedsData = input[0].split(" ")
let seeds = seedsData[1..seedsData.len() - 1]

for seed in seeds: seedToLocation.add(traverseMap(seed.parseInt()))
echo seedToLocation.min()