package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type RangeData struct {
	destStart   int
	sourceStart int
	length      int
}

type SeedData struct {
	start  int
	length int
}

func parseMap(input []string) []RangeData {
	var output []RangeData
	for _, line := range input {
		data := strings.Split(line, " ")
		destStart, _ := strconv.Atoi(data[0])
		sourceStart, _ := strconv.Atoi(data[1])
		length, _ := strconv.Atoi(data[2])

		output = append(output, RangeData{
			destStart:   destStart,
			sourceStart: sourceStart,
			length:      length,
		})
	}
	return output
}

func findMatchInMap(seed int, mapData []RangeData) int {
	for _, data := range mapData {
		if seed >= data.sourceStart && seed <= data.sourceStart+data.length {
			return data.destStart + (seed - data.sourceStart)
		}
	}
	return -1
}

func traverseMap(seed int, mapData map[string][]RangeData, steps []string) int {
	result := seed
	for _, step := range steps {
		maybeResult := findMatchInMap(result, mapData[step])
		if maybeResult != -1 {
			result = maybeResult
		}
	}
	return result
}

func findLowestLocationFromSeeds(start int, length int, globalMap map[string][]RangeData, steps []string) int {
	firstElemLocation := traverseMap(start, globalMap, steps)
	lastElemLocation := traverseMap(start+length-1, globalMap, steps)

	if firstElemLocation == lastElemLocation {
		return firstElemLocation
	}
	lowestLocation := -1
	for i := 0; i < length; i++ {
		maybeLocation := traverseMap(start+i, globalMap, steps)
		if lowestLocation == -1 || maybeLocation < lowestLocation {
			lowestLocation = maybeLocation
		}
	}
	return lowestLocation
}

func findLowestSeedLocation(seeds []SeedData, mapData map[string][]RangeData, steps []string) int {
	channel := make(chan int)

	for _, seedData := range seeds {
		go func(seedData SeedData) {
			fmt.Println("calculating seed:", seedData.start, "length:", seedData.length)
			channel <- findLowestLocationFromSeeds(seedData.start, seedData.length, mapData, steps)
		}(seedData)
	}

	lowestLocation := -1
	for i := 0; i < len(seeds); i++ {
		result := <-channel
		if lowestLocation == -1 || result < lowestLocation {
			lowestLocation = result
		}
	}
	return lowestLocation
}

func main() {
	rawInput, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}

	input := strings.Split(string(rawInput), "\n\n")

	seedRangeData := strings.Split(input[0], " ")[1:]
	seeds := []SeedData{}

	for i := 0; i < len(seedRangeData); i += 2 {
		seed, _ := strconv.Atoi(seedRangeData[i])
		count, _ := strconv.Atoi(seedRangeData[i+1])
		seeds = append(seeds, SeedData{
			start:  seed,
			length: count,
		})
	}

	globalMap := make(map[string][]RangeData)
	steps := []string{}

	for _, line := range input[1:] {
		splittedLine := strings.Split(line, "\n")
		globalMap[splittedLine[0]] = parseMap(splittedLine[1:])
		steps = append(steps, splittedLine[0])
	}

	result := findLowestSeedLocation(seeds, globalMap, steps)
	fmt.Println(result)
}
