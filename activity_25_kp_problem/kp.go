package main

import (
	"fmt"
	"sync"
	"time"
)

var wg sync.WaitGroup

type Item struct {
	name   string
	value  int
	weight int
}

var items = []Item{
	{"clock", 175, 10},
	{"painting", 90, 9},
	{"radio", 20, 4},
	{"vase", 50, 2},
	{"book", 10, 1},
	{"computer", 200, 20},
	{"phone", 5, 10},
	{"tv", 350, 50},
	{"pillow", 3, 2},
	{"printer", 20, 10},
	{"coffee-maker", 30, 10},
	{"mugs", 10, 50}}

var solution struct {
	mu     sync.Mutex
	kp     []Item
	value  int
	weight int
}

// set the maximum weight of a knapsack
const MAX_WEIGHT = 100

// returns the value of a knapsack
func get_value(kp []Item) int {
	v := 0
	for _, item := range kp {
		v += item.value
	}
	return v
}

// returns the weight of a knapsack
func get_weight(kp []Item) int {
	w := 0
	for _, item := range kp {
		w += item.weight
	}
	return w
}

// single-threaded solution search
func search_st(kp []Item) {
	weight := get_weight(kp)
	if weight > MAX_WEIGHT {
		return
	}
	value := get_value(kp)
	if len(kp) > 0 && value > solution.value {
		for i, v := range kp {
			solution.kp[i] = v
		}
		solution.weight = weight
		solution.value = value
	}
	if len(kp) < len(items) {
		for _, v_new := range items {
			found := false
			for _, v := range kp {
				if v == v_new {
					found = true
					break
				}
			}
			if found {
				continue
			}
			var kp_new = make([]Item, len(kp)+1)
			for i := 0; i < len(kp); i++ {
				kp_new[i] = kp[i]
			}
			kp_new[len(kp)] = v_new
			search_st(kp_new)
		}
	}
}

// multithreaded solution search (recursive run part)
func run(kp []Item) {
	weight := get_weight(kp)
	if weight > MAX_WEIGHT {
		return
	}
	value := get_value(kp)
	if len(kp) > 0 && value > solution.value {
		// TODO #1 - 5 points: make sure that multiple goroutines can safely update the solution
		// Hint: use the solution mutex to guarantee exclusive access

	}
	if len(kp) < len(items) {
		for _, v_new := range items {
			found := false
			for _, v := range kp {
				if v == v_new {
					found = true
					break
				}
			}
			if found {
				continue
			}
			var kp_new = make([]Item, len(kp)+1)
			for i := 0; i < len(kp); i++ {
				kp_new[i] = kp[i]
			}
			kp_new[len(kp)] = v_new
			run(kp_new)
		}
	}
}

// multithreaded solution search
func search_mt(kp []Item) {
	// TODO #2 - 1 point: make sure that each goroutine properly signal main when they are done

}

func main() {
	// initialize the solution struct
	solution.kp = make([]Item, len(items))
	solution.weight = MAX_WEIGHT + 1
	solution.value = 0

	// begin timing
	start := time.Now()

	// uncomment the following lines to run the single-threaded solution
	var kp = make([]Item, 0)
	search_st(kp)

	// uncomment the following lines to run the multithreaded solution
	// TODO #3 - 2 points: make sure that main waits for all goroutines to end
	// Hint: use global wg


	// display the solution
	fmt.Println(solution)

	// show how long it took
	fmt.Println(time.Since(start))
}
