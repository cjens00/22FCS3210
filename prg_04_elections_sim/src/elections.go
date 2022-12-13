/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Elections simulation with coroutines
 * Student Name(s): Cameron Jensen
 */

package main

import "sync"

// "fmt"
// "math/rand"
// "sync"
// "time"

const PROPOSALS = 3
const VOTERS = 1000000

var wg sync.WaitGroup

// TODO #1: create a ballot and randomly assign 0 (no) or 1 (yes) to each proposal; then sleep for up to 10s (also random); at the end of the loop, write the ballot to the (given) ballot_box channel
func vote(ballot_box chan<- [PROPOSALS]int) {
	// Incoming channel
}

// TODO #2: return a tally of the results as a percent of 1s (yes) that was given to each proposal
func tally_results(ballot_box <-chan [PROPOSALS]int) [PROPOSALS]float64 {
	// Outgoing channel
	println("Tally Results")
	return [3]float64{1.0, 1.0, 1.0}
}

func main() {

	// TODO #3: time when the election started

	// TODO #4: create a ballot_box

	// TODO #4 create VOTERS goroutines

	// TODO #5: wait for all goroutines to end

	// TODO #6: display the results

	// TODO #7: show how long the election took in seconds

}
