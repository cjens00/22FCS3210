/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 23 - Rocket Launch Simulation
 */

package main

import (
	"sync"
)

type Control struct {
	mu    sync.Mutex
	state StateType
}

type StateType string

const SCHEDULED StateType = "scheduled"
const INITIATED StateType = "initiated"
const LAUNCHED StateType = "launched"
const ABORTED StateType = "aborted"

const ABORT_PROBABILITY = 20 // 0-100%

var wg sync.WaitGroup

// TODO: run an infinite loop until state is either ABORTED or LAUNCHED
// the operator should abort the launching based on ABORT_PROBABILITY
// the operator should "sleep" for random seconds (<=10s)
// add appropriate messages at each iteration
func operator(name string, control *Control) {

}

// TODO: launch a rocket by changing its state
// SCHEDULED is the initial state
// from SCHEDULED the rocket goes to (launch sequence) INITIATED
// it state is INITIATED, display a message with #sec til launch; if sec > 15s, sleep for 5s and then decrease time by 5;
// if sec <= 15s, sleep for 1s and then decrease time by 1s; if sec == 0, display a message saying that the rocket was launched
// and change the state to LAUNCHED;
// if the state is ABORTED, display a message (saying that the launch was abborted by the operator) and finish execution
func launch(sec int, control *Control) {

}

// TODO: run simulations changing ABORT_PROBABILITY and number of operators
func main() {
	var control Control
	control.state = SCHEDULED

	// TODO: change it based on #operators + 1 (the rocket)
	// wg.Add(?)

	// TODO: launch a rocket!

	// TODO: run operator goroutines

	wg.Wait()
}
