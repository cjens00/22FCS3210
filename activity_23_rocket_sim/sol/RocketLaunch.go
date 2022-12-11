/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 23 - Rocket Launch Simulation
 */

package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type Control struct {
	mu sync.Mutex
	state StateType
}

type StateType string

const SCHEDULED StateType = "scheduled"
const INITIATED StateType = "initiated"
const LAUNCHED  StateType = "launched"
const ABORTED   StateType = "aborted"

const ABORT_PROBABILITY = 20 // 0-100%

var wg sync.WaitGroup

func operator(name string, control *Control) {
	defer wg.Done()
	for {
		control.mu.Lock()
		if control.state == LAUNCHED {
			control.mu.Unlock()
			fmt.Printf("Operator %s is crying with joy after a successful launching sequence!\n", name)
			return
		} else if control.state == ABORTED {
			control.mu.Unlock()
			fmt.Printf("Operator %s left the control room!\n", name)
			return
		} else if rand.Intn(100) <= ABORT_PROBABILITY {
			fmt.Printf("Operator %s trying to abort the launch...\n", name)
			control.state = ABORTED
			fmt.Printf("Launch aborted says operator %s!\n", name)
			control.mu.Unlock()
			return
		} else {
			control.mu.Unlock()
			fmt.Printf("Operator %s is busy working...\n", name)
			time.Sleep(time.Duration(rand.Intn(10)) * time.Second)
		}
	}
}

func launch(sec int, control *Control) {
	defer wg.Done()
	control.mu.Lock()
	if control.state == SCHEDULED {
		control.state = INITIATED
		control.mu.Unlock()
		for {
			control.mu.Lock()
			if control.state == INITIATED {
				fmt.Printf("%ds for the launch...\n", sec)
				if sec == 0 {
					fmt.Println("Launched!!!")
					control.state = LAUNCHED
					control.mu.Unlock()
					return
				}
				control.mu.Unlock()
				if sec <= 15 {
					time.Sleep(time.Duration(1) * time.Second)
					sec--
				} else {
					time.Sleep(time.Duration(5) * time.Second)
					sec -= 5
				}
			} else {
				control.mu.Unlock()
				fmt.Println("Launch aborted by operator!")
				return
			}
		}
	} else {
		control.mu.Unlock()
		fmt.Println("Unexpected state!")
	}
}

func main() {
	var control Control
	control.state = SCHEDULED

	wg.Add(4) // #operators + 1

	go launch(30, &control)

	// operators
	go operator("John",  &control)
	go operator("Janet", &control)
	go operator("Katie", &control)

	wg.Wait()
}
