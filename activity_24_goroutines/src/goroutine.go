/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 24 (should've been 22) - Goroutine Example
 */

package main

import (	 
	"fmt"
	"sync"
	"time"
)
 
var wg sync.WaitGroup
 
func run(name string) {
	defer wg.Done()
	for i := 0; i < 5; i++ {
		fmt.Println(name, ":", i)
		time.Sleep(1 * time.Second)
	}
}

func main() {
	wg.Add(2)
	go run("t1")
	go run("t2")
	wg.Wait()
}


 
 