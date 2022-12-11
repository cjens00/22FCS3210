/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 22 - Multithreaded Sort
 */
package main

import (
	"fmt"
	"sync"
	"time"
)

var wg sync.WaitGroup

func sort(slice []int) {
	for i, _ := range slice {
		j := i
		k := i - 1
		for k >= 0 {
			if slice[j] < slice[k] {
				temp := slice[j]
				slice[j] = slice[k]
				slice[k] = temp
				j = k
				k--
			} else {
				break
			}
		}
	}
	wg.Done()
}

func merge(left []int, right []int) {
	var data = make([]int, len(left)+len(right))
	i := 0
	j := 0
	k := 0
	for ; i < len(left) && j < len(right); k++ {
		if left[i] < right[j] {
			data[k] = left[i]
			i++
		} else {
			data[k] = right[j]
			j++
		}
	}
	for ; i < len(left); i++ {
		data[k] = left[i]
		k++
	}
	for ; j < len(right); j++ {
		data[k] = right[j]
		k++
	}
	k = 0
	for i := 0; i < len(left); i++ {
		left[i] = data[k]
		k++
	}
	for j := 0; j < len(right); j++ {
		right[j] = data[k]
		k++
	}
}

func main() {
	const N = 100000      // size of the array
	const MAX_INT = 10000 // upperbound of the numbers in the array

	// TODO: declare an array of size N

	// TODO: populate the array with random integers from [0, MAX_INT)

	// begin timing
	start := time.Now()

	// uncomment this line to sort the array altogether
	// sort(data[:])

	// TODO: split the array in 2 slices

	// TODO: create 2 goroutines to sort the array in parallel

	// TODO: merge the 2 slices (now sorted)

	// show how long it took
	fmt.Println(time.Since(start))
}
