# Introduction

People in Gotham City (population est. 1M) will have the chance to agree or disagree on a number of proposals made by mayor Theodore Cobblepot.  A proposal gets approved with a simple majority.  Voters begin voting at (approximately) the same time and they take up to 10 seconds to vote.  People in Gotham City don’t care much about the proposals because they trust Batman will protect them no matter what.  Therefore, when they vote on the proposals they just flip a coin: heads means approval, tails disapproval.  In this assignment you will simulate the elections in Gotham City in two distinct ways using Lua and Go. 

***THIS ASSIGNMENT IS OPTIONAL (only the best 3 of the 4 projects will count towards your grade)***

# Part 1: Lua Coroutines 

Coroutines are functions that can have their execution suspended after a call to **yield**.  This feature is sometimes called **non-preemptive multitasking** where tasks voluntarily **yield** so others can execute.  In this assignment you will create coroutines to simulate individuals voting in Gotham City elections.  Because the process is cooperative, you will also need to write a **scheduler** so that each coroutine has a fair chance to progress. 

The **vote** function implements the coroutine's code that simulates an individual voting in the election. It first creates a **ballot** with randomly assigned 0 (no) or 1 (yes) values for each proposal. You should use Lua's table to implement an array of size **PROPOSALS=3** to represent a single **ballot**. For example, a **ballot** with yes for the first two proposals and no for the last proposal should look like the following: 

```
ballot = { 
    [1] = 1, 
    [2] = 1, 
    [3] = 0
}
```

After the **ballot** is created and filled, the **vote** coroutine should begin a timed loop of up to 10s (the exact amount of time should be set randomly). At each iteration of that loop, the coroutine should **yield**. When the timed loop ends, the **ballot** should be inserted in the (given) **ballot box** (a parameter of the function). 

Another function that you are asked to implement in this assignment is the **tally_results** which, as the name suggests, returns a tally of the election results as a percent of 1s (yes) that was given to each proposal. 

Because the simulation needs to be timed, make sure to record the starting time (hint: use **os.clock()** function). The simulation begins by creating a **ballot_box** (an empty table) and an array of coroutines (one for each **VOTERS=1M**).  

The scheduling of the coroutines should be done so each voter has a fair time to make progress. In other words, the schedule should run a loop, resuming each of the coroutines, until ALL coroutines are done executing. 

The final part of the simulation should display the results of the election and how long the whole process took (in seconds).  A correct implementation should take less than 15s.

Example run: 

```
Election Results:
1       49.9941
2       50.0061
3       49.9829
Time elapsed: 14.108387 seconds!
```

# Part 2: Go Goroutines

In this part of the assignment you will repeat the simulation done in part 1 but now using Go goroutines. The first function that you are asked to implement is the **vote** which will be used as the code for the **VOTERS=1M** goroutines to be created. Similarly to what you did using Lua, create a **ballot** (an array of size **PROPOSALS=3**), fill it with random values equal to 0 (no) or 1 (yes). Then have the goroutine sleep for a random amount of time up to 10s. Before returning, the goroutine should add the **ballot** in the **ballot_box**, implemented as a Go channel. 

Function **tally_results** is similar to the one implemented in part 1, except that the **ballot_box** is a Go channel. Also, because the simulation needs to be timed, make sure to record the starting time (hint: use **time.Now()** function). The simulation begins by creating a **ballot_box** (a channel). After that, the simulation should start **VOTERS=1M** goroutines. 

There is no explicit scheduling of the goroutines because this will be handled by Go's runtime. However, you want to make sure that all goroutines are done before finishing the simulation. Use **WaitGroup** for synchronization like we discussed in class. 

The final part of the simulation should display the results of the election and how long the whole process took (in seconds). Hint: use **time.Since** function. A correct implementation should take less than 15s. 

Example run: 

```
Election Results:
[49.9691 50.0565 50.0964]
Time elapsed: 12.347013 seconds!
```

# Submission

Zip **elections.lua** and **elections.go** into **elections.zip** and upload this file to Canvas. 

# Rubric

TBD



