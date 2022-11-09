# Instructions

Imagine that the following graph describes a maze. Use a predicate called **connected** to build a knowledge base that represents the maze. The predicate determines which points are connected, that is, from which points you can get to which points **in one step**. Furthermore, imagine that all paths are one-way streets, so that you can only walk them in one direction. 

![pic1.png](pics/pic1.png)

```
connected(1,2). 
connected(3,4). 
connected(5,6). 
connected(7,8). 
connected(9,10). 
connected(12,13). 
connected(13,14). 
connected(15,16). 
connected(17,18). 
connected(19,20). 
connected(4,1). 
connected(6,3). 
connected(4,7). 
connected(6,11). 
connected(14,9). 
connected(11,15). 
connected(16,12). 
connected(14,17). 
connected(16,19). 
```

Using the knowledge based built, write a (recursive) predicate path/2 that tells you from which points in the maze you can get to which other points when chaining together connections given the above KB. Then have Prolog answer the following queries: 

* can you get from point 5 to point 10? 
* which other points can you get to when starting at point 1? 
* which points can be reached from point 13?  

