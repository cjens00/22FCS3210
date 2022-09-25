# Iterators

According to Bjarne Stroustrup (creator of C++), an **iterator** is this notion of a pointer to an element in a collection. **Iterators** are normally implemented by objects that maintain a reference to an element in the collection. The name **iterator** can be a little misleading, because the iterator itself does not iterate: what iterates is the **for** construct that uses the iterator. 

To illustrate how **iterators** work, consider the [BST](src/bst_iterator.py) (Binary Search Tree) class implemented in Python. Your first taks in this assignment is to write an iterator for BST using in-order tree traversal. To incorporate an **iterator** to a class in Python you are required to override the two methods identified in the the TO-DOs: ```__iter__``` and ```__next__```. Different than Java, **iterators** in Python don't have a specific method to check if there are no more elements to return. 

```
TODO #1: return an iterator for BST; hint: use _in_order to build a list with the elements; then return "self"
def __iter__(self):
    pass
```

```
# TODO #2: return the first element from the list that you built in __iter__, updating the list before returning; hint: don't worry if the list is empty (the exception that is going to be thrown is used to notify that there are no more elements to return)
    def __next__(self):
```

```
# TODO #3: use the iterator that you created to show the labels of the elements of the tree
```

Expected output: 

```
Tree:
b
   a
   d
      c
      e

a b c d e 
```

# Submission

Submit ```bst_iterator.py``` on Canvas with your solution. 