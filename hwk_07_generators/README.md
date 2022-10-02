# Generators

A **generator**, as the name suggests, **generates** elements of a collection. The generation is done one element at a time. In other words, a **generator** yields the values one at a time which allows the elements to be consumed as they are generated (speeding up the processing and saving memory).

To illustrate how **generators** work, consider the [BST](src/bst_generator.py) (Binary Search Tree) class implemented in Python. Your first taks in this assignment is to write a **generator** for BST using in-order tree traversal. Use Python's **yield** keyword to have your **generator** return an element. 

```
# TODO #1: return one element at a time from the BST's given node and respecting in-order tree traversal
# hint: do it recursively; the base-case is when node is None
def in_order(node):
    pass
```

In "main", get a **generator** from the tree (i.e., call **in_order** and save the reference returned). Then use Python's **next** passing the **generator**'s reference to get the elements from the tree. Use a loop that ends when **next**'s call throws an exception. 

```
# TODO #2: use the iterator that you created to show the labels of the elements of the tree
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

Submit ```bst_generator.py``` on Canvas with your solution. 