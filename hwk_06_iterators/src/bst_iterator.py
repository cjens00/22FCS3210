'''
CS3210 - Principles of Programming Languages - Fall 2022
Instructor: Thyago Mota
Description: Homework 06 - An iterator for a BST (Binary Search Tree)
Student Name: Cameron Jensen
'''

class BST: 

    # the labels for the nodes are obtained from a list of strings
    def __init__(self, labels): 
        if not isinstance(labels, list):
            raise Exception("Labels must be a list!")
        if not labels:
            raise Exception("Labels must not be empty!")
        self.label = labels[0] # first label makes the root node 
        self.left = None
        self.right = None        
        for label in labels[1:]:
            BST._add(self, label)

    # adds a node with the given label to the BST using recursion
    def _add(node, label):
        if label < node.label:
            if node.left:
                BST._add(node.left, label)
            else:
                node.left = BST([label])
        else:
            if node.right:
                BST._add(node.right, label)
            else:
                node.right = BST([label])

    # helper method that builds a string representation of the BST using recursion
    def _print(node, tabs = ""): 
        out = ""
        if node:
            out += tabs + node.label + "\n"
            if node.left:
                out += BST._print(node.left, tabs + "   ")
            if node.right:
                out += BST._print(node.right, tabs + "   ")                
        return out

    def __str__(self):
        return BST._print(self, "")

    # helper method that builds a list with the elements of the BST in in-order order using recursion
    def _in_order(node):
        nodes = []
        if node:
            if node.left:
                nodes += BST._in_order(node.left)
            nodes.append(node.label)
            if node.right:
                nodes += BST._in_order(node.right)
        return nodes
    
    # Done #1
    def __iter__(self):
        self._iter_elements = BST._in_order(self)
        return self

    # Done #2
    def __next__(self):
        if len(self._iter_elements) == 0:
            raise StopIteration
        label = self._iter_elements[0]
        self._iter_elements = self._iter_elements[1:]
        return label

if __name__ == "__main__":
    # the code below builds and prints a tree using the given labels
    tree = BST(["b", "a", "d", "c", "e"])
    print("Tree:")
    print(tree)

    # Done #3
    nodes_as_string = ""
    for x in tree:
        nodes_as_string += str(x + " ")
    print(nodes_as_string)
