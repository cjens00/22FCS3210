queue = {}
-- push 
table.insert(queue, 1, 5)
table.insert(queue, 1, 8)
table.insert(queue, 1, 3)
-- pop
print(table.remove(queue, 1))
print(table.remove(queue, 1))
print(table.remove(queue, 1))
