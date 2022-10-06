# [3, 4, 5, 6, 7, 8, 9, 10]
target = list(range(3, 11))
print(target)

# alternatively
target = [ x for x in range(3, 11) ]
print(target)

# [10, 8, 6, 4, 2]
target = list(range(10, 1, -2))
print(target)

# alternatively
target = [ x for x in range(10, 1, -2) ]
print(target)

# [1, 16, 49, 100]
target = [ x**2 for x in range(11) if x % 3 == 1 ]
print(target)

# alternatively
target = [ x**2 for x in range(1, 11, 3) ]
print(target)

# only integers from 
# [ 2, 3.75, .04, 59, .3, 6, 7, 8.5, 9, 10 ]
source = [ 2, 3.75, .04, 59, .3, 6, 7, 8.5, 9, 10 ]
target = [ x for x in source if type(x) == int ]
print(target)

# alternatively
target = [ x for x in source if isinstance(x, int)  ]
print(target)

# 1-1000 divisible by 7
target = [ x for x in range(1001) if x % 7 == 0 ]
print(target)

# a = [1, 2, 3, 4, 5, 6, 7, 8, 9]
# b = [2, 7, 1, 12]
# (x, y) if x + y > 10
a = [1, 2, 3, 4, 5, 6, 7, 8, 9]
b = [2, 7, 1, 12]
target = [ (x, y) for x in a for y in b if x + y > 10 ]
print(target)
