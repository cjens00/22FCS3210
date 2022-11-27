function counter(from)
    count = from
    return function() 
        count = count + 1
        return count
    end
end

x = counter(10) -- x is a closure (instance of counter with from=10)
print(x()) -- displays 11
print(x()) -- displays 12
print(x()) -- displays 13

y = counter(100) -- y is another closure (instance of counter with from=100)
print(y()) -- displays 101
print(y()) -- displays 102
print(y()) -- displays 103

