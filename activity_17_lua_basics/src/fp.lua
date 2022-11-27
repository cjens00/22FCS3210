require("fp")

function even(x)
    return x % 2 == 0
end

lst = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }
out = filter(even, lst)
for v in pairs(out) do 
    print(v)
end

-- each(print, table.filter(function(x) return x % 3 == 0 end, range(10)))