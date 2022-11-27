function even(x)
    return x % 2 == 0
end

function filter(lst, pred) 
    result = {}
    for i, v in pairs(lst) do
        if pred(v) then 
            table.insert(result, v)
        end 
    end
    return result 
end

lst = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }
result = filter(lst, even)
for i, v in pairs(result) do 
    print(v)
end