function my_iterator(collection)
    local i = 0
    return function () 
        i = i + 1 
        next = collection[i] 
        return next 
    end
end

collection = { 5, 8, 2, 1, 9 }
for el in my_iterator(collection) do
    print(el)
end

students = { }
students[1] = "John"
students[2] = "Mary"
students[3] = "Xavier"
students["special"] = "Thyago"
print("Using pairs:")
for key, values in pairs(students) do
    print(key, values)
end
print("Using ipairs:")
for key, values in ipairs(students) do
    print(key, values)
end