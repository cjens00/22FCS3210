students = { 
    { id = 123, name = "John" },
    { id = 345, name = "Mary" }, 
    { id = 678, name = "Xavier" }
}
print("id? ")
id = tonumber(io.read())
for _, std in pairs(students) do 
    if std.id == id then
        print(std.name)
        break
    end
end