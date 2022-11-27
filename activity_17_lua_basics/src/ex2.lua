function it(list)
    local current = list
    return function()
        if current then
            local value = current.value 
            current = current.next 
            return value 
        end
    end
end

list = nil
list = { next = list, value = 5}
list = { next = list, value = 8}
list = { next = list, value = 3}
for el in it(list) do 
    print(el)
end