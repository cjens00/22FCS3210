-- Write an iterator that returns all substrings of a given string. 
function all_subs(str)
    local i = 1
    local j = 0
    return function() 
        if i > string.len(str) then
            return nil
        end
        if j <= string.len(str) then
            j = j + 1
            if j > string.len(str) then
                i = i + 1 
                if i > string.len(str) then
                    return nil
                end
                j = i
            end
        end
        return string.sub(str, i, j)
    end
end

for s in all_subs("Hello Lua!") do
    print(s)
end