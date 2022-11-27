-- defines a factorial function
function fact(n)
    if n == 0 then
        return 1
    else
        return n * fact(n - 1)
    end
end

io.write("n? ") -- or simply print("n? ")
n = io.read("*number") -- or simply io.read("*n")
print(n, "factorial is", fact(n))
print(string.format("%d! is %d", n, fact(n)))
