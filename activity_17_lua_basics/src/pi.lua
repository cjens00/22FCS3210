-- computes a pi approximation up to n-term
function pi_helper(n)
    if n == 1 then
        return 1
    else
        return ((-1)^(n+1))/(2*n-1) + pi_helper(n-1)
    end
end

function pi(n)
    return 4*pi_helper(n)
end

print("n? ")
n = io.read()
print(pi(n))