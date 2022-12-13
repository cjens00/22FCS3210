-- CS3210 - Principles of Programming Languages - Fall 2022
-- Instructor: Thyago Mota
-- Description: elections simulation with coroutines
-- Student Name(s): Cameron Jensen
--[[
    One Coroutine = One Voter Voting
    (~ and a partridge in a pear tree ~)
    (Happy Holidays '22!)
]] --

OS_WINDOWS = "windows"
OS_LINUX = "linux"
OS_MACOS = "macos"

-- Helper function to determine OS
-- https://stackoverflow.com/a/30960054/18372340
function determine_os()
    local BinaryFormat = package.cpath:match("%p[\\|/]?%p(%a+)")
    if BinaryFormat == "dll" then
        function os.name()
            return OS_WINDOWS
        end
    elseif BinaryFormat == "so" then
        function os.name()
            return OS_LINUX
        end
    elseif BinaryFormat == "dylib" then
        function os.name()
            return OS_MACOS
        end
    end
    BinaryFormat = nil
end

-- Multi-platform sleep function (tested on Linux/WSL-Ubuntu)
-- Code from http://lua-users.org/wiki/SleepFunction
function sleep(numSeconds)
    local os = determine_os()
    if os == OS_WINDOWS then
        os.execute("timeout " .. tonumber(numSeconds))
    elseif os == OS_LINUX or os == OS_MACOS then
        local reqPOSIX = require("posix")
        posix.sleep(numSeconds)
        reqPOSIX = nil
    end
    os = nil
end

PROPOSALS = 3
VOTERS = 1000000

--- Returns a random choice.
function _get_random_choice(max)
    local max = max or 100100
    local seed = os.time() + math.random(max) % (2 ^ 1024 - 1);
    math.randomseed(seed);
    local rand_span = 1100100;
    local random_value = math.random(rand_span)
    if random_value * 2 >= rand_span then
        max, seed, random_value, rand_span = nil, nil, nil, nil
        return 1
    end
    max, seed, random_value, rand_span = nil, nil, nil, nil
    return 0
end

-- TODO #1:  then begin a timed loop of up to 10s (also random); 
-- TODO #1:  at each iteration the function should yield; 
-- TODO #1:  at the end of the loop, insert the ballot in the (given) ballot_box.
function vote(ballot_box)
    ballot = {}
    for i = 1, #PROPOSALS + 1 do
        ballot[i] = _get_random_choice()
    end
    ballot_box[#ballot_box + 1] = ballot
end

--- Returns a tally of the results as a percent of ones given to each proposal.
---@param ballot_box any
function tally_results(ballot_box)
end

--- All other program logic begins in and resides here.
function main()
    -- TODO #3: Time when the election started
    -- TODO #4: Create a ballot_box
    -- TODO #4: Create an array of "vote" coroutines
    -- TODO #5: Schedule the coroutines so each has a fair chance of running until completion
    -- TODO #6: Display the results
    -- TODO #7: Show how long the election took in seconds

end

main()
