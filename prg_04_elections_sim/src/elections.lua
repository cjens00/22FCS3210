-- CS3210 - Principles of Programming Languages - Fall 2022
-- Instructor: Thyago Mota
-- Description: elections simulation with coroutines
-- Student Name(s): Cameron Jensen
-- Last Edited: 02:00, 12/14/2022

PROPOSALS = 3
VOTERS = 1000000

function vote(ballot_box)
    local ballot = {}
    for i = 1, PROPOSALS do
        table.insert(ballot, i, (math.random(0, 1)))
    end
    local startTime = os.clock()
    local voteTime = math.random(1, 10)
    local timeRemaining = voteTime - (os.clock() - startTime)
    while timeRemaining > 0 do
        coroutine.yield(timeRemaining)
        timeRemaining = voteTime - (os.clock() - startTime)
    end
    table.insert(ballot_box, #ballot_box + 1, ballot)
    return math.maxinteger
end

function tally_results(ballot_box)
    local results = {}
    for i = 1, PROPOSALS do
        table.insert(results, i, 0)
    end
    for i = 1, #ballot_box do
        for j = 1, #ballot_box[i] do
            if ballot_box[i][j] == 1 then
                results[j] = results[j] + 1
            end
        end
    end
    for i = 1, #results do
        results[i] = 100 / (VOTERS / results[i])
    end
    return results
end

function schedule_voters(voters, ballot_box)
    local sort_by_time_remaining = function(ascending)
        ascending = ascending or true
        if ascending then
            return function(a, b)
                if a == nil or a.timeRem == nil then
                    return false
                elseif b == nil or b.timeRem == nil then
                    return true
                else
                    return a.timeRem < b.timeRem
                end
            end
        else
            return function(a, b)
                if a == nil or a.timeRem == nil then
                    return false
                elseif b == nil or b.timeRem == nil then
                    return true
                else
                    return a.timeRem > b.timeRem
                end
            end
        end
    end

    local ignore
    for i = 1, #voters do
        voters[i] = {
            voter = voters[i],
            timeRem = 0
        }
        ignore, voters[i].timeRem = coroutine.resume(voters[i].voter, ballot_box)
    end
    ignore = nil

    -- Sort the table by time remaining in the coroutine (ascending)
    print(string.format("Sorting by time remaining (ascending)"))
    local sort_wrapper = function()
        table.sort(voters, sort_by_time_remaining())
    end
    pcall(sort_wrapper)
    print(voters[#voters].timeRem)

    local ignore
    for i = 1, #voters do
        while voters[i].timeRem ~= math.maxinteger do
            ignore, voters[i].timeRem = coroutine.resume(voters[i].voter, ballot_box)
        end
    end
    ignore = nil

end

function begin_election()
    -- Time when the election started
    local startTime = os.clock()

    -- Create a ballot_box
    local ballot_box = {}

    -- Create an array of "vote" coroutines
    local voters = {}
    for i = 1, VOTERS do
        table.insert(voters, i, coroutine.create(vote))
    end

    -- Schedule the coroutines so each has a 
    -- fair chance of running until completion
    schedule_voters(voters, ballot_box)

    -- Show how long the election took 
    -- in seconds and display the results
    local results = tally_results(ballot_box)
    local elapsedTime = os.clock() - startTime
    local fStrResultsTable = {}
    for i = 1, PROPOSALS do
        table.insert(fStrResultsTable, i, string.format("Prop %d: %2.4f", i, results[i]))
    end
    local fStrTime = string.format("Time Elapsed: %2.3fs", elapsedTime)
    for i = 1, #fStrResultsTable do
        print(fStrResultsTable[i])
    end
    print(fStrTime)
end

-- Main below this line --
function main()
    begin_election()
end

main()
