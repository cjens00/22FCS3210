-- CS3210 - Principles of Programming Languages - Fall 2022
-- Instructor: Thyago Mota
-- Description: elections simulation with coroutines
-- Student Name(s): Cameron Jensen
PROPOSALS = 3
VOTERS = 1000000

function vote(ballot_box)
    local ballot = {}
    for i = 1, PROPOSALS do
        table.insert(ballot, i, (math.random() >= 0.500))
    end
    local startTime = os.clock()
    local voteTime = math.random(1, 10)
    local timeRemaining = voteTime - (os.clock() - startTime)
    while timeRemaining > 0 do
        coroutine.yield(timeRemaining)
        timeRemaining = voteTime - (os.clock() - startTime)
    end
    table.insert(ballot_box, #ballot_box + 1, ballot)
end

function tally_results(ballot_box)
    for i = 1, #ballot_box do
        ballot_box[i] = tonumber(VOTERS / ballot_box[i])
    end
    return ballot_box -- is this already a reference? check.
end

function schedule_voters(voters)
    -- Max number of voters allowed at once
    local voter_capacity = 10000

    -- Partition voters into groups 
    -- of (max) size voter_capacity
    local voter_groups = {}
    local num_groups = (VOTERS / voter_capacity)
    if VOTERS % voter_capacity > 0 then
        num_groups = num_groups + 1
    end
    for i = 1, num_groups do
        local from = i * voter_capacity
        local to = math.min((i * voter_capacity) + voter_capacity, #voters)
        voter_groups[i] = table.unpack(voters, from, to)
    end

    -- Get rid of duplicate (and unpartitioned) voters
    voters = nil

    -- Define the procedure for each group of voters
    local sort_frequency = 10
    local sort_by_time_remaining = function(ascending)
        return function()
            if ascending then
            else
            end
        end
    end
    local start_voter_group = function(group_id)
        local this_group = voter_groups[group_id]
        for voter in this_group do
            voter = {
                voter = voter,
                timeRem = 0
            }
        end
        while #this_group > 0 do
            for i = 1, #this_group do
                if coroutine.status(this_group[i]) ~= "dead" then
                    this_group[i].timeRem = coroutine.resume((this_group[i]))
                else
                    table.remove(this_group, i)
                end
                if i % (#this_group / sort_frequency) == 0 then
                    table.sort(this_group, sort_by_time_remaining(true))
                end
            end
        end
    end

    -- Begin the group voting procedure, one group at a time
    for i = 1, #voter_groups do
        start_voter_group(group_id)
    end
end

function begin_election()
    -- Time when the election started
    local startTime = os.clock()

    -- Create a ballot_box
    local ballot_box = {
        [1] = 0,
        [2] = 0,
        [3] = 0
    }

    -- Create an array of "vote" coroutines
    local voters = {}
    for i = 1, VOTERS do
        table.insert(voters, i, coroutine.create(vote(ballot_box)))
    end

    -- Schedule the coroutines so each has a 
    -- fair chance of running until completion
    schedule_voters(voters)

    -- Show how long the election took 
    -- in seconds and display the results
    local results = tally_results(ballot_box)
    local finishTime = os.clock() - startTime
    local fStrResults = string.format("")
    print(fStrResults)
end

-- Main below this line --
function main()
    begin_election()
end

main()
