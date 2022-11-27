function zeros(board) 
    local i = 1
    local j = 0
    return function() 
        while i <= #board do 
            j = j + 1
            if j > #board[i] then
                i = i + 1 
                j = 0
            else
                if board[i][j] == 0 then 
                    return i, j
                end
            end
        end
    end
end

sudoku = "035269780\n680571093\n107034562\n026195040\n304080915\n901043608\n019320804\n208057036\n703018059"
board = {}
for line in string.gmatch(sudoku, "[^\n]*\n") do 
    row = {}
    for c in string.gmatch(line, ".") do 
        table.insert(row, tonumber(c))
    end
    table.insert(board, row)
end

for _, row in ipairs(board) do
    for _, col in ipairs(row) do
        io.write(col, " ")
    end
    print()
end

for i, j in zeros(board) do 
    print(i, j)
end