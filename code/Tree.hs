-- Tree data is either a branch to two new trees, and a value, or is NONE
data Tree = Branch Tree Tree Int
          | None
          deriving (Eq, Show)

-- Add a value to the tree
addBranch :: Int -> Tree -> Tree
-- if there are no values in the tree already, just add the value as the root node
addBranch newValue (None) = Branch None None newValue
-- if there are values in the tree already, add a new branch accordingly
addBranch newValue (Branch left right branchValue) =  
    if newValue > branchValue
        then Branch left (addBranch newValue right) branchValue
    else Branch (addBranch newValue left) right branchValue


-- Check if an integer is already in a tree
findValue :: Int -> Tree -> Bool
findValue searchValue (None) = False
findValue searchValue (Branch left right branchValue) =
    if searchValue == branchValue -- if the current node is the right one
        then True
    else
        if searchValue > branchValue -- if the search value is bigger than the node
            then findValue searchValue right -- search right
        else findValue searchValue left -- else search left

-- Remove a value from the tree
removeBranch :: Int -> Tree -> Tree
removeBranch value (Branch left right branchValue) =
        if value == branchValue -- if the current node is the right one
            then 
                if left == None -- (the left side has no values)
                    then
                        if right == None
                            --(no children)
                            then None
                        else right -- (only the right side has values)
                else -- (the left side has values)
                    if right == None
                        then left -- (only left has values)
                    else (Branch left right  -- (both sides have values)
                    --- UNFINISHED
                        
        else
            if value > branchValue -- if the value is bigger than the node
                then Branch left (removeBranch value right) -- search right
            else Branch (removeBranch value left) right -- else search left