module Constants where

import Data.List(intersperse)

idRequest = "Please input either a boolean value [true|false] or an integer value"

initialize = clearScreen ++ gotoStart
clearScreen = "\ESC[2J"
gotoStart = "\ESC[0;0H"

startMessage = "-------------------Stack Machine Interpreter-------------------\n\n"
    ++ "Stack Machine Language only reckognizes the following instructions: " 
    ++ displayInstructions knownInstructions
    ++ "\n*<id> stands for a name"
    ++ "\n*<val> stands for either true or false or a natural number"
    ++ "\n*<labelName> stands for the name of a label"
    ++ "\n\nDeclaration of labels is as follows: 'labelName: INSTRUCTION'"
    where
        displayInstructions [] = []
        displayInstructions xs = "\n\t" ++ unwords (intersperse "|" a) ++ displayInstructions b
            where (a,b) = splitAt truncateReference xs

        truncateReference = 10 

askForFile = "Please provide a file with Stack Machine Instructions: "

exitMessage = "Execution finished with the following stack state:"

promptStr = "SMI> "

knownInstructions = ["PUSH <val>","POP","SUB","MUL","DIV","AND","OR","LT","LE","GT","GE","EQ","NEQ"
                    ,"UMINUS","NOT","RVALUE <id>","LVALUE <id>","ASSIGN","GOTO <labelName>"
                    ,"GOTRUE <labelName>","GOFALSE LabelName", "Read <id>","Print <id>"]