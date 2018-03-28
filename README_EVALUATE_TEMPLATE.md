# Confusion table report
This module is composed by the file 'evaluate-confusion.lisp' it's purpose is to compare pairs of conllu files looking for a specific type of difference and building a report on it. Examples can be found in the file 'evaluate_template_examples.lisp'
## Basic usage
In order to buid a matrix table report using specified criteria a call to the function 'report-confusion-table', which recieves pairs of conllu files with the same name and compares the tokens using the recieved evaluation function.

### report-confusion-table
 - **work-dir**
   - **example**: ```#P"table_report/"```
   - The directory in which the reports will be written.
   - In the root of this folder a file with the confusion matrix called 'matrix.html' will be created reporting the confusion table.
   - In the root of this folder another folder 'diffs' will be created, and will store the .html files reporting the differences found in the sentences.
 
 - **golden-files**
   - **example**: ```"/golden_files/*.conllu"```
   - A list of the paths of the conllu files considered as the correctly annotated ones
  
 - **prediction-files**
   - **example**: ```"/prediction_files/*.conllu"```
   - A list of the paths of the pedicted conllu files

 - **evaluation-function**
   - **example**: ```(lambda (token sent) (cl-conllu:token-deprel token))```
   - The function used to compare the tokens of the sentences and determine if there are differences
   - This function must recieve:
     -  Token - A cl-conllu token
     -  Sentence - The cl-conllu sentence of the recieved token
   -  This function must return a string representing the evaluation of the desired value of the token. In the displayed example above, these values would be the 'deprel' values of the token.

 - **g-columns**
   - **example**: for the example in 'evaluation-function': ```'("root","nmod","appos")```
   - The allowed columns for the reported confusion table in the golden axis, i.e. the columns
  
 - **p-columns**
   - **example**: for the example in 'evaluation-function': ```'("root","nmod","appos")```
   - The allowed columns for the reported confusion table in the predicted axis, i.e. the rows

 - **column-sort-function**
   - **example**: ```(lambda (columns) (mapcar #'write-to-string (sort (mapcar #'parse-integer columns) #'> )))``` Orders the columns by converting to integer, sorting and converting to strings again
   - Orders the columns of the confusion table for a more appropriate formatting
 
 - **batch-write**
   - The number of pairs of files to be processed before flushing the found results to the diff files. The purpose is to use memory according to the memory resources.
 
  

