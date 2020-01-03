# Erlang-Practice


Pi Calculator
The program, using the Gregory-Leibnez series, calculates pi to a given number of decimal places. As per the Gregory-Leibnez series, the more terms we have in the formula, the greater the precision of the pi estimation is going to be which was a major driving factor in the design of the program.

Using this formula to calculate pi accurately would need to go through a certain amount of recursive iterations. For example, to calculate pi to 2 digits accurately, the number of iterations of the function is roughly 300. This increases exponentially so to get to 8 decimal places would take hundreds of thousands of iterations if not millions!

Duplicate Count
The erlang program takes as an input a list and, as an output, prints, for each element of the list, the number of times that element appears in the list. The output is a list of tuples where the first component for each tuple is an element of the original list, and the second component is the number of times that element appears in the list. 

The additional load file by list function requires the user to call the load function in shell, however once file is loaded, it calls count_duplicate which works in the same manner described above. The file is loaded as binary and converted into a string list, the list items are separated by space character using the string:tokens BIF and the Unicode integer is converted at printing by using ~p format.

Concurrent Program
The program ccharcount begins by calling the load function. The program loads in a text file as binary which is then converted to a list like a string. The split function is called which splits the file up into separate lists. The split function takes in the original list and the number of characters that would be in a list if the original list was split into 20 chunks. It reads the number of characters determined by length and adds them to a new list then each character is counted in said list. 

To add concurrency in the current application, I’ve implemented a method that will go through each chunk that the application splits the initial text into, and spawns a separate process to count the number of each alphabetical character.
This result is then sent to a singular joiner process. This process is spawned in the main “load” method, and waits for lists of results to be sent to it. 


