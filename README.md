## N-Queens Solver

This README provides information on running the N-Queens solver in Prolog. The program is can currently solve the problem for chessboards up to 6x6 dimensions.


#### Description

The N-Queens problem is a classic puzzle where the objective is to place N queens on an N×N chessboard so that no two queens threaten each other. This means no two queens can share the same row, column, or diagonal.

This Prolog program utilizes a backtracking algorithm to find solutions for boards from 1x1 up to 6x6 dimensions.

#### Prerequisites

You need to have Scryer Prolog installed on your machine to run this program. Installation instructions for Scryer Prolog can be found here https://github.com/mthom/scryer-prolog.

#### Setup

Clone the repository or download the files to your local machine.
Ensure your working directory contains the Prolog file move.pl (the program file containing the solver logic).

#### How to Run

Open your terminal and navigate to the directory containing your Prolog file. Start the Scryer Prolog environment.

Once Scryer Prolog is running, perform the following steps:

Load the Prolog program:

```
?- [move].
true.
```

Clear any previous configurations:

```
?- retractall(queen_set(_)).
true.
```
Execute the solver for a 6x6 board:

```
?- nsearch(Queens), write_square_and_traverse_wrapper(1,6, Queens).
```
This will display a solution to the 6x6 N-Queens problem, where 'β' represents a queen.

#### Understanding the Output

The output is a visual representation of the chessboard with 'β' indicating the presence of a queen. Each line represents a row on the chessboard.

#### Demo

https://asciinema.org/a/b0YimrR8IbYlh2qw9RDrzxwtd

#### Current Limitations and Future Plans

The program currently supports chessboards up to 6x6 dimensions. Attempting to solve for larger sizes may not produce results due to the limited scope of the implementation.

The current implementation is inefficient and verbose. I aim to optimize the codebase and reduce line count significantly.

I also plan to introduce more implementations in a diverse group of languages.

#### Note

The solution displayed is one of the possible solutions. Different runs may produce the same or different solutions due to the nature of the backtracking algorithm.

