
## Part one

You’re already almost 1.5km (almost a mile) below the surface of the
ocean, already so deep that you can’t see any sunlight. What you can
see, however, is a giant squid that has attached itself to the outside
of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of
numbers. Numbers are chosen at random, and the chosen number is marked
on all boards on which it appears. (Numbers may not appear on all
boards.) If all numbers in any row or any column of a board are marked,
that board wins. (Diagonals don’t count.)

The submarine has a bingo subsystem to help passengers (currently, you
and the giant squid) pass the time. It automatically generates a random
order in which to draw numbers and a random set of boards (your puzzle
input). For example:

    7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

    22 13 17 11  0
     8  2 23  4 24
    21  9 14 16  7
     6 10  3 18  5
     1 12 20 15 19

     3 15  0  2 22
     9 18 13 17  5
    19  8  7 25 23
    20 11 10 24  4
    14 21 16 12  6

    14 21 17 24  4
    10 16 15  9 19
    18  8 23 26 20
    22 11 13  6  5
     2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are
no winners, but the boards are marked as follows (shown here adjacent to
each other to save space):

22 13 17 11 0 3 15 0 2 22 14 21 17 24 4 8 2 23 4 24 9 18 13 17 5 10 16
15 9 19 21 9 14 16 7 19 8 7 25 23 18 8 23 26 20 6 10 3 18 5 20 11 10 24
4 22 11 13 6 5 1 12 20 15 19 14 21 16 12 6 2 0 12 3 7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there
are still no winners:

22 13 17 11 0 3 15 0 2 22 14 21 17 24 4 8 2 23 4 24 9 18 13 17 5 10 16
15 9 19 21 9 14 16 7 19 8 7 25 23 18 8 23 26 20 6 10 3 18 5 20 11 10 24
4 22 11 13 6 5 1 12 20 15 19 14 21 16 12 6 2 0 12 3 7

Finally, 24 is drawn:

22 13 17 11 0 3 15 0 2 22 14 21 17 24 4 8 2 23 4 24 9 18 13 17 5 10 16
15 9 19 21 9 14 16 7 19 8 7 25 23 18 8 23 26 20 6 10 3 18 5 20 11 10 24
4 22 11 13 6 5 1 12 20 15 19 14 21 16 12 6 2 0 12 3 7

At this point, the third board wins because it has at least one complete
row or column of marked numbers (in this case, the entire top row is
marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding
the sum of all unmarked numbers on that board; in this case, the sum is
188. Then, multiply that sum by the number that was just called when the
board won, 24, to get the final score, 188 \* 24 = 4512.

To guarantee victory against the giant squid, figure out which board
will win first. What will your final score be if you choose that board?

``` r
library(data.table)
library(tinytest)

# Data
eg_draws <- c(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6,
                            15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1)
draws <- c(87, 7, 82, 21, 47, 88, 12, 71, 24, 35, 10, 90, 4, 97, 30, 55, 36,
                     74, 19, 50, 23, 46, 13, 44, 69, 27, 2, 0, 37, 33, 99, 49, 77, 15,
                     89, 98, 31, 51, 22, 96, 73, 94, 95, 18, 52, 78, 32, 83, 85, 54,
                     75, 84, 59, 25, 76, 45, 20, 48, 9, 28, 39, 70, 63, 56, 5, 68, 61,
                     26, 58, 92, 67, 53, 43, 62, 17, 81, 80, 66, 91, 93, 41, 64, 14,
                     8, 57, 38, 34, 16, 42, 11, 86, 72, 40, 65, 79, 6, 3, 29, 60, 1)
eg_DT <- na.omit(fread('data/day-4-example-cards.txt', fill = TRUE))
DT <- na.omit(fread('data/day-4-cards.txt', fill = TRUE))

# Functions
split_cards <- function(card_txt, size = 5L) {
    lapply(seq.int(nrow(card_txt) / size), function(i) {
        end <- i * size
        as.matrix(card_txt[(end - (size - 1)):end], ncol = size, nrow = size)
    })
}

find_win <- function(card, draws) {
    v <- vapply(seq.int(draws), function(d) {
        card[card %in% draws[seq.int(d)]] <- NA
        row_check <- any(apply(card, 1, function(x) sum(is.na(x)) == 5))
        col_check <- any(apply(card, 2, function(x) sum(is.na(x)) == 5))
        if (row_check | col_check) {
            return(d)
        } else {
            return(NA)
        }
    }, 42)
    
    if (sum(is.na(v)) == length(draws)) {
        return(NA)
    } else {
        return(min(v, na.rm = TRUE))
    }
}

sum_win <- function(wins, cards, draws, strategy = 'min') {
    if (strategy == 'min') {
        which_win <- which.min(wins)
        card <- cards[[which_win]]
    } else if (strategy == 'max') {
        which_win <- which.max(wins)
        card <- cards[[which_win]]
    }
    win_draws <- draws[seq.int(wins[which_win])]
    card[card %in% win_draws] <- NA
    sum(card, na.rm = TRUE) * draws[wins[which_win]]
}

# Processing
expect_equal({
    eg_cards <- split_cards(eg_DT, 5L)
    eg_wins <- vapply(eg_cards, find_win, eg_draws, FUN.VALUE = 42)
    sum_win(eg_wins, eg_cards, eg_draws)
    },
    4512
)
```

    ## ----- PASSED      : <-->
    ##  call| eval(expr = expr, envir = envir)

``` r
cards <- split_cards(DT, 5L)
wins <- vapply(cards, find_win, draws, FUN.VALUE = 42)
sum_win(wins, cards, draws)
```

    ## [1] 89001

## Part two

On the other hand, it might be wise to try a different strategy: let the
giant squid win.

You aren’t sure how many bingo boards a giant squid could play at once,
so rather than waste time counting its arms, the safe thing to do is to
figure out which board will win last and choose that one. That way, no
matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens
after 13 is eventually called and its middle column is completely
marked. If you were to keep playing until this point, the second board
would have a sum of unmarked numbers equal to 148 for a final score of
148 \* 13 = 1924.

Figure out which board will win last. Once it wins, what would its final
score be?

``` r
# Processing
expect_equal(
    sum_win(eg_wins, eg_cards, eg_draws, strategy = 'max'),
    1924
)
```

    ## ----- PASSED      : <-->
    ##  call| eval(expr = expr, envir = envir)

``` r
sum_win(wins, cards, draws, strategy = 'max')
```

    ## [1] 7296
