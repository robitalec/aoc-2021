
## Part one

You barely reach the safety of the cave when the whale smashes into the
cave mouth, collapsing it. Sensors indicate another exit to this cave at
a much greater depth, so you have no choice but to press on.

As your submarine slowly makes its way through the cave system, you
notice that the four-digit seven-segment displays in your submarine are
malfunctioning; they must have been damaged during the escape. You’ll be
in a lot of trouble without them, so you’d better figure out what’s
wrong.

Each digit of a seven-segment display is rendered by turning on or off
any of seven segments named a through g:

      0:      1:      2:      3:      4:
     aaaa    ....    aaaa    aaaa    ....
    b    c  .    c  .    c  .    c  b    c
    b    c  .    c  .    c  .    c  b    c
     ....    ....    dddd    dddd    dddd
    e    f  .    f  e    .  .    f  .    f
    e    f  .    f  e    .  .    f  .    f
     gggg    ....    gggg    gggg    ....

      5:      6:      7:      8:      9:
     aaaa    aaaa    aaaa    aaaa    aaaa
    b    .  b    .  .    c  b    c  b    c
    b    .  b    .  .    c  b    c  b    c
     dddd    dddd    ....    dddd    dddd
    .    f  e    f  .    f  e    f  .    f
    .    f  e    f  .    f  e    f  .    f
     gggg    gggg    ....    gggg    gggg

So, to render a 1, only segments c and f would be turned on; the rest
would be off. To render a 7, only segments a, c, and f would be turned
on.

The problem is that the signals which control the segments have been
mixed up on each display. The submarine is still trying to display
numbers by producing output on signal wires a through g, but those wires
are connected to segments randomly. Worse, the wire/segment connections
are mixed up separately for each four-digit display! (All of the digits
within a display use the same connections, though.)

So, you might know that only signal wires b and g are turned on, but
that doesn’t mean segments b and g are turned on: the only digit that
uses two segments is 1, so it must mean segments c and f are meant to be
on. With just that information, you still can’t tell which wire (b/g)
goes to which segment (c/f). For that, you’ll need to collect more
information.

For each display, you watch the changing signals for a while, make a
note of all ten unique signal patterns you see, and then write down a
single four digit output value (your puzzle input). Using the signal
patterns, you should be able to work out which pattern corresponds to
which digit.

For example, here is what you might see in a single entry in your notes:

    acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
    cdfeb fcadb cdfeb cdbaf

(The entry is wrapped here to two lines so it fits; in your notes, it
will all be on a single line.)

Each entry consists of ten unique signal patterns, a \| delimiter, and
finally the four digit output value. Within an entry, the same
wire/segment connections are used (but you don’t know what the
connections actually are). The unique signal patterns correspond to the
ten different ways the submarine tries to render a digit using the
current wire/segment connections. Because 7 is the only digit that uses
three segments, dab in the above example means that to render a 7,
signal lines d, a, and b are on. Because 4 is the only digit that uses
four segments, eafb means that to render a 4, signal lines e, a, f, and
b are on.

Using this information, you should be able to work out which combination
of signal wires corresponds to each of the ten digits. Then, you can
decode the four digit output value. Unfortunately, in the above example,
all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five
segments and are more difficult to deduce.

For now, focus on the easy digits. Consider this larger example:

    be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
    fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
    fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
    cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
    efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
    gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
    gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
    cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
    ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
    gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
    fgae cfgab fg bagce

Because the digits 1, 4, 7, and 8 each use a unique number of segments,
you should be able to tell which combinations of signals correspond to
those digits. Counting only digits in the output values (the part after
\| on each line), in the above example, there are 26 instances of digits
that use a unique number of segments (highlighted above).

In the output values, how many times do digits 1, 4, 7, or 8 appear?

``` r
library(data.table)
library(chk)

example <- readLines('data/day-8-example.txt')
seq_ex <- seq.int(length(example))
eg_DT <- data.table(
    signal = gsub('\\|', '', example[seq_ex[seq_ex %% 2 != 0]]),
    output = example[seq_ex[seq_ex %% 2 == 0]]
)

signals <- readLines('data/day-8-signals.txt')
DT <- data.table(V1 = signals)[, c('signal', 'output') := tstrsplit(V1, ' \\| ')]

single_numb <- c(2, 3, 4, 7)

check_nchar_match <- function(DT, lens) {
    DT[, nchar_output := list(lapply(strsplit(output, ' '), nchar))]
    DT[, sum(unlist(nchar_output) %in% lens)]
}

chk_equal(
    check_nchar_match(eg_DT, single_numb),
    26
)

check_nchar_match(DT, single_numb)
```

    ## [1] 521

## Part two

Through a little deduction, you should now be able to determine the
remaining digits. Consider again the first example above:

    acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
    cdfeb fcadb cdfeb cdbaf

After some careful analysis, the mapping between signal wires and
segments only make sense in the following configuration:

dddd e a e a ffff g b g b cccc

So, the unique signal patterns would correspond to the following digits:

-   acedgfb: 8
-   cdfbe: 5
-   gcdfa: 2
-   fbcad: 3
-   dab: 7
-   cefabd: 9
-   cdfgeb: 6
-   eafb: 4
-   cagedb: 0
-   ab: 1

Then, the four digits of the output value can be decoded:

-   cdfeb: 5
-   fcadb: 3
-   cdfeb: 5
-   cdbaf: 3

Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example
above, the output value of each entry can be determined:

-   fdgacbe cefdb cefbgd gcbe: 8394
-   fcgedb cgb dgebacf gc: 9781
-   cg cg fdcagb cbg: 1197
-   efabcd cedba gadfec cb: 9361
-   gecf egdcabf bgf bfgea: 4873
-   gebdcfa ecba ca fadegcb: 8418
-   cefg dcbef fcge gbcadfe: 4548
-   ed bcgafe cdgba cbgef: 1625
-   gbdfcae bgc cg cgb: 8717
-   fgae cfgab fg bagce: 4315

Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode
the four-digit output values. What do you get if you add up all of the
output values?

``` r
library(mgsub)

numbs <- list(
    '0' = c('a', 'b',  'c', 'e', 'f', 'g'),
    '1' = c('c', 'f'),
    '2' = c('a', 'c',  'd', 'e', 'g'),
    '3' = c('a', 'c',  'd', 'f', 'g'),
    '4' = c('b', 'c',  'd', 'f'),
    '5' = c('a', 'b',  'd', 'f', 'g'),
    '6' = c('a', 'b',  'd', 'e', 'f', 'g'),
    '7' = c('a', 'c', 'f'),
    '8' = c('a', 'b',  'c', 'd', 'e', 'f', 'g'),
    '9' = c('a', 'b',  'c', 'd', 'f', 'g')
)
displays <- data.table(
    numb = names(numbs)
)[, signal := list(numbs[numb])]
displays[, sorted_signal := vapply(signal, paste, collapse = '', 'potato')]
displays[, len := vapply(signal, length, 42)][, n_len := .N, len]

sort_string <- function(string) {
    vapply(strsplit(string, NULL), function(string) paste(sort(string), collapse = ''), '')
}

decode_signal <- function(signal) {
    ind_sigs <- strsplit(signal, ' ')[[1]]
    split_sig <- unlist(lapply(ind_sigs, strsplit, ''))
    sig_letters <- data.table(sig_letter = split_sig)[, .N, sig_letter]

    # Instead of finding the signals from the code, just look at the 
    # number of times we expect each letter to be repeated in the signal
    # eg. the 'e' position should only be repeated 4 times
    count_in_letters <- data.table(
        letter = c('a', 'b', 'c', 'd', 'e', 'f', 'g'),
        N = c(8, 6, 8, 7, 4, 9, 7)
    )[, N_by_N := .N, N]
    
    only_unique <- count_in_letters[N_by_N == 1]
    sig_letters[only_unique, letter := letter, on = 'N']
    
    # Using the short 2 character signal, distinguish between c and a
    sig_letters[is.na(letter) & sig_letter %in% 
                                strsplit(ind_sigs[vapply(ind_sigs, nchar, 42) == 2], '')[[1]],
                            letter := 'c']
    sig_letters[is.na(letter) & N == 8, letter := 'a']
        
    # Using the 4 character signal, distinguish between d and g
    sig_letters[is.na(letter) & sig_letter %in% 
                                strsplit(ind_sigs[vapply(ind_sigs, nchar, 42) == 4], '')[[1]],
                            letter := 'd']
    sig_letters[is.na(letter) & N == 7, letter := 'g']
    
    return(sig_letters) 
}

parse_output <- function(decoded, output, key) {
    sorted <- vapply(strsplit(output, ' ')[[1]], function(x) {
        sort_string(mgsub(x, decoded$sig_letter, decoded$letter))
    }, 'potato')
    m <- merge(data.table(sorted_signal = sorted), displays, by = 'sorted_signal',
                         sort = FALSE)
    m[, as.integer(paste0(numb, collapse = ''))]
}

calc_sum_outputs <- function(DT, key) {
    DT[, parse_output(decode_signal(signal), output, key), by = output][, sum(V1)]
}


chk_equal(
    calc_sum_outputs(eg_DT, displays),
    61229
)

calc_sum_outputs(DT, displays)
```

    ## [1] 1016804
