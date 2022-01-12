# Speed-up-your-R-Work

The details of the codeset and plots are included in the attached Microsoft Word Document (.docx) file in this repository. 
You need to view the file in "Read Mode" to see the contents properly after downloading the same.

Rqdatatable - A Brief Introduction
=====================================
rqdatatable is an implementation of the rquery piped Codd-style relational algebra hosted on data.table. rquery allow the expression of complex transformations as a series of relational operators and rqdatatable implements the operators using data.table.

A Python version of rquery/rqdatatable is under initial development as data_algebra.

For example scoring a logistic regression model (which requires grouping, ordering, and ranking) is organized as follows. For more on this example please see “Let’s Have Some Sympathy For The Part-time R User”.

library("rqdatatable")

## Loading required package: wrapr

## Loading required package: rquery

    # data example
    dL <- build_frame(
       "subjectID", "surveyCategory"     , "assessmentTotal" |
       1          , "withdrawal behavior", 5                 |
       1          , "positive re-framing", 2                 |
       2          , "withdrawal behavior", 3                 |
       2          , "positive re-framing", 4                 )

    scale <- 0.237

    # example rquery pipeline
    rquery_pipeline <- local_td(dL) %.>%
      extend_nse(.,
                 probability :=
                   exp(assessmentTotal * scale))  %.>% 
      normalize_cols(.,
                     "probability",
                     partitionby = 'subjectID') %.>%
      pick_top_k(.,
                 k = 1,
                 partitionby = 'subjectID',
                 orderby = c('probability', 'surveyCategory'),
                 reverse = c('probability', 'surveyCategory')) %.>% 
      rename_columns(., c('diagnosis' = 'surveyCategory')) %.>%
      select_columns(., c('subjectID', 
                          'diagnosis', 
                          'probability')) %.>%
      orderby(., cols = 'subjectID')

We can show the expanded form of query tree.

    cat(format(rquery_pipeline))

    mk_td("dL", c(
      "subjectID",
      "surveyCategory",
      "assessmentTotal")) %.>%
     extend(.,
      probability := exp(assessmentTotal * 0.237)) %.>%
     extend(.,
      probability := probability / sum(probability),
      partitionby = c('subjectID'),
      orderby = c(),
      reverse = c()) %.>%
     extend(.,
      row_number := row_number(),
      partitionby = c('subjectID'),
      orderby = c('probability', 'surveyCategory'),
      reverse = c('probability', 'surveyCategory')) %.>%
     select_rows(.,
       row_number <= 1) %.>%
     rename_columns(.,
      c('diagnosis' = 'surveyCategory')) %.>%
     select_columns(., 
        c('subjectID', 'diagnosis', 'probability')) %.>%
     order_rows(.,
      c('subjectID'),
      reverse = c(),
      limit = NULL)

And execute it using data.table.

    ex_data_table(rquery_pipeline)

    ##   subjectID           diagnosis probability
    ## 1         1 withdrawal behavior   0.6706221
    ## 2         2 positive re-framing   0.5589742

One can also apply the pipeline to new tables.

    build_frame(
       "subjectID", "surveyCategory"     , "assessmentTotal" |
       7          , "withdrawal behavior", 5                 |
       7          , "positive re-framing", 20                ) %.>%
      rquery_pipeline

    ##   subjectID           diagnosis probability
    ## 1         7 positive re-framing   0.9722128

Initial bench-marking of rqdatatable is very favorable (notes here).

    To install rqdatatable please use install.packages("rqdatatable").

    Some related work includes:

        data.table
        disk.frame
        dbplyr
        dplyr
        dtplyr
        maditr
        nc
        poorman
        rquery
        SparkR
        sparklyr
        sqldf
        table.express
        tidyfast
        tidyfst
        tidyquery
        tidyr
        tidytable (formerly gdt/tidydt)


Note rqdatatable has an “immediate mode” which allows direct application of pipelines stages without pre-assembling the pipeline. “Immediate mode” is a convenience for ad-hoc analyses, and has some negative performance impact, so we encourage users to build pipelines for most work. Some notes on the issue can be found here.rqdatatable implements the rquery grammar in the style of a “Turing or Cook reduction” (implementing the result in terms of multiple oracle calls to the related system).rqdatatable is intended for “simple column names”, in particular as rqdatatable often uses eval() to work over data.table escape characters such as “\” and “\\” are not reliable in column names. Also rqdatatable does not support tables with no columns.

wrapr - A Brief Introduction
==============================
wrapr is an R package that supplies powerful tools for writing and debugging R code.

Introduction

Primary wrapr services include:

        %.>% (dot arrow pipe)
        unpack/to (assign to multiple values)
        as_named_list (build up a named list quickly)
        build_frame() / draw_frame() ( data.frame builders and formatters )
        bc() (blank concatenate)
        qc() (quoting concatenate)
        := (named map builder)
        %?% (coalesce)
        %.|% (reduce/expand args)
        uniques() (safe unique() replacement)
        partition_tables() / execute_parallel()
        DebugFnW() (function debug wrappers)
        λ() (anonymous function builder)
        let() (let block)
        evalb()/si() (evaluate with bquote / string interpolation)
        sortv() (sort a data.frame by a set of columns).
        stop_if_dot_args() (check for unexpected arguments)

library(wrapr)
    packageVersion("wrapr")
     #  [1] '2.0.8'
    date()
     #  [1] "Thu Jun 10 13:49:30 2021"

    %.>% (dot pipe or dot arrow)

    %.>% dot arrow pipe is a pipe with intended semantics:

        “a %.>% b” is to be treated approximately as if the user had written “{ . <- a; b };” with “%.>%” being treated as left-associative.

    Other R pipes include magrittr and pipeR.

The following two expressions should be equivalent:

    cos(exp(sin(4)))
     #  [1] 0.8919465

    4 %.>% sin(.) %.>% exp(.) %.>% cos(.)
     #  [1] 0.8919465

The notation is quite powerful as it treats pipe stages as expression parameterized over the variable “.”. This means you do not need to introduce functions to express stages. The following is a valid dot-pipe:

    1:4 %.>% .^2 
     #  [1]  1  4  9 16

The notation is also very regular as we show below:

    1:4 %.>% sin
     #  [1]  0.8414710  0.9092974  0.1411200 -0.7568025
    1:4 %.>% sin(.)
     #  [1]  0.8414710  0.9092974  0.1411200 -0.7568025
    1:4 %.>% base::sin
     #  [1]  0.8414710  0.9092974  0.1411200 -0.7568025
    1:4 %.>% base::sin(.)
     #  [1]  0.8414710  0.9092974  0.1411200 -0.7568025

    1:4 %.>% function(x) { x + 1 }
     #  [1] 2 3 4 5
    1:4 %.>% (function(x) { x + 1 })
     #  [1] 2 3 4 5

    1:4 %.>% { .^2 } 
     #  [1]  1  4  9 16
    1:4 %.>% ( .^2 )
     #  [1]  1  4  9 16

Regularity can be a big advantage in teaching and comprehension. Please see “In Praise of Syntactic Sugar” for more details. Some formal documentation can be found here.

    Some obvious “dot-free”" right-hand sides are rejected. Pipelines are meant to move values through a sequence of transforms, and not just for side-effects. Example: `5 %.>% 6` deliberately stops as `6` is a right-hand side that obviously does not use its incoming value. This check is only applied to values, not functions on the right-hand side.
    Trying to pipe into a an “zero argument function evaluation expression” such as `sin()` is prohibited as it looks too much like the user declaring `sin()` takes no arguments. One must pipe into either a function, function name, or an non-trivial expression (such as `sin(.)`). A useful error message is returned to the user: `wrapr::pipe does not allow direct piping into a no-argument function call expression (such as "sin()" please use sin(.))`.
    Some reserved words can not be piped into. One example is `5 %.>% return(.)` is prohibited as the obvious pipe implementation would not actually escape from user functions as users may intend.
    Obvious de-references (such as `$`, `::`, `@`, and a few more) on the right-hand side are treated performed (example: `5 %.>% base::sin(.)`).
    Outer parenthesis on the right-hand side are removed (example: `5 %.>% (sin(.))`).
    Anonymous function constructions are evaluated so the function can be applied (example: `5 %.>% function(x) {x+1}` returns 6, just as `5 %.>% (function(x) {x+1})(.)` does).
    Checks and transforms are not performed on items inside braces (example: `5 %.>% { function(x) {x+1} }` returns `function(x) {x+1}`, not 6).
    The dot arrow pipe has S3/S4 dispatch (please see ). However as the right-hand side of the pipe is normally held unevaluated, we don’t know the type except in special cases (such as the rigth-hand side being referred to by a name or variable). To force the evaluation of a pipe term, simply wrap it in `.()`.

The dot pipe is also user configurable through standard S3/S4 methods.

The dot pipe has been formally written up in the R Journal.

    @article{RJ-2018-042,
      author = {John Mount and Nina Zumel},
      title = {{Dot-Pipe: an S3 Extensible Pipe for R}},
      year = {2018},
      journal = {{The R Journal}},
      url = {https://journal.r-project.org/archive/2018/RJ-2018-042/index.html}
    }

unpack/to multiple assignments

Unpack a named list into the current environment by name (for a positional based multiple assignment operator please see zeallot, for another named base multiple assigment please see vadr::bind).

    d <- data.frame(
      x = 1:9,
      group = c('train', 'calibrate', 'test'),
      stringsAsFactors = FALSE)

    unpack[
      train_data = train,
      calibrate_data = calibrate,
      test_data = test
      ] := split(d, d$group)

    knitr::kable(train_data)

      x 	group
    1 	1 	train
    4 	4 	train
    7 	7 	train
    as_named_list

Build up named lists. Very convenient for managing workspaces when used with used with unpack/to.

    as_named_list(train_data, calibrate_data, test_data)
     #  $train_data
     #    x group
     #  1 1 train
     #  4 4 train
     #  7 7 train
     #  
     #  $calibrate_data
     #    x     group
     #  2 2 calibrate
     #  5 5 calibrate
     #  8 8 calibrate
     #  
     #  $test_data
     #    x group
     #  3 3  test
     #  6 6  test
     #  9 9  test

    build_frame() / draw_frame()

build_frame() is a convenient way to type in a small example data.frame in natural row order. This can be very legible and saves having to perform a transpose in one’s head. draw_frame() is the complimentary function that formats a given data.frame (and is a great way to produce neatened examples).

    x <- build_frame(
       "measure"                   , "training", "validation" |
       "minus binary cross entropy", 5         , -7           |
       "accuracy"                  , 0.8       , 0.6          )
    print(x)
     #                       measure training validation
     #  1 minus binary cross entropy      5.0       -7.0
     #  2                   accuracy      0.8        0.6
    str(x)
     #  'data.frame':   2 obs. of  3 variables:
     #   $ measure   : chr  "minus binary cross entropy" "accuracy"
     #   $ training  : num  5 0.8
     #   $ validation: num  -7 0.6
    cat(draw_frame(x))
     #  x <- wrapr::build_frame(
     #     "measure"                     , "training", "validation" |
     #       "minus binary cross entropy", 5         , -7           |
     #       "accuracy"                  , 0.8       , 0.6          )

qc() (quoting concatenate)

qc() is a quoting variation on R’s concatenate operator c(). This code such as the following:

    qc(a = x, b = y)
     #    a   b 
     #  "x" "y"

    qc(one, two, three)
     #  [1] "one"   "two"   "three"

qc() also allows bquote() driven .()-style argument escaping.

    aname <- "I_am_a"
    yvalue <- "six"

    qc(.(aname) := x, b = .(yvalue))
     #  I_am_a      b 
     #     "x"  "six"

Notice the := notation is required for syntacitic reasons.
:= (named map builder)

:= is the “named map builder”. It allows code such as the following:

    'a' := 'x'
     #    a 
     #  "x"

The important property of named map builder is it accepts values on the left-hand side allowing the following:

    name <- 'variableNameFromElsewhere'
    name := 'newBinding'
     #  variableNameFromElsewhere 
     #               "newBinding"

A nice property is := commutes (in the sense of algebra or category theory) with R’s concatenation function c(). That is the following two statements are equivalent:

    c('a', 'b') := c('x', 'y')
     #    a   b 
     #  "x" "y"

    c('a' := 'x', 'b' := 'y')
     #    a   b 
     #  "x" "y"

The named map builder is designed to synergize with seplyr.
    %?% (coalesce)

The coalesce operator tries to replace elements of its first argument with elements from its second argument. In particular %?% replaces NULL vectors and NULL/NA entries of vectors and lists.

Example:

    c(1, NA) %?% list(NA, 20)
     #  [1]  1 20

    %.|% (reduce/expand args)

    x %.|% f stands for f(x[[1]], x[[2]], ..., x[[length(x)]]). v %|.% x also stands for f(x[[1]], x[[2]], ..., x[[length(x)]]). The two operators are the same, the variation just allowing the user to choose the order they write things. The mnemonic is: “data goes on the dot-side of the operator.”

    args <- list('prefix_', c(1:3), '_suffix')

    args %.|% paste0
     #  [1] "prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"
    # prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"

    paste0 %|.% args
     #  [1] "prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"
    # prefix_1_suffix" "prefix_2_suffix" "prefix_3_suffix"

DebugFnW()

DebugFnW() wraps a function for debugging. If the function throws an exception the execution context (function arguments, function name, and more) is captured and stored for the user. The function call can then be reconstituted, inspected and even re-run with a step-debugger. Please see our free debugging video series and vignette('DebugFnW', package='wrapr') for examples.
λ() (anonymous function builder)

λ() is a concise abstract function creator or “lambda abstraction”. It is a placeholder that allows the use of the -character for very concise function abstraction.

Example:

    # Make sure lambda function builder is in our enironment.
    wrapr::defineLambda()

    # square numbers 1 through 4
    sapply(1:4, λ(x, x^2))
     #  [1]  1  4  9 16

let()

let() allows execution of arbitrary code with substituted variable names (note this is subtly different than binding values for names as with base::substitute() or base::with()).

The function is simple and powerful. It treats strings as variable names and re-writes expressions as if you had used the denoted variables. For example the following block of code is equivalent to having written “a + a”.

    a <- 7

    let(
      c(VAR = 'a'),

      VAR + VAR
    )
     #  [1] 14

This is useful in re-adapting non-standard evaluation interfaces (NSE interfaces) so one can script or program over them.

We are trying to make let() self teaching and self documenting (to the extent that makes sense). For example try the arguments “eval=FALSE” prevent execution and see what would have been executed, or debug=TRUE to have the replaced code printed in addition to being executed:

    let(
      c(VAR = 'a'),
      eval = FALSE,
      {
        VAR + VAR
      }
    )
     #  {
     #      a + a
     #  }

    let(
      c(VAR = 'a'),
      debugPrint = TRUE,
      {
        VAR + VAR
      }
    )
     #  $VAR
     #  [1] "a"
     #  
     #  {
     #      a + a
     #  }
     #  [1] 14

Please see vignette('let', package='wrapr') for more examples. Some formal documentation can be found here. wrapr::let() was inspired by gtools::strmacro() and base::bquote(), please see here for some notes on macro methods in R.evalb()/si() (evaluate with bquote / string interpolation)wrapr supplies unified notation for quasi-quotation and string interpolation.

    angle = 1:10
    variable <- "angle"

    # execute code
    evalb(
      plot(x = .(-variable), y = sin(.(-variable)))
    )

    # alter string
    si("plot(x = .(variable), y = .(variable))")
     #  [1] "plot(x = \"angle\", y = \"angle\")"

    The extra .(-x) form is a shortcut for .(as.name(x)).
    sortv() (sort a data.frame by a set of columns)

This is the sort command that is missing from R: sort a data.frame by a chosen set of columns specified in a variable.

    d <- data.frame(
      x = c(2, 2, 3, 3, 1, 1), 
      y = 6:1,
      z = 1:6)
    order_cols <- c('x', 'y')

    sortv(d, order_cols)
     #    x y z
     #  6 1 1 6
     #  5 1 2 5
     #  2 2 5 2
     #  1 2 6 1
     #  4 3 3 4
     #  3 3 4 3

Installation
==============
Install with:

install.packages("wrapr")

More Information

rquery - A Brief Introduction
===============================

rquery is a piped query generator based on Codd’s relational algebra (updated to reflect lessons learned from working with R, SQL, and dplyr at big data scale in production).
Introduction.rquery is a data wrangling system designed to express complex data manipulation as a series of simple data transforms. This is in the spirit of R’s base::transform(), or dplyr’s dplyr::mutate() and uses a pipe in the style popularized in R with magrittr. The operators themselves follow the selections in Codd’s relational algebra, with the addition of the traditional SQL “window functions.” More on the background and context of rquery can be found here.The R/rquery version of this introduction is here, and the Python/data_algebra version of this introduction is here.In transform formulations data manipulation is written as transformations that produce new data.frames, instead of as alterations of a primary data structure (as is the case with data.table). Transform system can use more space and time than in-place methods. However, in our opinion, transform systems have a number of pedagogical advantages.

In rquery’s case the primary set of data operators is as follows:

            drop_columns
            select_columns
            rename_columns
            select_rows
            order_rows
            extend
            project
            natural_join
            convert_records (supplied by the cdata package).

These operations break into a small number of themes:

            Simple column operations (selecting and re-naming columns).
            Simple row operations (selecting and re-ordering rows).
            Creating new columns or replacing columns with new calculated values.
            Aggregating or summarizing data.
            Combining results between two data.frames.
            General conversion of record layouts (supplied by the cdata package).

The point is: Codd worked out that a great number of data transformations can be decomposed into a small number of the above steps. rquery supplies a high performance implementation of these methods that scales from in-memory scale up through big data scale (to just about anything that supplies a sufficiently powerful SQL interface, such as PostgreSQL, Apache Spark, or Google BigQuery).

We will work through simple examples/demonstrations of the rquery data manipulation operators.
rquery operators.Simple column operations (selecting and re-naming columns)

The simple column operations are as follows.

            drop_columns
            select_columns
            rename_columns

These operations are easy to demonstrate.

We set up some simple data.

        d <- data.frame(
          x = c(1, 1, 2),
          y = c(5, 4, 3),
          z = c(6, 7, 8)
        )

        knitr::kable(d)

        x 	y 	z
        1 	5 	6
        1 	4 	7
        2 	3 	8

For example: drop_columns works as follows. drop_columns creates a new data.frame without certain columns.

library(rquery)

        ## Loading required package: wrapr

        library(rqdatatable)

        drop_columns(d, c('y', 'z'))

        ##   x
        ## 1 1
        ## 2 1
        ## 3 2

In all cases the first argument of a rquery operator is either the data to be processed, or an earlier rquery pipeline to be extended. We will take about composing rquery operations after we work through examples of all of the basic operations. We can write the above in piped notation (using the wrapr pipe in this case):

        d %.>%
          drop_columns(., c('y', 'z')) %.>%
          knitr::kable(.)

        x
        1
        1
        2

Notice the first argument is an explicit “dot” in wrapr pipe notation.select_columns’s action is also obvious from example.

        d %.>%
          select_columns(., c('x', 'y')) %.>%
          knitr::kable(.)

        x 	y
        1 	5
        1 	4
        2 	3

rename_columns is given as name-assignments of the form 'new_name' = 'old_name':

        d %.>%
          rename_columns(., 
                         c('x_new_name' = 'x', 
                           'y_new_name' = 'y')
                         ) %.>%
          knitr::kable(.)

        x_new_name 	y_new_name 	z
        1 	5 	6
        1 	4 	7
        2 	3 	8
Simple row operations (selecting and re-ordering rows)

The simple row operations are:

            select_rows
            order_rows

select_rows keeps the set of rows that meet a given predicate expression.

        d %.>%
          select_rows(., x == 1) %.>%
          knitr::kable(.)

        x 	y 	z
        1 	5 	6
        1 	4 	7

Notes on how to use a variable to specify column names in select_rows can be found here.

order_rows re-orders rows by a selection of column names (and allows reverse ordering by naming which columns to reverse in the optional reverse argument). Multiple columns can be selected in the order, each column breaking ties in the earlier comparisons.

        d %.>%
          order_rows(., 
                     c('x', 'y'),
                     reverse = 'x') %.>%
          knitr::kable(.)

        x 	y 	z
        2 	3 	8
        1 	4 	7
        1 	5 	6

General rquery operations do not depend on row-order and are not guaranteed to preserve row-order, so if you do want to order rows you should make it the last step of your pipeline.
Creating new columns or replacing columns with new calculated values

The important create or replace column operation is: extend

extend accepts arbitrary expressions to create new columns (or replace existing ones). For example:

        d %.>%
          extend(., zzz := y / x) %.>%
          knitr::kable(.)

        x 	y 	z 	zzz
        1 	5 	6 	5.0
        1 	4 	7 	4.0
        2 	3 	8 	1.5

We can use = or := for column assignment. In these examples we will use := to keep column assignment clearly distinguishable from argument binding.

extend allows for very powerful per-group operations akin to what SQL calls “window functions”. When the optional partitionby argument is set to a vector of column names then aggregate calculations can be performed per-group. For example.

        shift <- data.table::shift

        d %.>%
          extend(.,
                 max_y := max(y),
                 shift_z := shift(z),
                 row_number := row_number(),
                 cumsum_z := cumsum(z),
                 partitionby = 'x',
                 orderby = c('y', 'z')) %.>%
          knitr::kable(.)

        x 	y 	z 	max_y 	shift_z 	row_number 	cumsum_z
        1 	4 	7 	5 	    NA 	            1 	        7   
        1 	5 	6 	5 	    7 	            2 	        13
        2 	3 	8 	3 	    NA 	            1 	        8

Notice the aggregates were performed per-partition (a set of rows with matching partition key values, specified by partitionby) and in the order determined by the orderby argument (without the orderby argument order is not guaranteed, so always set orderby for windowed operations that depend on row order!).More on the window functions can be found here. Notes on how to use a variable to specify column names in extend can be found here.Aggregating or summarizing data.The main aggregation method for rquery is:

            project

project performs per-group calculations, and returns only the grouping columns (specified by groupby) and derived aggregates. For example:

        d %.>%
          project(.,
                 max_y := max(y),
                 count := n(),
                 groupby = 'x') %.>%
          knitr::kable(.)

        x 	max_y 	count
        1 	5    	2
        2 	3 	    1

Notice we only get one row for each unique combination of the grouping variables. We can also aggregate into a single row by not specifying any groupby columns.

        d %.>%
          project(.,
                 max_y := max(y),
                 count := n()) %.>%
          knitr::kable(.)

        max_y 	count
        5 	        3

Notes on how to use a variable to specify column names in project can be found here.Combining results between two data.frames.To combine multiple tables in rquery one uses what we call the natural_join operator. In the rquery natural_join, rows are matched by column keys and any two columns with the same name are coalesced (meaning the first table with a non-missing values supplies the answer). This is easiest to demonstrate with an example.

Let’s set up new example tables.

        d_left <- data.frame(
          k = c('a', 'a', 'b'),
          x = c(1, NA, 3),
          y = c(1, NA, NA),
          stringsAsFactors = FALSE
        )

        knitr::kable(d_left)

        k 	x 	y
        a 	1 	1
        a 	NA 	NA
        b 	3 	NA

        d_right <- data.frame(
          k = c('a', 'b', 'q'),
          y = c(10, 20, 30),
          stringsAsFactors = FALSE
        )

        knitr::kable(d_right)

        k 	y
        a 	10
        b 	20
        q 	30

To perform a join we specify which set of columns our our row-matching conditions (using the by argument) and what type of join we want (using the jointype argument). For example we can use jointype = 'LEFT' to augment our d_left table with additional values from d_right.

        natural_join(d_left, d_right,
                     by = 'k',
                     jointype = 'LEFT') %.>%
          knitr::kable(.)

        k 	x 	y
        a 	1 	1
        a 	NA 	10
        b 	3 	20

In a left-join (as above) if the right-table has unique keys then we get a table with the same structure as the left-table- but with more information per row. This is a very useful type of join in data science projects. Notice columns with matching names are coalesced into each other, which we interpret as “take the value from the left table, unless it is missing.” General conversion of record layouts is Record transformation is “simple once you get it”. However, we suggest reading up on that as a separate topic here.
Composing operationsWe could, of course, perform complicated data manipulation by sequencing rquery operations. For example to select one row with minimal y per-x group we could work in steps as follows:

        . <- d
        . <- extend(.,
                    row_number := row_number(),
                    partitionby = 'x',
                    orderby = c('y', 'z'))
        . <- select_rows(.,
                         row_number == 1)
        . <- drop_columns(.,
                          "row_number")
        knitr::kable(.)

        x 	y 	z
        1 	4 	7
        2 	3 	8

The above discipline has the advantage that it is easy to debug, as we can run line by line and inspect intermediate values. We can even use the Bizarro pipe to make this look like a pipeline of operations.

        d ->.;
          extend(.,
                 row_number := row_number(),
                 partitionby = 'x',
                 orderby = c('y', 'z')) ->.;
          select_rows(.,
                      row_number == 1)  ->.;
          drop_columns(.,
                       "row_number")    ->.;
          knitr::kable(.)

        x 	y 	z
        1 	4 	7
        2 	3 	8

Or we can use the wrapr pipe on the data, which we call “immediate mode” (for more on modes please see here).

        d %.>%
          extend(.,
                 row_number := row_number(),
                 partitionby = 'x',
                 orderby = c('y', 'z')) %.>%
          select_rows(.,
                      row_number == 1)  %.>%
          drop_columns(.,
                       "row_number")    %.>%
          knitr::kable(.)

        x 	y 	z
        1 	4 	7
        2 	3 	8

rquery operators can also act on rquery pipelines instead of acting on data. We can write our operations as follows:

        ops <- local_td(d) %.>%
          extend(.,
                 row_number := row_number(),
                 partitionby = 'x',
                 orderby = c('y', 'z')) %.>%
          select_rows(.,
                      row_number == 1)  %.>%
          drop_columns(.,
                       "row_number")

        cat(format(ops))

        ## mk_td("d", c(
        ##   "x",
        ##   "y",
        ##   "z")) %.>%
        ##  extend(.,
        ##   row_number := row_number(),
        ##   partitionby = c('x'),
        ##   orderby = c('y', 'z'),
        ##   reverse = c()) %.>%
        ##  select_rows(.,
        ##    row_number == 1) %.>%
        ##  drop_columns(.,
        ##    c('row_number'))

And we can re-use this pipeline, both on local data and to generate SQL to be run in remote databases. Applying this operator pipeline to our data.frame d is performed as follows.

        d %.>% 
          ops %.>%
          knitr::kable(.)

        x 	y 	z
        1 	4 	7
        2 	3 	8

And for SQL we have the following:

        raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        RSQLite::initExtension(raw_connection)
        db <- rquery_db_info(
          connection = raw_connection,
          is_dbi = TRUE,
          connection_options = rq_connection_tests(raw_connection))

        cat(to_sql(ops, db))

        ## SELECT
        ##  `x`,
        ##  `y`,
        ##  `z`
        ## FROM (
        ##  SELECT * FROM (
        ##   SELECT
        ##    `x`,
        ##    `y`,
        ##    `z`,
        ##    row_number ( ) OVER (  PARTITION BY `x` ORDER BY `y`, `z` ) AS `row_number`
        ##   FROM (
        ##    SELECT
        ##     `x`,
        ##     `y`,
        ##     `z`
        ##    FROM
        ##     `d`
        ##    ) tsql_23864987081883883673_0000000000
        ##  ) tsql_23864987081883883673_0000000001
        ##  WHERE `row_number` = 1
        ## ) tsql_23864987081883883673_0000000002

# clean up
DBI::dbDisconnect(raw_connection)

For more SQL examples, please see here.Pipeline principles.What we are trying to illustrate above: there is a continuum of notations possible between:

            Working over values with explicit intermediate variables.
            Working over values with a pipeline.
            Working over operators with a pipeline.


Conclusion:

rquery supplies a very teachable grammar of data manipulation based on Codd’s relational algebra and experience with pipelined data transforms (such as base::transform(), dplyr, and data.table).For in-memory situations rquery uses data.table as the implementation provider (through the small adapter package rqdatatable) and is routinely faster than any other R data manipulation system except data.table itself.For bigger than memory situations rquery can translate to any sufficiently powerful SQL dialect, allowing rquery pipelines to be executed on PostgreSQL, Apache Spark, or Google BigQuery.In addition the data_algebra Python package supplies a nearly identical system for working with data in Python. # Background

There are many prior relational algebra inspired specialized query languages. Just a few include:

            Alpha ~1971.
            ISBL / Information system based language ~1973
            QUEL ~1974.
            IBM System R ~1974.
            SQL ~1974.
            Tutorial D ~1994.
            data.table ~2006.
            LINQ ~2007.
            pandas ~2008.
            dplyr ~2014.
            Apache Calcite ~2014.

rquery is realized as a thin translation to an underlying SQL provider. We are trying to put the Codd relational operators front and center (using the original naming, and back-porting SQL progress such as window functions to the appropriate relational operator).

Some related work includes:

            data.table
            disk.frame
            dbplyr
            dplyr
            dtplyr
            maditr
            nc
            poorman
            rqdatatable
            SparkR
            sparklyr
            sqldf
            table.express
            tidyfast
            tidyfst
            tidyquery
            tidyr
            tidytable (formerly gdt/tidydt)

Installing

To install rquery please try install.packages("rquery").


