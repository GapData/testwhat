## ---- echo = FALSE-------------------------------------------------------

knitr::opts_chunk$set(error = TRUE)
tw <<- testwhat:::tw

setup_state <- function(STU_CODE, SOL_CODE, PEC = character()) {
  sol_env <- new.env()
  stu_env <- new.env()
  
  evaluate::evaluate(PEC,      envir=sol_env)
  evaluate::evaluate(SOL_CODE, envir=sol_env)
  evaluate::evaluate(PEC,      envir=stu_env)
  evaluate::evaluate(STU_CODE, envir=stu_env)
  
  tw$clear()

  state <- testwhat:::RootState$new(
    pec = PEC,
    student_code = STU_CODE,
    student_pd = testwhat:::build_pd(STU_CODE),
    student_env = stu_env,
    solution_code = SOL_CODE,
    solution_pd = testwhat:::build_pd(SOL_CODE),
    solution_env = sol_env,
    output_list = list(),
    test_env = new.env(parent=globalenv())
  )

  # testwhat will access the reporter and state from the tw object
  rep <- testwhat:::DC_reporter$new()
  tw$set(state = state, reporter = rep, stack = TRUE)
}

show_sct_error <- function() {
  feedback <- testwhat:::get_rep()$get_feedback()
  if(length(feedback) > 0L) {
    payload <- testwhat:::generate_payload(
      feedback, correct = FALSE, ex_type = 'NormalExercise'
    )
    stop(payload$message, call. = FALSE)
  }
}
run_sct <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      show_sct_error()
    }
  )
}

## ------------------------------------------------------------------------
library(testwhat)

## ------------------------------------------------------------------------
primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)

## ---- eval = FALSE-------------------------------------------------------
#  test_object("primes")

## ---- results = "hide"---------------------------------------------------
setup_state(
  SOL_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)",
  STU_CODE = "primes <- setNames(c(2, 3, 5, 7, 11), c('two', 'three', 'five', 'seven', 'eleven'))"
)
run_sct(
  test_object("primes")
)

## ------------------------------------------------------------------------
setup_state(
  SOL_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)",
  STU_CODE = "PRIMES <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)"
)
run_sct(
  test_object("primes")
)

## ------------------------------------------------------------------------
setup_state(
  SOL_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)",
  STU_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 13)"
)
run_sct(
  test_object("primes")
)

## ---- eval = FALSE-------------------------------------------------------
#  test_object("primes", eq_condition = "equal")

## ------------------------------------------------------------------------
setup_state(
  SOL_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, eleven = 11)",
  STU_CODE = "primes <- c(two = 2, three = 3, five = 5, seven = 7, twelve = 11)"
)
run_sct(
  test_object("primes", eq_condition = "equal")
)

