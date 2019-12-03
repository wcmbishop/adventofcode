# Intcode Object Class
# source: https://adventofcode.com/2019/day/2


Intcode <- R6::R6Class(
  "Intcode", 
  private = list(
    .pointer = NA_integer_,
    .initial_state = NA_character_,
    .memory = integer(),
    .parse_state = function(state) {
      stringr::str_split(state, ",")[[1]] %>% as.numeric() 
    }
  ),
  active = list(
    pointer = function(value) {
      if (missing(value)) {
        private$.pointer
      } else {
        stop("`$pointer` is read only", call. = FALSE)
      }
    },
    initial_state = function(value) {
      if (missing(value)) {
        private$.initial_state
      } else {
        stop("`$initial_state` is read only", call. = FALSE)
      }
    },
    memory = function(value) {
      if (missing(value)) {
        private$.memory
      } else {
        stop("`$.memory` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    initialize = function(state, pointer = 0L) {
      require(stringr)
      require(rlang)
      stopifnot(is.integer(pointer), length(pointer) == 1)
      stopifnot(is.character(state), length(state) == 1)
      private$.initial_state <- state
      private$.pointer <- pointer
      private$.memory <- private$.parse_state(state)
    },
    print = function(...) {
      cat("Intcode: \n")
      cat("  memory length: ", length(private$.memory), "\n", sep = "")
      cat("  instruction pointer:  ", private$.pointer, "\n", sep = "")
      invisible(self)
    },
    validate_address = function(address) {
      stopifnot(is.numeric(address), length(address) == 1)
    },
    get = function(address) {
      self$validate_address(address)
      private$.memory[address + 1]
    },
    set = function(address, value) {
      self$validate_address(address)
      private$.memory[address + 1] <- value
      invisible(self)
    },
    get_ref = function(address) {
      self$validate_address(address)
      private$.memory[private$.memory[address + 1] + 1]
    },
    set_ref = function(address, value) {
      self$validate_address(address)
      private$.memory[private$.memory[address + 1] + 1] <- value
      invisible(self)
    },
    run_instruction = function(op) {
      output_val <- {{op}}(self$get_ref(private$.pointer + 1),
                           self$get_ref(private$.pointer + 2))
      self$set_ref(private$.pointer + 3, output_val)
      private$.pointer <- private$.pointer + 4
      invisible(self)
    },
    run = function() {
      opcode <- self$get(private$.pointer)
      if (opcode == 1) {
        self$run_instruction(op = `+`)
        self$run()
      } else if (opcode == 2) {
        self$run_instruction(op = `*`)
        self$run()
      } else if (opcode == 99) {
        message("processing completed.")
      } else {
        stop(glue::glue(
          "unexpected opcode: '{value}' at address {address}",
          value = self$get(private$.pointer),
          address = private$.pointer)
        )
      }
      invisible(self)
    },
    reset = function() {
      private$.memory <- private$.parse_state(private$.initial_state)
      private$.pointer <- 0
      message("memory reset to initial state.")
      invisible(self)
    }
  )
)
