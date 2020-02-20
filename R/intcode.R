# Intcode Object Class
# source: https://adventofcode.com/2019/day/2


Intcode <- R6::R6Class(
  "Intcode", 
  # private ====
  private = list(
    .pointer = NA_integer_,
    .inputs = NA_integer_,
    .input_pointer = NA_integer_,
    .output = NA_integer_,
    .outputs = NA_integer_,
    .is_complete = NA,
    .display = NA,
    .initial_state = NA_character_,
    .memory = integer(),
    .relative_base = NA_integer_,
    .parse_state = function(state) {
      stringr::str_split(state, ",")[[1]] %>% as.numeric() 
    },
    .fetch_opcode = function() {
      opvalue <- self$get(private$.pointer) %>% as.character()
      as.integer(stringr::str_sub(opvalue, -2L, -1L))
    },
    .fetch_modes = function(n) {
      opvalue <- self$get(private$.pointer) %>% as.character()
      mode_str <- str_sub(opvalue, 1L, -3L)
      modes <- rep(0, n)
      for (i in seq_len(nchar(mode_str))) {
        modes[i] <- as.integer(str_sub(mode_str, -i, -i))
      }
      modes
    },
    .move_pointer = function(modes) {
      private$.pointer <- private$.pointer + length(modes) + 1
    },
    .get_input = function() {
      private$.inputs[private$.input_pointer]
    },
    .move_input_pointer = function() {
      private$.input_pointer <- private$.input_pointer + 1
    }
  ),
  # active ====
  active = list(
    pointer = function(value) {
      if (missing(value)) {
        private$.pointer
      } else {
        stop("`$.pointer` is read only", call. = FALSE)
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
    },
    inputs = function(value) {
      if (missing(value)) {
        private$.inputs
      } else {
        stop("`$.inputs` is read only", call. = FALSE)
        # stopifnot(is.numeric(value))
        # private$.input_pointer <- 1
        # private$.inputs <- value
      }
    },
    output = function(value) {
      if (missing(value)) {
        private$.output
      } else {
        stop("`$.output` is read only", call. = FALSE)
      }
    },
    outputs = function(value) {
      if (missing(value)) {
        private$.outputs
      } else {
        stop("`$.outputs` is read only", call. = FALSE)
      }
    },
    is_complete = function(value) {
      if (missing(value)) {
        private$.is_complete
      } else {
        stop("`$.is_complete` is read only", call. = FALSE)
      }
    },
    display = function(value) {
      if (missing(value)) {
        private$.display
      } else {
        stopifnot(is.logical(value), length(value) == 1)
        private$.display <- value
      }
    }
  ),
  # public ====
  public = list(
    initialize = function(state, pointer = 0L, inputs = 0L, display = FALSE) {
      require(stringr)
      require(rlang)
      stopifnot(is.integer(pointer), length(pointer) == 1)
      stopifnot(is.numeric(inputs))
      stopifnot(is.character(state), length(state) == 1)
      stopifnot(is.logical(display), length(display) == 1)
      private$.initial_state <- state
      private$.pointer <- pointer
      private$.inputs <- inputs
      private$.input_pointer <- 1
      private$.is_complete <- FALSE
      private$.display <- display
      private$.memory <- private$.parse_state(state)
      private$.relative_base <- 0
    },
    reset = function() {
      private$.memory <- private$.parse_state(private$.initial_state)
      private$.pointer <- 0
      private$.input_pointer <- 1
      private$.output <- NA_integer_
      private$.outputs <- NA_integer_
      private$.is_complete <- FALSE
      private$.relative_base <- 0
      message("memory reset to initial state.")
      invisible(self)
    },
    reset_outputs = function() {
      private$.output <- NA_integer_
      private$.outputs <- NA_integer_
      invisible(self)
    },
    print = function(...) {
      cat("Intcode: \n")
      cat("  memory length: ", length(private$.memory), "\n", sep = "")
      cat("  run complete: ", private$.is_complete, "\n", sep = "")
      cat("  instruction pointer:  ", private$.pointer, "\n", sep = "")
      cat("  relative base:  ", private$.relative_base, "\n", sep = "")
      cat("  inputs: ", paste(private$.inputs, collapse = ","), "\n", sep = "")
      cat("  input pointer:  ", private$.input_pointer, "\n", sep = "")
      cat("  outputs: ", paste(private$.outputs, collapse = ","), "\n", sep = "")
      cat("  output: ", private$.output, "\n", sep = "")
      invisible(self)
    },
    add_inputs = function(inputs) {
      stopifnot(is.numeric(inputs))
      private$.inputs <- c(private$.inputs, inputs)
      invisible(self)
    },
    validate_address = function(address) {
      stopifnot(is.numeric(address), length(address) == 1)
    },
    get = function(address) {
      self$validate_address(address)
      val <- private$.memory[address + 1]
      ifelse(is.na(val), 0, val)
    },
    set = function(address, value) {
      self$validate_address(address)
      private$.memory[address + 1] <- value
      private$.memory[is.na(private$.memory)] <- 0
      invisible(self)
    },
    get_ref = function(address) {
      self$validate_address(address)
      self$get(self$get(address))
      # private$.memory[private$.memory[address + 1] + 1]
    },
    set_ref = function(address, value) {
      self$validate_address(address)
      self$set(self$get(address), value)
      # private$.memory[private$.memory[address + 1] + 1] <- value
      invisible(self)
    },
    get_by_mode = function(address, mode) {
      if (mode == 0) {
        value <- self$get_ref(address)  # position
      } else if (mode == 1) {
        value <- self$get(address)      # immediate
      } else if (mode == 2) {
        value <- self$get(private$.relative_base + self$get(address))  # relative
      } else {
        stop(paste("unexpected mode:", mode))
      }
      value
    },
    set_by_mode = function(address, value, mode) {
      if (mode == 0) {
        self$set_ref(address, value)  # position
      } else if (mode == 1) {
        self$set(address, value)      # immediate
      } else if (mode == 2) {
        self$set(private$.relative_base + self$get(address), value)  # relative
      } else {
        stop(paste("unexpected mode:", mode))
      }
      invisible(self)
    },
    step = function() {
      opcode <- private$.fetch_opcode()
      if (private$.display) {
        modes <- private$.fetch_modes(n = 1)
        message(glue::glue("pointer: {p}, op: {op}, mode: {mode}",
                           p = private$.pointer, op = opcode, mode = modes[1]))
        # message(paste("memory:", paste(private$.memory, collapse = ",")))
      }
      op <- switch(
        as.character(opcode),
        "1" = self$op1,
        "2" = self$op2,
        "3" = self$op3,
        "4" = self$op4,
        "5" = self$op5,
        "6" = self$op6,
        "7" = self$op7,
        "8" = self$op8,
        "9" = self$op9,
        "99" = self$op99
      )
      if (is.null(op)) {
        stop(glue::glue(
          "unexpected opcode {opcode}: '{value}' at address {address}",
          opcode = opcode,
          value = self$get(private$.pointer),
          address = private$.pointer)
        )
      }
      op()
      invisible(self)
    },
    run = function() {
      while (TRUE) {
        self$step()
        if (private$.is_complete)
          break
      }
      invisible(self)
    },
    op1 = function() {
      # add
      modes <- private$.fetch_modes(n = 3)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
      self$set_by_mode(private$.pointer + 3, p1 + p2, modes[3])
      private$.move_pointer(modes)
    },
    op2 = function() {
      # multiple
      modes <- private$.fetch_modes(n = 3)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
      self$set_by_mode(private$.pointer + 3, p1 * p2, modes[3])
      private$.move_pointer(modes)
    },
    op3 = function() {
      # set from input
      input <- private$.get_input()
      if (!is.na(input)) {
        modes <- private$.fetch_modes(n = 1)
        self$set_by_mode(private$.pointer + 1, private$.get_input(), modes[1])
        private$.move_pointer(modes)
        private$.move_input_pointer()
      } else {
        message("need input...")
      }
    },
    op4 = function() {
      # set output
      modes <- private$.fetch_modes(n = 1)
      private$.output <- self$get_by_mode(private$.pointer + 1, modes[1])
      private$.outputs <- c(private$.outputs, private$.output) %>% 
        na.omit() %>% as.numeric()
      private$.move_pointer(modes)
      message(private$.output)
    },
    op5 = function() {
      # jump-if-true
      modes <- private$.fetch_modes(n = 2)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      if (p1 != 0) {
        p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
        private$.pointer <- p2
      } else {
        private$.move_pointer(modes)
      }
    },
    op6 = function() {
      # jump-if-false
      modes <- private$.fetch_modes(n = 2)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      if (p1 == 0) {
        p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
        private$.pointer <- p2
      } else {
        private$.move_pointer(modes)
      }
    },
    op7 = function() {
      # less than
      modes <- private$.fetch_modes(n = 3)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
      self$set_by_mode(private$.pointer + 3, ifelse(p1 < p2, 1, 0), modes[3])
      private$.move_pointer(modes)
    },
    op8 = function() {
      # equals
      modes <- private$.fetch_modes(n = 3)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      p2 <- self$get_by_mode(private$.pointer + 2, modes[2])
      self$set_by_mode(private$.pointer + 3, ifelse(p1 == p2, 1, 0), modes[3])
      private$.move_pointer(modes)
    },
    op9 = function() {
      # update relative base
      modes <- private$.fetch_modes(n = 1)
      p1 <- self$get_by_mode(private$.pointer + 1, modes[1])
      private$.relative_base <- private$.relative_base + p1
      private$.move_pointer(modes)
    },
    op99 = function(run = FALSE) {
      private$.is_complete <- TRUE
      message("processing completed.")
    }
  )
)
