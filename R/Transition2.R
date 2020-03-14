Transition2 <- R6::R6Class(
  classname = "Transition2",
  public = list(
    initialize = function(Ent,
                          model,
                          target = NULL,
                          preProcess_fns = NULL) {
      # check inputs
      assert_entity(Ent)
      assert_transition_supported_model(x = model)
      assert_target(target, null.ok = TRUE)
      checkmate::assert_function(preProcess_fns, null.ok = TRUE)

      # store inputs
      private$.Ent <- Ent
      private$.model <- model
      private$.target <- target
      private$.preProcess_fns <- preProcess_fns

      # preprocess entity data, this include any mutations and filtration
      if (!is.null(private$.preProcess_fns)) {
        private$.Ent_data <- private$.preProcess_fns(private$.Ent$get_data())
      } else {
        private$.Ent_data <- private$.Ent$get_data()
      }

      # use the data for simulation
      private$.result <- microsimulate(private$.Ent_data, model, target)

      invisible()
    },

    update_entity = function(attr) {

    }
  ),

  active = list(
    Ent = function() {
      get(".Ent", envir = private)
    },
    Ent_data = function() {
      get(".Ent_data", envir = private)
    },
    model = function() {
      get(".model", envir = private)
    },
    target = function() {
      get(".target", envir = private)
    },
    result = function() {
      get(".result", envir = private)
    }
  ),

  private = list(
    .Ent = NULL,
    .model = NULL,
    .target = NULL,
    .preProcess_fns = NULL,
    .Ent_data = NULL,
    .result = NULL
  )
)

