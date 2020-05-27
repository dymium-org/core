ModelMlogit <- R6::R6Class(
  classname = "ModelMlogit",
  inherit = Model,
  public = list(
    simulate = function(world = NULL, chooser_data, alternative_data, chooser_id_col, alternative_id_col, chosen_col, n_choices = 30) {
      checkmate::assert_r6(world, classes = "World", null.ok = TRUE)
      checkmate::assert_r6(self, classes = "Model")
      checkmate::assert_r6(private, classes = "Model")
      checkmate::assert_data_table(chooser_data)
      checkmate::assert_data_table(alternative_data)
      checkmate::assert_names(names(chooser_data), must.include = c(chooser_id_col, chosen_col))
      checkmate::assert_names(names(alternative_data), must.include = alternative_id_col)
      checkmate::assert_count(n_choices, positive = T, null.ok = FALSE, na.ok = FALSE)
    }
  )
)




# }


