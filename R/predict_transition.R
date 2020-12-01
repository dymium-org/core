#' Predict a transition
#'
#'
#' @template param_world
#' @template param_entity
#' @template param_model
#' @template param_target
#' @template param_ids
#'
#' @return [predict_transition] returns choices or results, whereas [predict_transition_prob]
#'  returns choice probabilities.
#' @export
#'
#' @examples
#'
#'
predict_transition = function(entity, model, target, world, ids) {
  predict_transition_prob(entity, model, world, ids)
  if (!is.missing(target)) {
    align_prediction
  }
}

predict_transition_prob = function(entity, model, world, ids) {

}

predict_transition_raw = function(entity, model, world, ids) {

}
