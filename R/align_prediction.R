align_prediction = function(probs, target) {
  checkmate::assert_data_frame(probs, col.names = "unique")
  assert_target(target)
}
