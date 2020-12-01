ModelWrapper =
  R6::R6Class(
    classname = "ModelWrapper",
    inherit = Model,
    public = list(),
    active = list()
  )



get_prediction = function() {}

# ModelTidymodel =
#   R6::R6Class(
#     classname = "ModelTidymodel",
#     inherit = ModelWrapper,
#     public = list(
#       predict = function() {
#
#       }
#     ))
#
# ModelGlm = R6::R6Class(classname = "ModelGlm", inherit = ModelWrapper)
# ModelCaret = R6::R6Class(classname = "ModelCaret", inherit = ModelWrapper)
# ModelMlr = R6::R6Class(classname = "ModelMlr3", inherit = ModelWrapper)
# ModelMlr3 = R6::R6Class(classname = "ModelMlr3", inherit = ModelWrapper)
# ModelMlogit = R6::R6Class(classname = "ModelMlogit", inherit = ModelWrapper)
