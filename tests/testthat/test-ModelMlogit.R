test_that("multiplication works", {
  if (require("mlogit")) {

    toy_individual_mlogit_data <-
      data.table::CJ(pid = toy_individuals[["pid"]],
                     choice_var = toy_individuals[, unique(marital_status)]) %>%
      merge(., toy_individuals, by = "pid") %>%
      .[, chosen := ifelse(choice_var == marital_status, TRUE, FALSE)] %>%
      mlogit::mlogit.data(
        data = .,
        choice = "chosen",
        shape = "long",
        id.var = "pid",
        alt.var = "choice_var"
      )

    m <- mlogit::mlogit(chosen ~ 0 | sex + age,  data = toy_individual_mlogit_data)
    checkmate::expect_matrix(predict(m, toy_individual_mlogit_data))
    mnl <- ModelMlogit$new(m)
    mnl$simulate()


    #' an equipvalent model in nnet
    # library("nnet")
    # mnn <- multinom(marital_status ~ age + sex, data = toy_individuals)
    # predict(mnn, toy_individuals, type = "prob") %>% colMeans()

  }


})
