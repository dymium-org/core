IND <- list()
IND$MARITAL_STATUS$MARRIED <- "married"
IND$MARITAL_STATUS$DIVORCED <- "divorced"
IND$MARITAL_STATUS$SEPARATED <- "separated"
IND$MARITAL_STATUS$NEVER_MARRIED <- "never married"
IND$MARITAL_STATUS$NOT_APPLICABLE <- "not applicable"
IND$MARITAL_STATUS$WIDOWED <- "widowed"
IND$MARITAL_STATUS$DE_FACTO <- "de facto"
IND$SEX$MALE <- "male"
IND$SEX$FEMALE <- "female"
IND$ID_COLS <- c("partner_id", "father_id", "mother_id")

EVENT <- list()
EVENT$LEFT_HOUSEHOLD <- "LEFT_HOUSEHOLD"
EVENT$JOINED_HOUSEHOLD <- "JOINED_HOUSEHOLD"
EVENT$JOINED_EXISTING_HOUSEHOLD <- "JOINED_EXISTING_HOUSEHOLD"
EVENT$JOIN_PARTNER_HOUSEHOLD <- "JOIN_PARTNER_HOUSEHOLD"
EVENT$JOINED_PARTNER_HOUSEHOLD <- "JOINED_PARTNER_HOUSEHOLD"
EVENT$CREATE_NEW_HOUSEHOLD <- "CREATE_NEW_HOUSEHOLD"
EVENT$MOVED_WITH_PARENT <- "MOVED_WITH_PARENT"

AGENTS <-
  c(
    "individual",
    "household",
    "firm",
    "building",
    "residential_building",
    "commercial_building",
    "industrial_building",
    "transport_nodes",
    "transport_links",
    "zone"
  )
