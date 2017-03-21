source("ACON/1SQL.R")
source("ACON/2Datamanipulation.R")
source("ACON/3Filtering.R")
source("ACON/3functions.R")

clientsquarterchart <- yearqtrplot(clientsquarter,
                                   "n",
                                   "ACON Clients per Quarter",
                                   "Unique Clients (#)")
HIVTestsMSMquarterchart <- yearqtrplot(HIVTestsMSMquarter,
                                        "n",
                                        "ACON HIV Tests per Quarter (MSM)",
                                        "HIV Tests (#)")
