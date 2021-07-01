# Data Preparation Brazil
# Henrique Sposito

# For Brazil, at the time being, Cesar 2020 dataset on oral remarks is being used.
# The data is scraped from the presidential library and covers all official presidential speeches
# and remarks from January 1985 to July 2020.
load("~/GitHub/Poldis/data/BR_oral.rda")
summary(BR_oral)
# Should I start from 1990 here because of direct elections?

# Please cite:
# Fagundes Cezar, Rodrigo, 2020, "Brazilian Presidential Speeches from 1985 to July 2020",
# https://doi.org/10.7910/DVN/M9UU09, Harvard Dataverse, V1, DEACCESSIONED VERSION

# For debates, campaign remarks and interviews cleaning for speakers and time range was done at collection portion.
# For more details about collection, please see the Codebook.
