
library(ape)
library(phytools)
library(lme4)
library(nlme)
library(tidyverse)


# The phylogenetic tree (as downloaded and used in the project "SporeSizeFungalKingdom")

Li_tree <- read.tree("Li_etal_tree.treefile")

# The functional data associated to the tree
# Species_in_Li_Tree4_added.csv is a copy of a file with the same name created in the project "SporeSizeFungalKingdom", there in the
# script "li_tree_update_funct&spore.R" it is explained how it was assembled

Li_tree_functions <- read.csv("Species_in_Li_Tree4_added.csv")
Li_tree_functions <- unique(Li_tree_functions[, c("orginal_tree_names", 
                                           "trophicMode", "guild",
                                           "simpleFunct")])
Li_tree_functions$guild <- gsub(" ", "_", Li_tree_functions$guild)
Li_tree_functions$orginal_tree_names <- gsub(" ", "_", Li_tree_functions$orginal_tree_names)

# At this stage there are 683 species without function
length(which(is.na(Li_tree_functions$trophicMode)))


# We are aiming at fewer states based on the network analysis of all guilds (Guild_Data). In that one, the following groups came out:

# 1. saprotrophs-cold_blooded_animals
# 2. wood_Saprotrophs-warm_blooded_animals
# 3. mycorrhizal-endophyte
# 4. lichen-microbe
# 5. plant_pathogens

# 1. saprotrophs-cold_blooded_animals
Li_tree_functions$saprotrophs__cold_blooded_animals <- 0
Li_tree_functions$saprotrophs__cold_blooded_animals[grep("undefined_saprotroph|insect|amphibian|fish|soil",
                                                         Li_tree_functions$guild,
                                                         ignore.case = T)] <- 1
Li_tree_functions$saprotrophs__cold_blooded_animals[which(Li_tree_functions$guild == "Saprotroph")] <- 1


#2. wood_Saprotrophs-warm_blooded_animals
Li_tree_functions$wood_saprotrophs__warm_blooded_animals <- 0
Li_tree_functions$wood_saprotrophs__warm_blooded_animals[grep("dung|human|mammal|wood|nematode|epiphyte",
                                                         Li_tree_functions$guild,
                                                         ignore.case = T)] <- 1
#3. mycorrhizal-endophate
Li_tree_functions$mycorrhizal__endophyte <- 0
Li_tree_functions$mycorrhizal__endophyte[grep("mycorrhizal|endophyte|mycorrizal",
                                                              Li_tree_functions$guild,
                                                              ignore.case = T)] <- 1
#4. Lichen_microb
Li_tree_functions$Lichen__microbe <- 0
Li_tree_functions$Lichen__microbe[grep("lichen|fung|protist",
                                              Li_tree_functions$guild,
                                              ignore.case = T)] <- 1
#5. plant_pathogen
Li_tree_functions$plant_pathogen <- 0
Li_tree_functions$plant_pathogen[grep("plant_pathogen",
                                       Li_tree_functions$guild,
                                       ignore.case = T)] <- 1

# Checking which fungi do not have funcitional data

m <-
which(rowSums(Li_tree_functions[, c("saprotrophs__cold_blooded_animals",
                                    "wood_saprotrophs__warm_blooded_animals",
                                    "mycorrhizal__endophyte",          
                                 "Lichen__microbe","plant_pathogen") ]) == 0)

length(m) # 709 species do not have data

length(which(is.na(Li_tree_functions$guild[m]))) # 663 and most of those is because we do not have
# guild data reported in the first place

# Meaning only 26 have guild data but the guilds cannot be classified under the system above


# As a quick and dirty way of starting the analysis, we concentrate first on the species for which
# a) we have data and b) we the classification works

Li_tree_functions <- Li_tree_functions[-m, ]

pruned.tree <- drop.tip(Li_tree,
                 Li_tree$tip.label[!Li_tree$tip.label%in%Li_tree_functions$orginal_tree_names])

# Thus, for 967 species we have phylogenetic data and functional guild data that matches


