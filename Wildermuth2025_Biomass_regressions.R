## Code by Benjamin Wildermuth
## Part of the analysis for the paper:
## Every species counts: Arthropod species loss, but not their identity, underpins biomass declines
## Nature Ecology & Evolution

############### taxon-specific regressions used to calculate individual biomass from body length ###################
#               Regressions are derived from the appendix of: Sohlström, E. H. et al.                              #
#    Applying generalized allometric regressions to predict live body mass of tropical and temperate arthropods.   #
#                       Ecology and Evolution 8, 12737–12749 (2018)                                                #
############### example structure! Own data have to be inserted ####################################################

# for Orthoptera
dat[dat$Order=="Orthoptera",]$biomass <- 10^(-0.640 + 2.267 * log10(dat[dat$Order=="Orthoptera",]$length))

# for spiders (Dictynidae, Linyphiidae)
dat[dat$Order=="Araneae",]$biomass <- 10^(-0.733 + 2.623 * log10(dat[dat$Order=="Araneae",]$length))
head(dat)
# web-building spiders
dat[dat$Family %in% c("Agelenidae", "Araneidae",
                      "Hahniidae", "Nesticidae",
                      "Pholcidae", "Pisauridae",
                      "Tetragnathidae", "Theridiidae", 
                      "Theridiosomatidae"),]$biomass <- 10^(-0.777 + 2.693 * log10(dat[dat$Family %in% c("Agelenidae", "Araneidae", 
                                                                                                          "Hahniidae", "Nesticidae", 
                                                                                                          "Pholcidae", "Pisauridae", 
                                                                                                          "Tetragnathidae", "Theridiidae", 
                                                                                                          "Theridiosomatidae"),]$length))
# hunting spiders
dat[dat$Family %in% c("Anyphaenidae", "Cheiracanthiidae",
                      "Clubionidae", "Dysderidae",
                      "Gnaphosidae", "Liocranidae",
                      "Liocranidae", "Mimetidae", 
                      "Miturgidae", "Philodromidae",
                      "Phrurolithidae", "Salticidae",
                      "Thomisidae", "Zodariidae"),]$biomass <- 10^(-0.501 + 2.256 * log10(dat[dat$Family %in% c("Anyphaenidae", "Cheiracanthiidae",
                                                                                                                 "Clubionidae", "Dysderidae",
                                                                                                                 "Gnaphosidae", "Liocranidae",
                                                                                                                 "Liocranidae", "Mimetidae", 
                                                                                                                 "Miturgidae", "Philodromidae",
                                                                                                                 "Phrurolithidae", "Salticidae",
                                                                                                                 "Thomisidae", "Zodariidae"),]$length))

# for beetles
dat[dat$Order=="Coleoptera",]$biomass <- 10^(-0.938 + 2.501 * log10(dat[dat$Order=="Coleoptera",]$length))
# Staphylinidae
dat[dat$Family=="Staphylinidae",]$biomass <- 10^(-1.578 + 2.635 * log10(dat[dat$Family=="Staphylinidae",]$length))

# for Hemiptera (cicads, Aphids) 
dat[dat$Order=="Hemiptera",]$biomass <- 10^(-0.902 + 2.386 * log10(dat[dat$Order=="Hemiptera",]$length))
# Heteroptera
dat[dat$Family %in% c("Alydidae", "Anthocoridae",
                      "Berytidae", "Coreidae",
                      "Corixidae", "Cymidae",
                      "Gerridae", "Heterogastridae",
                      "Lygaeidae", "Miridae",
                      "Nabidae", "Notonectidae",
                      "Oxycarenidae", "Pentatomidae",
                      "Piesmatidae", "Pyrrhocoridae",
                      "Rhopalidae", "Rhyparochromidae",
                      "Saldidae", "Scutelleridae",
                      "Tingidae"),]$biomass <- 10^(-0.906 + 2.373 * log10(dat[dat$Family %in% c("Alydidae", "Anthocoridae",
                                                                                                 "Berytidae", "Coreidae",
                                                                                                 "Corixidae", "Cymidae",
                                                                                                 "Gerridae", "Heterogastridae",
                                                                                                 "Lygaeidae", "Miridae",
                                                                                                 "Nabidae", "Notonectidae",
                                                                                                 "Oxycarenidae", "Pentatomidae",
                                                                                                 "Piesmatidae", "Pyrrhocoridae",
                                                                                                 "Rhopalidae", "Rhyparochromidae",
                                                                                                 "Saldidae", "Scutelleridae",
                                                                                                 "Tingidae"),]$length))

# for Hymenoptera
dat[dat$Order=="Hymenoptera",]$biomass <- 10^(-1.486 + 3.018 * log10(dat[dat$Order=="Hymenoptera",]$length))
# Hymenoptera 1
dat[dat$Family %in% c("Formicidae", "Dryinidae",
                      "Muttilidae", "Embolemidae"),]$biomass <- 10^(-1.309 + 2.744 * log10(dat[dat$Family %in% c("Formicidae", "Dryinidae",
                                                                                                                  "Muttilidae", "Embolemidae"),]$length))
# Hymenoptera 2
dat[dat$Family %in% c("Apidae", "Colletidae",
                      "Megachilidae", "Pamphilidae",
                      "Scoliidae", "Sphecidae",
                      "Tenthredinidae", "Vespidae"),]$biomass <- 10^(-1.117 + 2.730 * log10(dat[dat$Family %in% c("Apidae", "Colletidae",
                                                                                                                   "Megachilidae", "Pamphilidae",
                                                                                                                   "Scoliidae", "Sphecidae",
                                                                                                                   "Tenthredinidae", "Vespidae"),]$length))
# Hymenoptera 3
dat[dat$Family %in% c("Ceraphronidae", "Chalcididae",
                      "Diapriidae", "Eulophidae",
                      "Eupelmidae", "Eurytomidae",
                      "Evaniidae", "Perilampidae",
                      "Proctotrupidae", "Proctotrupidae",
                      "Scelionidae"),]$biomass <- 10^(-1.530 + 2.929 * log10(dat[dat$Family %in% c("Ceraphronidae", "Chalcididae",
                                                                                                    "Diapriidae", "Eulophidae",
                                                                                                    "Eupelmidae", "Eurytomidae",
                                                                                                    "Evaniidae", "Perilampidae",
                                                                                                    "Proctotrupidae", "Proctotrupidae",
                                                                                                    "Scelionidae"),]$length))
# Hymenoptera 4
dat[dat$Family=="Ichneumonidae",]$biomass <- 10^(-1.197 + 2.379 * log10(dat[dat$Family=="Ichneumonidae",]$length))
# Hymenoptera 5
dat[dat$Family %in% c("Bethylidae", "Braconidae",
                      "Gasteruptiidae", "Tiphiidae"),]$biomass <- 10^(-1.345 + 2.771 * log10(dat[dat$Family %in% c("Bethylidae", "Braconidae",
                                                                                                                    "Gasteruptiidae", "Tiphiidae"),]$length))