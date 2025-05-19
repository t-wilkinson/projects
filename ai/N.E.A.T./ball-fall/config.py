'''
Globals
'''
POPULATION = 50	# populaiton of each species
GENERATIONS = 50

# Mutation
chance_mutate_weight = 0.8
chance_mutate_weight_adjust = 0.9
chance_mutate_weight_randomize = 0.1

species_big_size = 100
species_size_to_keep = 0.5
species_size_threshold = 5
species_stagnations_allowed = 15

chance_inherit_disabled_gene = 0.75
chance_mutate_no_crossover = 0.25
chance_interspecies_mate = 0.001
chance_new_node = 0.03
chance_new_link_big_species = 0.3
chance_new_link = 0.05

# Compatability distance coefficients
C1 = 1.0
C2 = 1.0
C3 = 0.4
compatability_distance_threshold = 3.0

