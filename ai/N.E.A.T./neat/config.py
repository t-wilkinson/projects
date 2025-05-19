'''
Provides easy access to commonly used functions.
Initializes constants.
'''
import node
import link
import activations

Node = node.Node()
Link = link.Link()
Activation = activations.Activation()


# ---------------- #
# Hyper Paramaters #
# ---------------- #

# Sizes
population = 100
generations = 30

# Node type
node_type = 'cppn'

# Weight Mutations
weight_mutation_strength = 2.5
weight_max_size = 8.0

# Mutation Chance
chance_inherit_disabled_gene = 0.75
chance_mutate_weight = 0.8
chance_mutate_weight_adjust = 0.9
chance_mutate_weight_randomize = 0.1
chance_mutate_no_crossover = 0.25

# Structure Mutations
chance_mutate_node = 0.03
chance_new_node = 0.03
chance_new_link_big_species = 0.3
chance_new_link_small_species = 0.05

# Crossover
chance_crossover_one_point = 0.25
chance_crossover_two_point = 0.25
chance_crossover_uniform = 0.5
chance_mate_interspecies = 0.001

# Species
species_target_size = 10
species_size_cutoff = 100
species_size_survival = 0.2
species_size_threshold = 5
species_stagnations_allowed = 15

# Compatability Constants
coefficient_disjoint = 2.0
coefficient_excess = 2.0
coefficient_weight = 1.0
compatability_threshold = 6.0
compatability_modifier = 0.3

# Feed Forward
weight_min = 0.2
weight_max = 3.0
