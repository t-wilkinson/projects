'''
Define global configurations
'''
import numpy as np


'''
Globals
'''
# data size
GENERATIONS = 100
POPULATION = 100

# coefficients for determining compatability
C1 = 1.0
C2 = 1.0
C3 = 0.4

# mutation rates
weight_connection_mutatation_chance = 0.8
weight_adjustment_chance = 0.9
weight_random_chance = 0.1

inherit_disabled_gene_chance = 0.75
mutation_no_crossover_chance = 0.25

interspecies_mating_chance = 0.001

crossover_chance = 0.10
new_gene_chance = 0.05
new_node_chance = 0.03

## COMPATABILITY_THRESHOLD
## ACTIVATION_THRESHOLD = 0.5
## DELTA_THRESHOLD = 3.0

# sigma funciton
sigma = lambda x: 1 / (1 + np.exp(-4.9 * x))

# gnome node dtype
node_dtype = [
	('Node', int), 
	('Type', str)
]

# gnome connection node dtype
connection_dtype = [
	('In', int), 
	('Out', int), 
	('Weight', np.float_), 
	('Enabled', bool), 
	('Innovation', int)
]

