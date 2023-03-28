'''
Neural Network
used to create and modify neural networks
'''
import numpy as np
from sklearn.preprocessing import normalize


'''
Globals
'''
connection_weight_mutation_chance = 0.8
chance_of_each_weight_uniform_mutation = 0.9
chance_of_each_weight_randomly_mutated = 0.1
chance_disable_inherited_gene = 0.75
chance_mutation_no_crossover = 0.25
chance_interspecies_mating = 0.001
chance_new_node = 0.03


'''
Class
'''
class NeuralNetwork:

	def __init__(self, topology, genes=None):
		self.topology = topology
		# self.MAX_HIDDEN_LAYERS = np.amax(topology)
		self.MAX_HIDDEN_LAYERS = 20

		if genes is None:
			self._set_layer_dimensions_()
			self._set_matrices_()
			self._init_weights_()
		else:
			self.weight = genes[0]
			self.bias = genes[-1]
			self.mutate_genes()

	def mutate_genes(self):
		for layer_index, layer in enumerate(self.weight):
			for row_index, row in enumerate(layer):
				for weight_index, weight in enumerate(row):

					# determine whether weight is 0
					link_exists = bool(weight)
					if link_exists:
						link_mutation_chance = np.random.uniform()

						# whether to mutate links
						if link_mutation_chance <= connection_weight_mutation_chance:
							uniform_chance = np.random.uniform()

							# adjust weight or new random weight
							if uniform_chance <= chance_of_each_weight_uniform_mutation:
								weight += uniform_chance / 10
							else:
								weight += np.random.randn()
					# if there is no link
					else:
						if layer_index != len(self.topology)-1:
							new_node_chance = np.random.uniform()
							if new_node_chance <= chance_new_node:
								weight = 1
								try:
									self.weight[layer_index+1][row_index][weight_index] = np.random.random.uniform()
									print('new node')
								except:
									pass

	def _set_layer_dimensions_(self):
		''' Initialize dimensions of layers. '''
		self.in_size = self.topology[0]
		self.out_size = self.topology[-1]
		self.hidden = len(self.topology) > 2

	def _set_matrices_(self):
		''' Set weight and bias array sizes. '''
		self.weight = []
		self.bias = []

		len_total_layers = len(self.topology)

		for layer in range(len_total_layers):
			# Input layer
			if layer == 0:
				in_size = self.in_size
				out_size = self.MAX_HIDDEN_LAYERS

			# Output layer
			elif layer == len_total_layers-1:
				in_size = self.MAX_HIDDEN_LAYERS
				out_size = self.out_size

			# Hidden layer
			else:
				in_size = out_size = self.MAX_HIDDEN_LAYERS

			# create weights and biases
			self.weight.append(np.zeros((out_size, in_size)))
			self.bias.append(np.zeros(out_size))

	def _init_weights_(self):
		''' Initialize weights and biases. '''
		for index, matrix in enumerate(self.weight[:-1]):

			num_neurons_next_layer = self.topology[index + 1]

			for row_index, row in enumerate(matrix):
				if row_index < num_neurons_next_layer:
					matrix[row_index] = np.random.randn(len(row))
					self.bias[index][row_index] = 1

	def _activation_(self, X, size_output):
		''' Apply activation function. '''
		output = np.zeros(len(X))
		for index in range(size_output):
			output[index] = 1 / (1 + np.exp(-4.9 * X[index]))

		return output

	def feed_forward(self):
		X = np.asarray(self.X)
		for index, (matrix, b) in enumerate(zip(self.weight[:-1], self.bias)):

			size_output = self.topology[index + 1]

			if index == 0:

				X = normalize(X[:, np.newaxis], axis=0).ravel()
				dot_ = np.dot(matrix, X)

			else:
				dot_ = np.dot(matrix, output)

			output = self._activation_(dot_ + b, size_output)

		self.output = output[output != 0]

	def decision(self):
		return self.output >= 0.5

	def print_matrices(self):
		print('Matrices')
		for index, layer in enumerate(self.weight):
			print(f'Weight {index+1}')
			print(layer)
			print('\n')

		for bias in self.bias:
			print(f'Bias: {bias}')

	def copy_genes(self):
		weight = np.copy(self.weight)
		bias = np.copy(self.bias)

		return self.topology, [weight, bias]


if __name__ == '__main__':
	topology = [8, 5, 10, 1]
	net = NeuralNetwork(topology)

	net.X = input_x = [0, 1, 0, 1, 1, 0, 1, 1]
	net.feed_forward()

	print(f'\nTopology: {topology}')
	print(f'\nInput: {input_x}')
	print(f'\nOutput: {net.output}')
