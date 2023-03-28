''' 
Create a sophisticated neural network using NEAT or
Neuroevolution of Augmenting Topologies
''' 
import numpy as np

from config import *
from neuralnetwork import NeuralNetwork
from ball_fall import Game


'''
Globals
'''
game = Game(POPULATION)


'''
NEAT
'''
class NEAT:
	''' Main class. '''
	def __init__(self, inputs, outputs):
		# create generations
		self.generations = []
		topology = [inputs, 2, 2, outputs]
		self.generations.append(
			[NeuralNetwork(topology) for _ in range(POPULATION)]
		)

	def start(self):
		for generation_index, current_generation in enumerate(self.generations):
			print(f'GENERATION: {generation_index}')

			self.fitness(current_generation)
			self.selection(current_generation)
			self.replication()

	def fitness(self, current_generation):
		''' Calculate fitness based on results. '''

		# get current generation of players in new game
		self.balls = game.new_game(current_generation)
		self.ball_times = sorted(self.balls, key=lambda ball: ball.time, reverse=True)

		# sort players by time survived
		self.ranked_balls = sorted(self.balls, key=lambda ball: ball.time, reverse=True)

	def selection(self, current_generation):
		''' Select most fit of current generation. '''

		# select top half of current generation
		self.top_balls = self.ranked_balls[:len(current_generation) // 2]

		print(f'Best times: {[round(ball.time, 1) for ball in self.ball_times[:5]]}')
		print(f'Worst time: {round(self.ball_times[-1].time, 1)}')
		print()

	def replication(self):
		''' Replicate top players, apply mutations to select organisms. '''

		new_generation = []

		for ball in self.top_balls[:4]:
			topology, genes = ball.neural_network.copy_genes()
			new_network = NeuralNetwork(topology, genes)
			new_generation.append(new_network)

		for ball in self.top_balls[:-2]:
			progeny = []
			for _ in range(2):
				topology, genes = ball.neural_network.copy_genes()
				new_network = NeuralNetwork(topology, genes)
				progeny.append(new_network)

			new_generation.extend(progeny)

		# append next generation to test
		self.generations.append(new_generation)


if __name__ == '__main__':
	
	INPUTS = 4
	OUTPUTS = 2

	neat = NEAT(INPUTS, OUTPUTS)
	neat.start()
