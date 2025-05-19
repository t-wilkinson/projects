from population import Population

from matplotlib import pyplot as plt
import numpy as np


def graph(organism):
    width, height = 50, 50

    data = np.zeros((height, width, 3), dtype=np.uint8)
    for x in np.linspace(0, 1, width, endpoint=False):
        for y in np.linspace(0, 1, height, endpoint=False):
            output, = organism.feed([x, y])
            data[int(x*width), int(y*width)] = [output, output, output]

    plt.imshow(data, interpolation='nearest')
    plt.show()


def simulation(organism):
    organism.fitness = len(organism)
    return

    # result, = organism.feed([1, 1])
    # graph(organism)
    # try:
    #     result = 1 / result
    # except ArithmeticError:
    #     organism.fitness = 100
    # else:
    #     organism.fitness = result


p = Population(2, 1, simulation)
for species in p:
    species.sort(reverse=True)
    species[0].show()
