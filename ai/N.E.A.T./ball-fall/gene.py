import config
import numpy as np
from random import uniform


class Node:
    Nodes = []

    def __init__(self, innovation: int, layer: str):
        if innovation not in Node.Nodes:
            Node.Nodes.append(innovation)
        self.innovation = innovation
        self.layer = layer
        self.bias = uniform(-2.0, 2.0)
        self.results = []


    ''' Feed forward. '''
    def reset(self):
        self.cooling = False

    def fire(self):
        result = self.activate(sum(self.results, self.bias))
        self.results.clear()
        self.cooling = True
        return result

    def ready(self):
        return len(self.results) >= self.recieving and not self.cooling

    def feed(self, result):
        self.results.append(result)

    def mutate(self):
        if uniform(0, 1) < config.chance_mutate_weight_adjust:
            self.bias += np.random.normal(-1.0, 1.0)
        else:
            self.bias = uniform(-2.0, 2.0)


    ''' Activation functions. '''
    def sigmoid(self, X):
        return 1 / (1 + np.exp(-X))

    def tanh(self, X):
        return np.tanh(X)

    def relu(self, X):
        return X * (X > 0)


    ''' Dunders. '''
    def __eq__(self, node):
        return self.innovation == node.innovation

    def __hash__(self):
        return self.innovation

    def __str__(self):
        return f'{self.innovation: >4}'


class InputNode(Node):
    def __init__(self, innovation):
        super().__init__(innovation, 'input')

    def ready(self):
        return True

    def fire(self):
        return self.result

class HiddenNode(Node):
    def __init__(self, innovation):
        super().__init__(innovation, 'hidden')
        self.activate = self.relu

class OutputNode(Node):
    def __init__(self, innovation):
        super().__init__(innovation, 'output')
        self.activate = self.sigmoid

    def activate(self, X):
        return self.sigmoid(X)

    def feed(self, result):
        self.results.append(result)
        if self.ready():
            self.result = self.activate(sum(self.results, self.bias))


class Gene:
    Genes = {}
    gene_counter = 0
    node_counter = 0

    def __init__(self, link: tuple, weight: float=None):
        self.out = link[1]
        self.active = True
        self.innovation = Gene.Genes.get(frozenset(link)) or self.new_link(link)
        self.weight = weight if weight else uniform(-2.0, 2.0)

    def new_link(self, link):
        ''' Keep track of new links. '''
        Gene.gene_counter += 1
        Gene.Genes[frozenset(link)] = Gene.gene_counter
        return Gene.gene_counter

    def randomize(self):
        self.active = uniform(0, 1) > config.chance_inherit_disabled_gene

    def mutate(self):
        self.out.mutate()
        if uniform(0, 1) < config.chance_mutate_weight_adjust:
            self.weight += np.random.normal()
        else:
            self.weight = uniform(-2.0, 2.0)

    def clone(self):
        return deepcopy(self)

    ''' Dunders. '''
    def __hash__(self):
        return self.out.innovation

    def __eq__(self, gene):
        return self.innovation == gene.innovation

    def __str__(self):
        active = 'ON' if self.active else 'OFF'
        return '' + \
        f'{self.out.innovation: <4}|' + \
        f'{self.weight: 9.4f}   |' + \
        f'{self.innovation: ^12}|' + \
        f'{active: ^8}|'

    ''' Class methods. '''
    @classmethod
    def new_node(cls, link):
        ''' Keep track of new nodes. '''
        keys = tuple(cls.Genes.keys())
        for (i, gene1) in enumerate(keys):
            for gene2 in keys[i+1:]:
                if gene1 ^ gene2 >= set(link):
                    if gene1 & gene2:
                        return next(iter(gene1 & gene2))

        Gene.node_counter += 1
        return HiddenNode(Gene.node_counter-1)

    @classmethod
    def reset(cls):
        cls.Genes.clear()




