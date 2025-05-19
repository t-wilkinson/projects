import config
from random import random


class _Node(list):
    '''
    Each node has an activation function
    and list of connected links.
    '''

    def __init__(self, innovation: int, layer: int):
        super().__init__()
        self.result = 0
        self.results = []
        self.layer = layer
        self.innovation = innovation

    def __eq__(self, node: object):
        return self.innovation == node.innovation

    def __hash__(self):
        return self.innovation

    def link(self, node: object, weight: float = None):
        link = config.Link(self, node, weight)
        self.append(link)

    def fire(self):
        self.result = self.activation(sum(self.results))

    def feed(self):
        for link in self:
            if link.active:
                link.node.results.append(link.weight * self.result)


class cppn(_Node):
    def __init__(self, layer, *args):
        super().__init__(layer, *args)
        if layer == 1:
            self.activation = config.Activation['tanh']
        else:
            self.activation = config.Activation()

    def mutate(self):
        if self.layer != 1 and random() < config.chance_mutate_node:
            self.activation = config.Activation()
        for link in self:
            link.mutate()


class neat(_Node):
    def __init__(self, *args):
        super().__init__(*args)
        self.activation = config.Activation['sigmoid']

    def mutate(self):
        for link in self:
            link.mutate()


class Node:
    '''
    Handles node creation.
    '''

    def __init__(self):
        self.count = 1

    def __call__(self, layer: int) -> object:
        node = globals()[config.node_type](self.count, layer)
        self.count += 1
        return node
