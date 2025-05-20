import config
from random import random
from numpy.random import normal


class _Link:
    '''
    Each link has a weight a node which is denoted a linkage.
    '''
    def __init__(self, node: object, weight: float):
        self.active = True
        self.node = node
        self.weight = weight or normal(0, config.weight_mutation_strength)

    def __hash__(self):
        return self.node.innovation

    def __eq__(self, link):
        return self.innovation == link.innovation

    def active(self):
        return self.active

    def disable(self):
        self.active = False

    def mutate(self):
        if random() < config.chance_mutate_weight_adjust:
            self.weight += normal()
            self.weight = max(
                -config.weight_max_size,
                min(config.weight_max_size, self.weight))
        else:
            self.weight = normal(0, config.weight_mutation_strength)


class Link(dict):
    '''
    Handles link creation.new_node
    '''
    def __init__(self):
        super().__init__()
        self.count = 0

    def __call__(self, in_node: object, node: object, weight: float) -> object:
        link = _Link(node, weight)
        connection = frozenset((in_node, node))
        link.innovation = self.get(connection) or self.new_link(connection)
        return link

    def new_link(self, connection: frozenset) -> int:
        self[connection] = self.count
        self.count += 1
        return self.count - 1

    def next_node(cls, connection: object, layer: int):
        for (i, gene1) in enumerate(cls):
            for gene2 in tuple(cls)[i + 1:]:
                if gene1 ^ gene2 >= set(connection):
                    if gene1 & gene2:
                        return next(iter(gene1 & gene2))
        return config.Node(layer)
