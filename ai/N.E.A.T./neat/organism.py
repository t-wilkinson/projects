import config
from copy import deepcopy
from math import copysign
from random import random, choice, sample
from itertools import product, chain, zip_longest
from matplotlib import pyplot as plt
from numpy.random import normal


class Organism(dict):
    '''
    Compositional Pattern Producing Network.
    Each organism owns a list of layers,
    each of which hold nodes.
    '''

# Dunders
    def __init__(self, inputs, outputs):
        super().__init__()
        self[1] = [config.Node(1) for _ in range(outputs)]
        self[0] = [config.Node(0) for _ in range(inputs+1)]
        for (out, node) in product(*self):
            node.link(out)

    def __missing__(self, key):
        self[key] = result = []
        return result

    def __getitem__(self, key):
        if isinstance(key, slice):
            return list(self.values())[key]
        value = self.get(key)
        if value is None:
            return self.__missing__(key)
        else:
            return super().__getitem__(key)

    def __iter__(self) -> iter:
        return iter(self.values())

    def __len__(self) -> int:
        return len(tuple(self.values()))

    def __lt__(self, organism: object) -> bool:
        return self.fitness < organism.fitness

    def __add__(self, organism: object) -> int:
        if isinstance(organism, (int, float)):
            return self.fitness + organism
        else:
            return self.fitness + organism.fitness
    __radd__ = __add__

    def __xor__(self, organism: object) -> object:
        unfitter, fitter = sorted([self, organism])
        kid = fitter.clone()
        matching = [
            (unfit, fit)
            for layers in zip(unfitter, kid)
            for nodes in zip(*layers)
            for unfit, fit in zip(*nodes)
            if unfit == fit
        ]

        for unfit, fit in self.crossover(matching):
            if not(fit.active or unfit.active):
                fit.active = random() > config.chance_inherit_disabled_gene
            fit.weight = unfit.weight

        return kid

# Helper Functions
    def sorted(self) -> list:
        return sorted(self, key=lambda layer: layer[0].layer)

    def links(self) -> iter:
        return chain.from_iterable(chain.from_iterable(self))

    def clone(self) -> object:
        return deepcopy(self)

    def kill(self):
        self.fitness = -1

    def randomize(self):
        for link in self.links():
            link.weight = normal()

# Feed Forward
    def feed(self, inputs: list) -> list:
        layers = self.sorted()
        for (node, value) in zip(layers[0], inputs):
            node.result = value
        layers[0][-1].result = 1

        for layer in layers:
            for node in layer:
                if node.layer != 0:
                    node.fire()
                node.feed()

        def scale(x):
            sign = copysign(1, x.result)
            x = abs(x.result)
            return (
                (x > config.weight_min) * sign * config.weight_max
                * (x - config.weight_min) / (1 - config.weight_min))
        return tuple(map(scale, self[1]))

# Mutation
    def mutate(self, chance_new_link: float):
        if random() < config.chance_mutate_weight:
            for layer in self:
                for node in layer:
                    node.mutate()
        # if random() < chance_new_link:
        #     self.new_link()
        if random() < config.chance_new_node:
            self.new_node()

    def new_node(self):
        node = choice(choice(self[1:]))
        link = choice(node)
        if link.active:
            link.disable()
        else:
            return

        layer = (node.layer + link.node.layer) / 2
        new_node = config.Link.next_node((node, link.node), layer)
        self[layer].append(new_node)

        node.link(new_node, 1.0)
        new_node.link(link.node, link.weight)

    def new_link(self):
        layers = sorted(self.keys())
        all_links = set(
            (node1, node2)
            for (i, layer1) in enumerate(layers)
            for node1 in self[layer1]
            for layer2 in layers[i+1:]
            for node2 in self[layer2]
        )
        cur_links = set(
            (node, link.node)
            for layer in self
            for node in layer
            for link in node
        )

        available_links = tuple(all_links - cur_links)
        if available_links:
            in_node, node = choice(available_links)
            in_node.link(node)

# Mating
    def is_compatable(self, organism: object) -> bool:
        matching = []
        disjoint = excess = 0

        for layers in zip_longest(self, organism, fillvalue=[]):
            for nodes in zip_longest(*layers, fillvalue=[]):
                for (link1, link2) in zip_longest(*nodes, fillvalue=[]):
                    if link1 and link2:
                        if link1 == link2:
                            matching.append((link1.weight - link2.weight))
                        else:
                            disjoint += 1
                    else:
                        excess += 1

        weight_avg = sum(matching) / len(matching)
        normalize = max(len(self), len(organism))
        compatability = (
            config.coefficient_excess * excess / normalize +
            config.coefficient_disjoint * disjoint / normalize +
            config.coefficient_weight * weight_avg
        )
        return compatability < config.compatability_threshold

    def crossover(self, matching: list) -> list:
        try:
            point, point2 = sample(range(len(matching)), 2)
        except ValueError:
            return [choice(matching)]

        if random() < config.chance_crossover_one_point:
            return matching[point:] if random() < 0.5 else matching[:point]
        elif random() < config.chance_crossover_two_point:
            if random() < 0.5:
                return matching[point:] + matching[point2:]
            else:
                return matching[point:point2]
        else:
            return [match for match in matching if random() < 0.5]

# Display Phenotype
    def show(self):
        plt.close('all')
        fig, ax = plt.subplots()
        ax.axis('off')

        plt.autoscale(False)
        layers = self.sorted()
        x, y = [], []
        for layer in layers:
            for node in layer:
                x = (node.innovation + 7) % 16 / 16
                y = node.layer
                ax.annotate(
                    f'{node.innovation}:{node.activation.__name__}', (x, y))
                for link in node:
                    x2 = (link.node.innovation + 7) % 16 / 16
                    y2 = link.node.layer
                    plt.plot([x, x2], [y, y2], 'om-')
                    ax.annotate(f'{link.weight: 0.1f}', ((x2+x)/2, ((y2+y)/2)))

        plt.tight_layout()
        plt.show()

