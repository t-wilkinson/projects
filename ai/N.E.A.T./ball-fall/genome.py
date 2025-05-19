'''
Genome represents an organism
Each Genome has a list of Gene objects.
'''
import config
import random
import itertools as it

from copy import deepcopy
from gene import Gene, InputNode, OutputNode, HiddenNode
from collections import defaultdict, Counter


'''
Class
'''
class Genome:
    ''' . '''

    def __init__(self, inputs, outputs):
        # create list of input and output Nodes
        self.inputs = [InputNode(i) for i in range(inputs)]
        self.outputs = [OutputNode(i) for i in range(inputs, inputs+outputs)]
        Gene.node_counter = inputs + outputs

        # create every possible link from input to output nodes
        self.Genes = defaultdict(list)
        for in_node, out_node in it.product(self.inputs, self.outputs):
            self.Genes[in_node].append(Gene((in_node, out_node)))

    def __str__(self):
        return '|============================================|' + \
        '\n|                   Genes                    |' + \
        '\n|--------------------------------------------|' + \
        '\n|  IN:OUT |   Weight   | Innovation | Active |\n' + \
        '\n'.join('\n'.join(f'|{in_node}:{gene}'
            for gene in self.Genes[in_node])
            for in_node in self.Genes) + \
        '\n|============================================|'

    def clone(self):
        return deepcopy(self)

    def kill(self):
        self.Genes.clear()
        for in_node, out_node in it.product(self.inputs, self.outputs):
            self.Genes[in_node].append(Gene((in_node, out_node)))
        for gene in sum(self.Genes.values(), []):
            gene.mutate()
        self._add_node_()
        self._add_link_()

    def setup(self):
        ''' Prepare genome for feeding. '''
        nodes = Counter((gene.out for gene in sum(self.Genes.values(), []) if gene.active))
        for out_node, occurences in nodes.items():
            out_node.recieving = occurences

    def feed(self, inputs: list) -> list:
        ''' Feed genome inputs, return outputs. '''
        for node in set(sum(self.Genes.values(), [])):
            node.out.reset()

        # setup inputs
        for node, i in zip(self.inputs, inputs):
            node.result = i

        # loop until all outputs are recieved
        while not all((node.ready() for node in self.outputs)):
            for in_node in self.Genes:
                if in_node.ready():
                    result = in_node.fire()
                    for out_node in self.Genes[in_node]:
                        if out_node.active:
                            out_node.out.feed(result * out_node.weight)

        return [node.fire() for node in self.outputs]

    ''' Mutations. '''
    def mutate(self, species_size):
        if random.uniform(0, 1) < config.chance_mutate_weight:
            for gene in sum(self.Genes.values(), []):
                gene.mutate()
        if species_size >= config.species_big_size:
            if random.uniform(0, 1) < config.chance_new_link_big_species:
                self._add_link_()
        else:
            if random.uniform(0, 1) < config.chance_new_link:
                self._add_link_()
        if random.uniform(0, 1) < config.chance_new_node:
            self._add_node_()

    def _add_node_(self):
        '''
        Create new node to add to existing link.
        Reduce mal-effect of adding new node.
        '''
        in_node = random.choice(list(self.Genes.keys()))
        out_node = random.choice(self.Genes[in_node])
        out_node.active = False
        new_node = Gene.new_node((in_node, out_node.out))

        # To node -> new node has weight of 1
        self._add_gene_(in_node, new_node, 1.0)
        # new node -> OUT has weight of original link
        self._add_gene_(new_node, out_node.out, out_node.weight)

    def _add_link_(self):
        ''' Add link to two unconnected nodes. '''
        out_nodes = tuple(set([node for node in sum(self.Genes.values(), [])]))
        cur_links = {frozenset((k, v.out)) for (k, values) in self.Genes.items() for v in values}
        all_links = {frozenset((k, v.out)) for k in tuple(self.Genes) for v in out_nodes}
        available_links = all_links - cur_links

        for link in available_links:
            if len(link) == 2:
                node1, node2 = link
                if node1 in self.outputs:
                    self._add_gene_(node2, node1)
                elif node2 in self.outputs:
                    self._add_gene_(node1, node2)
                break

    def _add_gene_(self, in_node, out_node, weight=None):
        ''' Add new conneciton to genome. '''
        gene = Gene(link=(in_node, out_node), weight=weight)
        self.Genes[in_node].append(gene)


    ''' Mating methods. '''
    def crossover(self, genome):
        ''' Crossover two genomes, inherit genes from fit parent. '''
        unfitter, fitter = sorted([self, genome], key=lambda x: x.fitness)
        kid = fitter.clone()

        for (k1, v1), (k2, v2) in zip(unfitter.Genes.items(), kid.Genes.items()):
            for i, (unfit, fit) in enumerate(zip(v1, v2)):
                if unfit.innovation != fit.innovation:
                    continue
                if random.random() < 0.5:
                    fit.weight = unfit.weight

        return kid

    def compatable(self, genome):
        ''' Quantize compatability of two genomes. '''
        matching = []
        disjoint, excess = 0, 0
        for (v1, v2) in it.zip_longest(self.Genes.values(), genome.Genes.values(), fillvalue=[]):
            for (gene1, gene2) in it.zip_longest(v1, v2):
                if gene1 and gene2:
                    if gene1 == gene2:
                        matching.append((gene1.weight - gene2.weight))
                    else:
                        disjoint += 1
                else:
                    excess += 1

        # if no matching genes, genomes do not match
        if not matching:
            return False

        weight_avg = sum(matching) / len(matching)
        normalize = max(len(sum(self.Genes.values(), [])),
                        len(sum(genome.Genes.values(), [])))

        compatability = config.C1 * excess / normalize + \
                        config.C2 * disjoint / normalize + \
                        config.C3 * weight_avg

        return compatability < config.compatability_distance_threshold



