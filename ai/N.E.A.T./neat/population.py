import config
from random import shuffle
from species import Species
from organism import Organism
from itertools import chain


class Population(list):
    '''
    A Hypercube-Based Indirect Encoding for
    Evolving Large-Scale Neural Networks.
    A Population holds species.
    '''
    def __init__(self, inputs: list, outputs: list, simulation):
        super().__init__()
        self.simulation = simulation
        god = Organism(inputs, outputs)
        self.populate(god)
        for _ in range(config.generations):
            self.simulate()
            self.speciate()
            self.replicate()

    def populate(self, god: object):
        for _ in range(config.population):
            organism = god.clone()
            organism.randomize()
            self.append(Species(organism))

    def simulate(self):
        for species in self:
            species.simulate(self.simulation)

    def speciate(self):
        self.update_threshold()
        randomized = list(chain.from_iterable(self))
        shuffle(randomized)
        self.clear()
        for organism in randomized:
            for species in self:
                if organism in species:
                    break
            else:
                self.append(Species(organism))

    def replicate(self):
        config.Link.clear()
        for species in self:
            species.sort(reverse=True)
            species.fitness = sum(
                species[:int(len(species) * config.species_size_survival)])
        Species.fitness = sum(self)

        for species in self:
            interspecies = self[:]
            interspecies.remove(species)
            species.replicate(interspecies)

    def update_threshold(self):
        if len(self) < config.species_target_size:
            config.compatability_threshold -= config.compatability_modifier
        elif len(self) > config.species_target_size:
            config.compatability_threshold += config.compatability_modifier
        if config.compatability_threshold < config.compatability_modifier:
            config.compatability_threshold = config.compatability_modifier
