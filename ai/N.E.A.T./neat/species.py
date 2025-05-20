import config
from random import random, choice


class Species(list):
    '''
    Each species holds a group of similar organisms.
    Organisms compete with others in the same species.
    '''
    fitness = 0
    stagnation_fitness = 0

    def __init__(self, organism: object):
        super().__init__()
        self.count = 0
        self.append(organism)
        self.representative = organism

    def __add__(self, species: object) -> int:
        if isinstance(species, (int, float)):
            return self.fitness + species
        else:
            return self.fitness + species.fitness
    __radd__ = __add__

    def __contains__(self, organism: object) -> bool:
        if self.representative.is_compatable(organism):
            self.append(organism)
            return True

    def simulate(self, simulate):
        for organism in self:
            simulate(organism)

    def mate(self, parent1: object, interspecies: list) -> object:
        if random() < config.chance_mutate_no_crossover:
            kid = parent1.clone()
        else:
            if random() < config.chance_mate_interspecies:
                try:
                    parent2 = choice(choice(interspecies))
                except Exception:
                    parent2 = choice(self)

            else:
                parent2 = choice(self)
            kid = parent1 ^ parent2
        return kid

    def replicate(self, interspecies: list):
        num_offspring = int(config.population * self.fitness / Species.fitness)
        offspring = [self[0]]
        if len(self) >= config.species_size_threshold:
            chance_new_link = config.chance_new_link_big_species
        else:
            chance_new_link = config.chance_new_link_small_species

        for i in range(num_offspring):
            try:
                parent = self[i]
            except Exception:
                parent = choice(self)

            kid = self.mate(parent, interspecies)
            kid.mutate(chance_new_link)
            offspring.append(kid)

        self[:] = offspring

    def cull(self):
        pass
