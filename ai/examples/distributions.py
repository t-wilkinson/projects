import random
import math
import numpy as np
from matplotlib import pyplot as plt


class bernoulli():
    def __init__(self, p):
        self.p = p

    def pmf(self, x):
        """
        probability mass function
        """
        f = self.p**x*(1-self.p)**(1-x)
        return f

    def mean(self):
        """
        expected value of bernoulli random variable
        """
        return self.p

    def var(self):
        """
        variance of bernoulli random variable
        """
        return self.p*(1-self.p)

    def std(self):
        """
        standart deviation of bernoulli random variable
        """
        return bernoulli.var(self.p)**(1/2)

    def rvs(self, size=1):
        """
        random variates
        """
        rvs = []
        for _ in range(0,size):
            if random.random() <= self.p:
                a=1
                rvs = rvs.append(a)
            else:
                a=0
                rvs = rvs.append(a)
        return rvs


class multinoulli():
    def __init__(self, p):
        self.p = p

    def pmf(self, x: int):
        if x < 0 or x > len(self.p):
            raise IndexError('x must be a valid index of p')
        elif x == len(self.p):
            return 1 - sum(self.p, 0)
        else:
            return self.p[x]

class gaussian():
    def __init__(self, mean=0, variance=1):
        self._mean = mean
        self._variance = variance

    def pmf(self, x: float):
        precision = 1 / self._variance
        return math.sqrt(precision/(2 * math.pi)) * math.exp((-precision/2) * (x - self._mean)**2)

    def mean(self):
        return self._mean

    def show(self, size=50, **kwargs):
        start = self.mean() - size
        end = self.mean() + size
        inputs = np.linspace(start,end,size*10)
        plt.plot(inputs, list(map(self.pmf, inputs)), **kwargs)

class RandomVariable:
    def __init__(self, states):
        self._states = states

    def states(self):
        """Returns set of all states of distribution."""
        return self._states

    def probability(self, _x):
        """Returns probability of picking datapoint x."""
        return 1/len(self._states)

    def new(self):
        """Returns random value from distribution."""
        rand_index = random.randrange(0, len(self._states))
        return self._states[rand_index]

    def expectation(self):
        """Returns expected variable."""
        expected = 0
        for state in self._states:
            expected += state * self.probability(state)
        return expected
