import numpy as np
import numpy.random
from collections import defaultdict

# TODO: how to make graph accessible for backward prop
#   __call__ should pass through a graph/create one if it doesn't exist
# TODO: Module
#   __call__ should be able to use self to identify which node to attach information to
#   X should not be a Module.

class Graph:
    """Graph data structure for storing `Modules`
    Nodes represent modules, edges represent how data passes through.
    As information is processed, each module stores its output for the graph to access.
    """
    def __init__(self):
        self.graph = defaultdict(list)

    def create_node(self, module, output):
        self.graph[module].append(

    def get_operation(self, node):
        pass

    def get_inputs(self, node):
        pass

    def get_consumers(self, node):
        pass

class Output:
    """Should be include a graph structure which references functions, outputs, etc. to be used for backprop algorithm
    """
    def __init__(self):
        self.outputs = defaultdict(list)

    def __call__(self, in_node, out_node):
        self.outputs[in_node].append(out_node)

    def backward(self, grad):
        pass

class Module:
    def __init__(self):
        pass

    def __call__(self, X, graph=None):
        """
        How to identify input/output nodes from this function
        """
        output = self.forward(X)

        if graph is None:
            graph = Graph()
        else:
            graph(self, output)

        return output

class CrossEntropyLoss:
    def __init__(self):
        pass

    def __call__(self, graph, Y):
        self.Y = Y
        loss = np.log(Y)-np.log(graph.output)
        return loss

    def backward(self):
        # TODO: this is not technically correct
        return self.Y

    def step():
        pass

class Sequential(Module):
    def __init__(self, *models):
        super().__init__()
        self.models = models

    def forward(self, X):
        for model in self.models:
            X = model(X)
        return X

    def backward(self, grad):
        for model in reverse(self.models):
            grad = model.backward(grad)

class Softmax(Module):
    def __init__(self):
        super().__init__()

    def forward(self, X):
        X = np.exp(X)
        return X / np.sum(X)

    def backward(self, grad):
        return self.output - loss

class ReLU(Module):
    def __init__(self):
        super().__init__()

    def forward(self, X):
        return X * X > 0

class Linear(Module):
    def __init__(self, in_features, out_features):
        super().__init__()
        self.shape = (in_features, out_features)
        self.weights = np.random.normal(0, 1, size=self.shape)

    def forward(self, X):
        return X @ self.weights

    def backward(self, grad):
        self.weights -= self.output @ grad
        return self.weights @ grad

class Model(Module):
    def __init__(self):
        super().__init__()
        self.model = Sequential(
            Linear(28 * 28, 18),
            ReLU(),
            Linear(18, 18),
            ReLU(),
            Linear(18, 10),
            Softmax(),
        )

    def forward(self, X):
        return self.model(X)
