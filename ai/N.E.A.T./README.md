# Neuro Evolution of Augmenting Topologies (NEAT)
An attempt at an implementation of the [NEAT genetic algorithm](https://en.wikipedia.org/wiki/Neuroevolution_of_augmenting_topologies). A genetic algorithm which aims to produce the simplest possible topology of a graph structure for a given problem.

## File structure
### /neat
This is where the implementation of the algorithm resides. NEAT is a evolutionary algorithm. I have seperated modules to best represent the hierarchy of the algorithm as follows:
- Population: A collection of organisms, seperated into species.
- Species: A collection of organisms with similar topologies.
- Organism: An individual model.
- Link + Node + Activation: The activation functions, weights, biases, etc.

### /ball-fall
This is a game I made to test the algorithm (although a hard coded solution would be significantly better in practice).

## Running
The top topology for each species will be visualized using matplotlib showing weights, biases and activation functions. Pressing q(uit) will go to the next graph.

### Evolutionary algorithm
```bash
source env/bin/actiate
python3 neat/main.py
```

### Ball-fall game
```bash
source env/bin/actiate
cd ball-fall
python3 ball-fall/main.py
```
