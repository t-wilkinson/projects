from __future__ import print_function
import argparse
import numpy as np
import torch
import torch.nn as nn
from torch.optim import Optimizer
from torch.utils.data import DataLoader
from torchvision.transforms import ToTensor
from torchvision.datasets import FashionMNIST
import torchvision.utils as vutils

import matplotlib
import matplotlib.pyplot as plt
import random

matplotlib.use('TkAgg')

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

#
# FashionMNIST
#
# 60,000 training examples
# 10,000 test examples
# 28x28 greyscale


#
# CONFIG
#
batch_size = 20
discriminator_iterations = 1
epochs = 1000
manual_seed = 999

random.seed(manual_seed)
torch.manual_seed(manual_seed)

def load_data():
    training_dataset = FashionMNIST(
        root="../datasets/",
        train=True,
        download=True,
        transform=ToTensor(),
    )
    training_loader = DataLoader(dataset=training_dataset, batch_size=batch_size, shuffle=True)

    test_dataset = FashionMNIST(
        root="../datasets/",
        train=False,
        download=True,
        transform=ToTensor(),
    )
    test_loader = DataLoader(dataset=test_dataset, batch_size=batch_size, shuffle=True)
    return training_loader, test_loader


def sigmoid(X):
    Y = 1. / (1 + torch.exp(-X))
    return Y

#
# MODELS
#
class Generator(nn.Module):
    def __init__(self, seed_size):
        """
        :param int seed_size: Length of the random vector to feed the generator
        """
        super().__init__()
        self.seed_size = seed_size
        self.model = nn.Sequential(
            nn.Linear(seed_size, 18),
            nn.ReLU(),
            nn.Linear(18, 18),
            nn.ReLU(),
            nn.Linear(18, 18),
            nn.ReLU(),
            nn.Linear(18, 28*28)
        )
        self.sigmoid = nn.Sigmoid()

    def forward(self):
        z = np.random.rand(self.seed_size)
        z = torch.tensor(z).float().to(device)
        return self.model(self.sigmoid(z)) # normalize pixel values

    def generate_batch(self):
        """Create a tensor of generated images that can be fed into the discriminator"""
        generated = [None] * batch_size
        for i in range(batch_size):
            generated[i] = self()
        return torch.stack(generated, dim=0).to(device)


class Discriminator(nn.Module):
    def __init__(self):
        super().__init__()
        self.model = nn.Sequential(
            nn.Flatten(),
            nn.Linear(28*28, 18),
            nn.ReLU(),
            nn.Linear(18, 18),
            nn.ReLU(),
            nn.Linear(18, 18),
            nn.ReLU(),
            nn.Linear(18, 1),
        )
        self.sigmoid = nn.Sigmoid()

    def forward(self, X):
        X = self.model(X)
        return self.sigmoid(X.squeeze())


def visualize_data(training_data):
    # labels_map = {
    #     0: "T-Shirt",
    #     1: "Trouser",
    #     2: "Pullover",
    #     3: "Dress",
    #     4: "Coat",
    #     5: "Sandal",
    #     6: "Shirt",
    #     7: "Sneaker",
    #     8: "Bag",
    #     9: "Ankle Boot",
    # }

    # figure = plt.figure(figsize=(8, 8))
    # cols, rows = 3, 3
    # for i in range(1, cols * rows + 1):
    #     sample_idx = torch.randint(len(training_data), size=(1,)).item()
    #     img, label = training_data[sample_idx]
    #     figure.add_subplot(rows, cols, i)
    #     plt.title(labels_map[label])
    #     plt.axis("off")
    #     plt.imshow(img.squeeze(), cmap="gray")

    # plt.show()

    # Plot some training images
    real_batch = next(iter(training_data))
    plt.figure(figsize=(8,8))
    plt.axis("off")
    plt.title("Training Images")
    plt.imshow(np.transpose(vutils.make_grid(real_batch[0].to(device)[:64], padding=2, normalize=True).cpu(),(1,2,0)))


def run(training_data):
    generator = Generator(10)
    generator.to(device)
    discriminator = Discriminator()
    discriminator.to(device)

    gen_optimizer = torch.optim.SGD(generator.parameters(), lr=0.1, momentum=0, maximize=True)
    dis_optimizer = torch.optim.SGD(discriminator.parameters(), lr=0.1, momentum=0, maximize=True)

    training_data = iter(training_data)
    for epoch in range(epochs):
        for (X, _) in training_data:

            # Only update the discriminator here
            for param in generator.parameters():
                param.requires_grad = False
            for param in discriminator.parameters():
                param.requires_grad = True

            # Optimize discriminator before generator
            for iteration in range(discriminator_iterations):
                dis_optimizer.zero_grad()
                X = X.to(device)

                # Calculate loss of discriminator
                value_real = torch.log(discriminator(X))
                value_fake = torch.log(1 - discriminator(generator.generate_batch()))
                print(f'real:{value_real} fake:{value_fake}')

                # Optimize
                value = torch.sum(value_real + value_fake)
                value.backward()
                dis_optimizer.step()

                # print(f'epoch:{epoch} iteration:{iteration} value:{value}')

            # Only update the generator here
            for param in generator.parameters():
                param.requires_grad = True
            for param in discriminator.parameters():
                param.requires_grad = False

            # Train generator
            gen_optimizer.zero_grad()
            value = torch.sum(torch.log(discriminator(generator.generate_batch())))
            value.backward()
            gen_optimizer.step()

            # TODO: value is -inf
            print(f'epoch:{epoch} value:{value}')

        if epoch % 2 == 0:
            # Show current status of generator
            with torch.no_grad():
                image = torch.reshape(generator().cpu(), (28, 28))
                plt.title('generated')
                plt.axis('off')
                plt.imshow(image, cmap="gray", vmin=0, vmax=1)
                plt.show()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--visualize', action='store_true')
    args = parser.parse_args()

    training_data, test_data = load_data()

    if args.visualize:
        visualize_data(training_data)
    else:
        run(training_data)
