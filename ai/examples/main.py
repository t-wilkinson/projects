import os
import torch
from torch import nn
import numpy as np
from torch.utils.data import Dataset, DataLoader
from torchvision import datasets, transforms
from torchvision.transforms import ToTensor, Lambda
import matplotlib.pyplot as plt

def load_data():
    traning_data = datasets.FashionMNIST(
        root='data',
        train=True,
        download=True,
        transform=ToTensor(),
    )

    test_data = datasets.FashionMNIST(
        root="data",
        train=False,
        download=True,
        transform=ToTensor()
    )

    train_dataloader = DataLoader(traning_data, batch_size=64, shuffle=True)
    train_features, train_labels = next(iter(train_dataloader))
    print(f"Feature batch shape: {train_features.size()}")
    print(f"Labels batch shape: {train_labels.size()}")
    img = train_features[0].squeeze()
    label = train_labels[0]
# plt.imshow(img, cmap="gray")
# plt.show()
    print(f"Label: {label}")

def tensors():
    data = [[1, 2], [3, 4]]
    x_data = torch.tensor(data)
    x_rand = torch.rand_like(x_data, dtype=torch.float)
    ones_tensor = torch.ones((2,3,))
    tensor = torch.zeros((3,3,))

    if torch.cuda.is_available():
        tensor = tensor.to('cuda')

    tensor.matmul(tensor.T)

class NeuralNetwork(nn.Module):
    def __init__(self):
        super().__init__()
        self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(28*28, 512),
            nn.ReLU(),
            nn.Linear(512, 512),
            nn.ReLU(),
            nn.Linear(512, 10),
            nn.ReLU()
        )

    def forward(self, x):
        x = self.flatten(x)
        logits = self.linear_relu_stack(x)
        return logits

def run_model():
    model = NeuralNetwork().to(device)
    X = torch.rand(1, 28, 28, device=device)
    logits = model(X)
    predicted_probability = nn.Softmax(dim=1)(logits)
    y_pred = predicted_probability.argmax(1)
    print(f'Predicted {y_pred}')

    input_image = torch.rand(3, 28, 28)
    flatten = nn.Flatten()

def autograds():
    x = torch.ones(5)  # input tensor
    y = torch.zeros(3)  # expected output
    w = torch.randn(5, 3, requires_grad=True)
    b = torch.randn(3, requires_grad=True)
    z = torch.matmul(x, w)+b

    z = torch.matmul(x, w)+b
    print(z.requires_grad)

    with torch.no_grad():
        z = torch.matmul(x, w)+b
    print(z.requires_grad)
    z = torch.matmul(x, w)+b
    z.detach()

    loss = torch.nn.functional.binary_cross_entropy_with_logits(z, y)

    print('Gradient function for z =',z.grad_fn)
    print('Gradient function for loss =', loss.grad_fn)

    loss.backward()
    print(w.grad)
    print(b.grad)

def optimizing():
    pass

if __name__ == '__main__':
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print("Using {} device".format(device))
