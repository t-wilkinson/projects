from __future__ import print_function

import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim

from model import Normalization, ContentLoss, StyleLoss

from PIL import Image

import torchvision.transforms as transforms
import torchvision.models as models
import torchvision.utils as utils

import copy

import sys
import argparse

### Sequential ###
# desired depth layers to compute style/content losses :
content_layers_default = ['conv_4']
style_layers_default = ['conv_1', 'conv_2', 'conv_3', 'conv_4', 'conv_5']

def get_style_model_and_losses(cnn, normalization_mean, normalization_std,
                               style_img, content_img,
                               content_layers=content_layers_default,
                               style_layers=style_layers_default):
    cnn = copy.deepcopy(cnn)

    # normalization module
    normalization = Normalization(normalization_mean, normalization_std).to(device)

    # just in order to have an iterable access to or list of content/syle losses
    content_losses = []
    style_losses = []

    # assuming that cnn is a nn.Sequential, so we make a new nn.Sequential
    # to put in modules that are supposed to be activated sequentially
    model = nn.Sequential(normalization)

    i = 0  # increment every time we see a conv
    for layer in cnn.children():
        if isinstance(layer, nn.Conv2d):
            i += 1
            name = 'conv_{}'.format(i)
        elif isinstance(layer, nn.ReLU):
            name = 'relu_{}'.format(i)
            # The in-place version doesn't play very nicely with the ContentLoss
            # and StyleLoss we insert below. So we replace with out-of-place
            # ones here.
            layer = nn.ReLU(inplace=False)
        elif isinstance(layer, nn.MaxPool2d):
            name = 'pool_{}'.format(i)
        elif isinstance(layer, nn.BatchNorm2d):
            name = 'bn_{}'.format(i)
        else:
            raise RuntimeError('Unrecognized layer: {}'.format(layer.__class__.__name__))

        model.add_module(name, layer)

        if name in content_layers:
            # add content loss:
            target = model(content_img).detach()
            content_loss = ContentLoss(target)
            model.add_module("content_loss_{}".format(i), content_loss)
            content_losses.append(content_loss)

        if name in style_layers:
            # add style loss:
            target_feature = model(style_img).detach()
            style_loss = StyleLoss(target_feature)
            model.add_module("style_loss_{}".format(i), style_loss)
            style_losses.append(style_loss)

    # now we trim off the layers after the last content and style losses
    for i in range(len(model) - 1, -1, -1):
        if isinstance(model[i], ContentLoss) or isinstance(model[i], StyleLoss):
            break

    model = model[:(i + 1)]

    return model, style_losses, content_losses

### L-BFGS algorithm which optimizes unconstrained nonlinear problems ###
def get_input_optimizer(input_img):
    # this line to show that input is a parameter that requires a gradient
    optimizer = optim.LBFGS([input_img.requires_grad_()])
    return optimizer


### Perform style transfer ###
def run_style_transfer(cnn, normalization_mean, normalization_std,
                       content_img, style_img, input_img, num_steps=300,
                       style_weight=1000000, content_weight=1):
    """Run the style transfer."""
    print('Building the style transfer model..')
    model, style_losses, content_losses = get_style_model_and_losses(cnn,
        normalization_mean, normalization_std, style_img, content_img)
    optimizer = get_input_optimizer(input_img)

    print('Optimizing..')
    run = [0]
    while run[0] <= num_steps:

        def closure():
            # correct the values of updated input image
            input_img.data.clamp_(0, 1)

            optimizer.zero_grad()
            model(input_img)
            style_score = 0
            content_score = 0

            for sl in style_losses:
                style_score += sl.loss
            for cl in content_losses:
                content_score += cl.loss

            style_score *= style_weight
            content_score *= content_weight

            loss = style_score + content_score
            loss.backward()

            run[0] += 1
            if run[0] % 50 == 0:
                print("run {}:".format(run), end=' ')
                print('Style Loss : {:4f} Content Loss: {:4f}'.format(
                    style_score.item(), content_score.item()))

            return style_score + content_score

        optimizer.step(closure)

    # a last correction...
    input_img.data.clamp_(0, 1)

    return input_img

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--content-image', '-i', required=True)
    parser.add_argument('--style-image', '-s', required=True)
    parser.add_argument('--output-image', '-o', required=True)
    args = parser.parse_args()

### Convert images to proper form ###
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

# desired size of the output image
    imsize = 512 if torch.cuda.is_available() else 128  # use small size if no gpu

    loader = transforms.Compose([
        transforms.ToTensor()       # transform it into a torch tensor
    ])

    def image_loader(image_name):
        image = Image.open(image_name)
        image = image.convert('RGB')

        # resize image so largest side is `imsize`
        imsize2 = round(imsize * image.height / image.width)
        size = (imsize, imsize2) if imsize > imsize2 else (imsize2, imsize)
        image = image.resize(size)

        # fake batch dimension required to fit network's input dimensions
        image = loader(image).unsqueeze(0)
        image = image.to(device, torch.float)
        return image

    style_img = image_loader(args.style_image)
    content_img = image_loader(args.content_image)

    assert style_img.size() == content_img.size(), \
        "we need to import style and content images of the same size"

### Our model ###
    cnn = models.vgg19(pretrained=True).features.to(device).eval()

### Normalize images for VGG ###
    cnn_normalization_mean = torch.tensor([0.485, 0.456, 0.406]).to(device)
    cnn_normalization_std = torch.tensor([0.229, 0.224, 0.225]).to(device)

### Image Selection ###
    input_img = torch.randn(content_img.data.size(), device=device)

### Run the algorithm ###
    output = run_style_transfer(cnn, cnn_normalization_mean, cnn_normalization_std,
                                content_img, style_img, input_img)

    utils.save_image(output, args.output_image)
