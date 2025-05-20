'''
Create activations for use of cppn nodes
'''
import sys
import math
from random import choice
from itertools import chain
NORM_EPSILON = math.pow(sys.float_info.epsilon, 0.25)


# --------- #
# Symmetric #
# --------- #
abs


def hat(z):
    return max(0.0, 1.0 - abs(z))


def square(z):
    return z ** 2


def identity(z):
    return z


def cube(z):
    return z ** 3


def clamped(z):
    return max(-1.0, min(1.0, z))


def skewed_log1p(z):
    return math.copysign(1.0, z)*(math.log1p(abs(z*2))-1)


def log1p(z):
    return math.copysign(1.0, z)*math.log1p(abs(z*math.exp(0.5)))


def step(z):
    if z < 0.0:
        return -1
    if z > 0.0:
        return 1
    return z


def inv(z):
    try:
        result = 1.0 / z
    except ArithmeticError:
        return 0.0
    else:
        return result


def sinc(x):
    try:
        result = math.sin(x) / x
    except ArithmeticError:
        return 1
    else:
        return result


def tanh(z):
    try:
        result = math.tanh(z*2.5)
    except ArithmeticError:
        if z > 0.0:
            return 1.0
        return -1.0
    else:
        return result


def gauss(z):
    try:
        result = math.exp(-5.0 * z**2)
    except ArithmeticError:
        if abs(z) > 0.5:
            return 0.0
        return 1.0
    else:
        return result


def sigmoid(z):
    try:
        result = 1.0 / (1.0 + math.exp(-5.0*z))
    except ArithmeticError:
        if z > 0.0:
            return 1.0
        return 0.0
    else:
        return result


# ---------- #
# Repetition #
# ---------- #
def mod_down(z):
    return z % 1.0


def mod_up(z):
    return 1.0 - z % 1.0


def mod(z):
    return mod_down(z) if z >= 0.0 else mod_up(z)


def sin(z):
    z = max(-60.0, min(60.0, 5.0 * z))
    return math.sin(z)


def square_wave(z):
    return step(sin(z))


def triangle_wave(z):
    return min(1.0, max(-1.0, ((2/math.pi)*math.asin(sin(z)))))


def rectangular(z):
    if abs(z) == 0.5:
        return 0.5
    if abs(z) < 0.5:
        return 1.0
    return 0.0


# ----- #
# Other #
# ----- #
def relu(z):
    return z * (z > 0.0)


def gelu(z):
    return z * sigmoid(1.702 * z)


def exp(z):
    z = max(-60.0, min(60.0, z))
    return math.exp(z)


def expanded_log(z):
    if abs(z*2) < NORM_EPSILON:
        z = math.copysign((NORM_EPSILON/2), z)
    return math.copysign(1.0, z)*math.log(abs(z*2), 2)


def log(z):
    z = max(sys.float_info.epsilon, z)
    try:
        result = math.log(z)
    except ArithmeticError:
        return math.log(1e-7)
    else:
        return result


def softplus(z):
    try:
        result = 0.2 * math.log1p(math.exp(z*5.0))
    except ArithmeticError:
        if z > NORM_EPSILON:
            return z
        elif z < -1*NORM_EPSILON:
            return 0.0
        return (NORM_EPSILON/2.0)
    else:
        return result


symmetric = [
    abs, hat, square, sinc, gauss,
    identity, cube, skewed_log1p,
    step, tanh, clamped, log1p, inv, sigmoid
]
repetition = [
    mod_down, mod_up, mod, square_wave,
    triangle_wave, rectangular, sin
]
other = [
    relu, gelu, exp, log,
    softplus, expanded_log
]


class Activation:
    '''
    Return random activation function
    '''
    def __init__(self):
        self.types = [symmetric, repetition, other]

    def __call__(self):
        return choice(choice(self.types))

    def __getitem__(self, k):
        return next(func for func in chain(*self.types) if func.__name__ == k)


a = Activation()
# print(a['tanh'])
# print(a['sigmoid'])
