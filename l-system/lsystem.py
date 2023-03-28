import pyglet
from pyglet import graphics
from pyglet.graphics import vertexdomain
import json
import math
import numpy as np
from time import time
import sys

pyglet.options['debug_gl', 'shadow_window'] = [False, False]
vertexdomain.create_attribute_usage('v2f/stream')
vertexdomain.create_attribute_usage('c3B/stream')
batch = pyglet.graphics.Batch()


def sort(file):
    with open(file, 'r') as f:
        result = json.load(f)
    with open(file, 'w') as f:
        json.dump(dict(sorted(result.items())), f, indent=4)
    return result


def wrap_values(values, *keys, wrapper):
    """wrap_values values in obj with wrapper for key in keys"""
    for key in keys:
        if key in values:
            values[key] = wrapper(values[key])


class LSystem:
    """Single lsystem which builds and iterates the string and contains vertices."""
    def __init__(self, data, default):
        self.__dict__ = {**data, **default}
        self.theta = np.radians(self.theta)

    def __repr__(self):
        return f'_LSystem_{self.axiom, self.rules, round(math.degrees(self.theta))}'
    __str__ = __repr__

    def setup(self):
        self.set_iteration()
        self.set_rgb()
        self.verts += self.start_pos
        self.set_vertex_list()

    def set_iteration(self):
        # grow word
        self.word = self.axiom
        for _ in range(self.iteration):
            self.word = ''.join(self.rules.get(char, char)
                                for char in self.word)

        # look-up dict
        operations = {
            '-': -self.theta,
            '+': self.theta,
            '|': math.pi
        }

        # define verts
        verts = np.array([operations.get(char, 0)
                         for char in self.word], dtype=np.float_)
        verts = np.cumsum(verts)[:, np.newaxis]
        verts = np.hstack((np.cos(verts), np.sin(verts)))
        verts = np.cumsum(verts, axis=0)

        # sum across array to get vertices
        self.center = np.average(verts, axis=0)
        self.verts = verts

    def set_rgb(self):
        rgb = np.arange(len(self.word), dtype=np.uint8)[:, np.newaxis]

        # apply lambda function
        self.rgb = np.hstack((
            self.r(rgb),
            self.g(rgb),
            self.b(rgb)
        )).flatten()

    def set_vertex_list(self):
        self.vertex_list = batch.add(
            len(self.word), pyglet.gl.GL_LINE_STRIP, None,
            ('v2f', self.verts.flatten()),
            ('c3B', self.rgb)
        )

    def set_line_width(self):
        pyglet.gl.glLineWidth(self.line_width)

    def rotate(self):
        # translation matrix
        c, s = math.cos(self.rotation[-1]), np.sin(self.rotation[-1])
        translation_matrix = np.array([[c, -s], [s, c]], dtype=np.float_)

        # apply translation matrix around fractal center
        verts = self.verts - self.center
        self.verts = verts.dot(translation_matrix) + self.center

    def draw(self):
        # transform fractal
        np.cumsum(self.translation, axis=1, out=self.translation)
        np.cumsum(self.rotation, out=self.rotation)
        np.cumsum(self.scale, out=self.scale)
        np.cumsum(self.color, out=self.color)

        self.center = np.average(self.verts, axis=0)
        self.rotate()

        # update vertices and scale them relative to their center
        verts = (self.verts - self.center) * \
            self.scale[-1] + self.center + self.translation[:, -1]
        self.vertex_list.vertices = verts.flatten()
        self.vertex_list.colors = self.rgb + self.color[-1].astype(np.uint8)
        self.vertex_list.draw(pyglet.gl.GL_LINE_STRIP)


class LSystemEnvironment(pyglet.window.Window):
    """Environment to manages multiple lsystems, draw them, etc."""

    def __init__(self, default='default', **kwargs):
        super().__init__(vsync=False, **kwargs)

        with open('property.json', 'r') as f:
            self.default = json.load(f)[default]
        self.default['start_pos'] = self.default['start_pos'] or self.center
        self.wrap_values(self.default)

    def __iter__(self):
        return iter(self.instances)

    def __setitem__(self, key, value):
        if isinstance(key, str):
            self[key] = value

    def __getitem__(self, key):
        if isinstance(key, str):
            return self.instances.get(key)
        elif isinstance(key, tuple) and len(key) > 0:
            return {k: self.instances.get(k) for k in key}
        else:
            return self.instances

    def __call__(self, *systems, **kwargs):
        """Takes lsystems and parameters to draw them.

        translation, rotation, scale, and color should be an array of two values. The first value is the step size, the second value is the starting value.
        """
        (instances, parameters) = self.setup(systems, kwargs)
        start_time = time()

        while time() - start_time < parameters['draw_time']:
            self.clear()
            for instance in instances.values():
                instance.set_line_width()
                instance.draw()
            self.flip()
            self.dispatch_events()

    def wrap_values(self, obj):
        wrap_values(obj, 'r', 'g', 'b', wrapper=lambda func: eval('lambda x:' + func))
        wrap_values(obj, 'start_pos', 'translation',
             'rotation', 'scale', 'color', wrapper=np.array)

    def setup(self, systems, kwargs):
        self.wrap_values(kwargs)
        parameters = {**self.default, **kwargs}
        instances = {
            name: LSystem(system, parameters)
            for (name, system) in sort('lsystems.json').items()
            if name in systems
        }

        for instance in instances.values():
            instance.setup()

        return (instances, parameters)

    def onkeypress(self, symbol, modifier):
        self.close()
        sys.exit(0)

    @property
    def center(self):
        return [self.width / 2, self.height / 2]
