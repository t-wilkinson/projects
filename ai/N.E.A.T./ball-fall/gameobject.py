'''
GameObject for combining pymunk shapes and pyglet sprites
'''
import pymunk
import pyglet

import numpy as np

from time import time


'''
Globals
'''
space = pymunk.Space()
space.gravity = (0.0, -350.0)

BATCH = {
    'FRONTGROUND': pyglet.graphics.Batch(),
    'BACKGROUND': pyglet.graphics.Batch(),
}

WIDTH = 512
HEIGHT = 1080

BRICK_SCALE = 4
CIRCLE_SCALE = 3

ACC = 0.025            # acceleration
CUR_SPEED = 0        # current speed
TAR_SPEED = 0        # target speed
MAX_SPEED = 550        # max speed
MAX_ANGLE = 5        # max angular velocity

path = './images/'
img = {                # store image for easier access
    'ball': pyglet.image.load(path + 'ball.png'),
    'dead_player': pyglet.image.load(path + 'ball2.png'),
    'brick': pyglet.image.load(path + 'brick.png'),
    'background': pyglet.image.load(path + 'galaxy.png'),
}

shapes = {        # distinguish different shapes while colliding
    'brick': 1,
    'ball': 2,
}


'''
Functions for pymunk shapes
'''
def add_boundaries(width, height):
    ''' Prevent player from going off screen. '''
    space.add(
        pymunk.Segment(space.static_body, (0, 0), (0, height + 64), 0),
        pymunk.Segment(space.static_body, (width, 0), (width, height + 64), 0),
        pymunk.Segment(space.static_body, (0, 0), (width, 0), 0),
    )


def ball_on_ball(*args):
    ''' Do not collide balls. '''
    return False


def ball_space(space):
    ''' Create ball shape for ball image. '''
    mass = 100
    scale = 3
    radius = 8 * scale

    inertia = pymunk.moment_for_circle(mass, 0, radius)
    body = pymunk.Body(mass, inertia, body_type=pymunk.Body.DYNAMIC)

    shape = pymunk.Circle(body, radius)
    shape.elasticity = 0.0
    shape.friction = 0.9
    shape.collision_type = shapes['ball']

    collision_handler = space.add_collision_handler(
        shapes['ball'],
        shapes['ball'],
    )
    collision_handler.begin = ball_on_ball

    space.add(body, shape)

    return shape


def brick_space(space, x, y):
    ''' Create brick shape. '''
    body = pymunk.Body(body_type=pymunk.Body.KINEMATIC)
    body.position = (x, y)
    body.velocity = (0, 100)

    shape = pymunk.Poly.create_box(body, (16 * BRICK_SCALE, 8 * BRICK_SCALE))
    shape.elasticity = 0.0
    shape.friction = 1.0

    space.add(body, shape)

    return shape


'''
Classes to combine pymunk shapes and pyglet sprites
'''
class Sprite(pyglet.sprite.Sprite):
    ''' Create pyglet sprite. '''
    def __init__(self, img, x, y, batch=BATCH['BACKGROUND']):
        super().__init__(img, x, y, batch=batch)


class GameObject(Sprite):
    ''' Base class for creating bodies in game space. '''
    _instances = []

    def __init__(self, img, space, scale):
        self.space = space
        img.anchor_x = img.width // 2
        img.anchor_y = img.height // 2
        super().__init__(img,
            self.space.body.position.x,
            self.space.body.position.y,
            BATCH['FRONTGROUND']
        )
        self.scale = scale

        # Scale image without getting blurry
        pyglet.gl.glTexParameteri(
            pyglet.gl.GL_TEXTURE_2D,
            pyglet.gl.GL_TEXTURE_MAG_FILTER,
            pyglet.gl.GL_NEAREST,
        )

    def _update_(self):
        self.x = self.space.body.position.x
        self.y = self.space.body.position.y


class Brick(GameObject):
    columns = np.linspace(32, WIDTH-32, 8)
    rows = np.linspace(HEIGHT, 0, 7)
    empty_columns = []

    def __init__(self, x, y):
        super().__init__(img=img['brick'], space=brick_space(space, x, y), scale=BRICK_SCALE)
        Brick._instances.append(self)

    @classmethod
    def reset(cls):
        ''' New Game. '''
        for brick in cls._instances:
            space.remove(brick.space, brick.space.body)
            brick.delete()
        cls._instances.clear()
        cls.empty_columns.clear()

        for y in cls.rows:
            cls.new_layer(y)

    @classmethod
    def new_layer(cls, y):
        ''' Create new brick at height y. '''
        np.random.shuffle(cls.columns)
        for x in cls.columns[:-2]:
            brick = Brick(x, y)
        cls.empty_columns.append(list(cls.columns[-2:]))

    @classmethod
    def update(cls):
        ''' Update bricks, cycle them. '''
        for brick in cls._instances:
            if brick.y > HEIGHT + 64:
                cls._instances = cls._instances[6:]
                cls.empty_columns = cls.empty_columns[1:]
                cls.new_layer(brick.y - HEIGHT - 160)
                break
            brick._update_()

    @classmethod
    def draw_bricks(cls):
        for brick in cls._instances:
            brick.draw()


class Ball(GameObject):
    def __init__(self, genome):
        super().__init__(img=img['ball'], space=ball_space(space), scale=CIRCLE_SCALE)
        self.genome = genome
        self.tar_speed = 0
        self.reset()

    def reset(self):
        self.space.body.position = (WIDTH // 2, HEIGHT // 2 - 10)
        self.space.body.velocity = (0, 0)

    def update(self):
        self._update_()
        self.decide()

        # determine ball speed
        self.linearly_interpolate()

        # sync ball rotation
        ang_vel = self.space.body.angular_velocity
        self.space.body.angular_velocity = np.sign(ang_vel) * min(abs(ang_vel), MAX_ANGLE)
        self.rotation = -np.degrees(self.space.body.angle)

        # prevent ball from bouncing up
        x_vel, y_vel = self.space.body.velocity
        self.space.body.velocity = (x_vel, min(y_vel, 100))

    def linearly_interpolate(self):
        ''' Accelerate ball up to target speed. '''
        x_speed = ACC * self.tar_speed + (1 - ACC) * self.space.body.velocity[0]
        self.space.body.velocity = (x_speed, self.space.body.velocity[1])

    def decide(self):
        ''' Make a decision based on inputs. '''
        # empty brick x positions
        empty_bricks = Brick.empty_columns

        # index of value closest to self.y
        row_index = np.argmax(self.y - Brick.rows >= 0)
        brick_pos = empty_bricks[row_index]

        inputs = [self.x] + brick_pos
        direction, = self.genome.feed(inputs)
        self.tar_speed = MAX_SPEED if direction > 0.5 else -MAX_SPEED

    def off_screen(self):
        return self.y > HEIGHT + 32

