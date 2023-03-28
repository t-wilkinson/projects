'''
Create a falling ball game
'''
import pyglet
import pymunk

from time import time
from pyglet.window import key
from gameobject import Ball, Brick, Sprite, add_boundaries, space, BATCH


'''
Globals
'''
WIDTH = 512
HEIGHT = 1080


'''
Game Loop
'''
class Game(pyglet.window.Window):
	def __init__(self):
		super().__init__(width=WIDTH, height=HEIGHT)

		self.set_location(700, 0)
		add_boundaries(WIDTH, HEIGHT)
		pyglet.clock.schedule(self.update)

		self.backgrounds = []
		for i in range(2):
			self.backgrounds.append(
				Sprite(
					img=pyglet.image.load('./images/galaxy.png'),
					x=-400,
					y=i*1080
				)
			)
		self.balls = [Ball() for _ in range(POPULATION)]

	def new_game(self, neural_network):
		''' Setup game and start game loop. '''

		# self.alive is every alive instance of Ball
		self.alive = self.balls[:]
		for ball, neural_network in zip(self.alive, neural_network):
			ball.neural_network = neural_network
			ball.reset()

		Brick.reset()

		self.start_time = time()
		pyglet.app.run()

		return self.balls

	def on_draw(self):
		''' Clear window, draw game objects. '''
		self.clear()

		BATCH['BACKGROUND'].draw()
		BATCH['FRONTGROUND'].draw()

		pyglet.text.Label(
			f'{round(time() - self.start_time, 1)}',
			font_size=24,
			x=0, y=HEIGHT,
			anchor_y='top'
		).draw()

		for _ in range(10):
			space.step(1 / 240)

	def update(self, dt):
		'''  Update game variables. '''
		# Update game objects
		Brick.update()
		for i, ball in enumerate(self.alive):
			ball.update()
			if ball.off_screen():
				ball.time = time() - self.start_time
				self.alive.pop(i)

		if not self.alive:
			pyglet.app.exit()

		# Scroll backgrounds
		for background in self.backgrounds:
			background.y += dt
			if background.y >= background.height:
				background.y = -background.height

		for _ in range(10):
			space.step(1 / 240)

	def on_key_press(self, symbol, modifier):
		if symbol == key.E:
			pyglet.app.exit()
		if symbol == key.Q or symbol == key.ESCAPE:
			self.close()
			pyglet.app.exit()


if __name__ == "__main__":
	pass
