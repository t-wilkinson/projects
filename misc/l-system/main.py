import numpy as np
import lsystem


def run_lsystem():
    rgb_params = {
        "r": "255 - x",
        "g": "x % 51 * 5",
        "b": "x // 3",
    }

    environment = lsystem.LSystemEnvironment(fullscreen=True)
    # environment('pentaplexity', 'cross')
    # environment('tiled_square', 'pentaplexity', rotation=[0.0, -0.001])
    # environment('sierpinski_square', 'crystal')
    # environment('crystal', color=[0.2, 20.1], start_pos=[2560/2, 1080/2], scale=[0.0, 10.0], rotation=[ 0.0, 0.0])

    environment('sierpinski_arrowhead', **rgb_params, iteration=6, color=[0.2, 20.1], scale=[0.0, 4.0], rotation=[ 0.0, 0.0])
    environment('board', color=[0.50, 0], scale=[0.02, 5.0], rotation=[0.0, 0.0008], r="x * 2+ 90", g="x + 40", b="x + 120")
    environment('rings', color=[0.2, 0], iteration=3, scale=[0.0, 10.0], rotation=[0.000002, -0.002], r="255 - x", g="255 - x", b="255 - x")
    environment.on_close()


if __name__ == '__main__':
    run_lsystem()
