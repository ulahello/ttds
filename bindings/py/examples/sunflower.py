"""Draw a sunflower with the TTDS API (see https://github.com/B4TB/ttds)"""

from math import sqrt, cos, sin, pi, tau
from ttds_py import Color
import random
import time
import ttds_py

NAME = "sunflower"

# FIXME
NET_LOC = "127.0.0.1:8080"
WIDTH: int = 1280
HEIGHT: int = 800

BG: Color = Color(0xFA, 0xC0, 0x29)
FG: Color = Color(0xDB, 0x00, 0x72)
FULL_DIAMETER: int = min(WIDTH, HEIGHT)

gr: float = (sqrt(5) - 1) / 2

def spiral(idx: int, circle_count: int, max_radius: int) -> (float, float, float):
    p: float = idx / circle_count
    px: float = sqrt(p) * cos(tau * gr * idx)
    py: float = sqrt(p) * sin(tau * gr * idx)

    x: float = (1 + px) * (FULL_DIAMETER / 2 - max_radius) + max_radius
    y: float = (1 + py) * (FULL_DIAMETER / 2 - max_radius) + max_radius
    r: float = 1 + max_radius * p

    return (
        # make sure the sunflower is centered on the screen
        x + (WIDTH - FULL_DIAMETER) / 2,
        y + (HEIGHT - FULL_DIAMETER) / 2,
        r,
    )

def sunflower_loop(pane: ttds_py.Pane, min_max_radius: int, max_max_radius: int, tick_time: float, hold_time: float):
    direction: int = 0

    while True:
        # set up parameters for this round's sunflower
        max_radius: int = random.randint(min_max_radius, max_max_radius)
        distance: float = max_radius * 2.2
        circle_count: int = round(pi * pow((FULL_DIAMETER - max_radius) / 2, 2) / pow(distance + max_radius, 2))

        make   = range(1, circle_count + 1)
        unmake = range(circle_count, 0, -1)
        assert list(make) == list(reversed(unmake))
        directions = [make, unmake]

        # draw
        for i in directions[(direction + 0) % 2]:
            (x, y, r) = spiral(i, circle_count, max_radius)
            pane.circle(round(x), round(y), round(r), FG)
            time.sleep(tick_time)

        time.sleep(hold_time)

        # undraw
        for i in directions[(direction + random.randint(0, 1)) % 2]:
            (x, y, r) = spiral(i, circle_count, max_radius)
            pane.circle(round(x), round(y), round(r), BG)
            time.sleep(tick_time * 2 / 3)

        time.sleep(hold_time)

        # swap direction for funsies
        direction += 1

conn = ttds_py.Connection(NET_LOC)
with ttds_py.Pane(NAME, BG, conn) as pane:
    try:
        sunflower_loop(pane, tick_time=0.5, hold_time=2,
                       min_max_radius=FULL_DIAMETER // 128,
                       max_max_radius=FULL_DIAMETER // 21)
    except KeyboardInterrupt:
        pass
