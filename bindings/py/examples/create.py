#!/usr/bin/env python3

"""Create a pane and keep it around for a few seconds."""

import ttds_py
import time

NET_LOC = '100.95.28.105:8080'
NAME = 'unique-name'

# Create a connection to NET_LOC. For testing purposes, I've got this hardcoded
# to my testing machine's IP, but you'll probably want to make it
# ttds.tali.network.
conn = ttds_py.Connection(NET_LOC)

# Create a pane with the given name and set it to a dark blue-ish color.
with ttds_py.Pane(NAME, ttds_py.Color(12, 12, 34), conn) as pane:
    # Keep it around for a second. Once sleep is done and the with block's
    # scope is exited, the pane will be deleted.
    #
    # Note that five seconds might not be long enough for your pane to come up
    # on the display if others are switching theirs in and out.
    time.sleep(5)
