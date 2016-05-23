#!/usr/bin/env python

import asyncio
import websockets
import json

state1 = None
state2 = None

@asyncio.coroutine
def hello(websocket, path):
    global state1, state2

    newState = yield from websocket.recv()
    newState = json.loads(newState)

    if len(newState) == 3:
        state1 = newState
        if state2:
            yield from websocket.send(json.dumps(state2))
    elif len(newState) == 1:
        state2 = newState
        if state1:
            yield from websocket.send(json.dumps(state1))

start_server = websockets.serve(hello, 'localhost', 5678)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
