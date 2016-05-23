#!/usr/bin/env python3

import asyncio
import websockets

@asyncio.coroutine
def hello(websocket, path):
    print("Hi")
    state1 = yield from websocket.recv()
    print("state1: " + state1)
    yield from websocket.send(state1)

start_server = websockets.serve(hello, 'localhost', 8765)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
