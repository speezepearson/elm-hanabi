import argparse
from pathlib import Path

from aiohttp import web

import stateserver

parser = argparse.ArgumentParser()
parser.add_argument('-d', '--state-dir', type=Path, default=Path.cwd())
parser.add_argument('-p', '--port', type=int, default=48402)
args = parser.parse_args()

app = web.Application()
app.add_routes([
    web.RouteDef(path=f'/states{r.path}', method=r.method, handler=r.handler, kwargs=r.kwargs)
    for r in stateserver.make_routes(state_dir=args.state_dir)
])
app.add_routes([web.static(prefix='/static', path=Path.cwd()/'static')])
web.run_app(app, port=args.port)
