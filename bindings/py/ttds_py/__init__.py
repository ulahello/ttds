from urllib.parse import urlunparse, urlencode, quote
from dataclasses import dataclass, field
from requests import request
from typing import Optional


@dataclass
class Color:
    r: int
    g: int
    b: int

    def __init__(self, r: int, g: int, b: int):
        assert 0 <= r and r <= 255
        assert 0 <= g and g <= 255
        assert 0 <= b and b <= 255

        self.r = r
        self.g = g
        self.b = b

    def __str__(self):
        return f"#{self.r:02x}{self.g:02x}{self.b:02x}"


class Connection:
    _host: str
    _use_tls: bool

    def __init__(self, host: str, use_tls: bool = False):
        self._host = host
        self._use_tls = use_tls

    def request(
        self, *args, method: Optional[str] = None, auth: Optional[str] = None, **kwargs
    ) -> str:
        escape = lambda x: quote(str(x), safe="")
        route = "/".join(escape(arg) for arg in args)
        query = urlencode(kwargs)

        scheme = "https" if self._use_tls else "http"
        target = urlunparse((scheme, self._host, route, "", query, ""))

        headers = None if auth is None else {"Auth": auth}
        r = request(method or "POST", target, headers=headers)
        r.raise_for_status()

        return str(r.content, "utf-8")


@dataclass
class Pane:
    name: str
    color: Color
    conn: Connection

    _token: Optional[str] = field(default=None)  # TODO: Make this a UUIDv4.

    def __enter__(self):
        self._token = self.conn.request("pane", self.name, "create", color=self.color)
        return self

    def __exit__(self, exc_type, exc_val, exc_traceback):
        assert self._token is not None
        self.conn.request("pane", self.name, auth=self._token, method="DELETE")

    def rect(self, x: int, y: int, w: int, h: int, color: Color):
        self._draw("rect", x=x, y=y, w=w, h=h, color=color)

    def circle(self, x: int, y: int, r: int, color: Color):
        self._draw("circle", x=x, y=y, r=r, color=color)

    def line(self, x: int, y: int, x2: int, y2: int, color: Color):
        self._draw("line", x=x, y=y, x2=x2, y2=y2, color=color)

    def copy_rect(self, x: int, y: int, w: int, h: int, x2: int, y2: int):
        self._draw("copy_rect", x=x, y=y, w=w, h=h, x2=x2, y2=y2)

    def bezier2(self, x0, y0, x1, y1, x2, y2, color: Color):
        self._draw("bezier2", x0=x0, y0=y0, x1=x1, y1=y1, x2=x2, y2=y2, color=color)

    def _draw(self, shape, **kwargs):
        assert self._token is not None
        self.conn.request("pane", self.name, shape, auth=self._token, **kwargs)
