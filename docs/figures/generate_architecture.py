#!/usr/bin/env python3
"""Generate docs/figures/architecture.excalidraw.

The scene follows the hand-drawn rules in the nixos repository:
full element fields, bound box labels, separate arrow labels, a white
background, the fixed palette, and a 1520-unit-wide composition.
"""

from __future__ import annotations

import json
from pathlib import Path

OUTPUT = Path(__file__).with_name("architecture.excalidraw")

INK = "#1e1e1e"
MUTED = "#5c5f66"
NEUTRAL = "#495057"

BLUE = ("#1971c2", "#d0ebff")
PURPLE = ("#9c36b5", "#f3d9fa")
TEAL = ("#0c8599", "#c5f6fa")
AMBER = ("#f08c00", "#fff3bf")

SCENE_WIDTH = 1520
SCENE_HEIGHT = 860

elements: list[dict] = []
seed = 1000


def next_seed() -> int:
    global seed
    seed += 37
    return seed


def base_element(
    element_id: str,
    element_type: str,
    x: float,
    y: float,
    width: float,
    height: float,
    stroke: str,
    fill: str,
    *,
    roundness: dict | None = None,
) -> dict:
    return {
        "id": element_id,
        "type": element_type,
        "x": x,
        "y": y,
        "width": width,
        "height": height,
        "angle": 0,
        "strokeColor": stroke,
        "backgroundColor": fill,
        "fillStyle": "hachure",
        "strokeWidth": 2,
        "strokeStyle": "solid",
        "roughness": 2,
        "opacity": 100,
        "groupIds": [],
        "frameId": None,
        "roundness": roundness,
        "seed": next_seed(),
        "version": 1,
        "versionNonce": next_seed(),
        "isDeleted": False,
        "boundElements": [],
        "updated": 1,
        "link": None,
        "locked": False,
    }


def text_element(
    element_id: str,
    x: float,
    y: float,
    width: float,
    height: float,
    text: str,
    font_size: int,
    color: str,
    *,
    container_id: str | None = None,
    align: str = "center",
    valign: str = "middle",
) -> dict:
    element = base_element(
        element_id,
        "text",
        x,
        y,
        width,
        height,
        color,
        "transparent",
        roundness=None,
    )
    element.update(
        {
            "text": text,
            "fontSize": font_size,
            "fontFamily": 1,
            "textAlign": align,
            "verticalAlign": valign,
            "containerId": container_id,
            "originalText": text,
            "lineHeight": 1.25,
        }
    )
    return element


def box(
    element_id: str,
    x: float,
    y: float,
    width: float,
    height: float,
    role: tuple[str, str],
    label: str,
    *,
    font_size: int = 22,
) -> None:
    stroke, fill = role
    rectangle = base_element(
        element_id,
        "rectangle",
        x,
        y,
        width,
        height,
        stroke,
        fill,
        roundness={"type": 3},
    )
    text_id = f"{element_id}-text"
    rectangle["boundElements"] = [{"type": "text", "id": text_id}]
    lines = label.count("\n") + 1
    text_height = lines * font_size * 1.25
    elements.append(rectangle)
    elements.append(
        text_element(
            text_id,
            x,
            y + (height - text_height) / 2,
            width,
            text_height,
            label,
            font_size,
            stroke,
            container_id=element_id,
        )
    )


def note(
    element_id: str,
    x: float,
    y: float,
    text: str,
    font_size: int = 18,
    color: str = MUTED,
    *,
    width: float = 800,
) -> None:
    lines = text.count("\n") + 1
    elements.append(
        text_element(
            element_id,
            x,
            y,
            width,
            lines * font_size * 1.25,
            text,
            font_size,
            color,
            align="left",
            valign="top",
        )
    )


def arrow(
    element_id: str,
    start: tuple[float, float],
    end: tuple[float, float],
    label: str,
    label_position: tuple[float, float],
) -> None:
    x1, y1 = start
    x2, y2 = end
    dx = x2 - x1
    dy = y2 - y1
    element = base_element(
        element_id,
        "arrow",
        x1,
        y1,
        abs(dx),
        abs(dy),
        NEUTRAL,
        "transparent",
        roundness=None,
    )
    element.update(
        {
            "points": [[0, 0], [dx, dy]],
            "lastCommittedPoint": None,
            "startBinding": None,
            "endBinding": None,
            "startArrowhead": None,
            "endArrowhead": "arrow",
            "elbowed": False,
        }
    )
    elements.append(element)
    label_x, label_y = label_position
    elements.append(
        text_element(
            f"{element_id}-label",
            label_x,
            label_y,
            len(label) * 18 * 0.62,
            18 * 1.25,
            label,
            18,
            NEUTRAL,
            align="left",
            valign="top",
        )
    )


# Title and framing note.
note(
    "title",
    60,
    40,
    "ocaml-redis request and replication path",
    font_size=32,
    color=INK,
    width=900,
)
note(
    "subtitle",
    60,
    88,
    "Current RESP2 subset, one process",
    font_size=18,
    color=MUTED,
    width=900,
)

# Column 1: wire input.
box("clients", 80, 180, 250, 90, BLUE, "TCP clients")
box("resp", 80, 310, 250, 90, BLUE, "RESP framing\narrays, bulk strings")
box("parser", 80, 440, 250, 90, BLUE, "Command parser\narity, options")

# Column 2: session and replication state owned by the runtime.
box(
    "server",
    500,
    170,
    360,
    360,
    PURPLE,
    "Server event loop\n"
    "client sessions\n"
    "auth, MULTI, WATCH\n"
    "blocked queues\n"
    "BLPOP, XREAD, WAIT\n"
    "replication\n"
    "upstream and peers\n"
    "one Unix.select loop",
)

# Column 3: semantics.
box("engine", 1050, 230, 300, 100, TEAL, "Engine semantics\ncommand outcomes")
box("store", 1050, 410, 300, 90, TEAL, "Store\nfamilies, keys")
box("expiry", 1050, 560, 300, 90, AMBER, "Expiry + versions\nlazy deadlines")
note(
    "store-owner",
    1050,
    700,
    "one authoritative store owner",
    font_size=18,
    color=MUTED,
    width=320,
)

# Wire flow, top to bottom, then into the runtime.
arrow("a-bytes", (205, 270), (205, 310), "bytes", (220, 274))
arrow("b-frames", (205, 400), (205, 440), "frames", (220, 404))
arrow("c-request", (330, 485), (500, 485), "requests", (350, 455))

# Runtime and engine exchange.
arrow("d-execute", (860, 285), (1050, 285), "execute", (880, 258))
arrow("e-reply", (1050, 315), (860, 315), "reply, wakes", (866, 324))

# Engine and store exchange.
arrow("f-keys", (1150, 330), (1150, 410), "key ops", (1162, 352))
arrow("g-values", (1250, 410), (1250, 330), "values", (1262, 352))

# Store asks expiry helpers, and expiry answers.
arrow("h-ttl", (1120, 500), (1120, 560), "TTL", (1128, 515))
arrow("i-expired", (1220, 560), (1220, 500), "expired", (1245, 515))

# Source note.
note(
    "source",
    60,
    790,
    "Source: lib/resp.ml, command.ml, engine.ml, store.ml, server.ml",
    font_size=18,
    color=MUTED,
    width=1400,
)

scene = {
    "type": "excalidraw",
    "version": 2,
    "source": "https://excalidraw.com",
    "elements": elements,
    "appState": {
        "viewBackgroundColor": "#ffffff",
        "gridSize": None,
        "gridStep": 5,
        "gridModeEnabled": False,
        "theme": "light",
    },
    "files": {},
}

OUTPUT.write_text(json.dumps(scene, indent=2) + "\n", encoding="utf-8")
print(f"wrote {OUTPUT} with {len(elements)} elements")
