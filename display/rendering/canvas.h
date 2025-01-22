#pragma once

#include <dirent.h>
#include <stddef.h>
#include <stdint.h>

/* Encode a color. Alpha is assumed to always be 0xff. */
struct color {
	uint8_t r, g, b;
};

struct rect {
	uint16_t x, y, w, h;
};

struct circle {
	uint16_t x, y;
	uint16_t r;
};

struct canvas {
	uint16_t width, height;
	uint32_t stride;
	uint8_t *buffer;
};

struct canvas *canvas_init_bgra(uint16_t width, uint16_t height);

void canvas_deinit(struct canvas *);

void rendering_fill(struct canvas *, struct color);

void rendering_draw_rect(struct canvas *, const struct rect *, struct color);

void rendering_draw_circle(
    struct canvas *, const struct circle *, struct color);

void rendering_dump_bgra_to_rgba(
    const struct canvas *c, DIR *dir, const char *path);
