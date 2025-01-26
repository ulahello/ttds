#pragma once

#include "canvas.h"

struct rendering_vtable {
	void *(*rendering_init)(void);
	void (*rendering_cleanup)(void *r_ctx);
	void (*rendering_ctx_log)(const void *r_ctx);
	void (*rendering_show)(void *r_ctx, struct canvas *);
	struct canvas *(*canvas_init)(void *r_ctx);
};

enum backend {
	BACKEND_DRM,
};

extern const struct rendering_vtable supported_backends[];
extern const size_t backend_count;
