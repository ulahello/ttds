#include "threads/ui.h"

#include "../rendering/rendering.h"
#include "abort.h"
#include "termination.h"

#include <assert.h>
#include <bits/pthreadtypes.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/poll.h>
#include <unistd.h>

#define MAX_PANES 1024

struct pane {
	char *name;
	struct canvas *canvas;
	pthread_mutex_t lock;
};

static void *rotate_panes(void *);

struct pane_storage {
	_Atomic size_t count;
	struct pane panes[MAX_PANES];
};

struct ui_ctx {
	struct rendering_ctx *r_ctx;
	struct pane_storage panes;

	int cancellation_fd;
};

char *ui_failure_strs[] = {
	[UI_OK] = "no failure",
	[UI_DUPLICATE] = "duplicate pane",
	[UI_OOM] = "oom",
};

char *ui_failure_str(enum ui_failure f)
{
        return ui_failure_strs[f];
}

struct ui_ctx *ui_ctx_new(void)
{
	struct ui_ctx *ctx = malloc(sizeof(struct ui_ctx));
	if (!ctx)
		FATAL_ERR("ui: failed to allocate ctx");

	ctx->r_ctx = rendering_init();
	rendering_ctx_log(ctx->r_ctx);

	ctx->panes.panes[0].canvas = canvas_init(ctx->r_ctx);
	ctx->panes.panes[0].name = strdup("root");

	if (!ctx->panes.panes[0].name)
		FATAL_ERR("ui: ctx_new: pane creation OOM");

	pthread_mutex_init(&ctx->panes.panes[0].lock, NULL);

	atomic_init(&ctx->panes.count, 1);

	struct color bg = {
		.r = 0x22,
		.g = 0x22,
		.b = 0x88,
	};

	rendering_fill(ctx->panes.panes[0].canvas, bg);

	// Make sure that the cancellation fd is invalid until ui_thread gets
	// around to making the pipe.
	ctx->cancellation_fd = -1;

	return ctx;
}

void *ui_thread(void *arg)
{
	struct ui_ctx *ctx = arg;

	int cancellation_pipe[2];
	if (pipe(cancellation_pipe) != 0)
		FATAL_ERR("ui: pipe: %s", STR_ERR);

	ctx->cancellation_fd = cancellation_pipe[0];

	pthread_t pane_handle;
	pthread_create(&pane_handle, NULL, rotate_panes, ctx);

	term_block();
	write(cancellation_pipe[1], "\0", 1);
	pthread_join(pane_handle, NULL);

	fprintf(stderr, "rendering: terminating\n");

	size_t len =
	    atomic_load_explicit(&ctx->panes.count, memory_order_acquire);
	for (size_t i = 0; i < len; i++) {
		canvas_deinit(ctx->panes.panes[i].canvas);
		free(ctx->panes.panes[i].name);
	}

	rendering_cleanup(ctx->r_ctx);
	free(ctx);

	return NULL;
}

static void *rotate_panes(void *arg)
{
	int r;

	struct ui_ctx *ctx = arg;
	struct pollfd fds[1];
	fds[0].fd = ctx->cancellation_fd;
	fds[0].events = POLLIN;

	for (size_t i = 0;; i++) {

		size_t len = atomic_load(&ctx->panes.count);
		size_t idx = i % len;
		struct pane *p = &ctx->panes.panes[idx];

		r = pthread_mutex_lock(&p->lock);
		if (r != 0)
			FATAL_ERR("ui: couldn't take lock: %s", STR_ERR);

		fprintf(stderr, "ui: flipping pane: %s\n", p->name);
		rendering_show(ctx->r_ctx, p->canvas);

		r = pthread_mutex_unlock(&p->lock);
		if (r != 0)
			FATAL_ERR("ui: couldn't return lock: %s", STR_ERR);

		// Do a cancellable 1-second sleep.
		poll(fds, 1, 1000);
		if (fds[0].revents &= POLLIN)
			break;
	}

	return NULL;
}

/* Create a pane. */
enum ui_failure ui_pane_create(
    struct ui_ctx *ctx, char *name, struct color fill)
{
	// Really huge assumption that only one thread is calling into this or
	// the deletion function at once.

	size_t idx =
	    atomic_load_explicit(&ctx->panes.count, memory_order_acquire);

	for (size_t i = 0; i < idx; i++) {
		struct pane *p = &ctx->panes.panes[i];
		pthread_mutex_lock(&p->lock);
		if (strcmp(p->name, name) == 0) {
			pthread_mutex_unlock(&p->lock);
			return UI_DUPLICATE;
		}

		pthread_mutex_unlock(&p->lock);
	}

	assert(idx < MAX_PANES);
	struct pane *p = &ctx->panes.panes[idx];

	p->name = strdup(name);
	if (!p->name)
		return UI_OOM;

	p->canvas = canvas_init(ctx->r_ctx);
	if (!p->canvas) {
		free(p->name);
		return UI_OOM;
	}

	pthread_mutex_init(&ctx->panes.panes[idx].lock, NULL);
	rendering_fill(p->canvas, fill);

	atomic_store_explicit(&ctx->panes.count, idx + 1, memory_order_release);

	return UI_OK;
}
