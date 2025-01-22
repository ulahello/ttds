#include "testing.h"

#include "abort.h"
#include "rendering/canvas.h"

#include <assert.h>
#include <dirent.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#define PI 3.141592653589793238462
#define TAU (2. * PI)

static inline int32_t min(int32_t a, int32_t b)
{
	return a < b ? a : b;
}

const struct color BG = { .r = 0x3A, .g = 0x22, .b = 0xBD };
const struct color FG = { .r = 0xFF, .g = 0xFF, .b = 0xFF };

struct test {
	struct color fill_color;
	void (*draw_fn)(struct canvas *);
	const char *output_path;
	uint16_t width, height;
};

void run_these_tests(
    const char *dump_dir_path, const struct test tests[], size_t num_tests)
{
	// Open (or create) the dump directory. Test output paths are relative
	// to this.
	if (mkdir(dump_dir_path,
		S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH |
		    S_IXOTH) < 0) {
		if (errno != EEXIST)
			FATAL_ERR("Failed to create directory: %s: %s",
			    dump_dir_path, STR_ERR);
	}
	DIR *dump_dir = opendir(dump_dir_path);
	if (dump_dir == NULL)
		FATAL_ERR(
		    "Failed to open directory: %s: %s", dump_dir_path, STR_ERR);

	for (size_t i = 0; i < num_tests; i++) {
		// Initialize in-memory canvas.
		struct canvas *canvas =
		    canvas_init_bgra(tests[i].width, tests[i].height);
		if (canvas == NULL)
			FATAL_ERR("out of memory");

		// Clear canvas.
		rendering_fill(canvas, tests[i].fill_color);

		// Run the test.
		(tests[i].draw_fn)(canvas);

		// Save canvas as raw RGBA pixel data.
		rendering_dump_bgra_to_rgba(
		    canvas, dump_dir, dump_dir_path, tests[i].output_path);
		fprintf(stderr, "Wrote RGBA pixel data to file: %s/%s\n",
		    dump_dir_path, tests[i].output_path);

		canvas_deinit(canvas);
	}
}

void test_rects(struct canvas *c)
{
	// Draw tiny glider.
	const uint16_t glider[5][2] = {
		{ 1, 0 },
		{ 0, 1 },
		{ 0, 2 },
		{ 1, 2 },
		{ 2, 2 },
	};
	const uint16_t x0 = c->width * 7 / 10;
	const uint16_t y0 = c->height / 2;
	for (size_t i = 0; i < sizeof(glider) / sizeof(*glider); i++) {
		uint16_t size = 2;
		uint16_t x = x0 + glider[i][0] * size;
		uint16_t y = y0 + glider[i][1] * size;
		struct rect rect = { .w = size, .h = size, .x = x, .y = y };
		rendering_draw_rect(c, &rect, FG);
	}

	// Draw some rectangles.
	const struct rect rects[] = {
		{ 0 }, // infinitely thin rectangle, no effect
		{ .x = c->width / 5,
		    .y = c->height * 2 / 3,
		    .w = c->width / 3,
		    .h = c->height / 4 },
		{ .x = c->width / 4,
		    .y = c->height / 3,
		    .w = c->width / 7,
		    .h = 1 },
		{ .x = c->width / 2,
		    .y = c->height * 2 / 7,
		    .w = c->width / 9,
		    .h = c->height / 6 },
	};
	for (size_t i = 0; i < sizeof(rects) / sizeof(*rects); i++) {
		rendering_draw_rect(c, &rects[i], FG);
	}
}

void test_circles(struct canvas *c)
{
	const float gr = (sqrtf(5.) - 1.) / 2.;

	// TODO: causes infinite loop as of
	// 839a81d047fe6b3ffc867149804c7d6b413ecabf
	/* rendering_draw_circle(c, &(struct circle) { 0 }, BG); */

	// Draw some circles imitating a sunflower!
	const size_t rmax = 3;
	const float distance = rmax * 2.2;

	const size_t circle_count = PI *
	    powf((min(c->width, c->height) - rmax) / 2., 2.) /
	    powf(distance + rmax, 2.);
	for (size_t i = 1; i <= circle_count; i++) {
		float p = (float)i / circle_count;
		float px = sqrt(p) * cosf(TAU * gr * i);
		float py = sqrt(p) * sinf(TAU * gr * i);
		float x = (1. + px) * ((float)c->width / 2. - rmax) + rmax;
		float y = (1. + py) * ((float)c->height / 2. - rmax) + rmax;
		struct circle circle = {
			.x = roundf(x), .y = roundf(y), .r = 1. + rmax * p
		};
		rendering_draw_circle(c, &circle, FG);
	}
}

void run_tests(const char *dump_dir)
{
	struct test tests[] = {
		{
		    .fill_color = BG,
		    .draw_fn = test_rects,
		    .output_path = "rects.data",
		    .width = 32,
		    .height = 32,
		},
		{
		    .fill_color = BG,
		    .draw_fn = test_circles,
		    .output_path = "circles.data",
		    .width = 128,
		    .height = 128,
		},
	};

	run_these_tests(dump_dir, tests, sizeof(tests) / sizeof(*tests));
}
