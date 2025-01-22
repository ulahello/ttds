#include "testing.h"

#include "abort.h"
#include "rendering/canvas.h"

#include <assert.h>
#include <dirent.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

const struct color BG = { .r = 0x3A, .g = 0x22, .b = 0xBD };
const struct color FG = { .r = 0xFF, .g = 0xFF, .b = 0xFF };

struct test {
	struct color fill_color;
	void (*draw_fn)(struct canvas *);
	const char *output_path;
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

	// Initialize in-memory canvas.
	struct canvas *canvas = canvas_init_bgra(32, 32);
	if (canvas == NULL)
		FATAL_ERR("out of memory");

	for (size_t i = 0; i < num_tests; i++) {
		// Clear canvas.
		rendering_fill(canvas, tests[i].fill_color);

		// Run the test.
		(tests[i].draw_fn)(canvas);

		// Save canvas as raw RGBA pixel data.
		rendering_dump_bgra_to_rgba(
		    canvas, dump_dir, tests[i].output_path);
		fprintf(stderr, "Wrote RGBA pixel data to file: %s/%s\n",
		    dump_dir_path, tests[i].output_path);
	}

	canvas_deinit(canvas);
}

void test_glider(struct canvas *c)
{
	const uint16_t glider[5][2] = {
		{ 1, 0 },
		{ 0, 1 },
		{ 0, 2 },
		{ 1, 2 },
		{ 2, 2 },
	};
	const uint16_t x0 = c->width * 7 / 10;
	const uint16_t y0 = c->height / 2;
	for (size_t i = 0; i < sizeof(glider) / sizeof(glider[0]); i++) {
		uint16_t x = x0 + glider[i][0];
		uint16_t y = y0 + glider[i][1];
		struct rect rect = { .w = 1, .h = 1, .x = x, .y = y };
		rendering_draw_rect(c, &rect, FG);
	}
}

void run_tests(const char *dump_dir)
{
	struct test tests[] = {
		{
		    .fill_color = BG,
		    .draw_fn = test_glider,
		    .output_path = "glider.data",
		},
	};

	run_these_tests(dump_dir, tests, sizeof(tests) / sizeof(tests[0]));
}
