#include "abort.h"
#include "termination.h"
#include "threads/ui.h"

#include <bits/pthreadtypes.h>
#include <ctype.h>
#include <pthread.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/poll.h>
#include <unistd.h>

#define MAX_CMD_LEN 1024

typedef void (*act_t)(struct ui_ctx *, char *target, size_t argc, char **argv);

struct command {
	char *target_name;
	char *action;
	size_t argc;
	char **argv;
};

struct parse_result {
	bool ok;
	union {
		struct command command;
		char *err;
	} val;
};

struct cmd_ctx {
	struct ui_ctx *ui_ctx;
	int cancellation_fd;
};

static void *cmd_inner(void *arg);
static struct parse_result parse(char *input);
static char *run(struct ui_ctx *, const struct command *);
static char *eat_whitespace(char *);
static char **collect_args(char *, size_t *, char **);

static void act_create(struct ui_ctx *, char *target, size_t argc, char **argv);

static bool parse_color(const char *in, struct color *out);

static struct {
	char *name;
	act_t hook;
} actions[] = {
	{ "CREATE", act_create },
};

void *cmd_thread(void *arg)
{
	struct cmd_ctx ctx;

	int cancellation_pipe[2];
	pipe(cancellation_pipe);

	// TODO: print buffer dimensions to stdout in JSON format
	ctx.ui_ctx = (struct ui_ctx *)arg;
	ctx.cancellation_fd = cancellation_pipe[0];

	pthread_t reader;
	if (pthread_create(&reader, NULL, cmd_inner, &ctx) != 0) {
		FATAL_ERR("input: failed to spawn thread: %s", STR_ERR);
	}

	term_block();

	uint8_t data = 0;
	if (write(cancellation_pipe[1], &data, 1) != 1) {
		fprintf(stderr, "input: failed to cancel reader thread\n");
	} else {
		pthread_join(reader, NULL);
	}

	return NULL;
}

static void *cmd_inner(void *arg)
{
	struct cmd_ctx *ctx = (struct cmd_ctx *)arg;
	int cancellation_fd = ctx->cancellation_fd;

	struct pollfd fds[2];

	fds[0].fd = cancellation_fd;
	fds[0].events = POLLIN;

	fds[1].fd = 0; // stdin
	fds[1].events = POLLIN;

	char line[MAX_CMD_LEN];

	for (;;) {
		if (poll(fds, 2, 0) < 0)
			FATAL_ERR("commands: poll failed: %s", STR_ERR);

		if (fds[0].revents &= POLLIN)
			break;

		if (!(fds[1].revents & POLLIN))
			continue;

		if (!fgets(line, MAX_CMD_LEN, stdin))
			FATAL_ERR(
			    "commands: couldn't read from stdin: %s", STR_ERR);

		struct parse_result r = parse(line);

		if (!r.ok) {
			printf("parsing failed: %s\n", r.val.err);
			free(r.val.err);
			continue;
		}

		char *err = run(ctx->ui_ctx, &r.val.command);
		if (err) {
			printf("%s\n", err); // write to the client
			fprintf(stderr, "cmd: run: %s\n",
			    err); // write to the debug log
		}
	}

	return NULL;
}

static struct parse_result parse(char *input)
{
	// 0-copy tedious parser crap. it is so easy to get this wrong. I would
	// use a parser generator or another prog lang, but it's not really
	// worth it.
	//
	// Commands look like this:
	// <target>: <action> <args>...
	//
	// These can be separated by as many ASCII whitespace characters as
	// desired. Don't use broader UTF-8 whitespace; I'll cry.
	struct parse_result result;

	// Find the :
	char *target_end = input;
	for (; *target_end != ':' && *target_end != '\0'; target_end++)
		;

	if (target_end == input || *target_end == '\0') {
		result.ok = false;
		result.val.err = strdup("target name must be provided.");
		return result;
	}

	*target_end = '\0';

	char *action = eat_whitespace(&target_end[1]);

	if (*action == '\0') {
		result.ok = false;
		result.val.err = strdup("action required");
		return result;
	}

	char *action_end = &action[1];
	while (*action_end != '\0' && !isspace(*action_end))
		action_end++;

	result.ok = true;

	if (*action_end != '\0') {
		*action_end = '\0';
		result.val.command.argc = 0;
		result.val.command.argv = collect_args(
		    &action_end[1], &result.val.command.argc, NULL);
	}

	result.val.command.target_name = input;
	result.val.command.action = action;
	// XXX: set command.args!

	return result;
}

static char *run(struct ui_ctx *ctx, const struct command *c)
{
	for (size_t i = 0; i < sizeof(actions) / sizeof(*actions); i++) {
		if (strcmp(c->action, actions[i].name) == 0) {
			// lol. should malloc an argv for each command. gotta do
			// that for bg color
			actions[i].hook(ctx, c->target_name, c->argc, c->argv);
			if (c->argv)
				free(c->argv);
			return NULL;
		}
	}

	char *err_buf = malloc(512);
	snprintf(err_buf, 512, "no such action found: %s", c->action);
	return err_buf;
}

static char *eat_whitespace(char *x)
{
	while (*x != '\0' && isspace(*x))
		x++;

	return x;
}

static char **collect_args(char *input, size_t *argc, char **argv)
{
	input = eat_whitespace(input);
	if (*input == '\0')
		return argv;

	char *input_end = input;
	while (!isspace(*input_end) && *input_end != '\0')
		input_end++;

	(*argc)++;
	size_t alloc_size =
	    *argc * sizeof(char *); // TODO: FATAL_ERR on overflow
	argv = realloc(argv, alloc_size);
	if (!argv)
		FATAL_ERR("commands: collect_args: OOM");

	argv[*argc - 1] = input;

	if (*input_end != '\0') {
		*input_end = '\0';
		return collect_args(&input_end[1], argc, argv);
	}

	return argv;
}

static void act_create(
    struct ui_ctx *ctx, char *target, size_t argc, char **argv)
{
	printf("CREATE: %s\n", target);
	if (argc != 1) {
		printf("failure: CREATE requires exactly one argument\n");
		return;
	}

	struct color fill;
	if (!parse_color(argv[0], &fill)) {
		printf("failure: given color is not valid.\n");
		return;
	}

	enum ui_failure r = ui_pane_create(ctx, target, fill);
	if (r != UI_OK) {
		fprintf(stderr, "act_create: failed: %s\n", ui_failure_str(r));
		printf("failed: %s\n", ui_failure_str(r));
	}
}

static bool parse_color(const char *in, struct color *out)
{
	if (in[0] != '#')
		return false;

	if (strlen(in) != 7)
		return false;

	errno = 0;
	long result = strtol(&in[1], NULL, 16);
	if (errno != 0)
		return false;

	out->r = result >> 16;
	out->g = result >> 8 & 0xff;
	out->b = result & 0xff;

	return true;
}
