import { Pane, settings } from "./mod";
import randomColor from "randomcolor";
import { parseArgs } from "node:util";
import {
  error,
  isError,
  NumberParameter,
  type Parameter,
  StringParameter,
} from "./cli/param.ts";
import repl from "node:repl";
import process from 'node:process';

function consoleError(message: string) {
  console.error(`%c${message}`, "color: red");
}

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    mock: {
      type: "boolean",
      short: "m",
    },
  },
  args: process.argv.slice(2)
});
const baseURL = positionals[0]?.trim();

if (baseURL === null || baseURL === undefined) {
  consoleError(
    `The base URL is not specified (usage: executable <url>, i.e. http://127.0.0.1:8080)`,
  );
  process.exit(1);
}

try {
  new URL(baseURL);
} catch {
  consoleError(
    `The base URL ${baseURL} is malformed (usage: executable <url>, i.e. http://127.0.0.1:8080)`,
  );
  process.exit(1);
}

settings.mock = values.mock ?? false;

const panes = new Map<number, Pane>();
let paneTicker = 0;

const PaneParameter: Parameter<number> = {
  parse(str) {
    const param = NumberParameter.parse(str);
    if (typeof param === "object") return param;

    if (panes.has(param)) return param;

    return error(`Pane ${param} does not exist! Create a new one with "pane."`);
  },
};

type Command<T extends unknown[]> = {
  params?: { [K in keyof T]: Parameter<T[K]> };
  exec(...args: T): Promise<void> | void;
};

const commands: Record<string, Command<unknown[]>> = {
  help: {
    exec() {
      console.error(
        "note: if no [color] is specified, a 'pretty' random color is chosen.",
      );
      console.error();
      console.error(
        "pane             [color]: creates a new pane; returns its CLI ID (and an internal UUID)",
      );
      console.error("circle p x y r   [color]: create a new circle");
      console.error("rect   p x y w h [color]: create a new rectangle");
      console.error("delete p                : deletes a pane");
      console.error("list                    : lists all panes");
    },
  },
  pane: {
    // TODO: color parameter
    params: [StringParameter],
    async exec(color) {
      const pane = await Pane.init({ baseURL, color: color ?? randomColor() });

      const tick = paneTicker++;

      panes.set(tick, pane);

      console.log("Pane ID:", tick);
    },
  } satisfies Command<[string]>,
  circle: {
    params: [
      PaneParameter,
      NumberParameter,
      NumberParameter,
      NumberParameter,
      StringParameter,
    ],
    async exec(pane, x, y, r, color) {
      await panes.get(pane)?.draw({
        type: "circle",
        x,
        y,
        r,
        color: color ?? randomColor(),
      });
    },
  } satisfies Command<[number, number, number, number, string]>,
  rect: {
    params: [
      PaneParameter,
      NumberParameter,
      NumberParameter,
      NumberParameter,
      NumberParameter,
      StringParameter,
    ],
    async exec(pane, x, y, w, h, color) {
      await panes.get(pane)?.draw({
        type: "rectangle",
        x,
        y,
        w,
        h,
        color: color ?? randomColor(),
      });
    },
  } satisfies Command<[number, number, number, number, number, string]>,
  delete: {
    params: [PaneParameter],
    async exec(pane) {
      await panes.get(pane)?.delete();
      panes.delete(pane);
    },
  } satisfies Command<[number]>,
  list: {
    exec() {
      console.log([...panes].join(", "));
    },
  },
};

repl.start({
  prompt: "> ",
  eval: async (command, context, filename, callback) => {
    command = command?.trim()
    if (!command) {
      consoleError("Please specify a command.");
      return callback(null)
    }

    const { positionals: [name, ...positionals] } = parseArgs({
      args: command?.split(" "),
      allowPositionals: true,
    });

    if (name in commands) {
      const command = commands[name];

      if ((command.params?.length ?? 0) < positionals.length) {
        if ((command.params?.length ?? 0) === 0) {
          consoleError(`This command does not take parameters.`);
        } else {
          consoleError(
            `${positionals.length} args passed in, but up to ${
              command.params?.length ?? 0
            } params needed.`,
          );
        }
        return callback(null);
      }

      const args = positionals.map((positional, i) =>
        command.params?.[i].parse(positional)
      );

      for (const arg of args) {
        if (isError(arg)) {
          consoleError(arg.message);
          return callback(null);
        }
      }

      try {
        await command.exec(...args);
      } catch (e) {
        consoleError(`An error occurred: ${e}`);
      }
    } else {
      consoleError(`Command ${name} not found. Type "help" for help.`);
    }

    return callback(null)
  },
});
