// extracted from npm:uuid
const UUID_REGEX = /^(?:[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}|00000000-0000-0000-0000-000000000000|ffffffff-ffff-ffff-ffff-ffffffffffff)$/i

function validate(uuid: string) {
  return UUID_REGEX.test(uuid);
}

const HASH_ENCODED = encodeURIComponent("#");
export function normalizeColor(color: string) {
  if (color.startsWith("#")) {
    color = color.replace("#", HASH_ENCODED);
  }

  if (!color.startsWith(HASH_ENCODED)) {
    color = HASH_ENCODED + color;
  }

  // dealing with colors RGB, not RRGGBB
  if (color.length === "%23".length + 3) {
    const [r, g, b] = color.split("").slice(HASH_ENCODED.length)
    return `${HASH_ENCODED}${r}${r}${g}${g}${b}${b}`;
  }

  return color;
}

// this is a bad mock that does Nothing.
export const settings = { mock: false };

/**
 * Makes a request to the base URL. This is automatically `POST`.
 */
async function makeRequest(
  baseURL: string,
  path: string,
  options?: RequestInit,
): Promise<Response> {
  const fetchURL = new URL(path, baseURL);

  if (settings.mock) {
    console.debug(path, options);

    return new Response(
      crypto.randomUUID(),
      {
        status: 200,
      },
    );
  } else {
    return await fetch(fetchURL, {
      method: "POST",
      ...options,
    });
  }
}

interface CommonDrawOptions {
  color: string;
}

interface RectangleDrawOptions extends CommonDrawOptions {
  type: "rect";
  w: number;
  h: number;
  x: number;
  y: number;
}

interface CircleDrawOptions extends CommonDrawOptions {
  type: "circle";
  x: number;
  y: number;
  r: number;
}

interface LineDrawOptions extends CommonDrawOptions {
  type: "line";
  x: number;
  y: number;
  x2: number;
  y2: number;
}

interface CopyRectOptions extends CommonDrawOptions {
  type: "copy_rect";
  w: number;
  h: number;
  x: number;
  y: number;
  x2: number;
  y2: number;
}

interface Bezier2Options extends CommonDrawOptions {
  type: "bezier2";
  x0: number;
  y0: number;
  x1: number;
  y1: number;
  x2: number;
  y2: number;
}

interface TriangleOptions extends CommonDrawOptions {
  type: "triangle";
  x0: number;
  y0: number;
  x1: number;
  y1: number;
  x2: number;
  y2: number;
}

type DrawOptions = CircleDrawOptions | RectangleDrawOptions | LineDrawOptions | CopyRectOptions | Bezier2Options | TriangleOptions;

interface InitOptions {
  name?: string;
  color: string;
  baseURL: string;
}

export class Pane {
  #name: string;
  #internalUUID: string;
  #baseURL: string;

  /**
   * Creates a new pane. Requires an already created pane from UUID.
   * If you need a new UUID, use `Pane.init`.
   * @param uuid The UUID of the existing pane.
   * @param internalUUID The UUID given from the server (the "private" UUID)
   * @param baseURL The URL of the base server (with port, no trailing slash)
   */
  constructor(name: string, internalUUID: string, baseURL: string) {
    this.#name = name;
    this.#internalUUID = internalUUID;
    this.#baseURL = baseURL;
  }

  /**
   * Creates a new pane.
   * @param name the (display) name of the pane
   * @param color the initial color of the pane (TODO: resolve this to be hash escaped?)
   */
  static async init({ name, color, baseURL }: InitOptions): Promise<Pane> {
    // make a random name if unspecified, with the default Blue color.
    const resolvedName = name ?? crypto.randomUUID();

    const response = await makeRequest(
      baseURL,
      `pane/${resolvedName}/create?color=${normalizeColor(color ?? "#0000ff")}`,
    );

    const textResponse = await response.text();

    if (validate(textResponse)) {
      return new Pane(resolvedName, textResponse, baseURL);
    } else {
      throw new Error(`An error occurred: '${textResponse}'`);
    }
  }

  async draw(optionsParam: DrawOptions) {
    // We explicitly don't deconstruct type to get the type inference.
    const { color, type } = optionsParam;
    let options: Partial<DrawOptions> = optionsParam;

    delete options["type"];
    delete options["color"];

    const entries = Object.entries(options).map(([k, v]) => `${k}=${v}`).join("&");

    let drawRequest: string;
    drawRequest = `${type}?color=${normalizeColor(color)}&${entries}`;

    const response = await makeRequest(
      this.#baseURL,
      `pane/${this.#name}/${drawRequest}`,
      {
        headers: {
          "Auth": this.#internalUUID,
        },
      },
    );

    if (response.status !== 200) {
      throw new Error(await response.text());
    }
  }

  /** Gets the count of all current panes. **Note**: Must be admin to see this. */
  async count(): Promise<number> {
    const response = await makeRequest(this.#baseURL, `raw/root:%20COUNT`, { headers: { "Auth": this.#internalUUID }})

    if (response.status !== 200) {
      throw new Error(await response.text())
    }

    return parseInt(await response.text());
  }

  async delete() {
    const response = await makeRequest(
      this.#baseURL,
      `pane/${this.#name}`,
      {
        method: "DELETE",
        headers: {
          "Auth": this.#internalUUID,
        },
      },
    );

    if (response.status !== 200) {
      throw new Error(await response.text());
    }
  }

  name(): string {
    return this.#name;
  }

  uuid(): string {
    return this.#internalUUID;
  }
}
