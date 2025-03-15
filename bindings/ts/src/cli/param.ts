const errorSymbol = Symbol("error");

type ParamError = { message: string; [errorSymbol]: "tag" };

export interface Parameter<T> {
  parse(value: string): T | ParamError;
}

export function error(message: string): ParamError {
  return { message, [errorSymbol]: "tag" };
}

// i could use some object validation library. but i dont care
export const isError = (error: unknown): error is ParamError =>
  typeof error === "object" &&
  error !== null &&
  errorSymbol in error &&
  error[errorSymbol] == "tag" &&
  "message" in error &&
  typeof error.message === "string";

export const StringParameter: Parameter<string> = { parse: (str) => str };
export const NumberParameter: Parameter<number> = {
  parse(str) {
    try {
      return parseInt(str);
    } catch {
      return error(`${str} is not a number.`);
    }
  },
};

export function optional<T>(parameter: Parameter<T>): Parameter<T | undefined> {
  return {
    parse(str) {
      const result = parameter.parse(str);
      if (isError(result)) return undefined;
      return result;
    },
  };
}
