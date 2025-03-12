# Welcome!

When Zajj saw the posters, she was reportedly quite confused by the fact that we
were so aggressively advertising a graphics hackathon while also issuing the
all-caps injunction to "STOP DOING GRAPHICS." This is because all computer
graphics programs are either 1) beautiful results of mathematical abstraction and truly
elegant artifacts of human expression, or 2) the most cursed bullshit you've
seen in your life, and this hackathon fits firmly into number 2. Stop
doing graphics in general, but, while it's here, you may as well have some fun.

## Software Structure

Without spending too much time on the internals (though do ask me about them if
you're curious; they are *deeply* fucked up), the 'canvas' that you'll hopefully
draw on is really three separate but related bits of software.

1. There's the code that controls the screen. This bears no mention here,
   but there be dragons.
2. The code that provides an **HTTP API.** If you're you're familiar with what this
   is and how it operates, you can skim this document. Otherwise, you may want
   to keep reading.
3. Language-specific **API bindings.** If you're unfamiliar with how HTTP works,
   you'll definitely care about these.

## What the Fuck is an HTTP

HTTP is the protocol that communicates with websites. Servers by default have no
concept of what 'the web' is, instead communicating over lower-level protocols
called TCP and UDP. These allow computers to send streams of data to each other,
and if that stream is structured in a certain way, it's considered to be HTTP.
As an example, here's an HTTP request you probably send some variation of quite
frequently:

```http
GET /
Host: google.com
```

A variation of this goes out from your browser to google.com's servers, which
then read that you're asking to `GET` the page at `/` on the given `Host`, and
respond with (among other things) the content of the webpage.

Now that you're on google.com, you'll spend some time typing something into the
search bar, and you'll press search, requesting a new page:

```http
GET /search?q=my%20query
Host: google.com
```

Here, we're asking for `/search` with the **parameter** `q` set to `my query`.
(Note that instead of a space, we have the string `%20`. This is important if
you want to write raw HTTP requests for this event, but otherwise don't worry
about it.)

There are two more complications you should have in your back pocket:

1. The line of text that says `Host: google.com` is just a specific incarnation
   of something called a **header.** These can be whatever you want and contain
   whatever data you want. In the HTTP API here, most requests require an
   `Auth:` header that allows clients to authenticate themselves with the server.
2. `GET` isn't the only **HTTP method.** If you're intending to change the state
   of a server, e.g., maybe draw something on a device it controls, you'll use
   `POST`. Other methods are named intuitively enough. E.g., `DELETE` is meant to
   delete a resource.

Having a prefect understanding of all of this is by no means necessary to start
writing code, but having a familiarity with these concepts will make things a
lot more understandable if (when!) they go wrong.

## Language Bindings

You do *not* need to write HTTP requests to draw software. Instead, we've
written some code to allow you to use Python, JavaScript, or TypeScript to
accomplish the same tasks. If you want to use a different language (somebody
*please* use APL, it would be so funny), you'll need to bind to the [raw
API](../web/README.md#reference). Otherwise,

1. Python users, look [here](./python.md).
2. JS or TS users, look [here](./js.md).