# Stream Overlay Tools
This application provides tools for streamers, primarily focusing on those who
stream via OBS on Twitch.  At present tooling for providing overlays and
alerting on donations to [Extra Life][1].

## Motivation
At the time of writing, no tools allowed for alerting on donations to
[Extra Life][1], though many targeted the needs of streamers who accepted
donations directly.  Furthermore, layout and overlay tools tended to be
externally hosted, and finding good toolsets for achieving this locally proved
difficult.

## Setup
Currently local compilation is required, though binary packages for Linux, OS X,
and Windows will be made available in the future.  In order to compile this
software working installations of [Haskell][2], [cabal-install][3], and [Elm][4]
are required.

Once the environment is set up, issue a `make` command and wait for the
application to compile.  Once it completes, the binary should exist at
`.cabal-sandbox/bin/stream-overlay` and the `--help` switch will provide usage:

    ```
    $ .cabal-sandbox/bin/stream-overlay --help
    stream-overlay
    -c CONFIG        --config=CONFIG           Configuration file
    -v               --verbose                 Enable verbose messages
    -V               --version                 Print version
    -h, -?           --help                    Show help

    -I               --disableirc              Disable IRC functionality
    -n USERNAME      --nick=USERNAME           Twitch username for the IRC bot to use
    -t TOKEN         --token=TOKEN             Twitch OAuth token (without "oauth:")
    -C CHANNEL       --channel=CHANNEL         Twitch channel to join

    -p PORT          --webport=PORT            Port for the webserver to run on

    -e EXTRALIFE_ID  --extralife=EXTRALIFE_ID  ExtraLife account to watch
    ```

## Usage
The `stream-overlay` utility listens on port 8000 by default.  Upon first run,
a notice will be displayed that a directory is being populated with content for
the service.

### Overlay
The utility provides a stream overlay which can be used with OBS via a browser
plugin (e.g. "CLR Browser" on Windows, "Linux Browser" on Linux).  The overlay
provided by default is appropriate for capture of a webcam and 4x3 display,
masking everything else out.

#### Mask Editing
The mask provided by the overlay depends on svg files to define areas of
opacity to be displayed.  The default file is intended to be simple enough to
edit by hand, though svg editors can be used to achieve more advanced masks.

#### Adding Layouts
If multiple layouts are needed, the `index.html`, `overlay.svg`, and
`overlay.css` can be copied to a new location and edited as needed.

For example, if a second overlay is desired in order to facilitate the
streaming of Nintendo DS capture, the above files can be copied to `ds.html`,
`ds.css`, and `ds.svg` respectively.  The `ds.html` needs only to be updated to
use `ds.css` rather than `overlay.css`; the `ds.css` file should use `ds.svg`
for its masking alongside any other stylistic changes to the mask which are
desired.  Finally the `ds.svg` file will be modified to mask the appropriate
regions of the stream layout.  The new layout will be available at (by default)
`http://localhost:8000/ds.html`

### Twitch IRC
If the utility is provided with a username and OAuth token of a Twitch bot
account (see Twitch's [dev docs][5] for more information on this) and the
channel which the bot should join, it will announce donations to Extra Life
when they are processed.  Moderation facilities are not implimented at this
time, but are planned.

[1]: https://www.extra-life.org/
[2]: https://www.haskell.org
[3]: https://hackage.haskell.org/package/cabal-install
[4]: http://elm-lang.org/
[5]: https://dev.twitch.tv/docs/v5/guides/irc/
