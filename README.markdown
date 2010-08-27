# Clj-Chat

A simple chat server.

## Usage

    (use 'clj-chat.core)
    (-main)

Connect to the server.

## Extending

See
[this](http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/echo.clj)
for an example plug-in.

To load your plug-in, add the namespace postfix (for example,
namespace foo.bar.baz would have the postfix baz) to the set in
[plugins.config](http://github.com/MayDaniel/clj-chat/blob/master/plugins.config).

The var 'plugins' is created upon calling -main, allowing you to
load/unload/reload plugins without killing the server.

## Installation

Clone the repository.

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
