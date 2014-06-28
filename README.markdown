# Clj-Chat

A simple chat server.

## Usage

MongoDB is required, but it's simple to [set up](http://www.mongodb.org/display/DOCS/Quickstart).

Start MongoDB.

    (use 'clj-chat.core)
    (-main)

Connect to the server on port 3333.

## Extending

There are a few example plug-ins [here](http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/).

To load your plug-in, add the config to [plugins.clj](http://github.com/MayDaniel/clj-chat/blob/master/plugins.clj) with the format {"plug-in-namespace-postfix" #{"command1" "command2"}}.

The var 'plugins' is created upon calling -main, allowing you to
load/unload/reload plugins without killing the server.

## License

Copyright (C) 2010 Daniel May

Distributed under the Eclipse Public License, the same as Clojure.
