### Mongo branch

Experimental branch: Refactoring clj-chat to use [MongoDB](http://www.mongodb.org/) in place of
refs where possible. (I can't serialize Writers.)

MongoDB is required, but it's simple to [set up](http://www.mongodb.org/display/DOCS/Quickstart).

# Clj-Chat

A simple chat server.

## Usage

Start MongoDB.

    (use 'clj-chat.core)
    (-main)

Connect to the server on port 3333.

## Extending(http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/)

There are a few example plug-ins [here](http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/).

To load your plug-in, add the config to [plugins.clj](http://github.com/MayDaniel/clj-chat/plugins.clj) with the format {"plug-in-namespace-postfix" #{"command1" "command"}}.

The var 'plugins' is created upon calling -main, allowing you to
load/unload/reload plugins without killing the server.

## Installation

Clone the repository.

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
