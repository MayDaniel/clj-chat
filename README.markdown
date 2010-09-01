# Mongo

Experimental branch: Refactoring clj-chat to use [MongoDB](http://www.mongodb.org/) in place of
refs where possible. (I don't think Writers can be serialized.)

MongoDB is required, but it's simple to [set up](http://www.mongodb.org/display/DOCS/Quickstart).

# Clj-Chat

A simple chat server.

## Usage

Start MongoDB.

    (use 'clj-chat.core)
    (-main)

Connect to the server.

## Extending

There are a few example plug-ins [here](http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/).

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
