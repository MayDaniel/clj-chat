# Clj-Chat

A simple chat server.

## Usage

    (use 'clj-chat.core)
    (-main)

Connect to the server.

## Extending

See [this](http://github.com/MayDaniel/clj-chat/blob/master/src/clj_chat/plugins/example.clj) for an example plug-in. You'll then need to add the namespace postfix (for example, namespace foo.bar.baz would have the postfix baz) to the set in [commands.config](http://github.com/MayDaniel/clj-chat/blob/master/commands.config).

## Installation

Clone the repository.

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
