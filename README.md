# Hangman

A laughably overengineered Hangman AI, using lockfree immutable bitset
indexes, a set algebra optimizer/evaluator, probabilistic sampling, caches,
controlled mutability, and a bunch of other neat stuff. Fully parallelized.

Check out hangman.bitset for the goodies!

## Usage

$ lein run

Without args, hangman prints a usage message.

$ lein run corpus.txt -v

Plays a bunch of random words from corpus.txt, logging each word and its score.

$ lein run corpus.txt words.txt

Plays all the words in words.txt.

It'll push ~200 games/sec with the default settings, which aim for an optimal
score. You can go way faster by choosing smaller sample sizes (-s) and a lower
target character probability (-p). If your dataset diverges significantly from
english entropy, try adjusting -p and the cache size.

## Building

$ lein uberjar
$ java -cp target/hangman-standalone-

## License

Copyright © 2013 Kyle Kingsbury

Distributed under the Eclipse Public License, the same as Clojure.
