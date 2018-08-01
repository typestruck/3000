# Bender

Bender is a IM bot, text generation and summary service, primarily intended to help my other projects.  

## Getting started 

Bender is written in Scheme, using the [CHICKEN](https://www.call-cc.org/) compiler.

* Install dependencies(aka, eggs) with ./install-dependencies.sh

* Compile with ./compile.sh

* Run the tests with ./test.sh

* Then ./bender for a local server running on port 1337

## Endpoints

### /generate?what=&lt;name | description&gt;&max-chars=&lt;integer&gt;

Text generation. Parameters are as follows:

* _what_ indicates the kind of text to generate. Current options are name, which produces a sentence in the formats &lt;adjective&gt; [, &lt;adjective&gt;] &lt;noun&gt; or [&lt;adjective&gt;] &lt;noun&gt; [who | that] [&lt;adverb&gt;] &lt;verb&gt; [&lt;adjective&gt;] [&lt;noun&gt;] [&lt;other&gt;] using the files of same name in /data; and description, which produces a markov chain generated text using the contents of data/source-text.

* _max-chars_ is the maximum number of characters to generate.

