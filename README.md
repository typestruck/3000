# 3000

3000 is a IM bot, text generation and summary service, written in Rust and intended to be embedded in other projects

## Available functions

### generate(what, max_characters)

Text generation. Parameters are as follows:

* _what_ indicates the kind of text to generate. Current options are name (a sentence in one of the formats &lt;adjective&gt; [, &lt;adjective&gt;] &lt;noun&gt; / [&lt;adjective&gt;] &lt;noun&gt; [who | that] [&lt;adverb&gt;] &lt;verb&gt; [&lt;adjective&gt;] [&lt;noun&gt;] [&lt;other&gt;]) and description (markov chain generated text using the contents of src/data/source-text)

* _max\_characters_ is the maximum number of characters to generate

