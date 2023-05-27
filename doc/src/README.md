# Presentation

This book will introduce you to parsing and transliteration, using Beans. Beans is written in 
[Rust](https://www.rust-lang.org), and henceforth this book will assume familiarity with this 
language. However, this book makes no assumptions on prior knowledge on parsing techniques. The
end goal is to allow someone who has never written or used a parser to quickly become productive
at writing and using parsing libraries.

Beans aims at being a general-purpose parser and lexer library, providing both enough
performance so that you should never *need* something faster (even though these options exist),
and enough expressiveness so that you never get stuck while using your parser. See the
[tradeoffs](details/tradeoff.md) section for more details.

Beans is free and open source, dual licensed MIT or GPL3+, at your choice.