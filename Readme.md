# Awesome-Libs Parser

The goal of this project is to create a tool which aggregates data from various awesome lists available on Github, such as [https://github.com/sorrycc/awesome-javascript](https://github.com/sorrycc/awesome-javascript).

To this end we first build a small Markdown parser to convert a raw markdown into an AST. Then we traverse the tree and collect the data we want.
