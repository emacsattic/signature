# Signature survey

Generate a signature survey for a glob pattern of your ruby project.
Only ruby supported so far and support is spotty because of ruby's
complicated syntax.

Inspired by: [http://c2.com/doc/SignatureSurvey/].

![Signatures](doc/img/screenshot.png)

- <> denoted classes
- {} denotes methods
- [] denotes loops
- () denotes conditionals
- ' denotes comments
- . denotes other types of rows

Only ruby supported so far, and support is spotty at best, but this is
proof of concept code. I want to make it possible to navigate to the
files through the `*signature*` buffer. And add some highlighting.

Other languages would be nice as well. javascript and coffeescript
would probably be first.

## Installation

Installation alternatives:

- Clone repository and add to `load-path`.

## Usage

Issue `M-x signature-report` and supply a glob pattern to the ruby
files to display signatures for.
