# Email Parse

A fast, RFC-compliant email address validator and parser.

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Performance](#performance)
5. [Links](#links)
6. [Patches](#patches)
7. [License](#license)

## Overview

Email address validation is deceptively hard.

What is a valid email address? You'll find many different answers for this
online, and multiple RFCs that sometimes conflict. [RFC 5321](https://datatracker.ietf.org/doc/html/rfc5321)
is the authoritative document.

This library complies with RFC 5321 and includes a comprehensive test suite. It
provides validation and parsing that's much more robust than most regex-based
solutions.

And it's fast.

## Installation

Email Parse is available on [Ultralisp](https://ultralisp.org/) and is easy to
install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install Email Parse:

```lisp
CL-USER> (ql:quickload :email-parse)
```

## Usage

You can parse email address strings:

```lisp
CL-USER> (require :email-parse)
NIL

CL-USER> (email-parse:parse "simple@example.com")
"simple"
"example.com"

CL-USER> (email-parse:parse "simple@example.com" :plist t)
(:LOCAL-PART "simple" :DOMAIN "example.com")
```

Or octet sequences:

```lisp
CL-USER> (require :email-parse)
NIL

CL-USER> (email-parse:parse-octets
          (trivial-us-ascii:ascii-string-code
           '(simple-array (unsigned-byte 8) (*))
           "simple@example.com")
          0
          (length "simple@example.com"))
"simple"
"example.com"

CL-USER> (email-parse:parse-octets
          (trivial-us-ascii:ascii-string-code
           '(simple-array (unsigned-byte 8) (*))
           "simple@example.com")
          0
          (length "simple@example.com")
          :plist t)
(:LOCAL-PART "simple" :DOMAIN "example.com")
```

## Performance

Parsing is fairly quick, and runs in linear time, though the parser does cons:

```lisp
CL-USER> (time (dotimes (c 100000)
                 (email-parse:parse-octets
                  (trivial-us-ascii:ascii-string-code
                   '(simple-array (unsigned-byte 8) (*))
                   "simple@example.com")
                  0
                  (length "simple@example.com"))))
Evaluation took:
  0.152 seconds of real time
  0.148390 seconds of total run time (0.144645 user, 0.003745 system)
  97.37% CPU
  504,555,988 processor cycles
  113,608,592 bytes consed

NIL
```

## Links

* [Repository](https://sr.ht/~pyramidion/email-parse/)

## Patches

Patches are welcome.

## License

Email Parse is licensed under the two-clause BSD license.

See LICENSE.
