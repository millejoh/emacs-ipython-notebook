[![Build Status](https://travis-ci.org/sigma/mocker.el.png?branch=master)](https://travis-ci.org/sigma/mocker.el)

Mocker.el is a mocking framework for Emacs lisp.

Its single entry point, `mocker-let` provides an `let` like interface to
defining mock objects. Actually, `mocker-let` is a wrapper around `flet`, which
can be seen as a way to manually generate mocks.

## Usage

### Basic usage

Let's start with a simple example:

```lisp
(mocker-let ((foo (x y z)
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 1 2 3)
     (foo 4 5 6)
     (bar 42)))
```

Each mock is defined in a function-style, and is associated with a set of
"records" that map expected inputs to desired outputs.

### Call order

By default, the order of definition within a mock has to be respected by the
wrapped code, so that in this situation it would be an error to observe `(foo
4 5 6)` before `(foo 1 2 3)`.

```lisp
(mocker-let ((foo (x y z)
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 4 5 6)
     (foo 1 2 3)
     (bar 42)))
```

In such a situation, you'll get a typed error with a message like
```
(mocker-record-error "Violated record while mocking `foo'. Expected input like: `(1 2 3)', got: `(4 5 6)' instead")
...
```

If order is not important, you can obtain the same effect as before by
specifying it:

```lisp
(mocker-let ((foo (x y z)
                  :ordered nil
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 4 5 6)
     (foo 1 2 3)
     (bar 42)))
```

### Counting calls

In many situations it can be pretty repetitive to list all the expected calls
to a mock. In some, the count might even be a range rather than a fixed number.
The `:min-occur` and `:max-occur` options allow to tune that. By default, they
are both set to 1, so that exactly 1 call is expected. As a special case,
setting `:max-occur` to nil will accept any number of calls.
An `:occur` shorthand is also provided, to expect an exact number of calls.

```lisp
(mocker-let ((foo (x)
                  ((:input '(1) :output 1 :min-occur 1 :max-occur 3))))
  (+ (foo 1) (foo 1)))
```

This example will accept between 1 and 3 calls to `(foo 1)`, and complain if
that constraint is not fulfilled.

Note the applied algorithm is greedy, so that as many calls as possible will
count as part of the earliest constraints.

### Flexible input/output

The examples above are fine, but they suppose input and output are just
constant expressions. A useful addition is the ability to match arbitrary input
and generate arbitrary output.

To this end, the `:input-matcher` and `:output-generator` options can be used
instead (actually think of `:input` and `:output` as convenience shortcuts for
constant matcher/generator).

```lisp
(mocker-let ((foo (x)
                  :ordered nil
                  ((:input-matcher 'oddp :output-generator 'identity :max-occur 2)
                   (:input-matcher 'evenp :output 0))))
  (+ (foo 1) (foo 2) (foo 3)))
```

Both `:input-matcher` and `:output-generator` values need to be functions (or
function symbols) accepting the same arguments as the mocked function itself.

## Extensibility

Each record definition actually builds a `mocker-record` object, that's
responsible for checking the actual behavior. By providing alternative
implementations of those records, one can adapt the mocking to special needs.

### Stubs

As a quick proof of concept, an implementation of a stub is provided with the
class `mocker-stub-record` which casualy ignores any input and always emits the
same output:

```lisp
(mocker-let ((foo (x)
                  ((:record-cls 'mocker-stub-record :output 42))))
  (foo 12345))
```

### Passthrough

In some occasions, you might want to mock only some calls for a function, and
let other calls invoke the real one. This can be achieved by using the
`mocker-passthrough-record`. In the following example, the first call to
`ignore` uses the real implementation, while the second one is mocked to return
`t`:

```lisp
(mocker-let ((ignore (x)
                        :records ((:record-cls mocker-passthrough-record
                                               :input '(42))
                                  (:input '(58) :output t))))
     (or (ignore 42)
         (ignore 58)))
```

### Provide your own

Customized classes can be provided, that can even introduce a mini-language for
describing the stub. This can be achieved by overloading
`mocker-read-record` correctly.

In case the customized record class is meant to be used in many tests, it might
be more convenient to use a pattern like:

```lisp
(let ((mocker-mock-default-record-cls 'mocker-stub-record))
  (mocker-let ((foo (x)
                    ((:output 42)))
               (bar (x y)
                    ((:output 1))))
    (+ (foo 12345)
       (bar 5 14))))
```

Also note that `mocker-stub-record` set their `:min-occur` to 0 and
`:max-occur` to nil, if not specified otherwise.

## Comparison to other mocking solutions

* el-mock.el (http://www.emacswiki.org/emacs/EmacsLispMock)

  * el-mock.el uses a small DSL for recording behavior, which is great for
    conciseness. mocker.el instead uses regular lisp as much as possible, which
    is more flexible.

  * el-mock.el does not allow recording multiple behaviors (the same call will
    always return the same value). This makes it difficult to use in real
    situation, where different call sites for the same function might have to
    behave differently.

## Examples

```lisp
;;; automatically answer some `y-or-n-p' questions
(mocker-let ((y-or-n-p (prompt)
                       ((:input '("Really?") :output t)
                        (:input '("I mean... for real?") :output nil))))
  ...)
```

```lisp
;;; blindly accept all `yes-or-no-p' questions
(mocker-let ((yes-or-no-p (prompt)
                          ((:record-cls mocker-stub-record :output t))))
  ...)
```

```lisp
;;; make `foo' generate the fibonacci suite, no matter how it's called
(mocker-let ((foo (x)
                  ((:input-matcher (lambda (x) t)
                    :output-generator (lexical-let ((x 0) (y 1))
                                        (lambda (any)
                                          (let ((z (+ x y)))
                                            (setq x y y z))))
                    :max-occur nil))))
  ...)
```
