#lang scribble/manual

@title{SICP Solutions — drdv}

@table-of-contents[]

@(require (for-label racket))
@; @(require (for-label (submod sicp-drdv/sicp2_part1 Exercise/2.1)))
@(require scribble/eval
		  (for-label "../sicp2_part1.rkt"))

@section[#:tag "chickens"]{Philadelphia Chickens}

Dancing tonight!


@section{Reprise}

See @secref{chickens}.

@; ============================================================

@section{Mitko}

@; ============================================================
@section{Chapter 2: Building Abstractions with Data}

@subsection{Exercise 2.1}

@defmodule[(submod sicp-drdv/sicp2_part1 Exercise/2.1)]{
  Rational arithmetic with sign normalization.
  Extends the basic @racket[make-rat] to handle all sign combinations
  so the denominator is always positive.
}

@defproc[(make-rat [n integer?] [d integer?]) pair?]{
  Constructs a reduced rational. Errors on zero denominator.
}

@defproc[(add-rat [x pair?] [y pair?]) pair?]{
  Adds two rationals.
}

@; ============================================================
@subsection{Exercise 2.2}

@defmodule[(submod sicp-drdv/sicp2_part1 Exercise/2.2)]{
  Points and segments in the plane.
}

@defproc[(make-segment [a pair?] [b pair?]) pair?]{
  Constructs a segment from point @racket[a] to point @racket[b].
}
