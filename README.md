# json-syntax

This library parses JSON into a `Value` type that is consistent with the
ABNF described in [RFC 7159](https://tools.ietf.org/html/rfc7159). The
parser is about six times faster than the parser that `aeson` provides.
This parser is however, non-resumable, so if resumable parsing is
important, `aeson` should be preferred. Results from the benchmark suite:

    benchmarked json/twitter/100
    time                 208.6 μs   (207.1 μs .. 210.1 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 211.8 μs   (210.7 μs .. 213.3 μs)
    std dev              4.493 μs   (3.299 μs .. 7.422 μs)
    
    benchmarked aeson/twitter/100
    time                 1.284 ms   (1.267 ms .. 1.296 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 1.282 ms   (1.276 ms .. 1.292 ms)
    std dev              26.07 μs   (17.87 μs .. 40.01 μs)

This library does not include any functions or typeclasses to help users
convert data from `Value`, the RFC-7159-inspired syntax tree data type.
Such functions and typeclasses are outside the scope of this library. If
anyone writes a library that helps users marshal their data in this way,
open a issue so that the `json-syntax` documentation can point users to it.
