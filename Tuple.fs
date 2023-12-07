module Tuple
    let first (a, _b) = a
    let second (_a, b) = b

    let init a b = (a, b)
