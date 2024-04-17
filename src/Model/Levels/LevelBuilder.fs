namespace Tactix

module LevelBuilder =

    let p = Primitive "P"
    let q = Primitive "Q"
    let r = Primitive "R"
    let s = Primitive "S"
    let t = Primitive "T"

    let pq = Function (p, q)
    let qp = Function (q, p)
    let qr = Function (q, r)
    let pr = Function (p, r)
    let pqr = Function (p, Function (q, r))

    let p_and_q = Product [p; q]
    let p_or_q = Sum [p; q]

    /// Builds terms from types.
    let terms types =
        types
            |> Seq.map Term.create
            |> set
