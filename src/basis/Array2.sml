(*
 * cl-sml: Standard ML Basis Library, structure Array2
 * (not part of HaMLet's basis).
 *
 * Note: an array is a one-dimensional array in row-major order with its
 * dimensions.
 *)

structure Array2 :> ARRAY2 =
struct
  type 'a array = {data : 'a Array.array, nrows : int, ncols : int}
  type 'a region = {base : 'a array, row : int, col : int,
                    nrows : int option, ncols : int option}
  datatype traversal = RowMajor | ColMajor

  fun array(r, c, x) =
      if r < 0 orelse c < 0 then raise Size
      else {data = Array.array(r * c, x), nrows = r, ncols = c}

  fun fromList nil = {data = Array.fromList nil, nrows = 0, ncols = 0}
    | fromList (rows as first::_) =
      let
        val c = List.length first
      in
        if List.exists (fn r => Bool.not(List.length r = c)) rows
        then raise Size
        else {data = Array.fromList(List.concat rows),
              nrows = List.length rows, ncols = c}
      end

  fun dimensions ({nrows, ncols, ...} : 'a array) = (nrows, ncols)
  fun nRows ({nrows, ...} : 'a array) = nrows
  fun nCols ({ncols, ...} : 'a array) = ncols

  fun index ({nrows, ncols, ...} : 'a array, i, j) =
      if i < 0 orelse i >= nrows orelse j < 0 orelse j >= ncols
      then raise Subscript
      else i * ncols + j

  fun sub(a : 'a array, i, j) = Array.sub(#data a, index(a, i, j))
  fun update(a : 'a array, i, j, x) = Array.update(#data a, index(a, i, j), x)

  fun row(a : 'a array, i) =
      if i < 0 orelse i >= #nrows a then raise Subscript
      else Vector.tabulate(#ncols a, fn j => sub(a, i, j))
  fun column(a : 'a array, j) =
      if j < 0 orelse j >= #ncols a then raise Subscript
      else Vector.tabulate(#nrows a, fn i => sub(a, i, j))

  (* f(i, j) for the rows r0 to r1 - 1 and columns c0 to c1 - 1 *)
  fun iterate RowMajor (r0, c0, r1, c1) f =
      let
        fun rows i = if i >= r1 then () else (cols(i, c0); rows(i + 1))
        and cols(i, j) = if j >= c1 then () else (f(i, j); cols(i, j + 1))
      in
        rows r0
      end
    | iterate ColMajor (r0, c0, r1, c1) f =
      let
        fun cols j = if j >= c1 then () else (rows(r0, j); cols(j + 1))
        and rows(i, j) = if i >= r1 then () else (f(i, j); rows(i + 1, j))
      in
        cols c0
      end

  fun tabulate t (r, c, f) =
      if r < 0 orelse c < 0 then raise Size else
      let
        val cells = Array.array(r * c, NONE)
      in
        iterate t (0, 0, r, c)
                (fn (i, j) => Array.update(cells, i * c + j, SOME(f(i, j))));
        {data = Array.tabulate(r * c, fn k => Option.valOf(Array.sub(cells, k))),
         nrows = r, ncols = c}
      end

  (* The rows and columns a region spans, as for iterate *)
  fun bounds ({base, row, col, nrows, ncols} : 'a region) =
      let
        val (r, c) = dimensions base
        val nr = case nrows of NONE => r - row | SOME n => n
        val nc = case ncols of NONE => c - col | SOME n => n
      in
        if row < 0 orelse col < 0 orelse nr < 0 orelse nc < 0
           orelse row + nr > r orelse col + nc > c
        then raise Subscript
        else (row, col, row + nr, col + nc)
      end

  fun full a = {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

  fun appi t f (region : 'a region) =
      iterate t (bounds region)
              (fn (i, j) => f(i, j, sub(#base region, i, j)))
  fun app t f a = appi t (fn (_, _, x) => f x) (full a)

  fun foldi t f b (region : 'a region) =
      let
        val acc = ref b
      in
        iterate t (bounds region)
                (fn (i, j) => acc := f(i, j, sub(#base region, i, j), !acc));
        !acc
      end
  fun fold t f b a = foldi t (fn (_, _, x, acc) => f(x, acc)) b (full a)

  fun modifyi t f (region : 'a region) =
      iterate t (bounds region)
              (fn (i, j) => update(#base region, i, j,
                                   f(i, j, sub(#base region, i, j))))
  fun modify t f a = modifyi t (fn (_, _, x) => f x) (full a)

  fun copy {src : 'a region, dst, dst_row, dst_col} =
      let
        val (r0, c0, r1, c1) = bounds src
        (* Read before writing: the regions may overlap. *)
        val values = foldi RowMajor (fn (_, _, x, l) => x::l) nil src
        val target = {base = dst, row = dst_row, col = dst_col,
                      nrows = SOME(r1 - r0), ncols = SOME(c1 - c0)}
        val rest = ref (List.rev values)
      in
        iterate RowMajor (bounds target)
                (fn (i, j) => case !rest of
                                x::xs => (update(dst, i, j, x); rest := xs)
                              | nil => ())
      end
end;
