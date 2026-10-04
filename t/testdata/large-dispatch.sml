fun dispatch pair =
  case pair of
      (0, x) => x
    | (1, x) => x + 1
    | (2, x) => x + 2
    | (3, x) => x + 3
    | (4, x) => x + 4
    | (5, x) => x + 5
    | (6, x) => x + 6
    | (7, x) => x + 7
    | (8, x) => x + 8
    | (9, x) => x + 9
    | (10, x) => x + 10
    | (11, x) => x + 11
    | _ => 0;

val dispatch_result = dispatch (11, 31);
