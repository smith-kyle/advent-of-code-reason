let testInput1 = "/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   ";

let testInput2 = "/>-<\\
|   |
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/";

let input = Node.Fs.readFileSync("./src/input/day13.txt", `ascii);

let parseInput = input =>
  (input |> Js.String.split("\n"))
  ->Belt.Array.map(line => line |> Js.String.split(""));

let max = xs => xs->Belt.Array.reduce(-1, (max, x) => x > max ? x : max);

let getCoords = grid =>
  Belt.List.makeBy(grid->Belt.Array.length, y =>
    Belt.List.makeBy(max(grid->Belt.Array.map(Belt.Array.length)), x =>
      (x, y)
    )
  )
  ->Belt.List.flatten;

let hasCollision = grid =>
  grid
  ->getCoords
  ->Belt.List.some(((x, y)) =>
      Belt.Array.get(grid[y], x)->Belt.Option.getWithDefault("") == "X"
    );

let scanForCarts = grid =>
  getCoords(grid)
  ->Belt.List.keep(((x, y)) => {
      let char = Belt.Array.get(grid[y], x)->Belt.Option.getWithDefault("");
      switch (char) {
      | ">"
      | "^"
      | "<"
      | "v" => true
      | _ => false
      };
    });

exception NotACart;
exception NotATrack;

type turn =
  | Left
  | Right
  | Straight;

let turnCartRight = curChar =>
  switch (curChar) {
  | ">" => "v"
  | "^" => ">"
  | "<" => "^"
  | "v" => "<"
  | _ => raise(NotACart)
  };

let turnCartLeft = curChar =>
  switch (curChar) {
  | ">" => "^"
  | "^" => "<"
  | "<" => "v"
  | "v" => ">"
  | _ => raise(NotACart)
  };

let moveCart = (grid, oldCharAtCoords, (x, y), nextTurn) => {
  let curChar = grid[y][x];
  let (newX, newY) =
    switch (curChar) {
    | ">" => (x + 1, y)
    | "^" => (x, y - 1)
    | "<" => (x - 1, y)
    | "v" => (x, y + 1)
    | _ => raise(NotACart)
    };
  let newOldCharAtCoords = grid[newY][newX];
  let newChar =
    switch (newOldCharAtCoords) {
    | "-"
    | "|" => curChar
    | "+" when nextTurn == Right => turnCartRight(curChar)
    | "+" when nextTurn == Left => turnCartLeft(curChar)
    | "+" when nextTurn == Straight => curChar
    | "\\" when curChar == "^" => "<"
    | "\\" when curChar == ">" => "v"
    | "\\" when curChar == "<" => "^"
    | "\\" when curChar == "v" => ">"
    | "/" when curChar == ">" => "^"
    | "/" when curChar == "^" => ">"
    | "/" when curChar == "<" => "v"
    | "/" when curChar == "v" => "<"
    | ">"
    | "<"
    | "v"
    | "^" => "X"
    | _x =>
      Js.log(_x);
      raise(NotATrack);
    };
  let turned = newOldCharAtCoords == "+";
  grid[y][x] = oldCharAtCoords;
  grid[newY][newX] = newChar;
  ((newX, newY), turned, newOldCharAtCoords);
};

let printGrid = grid =>
  (
    grid->Belt.Array.map(line => line |> Js.Array.joinWith(""))
    |> Js.Array.joinWith("\n")
  )
  ->Js.log;

let coordsToString = ((x, y)) =>
  string_of_int(x) ++ "-" ++ string_of_int(y);

let getNextTurn = turn =>
  switch (turn) {
  | Left => Straight
  | Straight => Right
  | Right => Left
  };

let part1 = input => {
  let coordsOfCollision = ref(None);
  let grid = input->parseInput;
  let carts = ref(scanForCarts(grid));
  let nextTurnAndOldCharByCoord =
    ref(
      (carts^)
      ->Belt.List.toArray
      ->Belt.Array.map(((x, y)) =>
          (
            coordsToString((x, y)),
            (Left, grid[y][x] == ">" || grid[y][x] == "<" ? "-" : "|"),
          )
        )
      ->Belt.Map.String.fromArray,
    );
  while ((coordsOfCollision^)->Belt.Option.isNone) {
    (carts^)
    ->Belt.List.forEach(((x, y)) => {
        let (nextTurn, oldChar) =
          (nextTurnAndOldCharByCoord^)
          ->Belt.Map.String.getExn(coordsToString((x, y)));
        let ((newX, newY), turned, newOldChar) =
          moveCart(grid, oldChar, (x, y), nextTurn);
        nextTurnAndOldCharByCoord :=
          (nextTurnAndOldCharByCoord^)
          ->Belt.Map.String.remove(coordsToString((x, y)))
          ->Belt.Map.String.set(
              coordsToString((newX, newY)),
              (turned ? getNextTurn(nextTurn) : nextTurn, newOldChar),
            );
        if ((coordsOfCollision^)->Belt.Option.isNone) {
          coordsOfCollision :=
            hasCollision(grid) ? Some((newX, newY)) : coordsOfCollision^;
        };
        ();
      });
    carts := scanForCarts(grid);
  };
  coordsOfCollision^;
};

let part2 = input => {
  let grid = input->parseInput;
  let carts = ref(scanForCarts(grid));
  let skipCoords = ref(Belt.Map.String.empty);
  let nextTurnAndOldCharByCoord =
    ref(
      (carts^)
      ->Belt.List.toArray
      ->Belt.Array.map(((x, y)) =>
          (
            coordsToString((x, y)),
            (Left, grid[y][x] == ">" || grid[y][x] == "<" ? "-" : "|"),
          )
        )
      ->Belt.Map.String.fromArray,
    );
  while ((carts^)->Belt.List.length > 1) {
    (carts^)
    ->Belt.List.forEach(((x, y)) =>
        if (!(skipCoords^)->Belt.Map.String.has(coordsToString((x, y)))) {
          let (nextTurn, oldChar) =
            (nextTurnAndOldCharByCoord^)
            ->Belt.Map.String.getExn(coordsToString((x, y)));
          let ((newX, newY), turned, newOldChar) =
            moveCart(grid, oldChar, (x, y), nextTurn);
          if (hasCollision(grid)) {
            print_string("Collision!");
            (carts^)->Belt.List.length->Js.log;
            grid[newY][newX] =
              (nextTurnAndOldCharByCoord^)
              ->Belt.Map.String.getExn(coordsToString((newX, newY)))
              ->(((_turn, char)) => char);
            nextTurnAndOldCharByCoord :=
              (nextTurnAndOldCharByCoord^)
              ->Belt.Map.String.remove(coordsToString((x, y)));
            skipCoords :=
              (skipCoords^)
              ->Belt.Map.String.set(coordsToString((newX, newY)), true);
          } else {
            nextTurnAndOldCharByCoord :=
              (nextTurnAndOldCharByCoord^)
              ->Belt.Map.String.remove(coordsToString((x, y)))
              ->Belt.Map.String.set(
                  coordsToString((newX, newY)),
                  (turned ? getNextTurn(nextTurn) : nextTurn, newOldChar),
                );
          };
          ();
        }
      );
    skipCoords := Belt.Map.String.empty;
    carts := scanForCarts(grid);
  };
  (carts^)->Belt.List.head;
};

part2(input)->Js.log;
