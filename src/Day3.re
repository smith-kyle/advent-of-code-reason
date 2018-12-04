let getXAndYCoordsPreOffset = (width, height) =>
  Belt.List.makeBy(width, x => Belt.List.makeBy(height, y => (x, y)))
  ->Belt.List.flatten;

let getXAndYCoords = (x, y, width, height) =>
  getXAndYCoordsPreOffset(width, height)
  ->Belt.List.map(((x', y')) => (x' + x, y' + y));

let coordsToString = ((x, y)) =>
  string_of_int(x) ++ "-" ++ string_of_int(y);

let getCoordsLookup = (coordsLookup, coords) =>
  coords
  ->Belt.List.map(coordsToString)
  ->Belt.List.reduce(coordsLookup, (lookup, coord) =>
      lookup
      ->Belt.Map.String.getWithDefault(coord, 0)
      ->(count => lookup->Belt.Map.String.set(coord, count + 1))
    );

type claim = {
  id: int,
  xOffset: int,
  yOffset: int,
  width: int,
  height: int,
};

let test1 = "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2";

let stringToClaim = s =>
  (Js.Re.fromString("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)") |> Js.Re.exec(s))
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.map(a => a->Array.sub(1, 5))
  ->Belt.Option.map(Array.map(Js.Nullable.toOption))
  ->Belt.Option.map(a =>
      Array.map(n => Belt.Option.getWithDefault(n, "0"), a)
    )
  ->Belt.Option.map(a => a |> Array.map(int_of_string))
  ->Belt.Option.map(a =>
      {id: a[0], xOffset: a[1], yOffset: a[2], width: a[3], height: a[4]}
    )
  ->Belt.Option.getWithDefault({
      id: (-1),
      xOffset: 0,
      yOffset: 0,
      width: 0,
      height: 0,
    });

let isNotOverlapping = (coords, coordsLookup) =>
  coords
  ->Belt.Array.map(coordsToString)
  ->Belt.Array.reduce(true, (isNotOverlapping, coord) =>
      coordsLookup
      ->Belt.Map.String.getWithDefault(coord, 1)
      ->(sum => sum < 2 ? isNotOverlapping : false)
    );

let getNumOverlapping = input =>
  (input |> Js.String.split("\n"))
  ->Belt.Array.map(stringToClaim)
  ->Belt.Array.map(({xOffset, yOffset, width, height}) =>
      getXAndYCoords(xOffset, yOffset, width, height)
    )
  ->Belt.Array.reduce(Belt.Map.String.empty, getCoordsLookup)
  ->Belt.Map.String.toList
  ->Belt.List.reduce(0, (sum, (_, count)) => count > 1 ? sum + 1 : sum);

let input = Node.Fs.readFileSync("./src/input/day3.txt", `ascii);
(input |> getNumOverlapping)->Js.log;

let getNotOverlappingId = input => {
  let claims =
    (input |> Js.String.split("\n"))->Belt.Array.map(stringToClaim);
  let coordsLookup =
    claims
    ->Belt.Array.map(({xOffset, yOffset, width, height}) =>
        getXAndYCoords(xOffset, yOffset, width, height)
      )
    ->Belt.Array.reduce(Belt.Map.String.empty, getCoordsLookup);
  let coordsWithId =
    claims->Belt.Array.map(({id, xOffset, yOffset, width, height}) =>
      (id, getXAndYCoords(xOffset, yOffset, width, height))
    );
  coordsWithId->Belt.Array.keep(((_id, coords)) =>
    isNotOverlapping(coords->Belt.List.toArray, coordsLookup)
  );
};

getNotOverlappingId(input)->Js.log;
