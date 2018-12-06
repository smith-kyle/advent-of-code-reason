let testInput = "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
";

let input = Node.Fs.readFileSync("./src/input/day6.txt", `ascii);

let getManhattanDistance = ((x1, y1), (x2, y2)) =>
  Js.Math.max_int(x1, x2)
  - Js.Math.min_int(x1, x2)
  + (Js.Math.max_int(y1, y2) - Js.Math.min_int(y1, y2));

let pointToString = ((x, y)) =>
  string_of_int(x) ++ "-" ++ string_of_int(y);

let isEquadistant = list =>
  switch (list) {
  | [(_, dist1), (_, dist2), ..._] => dist1 == dist2
  | _ => false
  };

let addInfPoints = (points, infLookup, pointsToCheck) =>
  pointsToCheck->Belt.Array.reduce(infLookup, (lookup, pointToCheck) =>
    points
    ->Belt.Array.map(point =>
        (point, getManhattanDistance(point, pointToCheck))
      )
    ->Belt.List.fromArray
    ->Belt.List.sort(((_, dist1), (_, dist2)) => dist1 > dist2 ? 1 : (-1))
    |> (
      list =>
        list
        ->isEquadistant
        ->(
            shouldNotUpdateList =>
              shouldNotUpdateList ?
                lookup :
                list
                ->Belt.List.headExn
                ->(
                    ((closestPoint, _)) =>
                      closestPoint->pointToString
                      |> Belt.Map.String.set(lookup, _, true)
                  )
          )
    )
  );

let getMaxXMaxY = points => (
  points
  ->Belt.Array.map(((x, _)) => x)
  ->Belt.Array.reduce(0, Js.Math.max_int),
  points
  ->Belt.Array.map(((_, y)) => y)
  ->Belt.Array.reduce(0, Js.Math.max_int),
);

let keepNonInfinitePoints = points =>
  (
    points->getMaxXMaxY
    |> (
      ((maxX, maxY)) => [|
        Belt.Array.makeBy(maxX + 2, i => (i - 1, (-1))),
        Belt.Array.makeBy(maxX + 2, i => (i - 1, maxY + 1)),
        Belt.Array.makeBy(maxY + 2, i => ((-1), i - 1)),
        Belt.Array.makeBy(maxY + 2, i => (maxX + 1, i - 1)),
      |]
    )
  )
  ->Belt.Array.reduce(Belt.Map.String.empty, addInfPoints(points))
  ->(
      infPointsLookup =>
        points->Belt.Array.keep(point =>
          !(
            point
            |> pointToString
            |> Belt.Map.String.getWithDefault(infPointsLookup, _, false)
          )
        )
    );

let lineToPoint = line =>
  line
  |> Js.String.split(", ")
  |> (
    a =>
      switch (a) {
      | [|x, y|] => (int_of_string(x), int_of_string(y))
      | _ => ((-1), (-1))
      }
  );

let inputToPoints = input =>
  (input |> Js.String.split("\n"))
  ->Belt.Array.keep(l => l != "")
  ->Belt.Array.map(lineToPoint);

let rec countClosestAround = (~radius=1, (x, y), points) => {
  let points = points->Belt.Array.keep(((x', y')) => x' != x || y' != y);
  let pointsAtRadius =
    [
      Belt.List.makeBy(radius * 2 + 1, i => (x - radius + i, y - radius)),
      Belt.List.makeBy(radius * 2 - 1, i => (x + radius, y - radius + i + 1)),
      Belt.List.makeBy(radius * 2 + 1, i => (x - radius + i, y + radius)),
      Belt.List.makeBy(radius * 2 - 1, i => (x - radius, y - radius + i + 1)),
    ]
    |> Belt.List.flatten
    |> Belt.List.toArray;
  let numPointsWhosClosestOtherPointisThePointPassedIn =
    pointsAtRadius->Belt.Array.reduce(0, (sum, pAR) =>
      getManhattanDistance(pAR, (x, y))
      < points
        ->Belt.Array.map(getManhattanDistance(pAR))
        ->Belt.Array.reduce(max_int, Js.Math.min_int) ?
        sum + 1 : sum
    );
  numPointsWhosClosestOtherPointisThePointPassedIn == 0 ?
    1 :
    numPointsWhosClosestOtherPointisThePointPassedIn
    + countClosestAround(~radius=radius + 1, (x, y), points);
};

let part1 = input =>
  input
  ->inputToPoints
  ->(
      allPoints =>
        allPoints
        ->keepNonInfinitePoints
        ->Belt.Array.map(nonInfPoint =>
            countClosestAround(nonInfPoint, allPoints)
          )
        ->Belt.Array.reduce(0, Js.Math.max_int)
    );

let part2 = input =>
  input
  ->inputToPoints
  ->(
      allPoints =>
        allPoints
        ->getMaxXMaxY
        ->(
            ((maxX, maxY)) =>
              Belt.List.makeBy(maxX + 1, x =>
                Belt.List.makeBy(maxY + 1, y => (x, y))
              )
          )
        ->Belt.List.flatten
        ->Belt.List.toArray
        ->Belt.Array.reduce(0, (sum, point) =>
            allPoints->Belt.Array.reduce(0, (acc, p) =>
              acc + getManhattanDistance(p, point)
            )
            < 10000 ?
              1 + sum : sum
          )
    );

part2(input) |> Js.log;
