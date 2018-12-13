let testInput = "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #";

let input = "initial state: #.##.#.##..#.#...##...#......##..#..###..##..#.#.....##..###...#.#..#...######...#####..##....#..###

##... => #
..#.# => #
#..#. => #
#.##. => #
.#.## => #
.##.. => #
..#.. => #
#..## => #
#.#.. => #
..### => #
...#. => #
##.#. => #
#.#.# => #
##### => #
.#.#. => #
.###. => #
###.# => #";

let parseInput = input =>
  (input->Js.String.trim |> Js.String.split("\n"))
  ->Belt.List.fromArray
  ->(
      ([initialState, ...xs]) => (
        (initialState |> Js.String.split(": "))[1],
        xs
        ->Belt.List.tailExn
        ->Belt.List.toArray
        ->Belt.Array.map(a => (a |> Js.String.split(" => "))->(a => a[0])),
      )
    )
  ->(
      ((initialState, notes)) => (
        initialState,
        notes->Belt.Array.map(a => (a, true))->Belt.Map.String.fromArray,
      )
    );

let getIndexOfFirstHash = state =>
  (
    state->Belt.Array.mapWithIndex((i, a) => (i, a))->Belt.List.fromArray
    |> List.find(((_i, a)) => a == "#")
  )
  ->(((i, _a)) => i);

let padStateWithDots = state => {
  let indexOfFirstHash = (state |> Js.String.split(""))->getIndexOfFirstHash;
  let indexOfLastHash =
    (state |> Js.String.split(""))
    ->Belt.List.fromArray
    ->Belt.List.reverse
    ->Belt.List.toArray
    ->getIndexOfFirstHash;
  let offset = Js.Math.max_int(0, 4 - indexOfFirstHash);
  let newState =
    (Array.make(offset, ".") |> Js.Array.joinWith(""))
    ++ state
    ++ (
      Array.make(Js.Math.max_int(0, 4 - indexOfLastHash), ".")
      |> Js.Array.joinWith("")
    );
  (newState, offset);
};

let createNewState = (state, noteLookup) =>
  Belt.Array.makeBy(state->String.length - 5, i => i)
  ->Belt.Array.map(i => state->String.sub(i, 5))
  ->Belt.Array.map(key => Belt.Map.String.has(noteLookup, key) ? "#" : ".")
  |> Js.Array.joinWith("");

let countHashes = (state, offset) =>
  (state |> Js.String.split(""))
  ->Belt.Array.mapWithIndex((i, a) => (i, a))
  ->Belt.Array.reduce(0, (sum, (i, n)) =>
      n == "#" ? sum + (i + offset) : sum
    );

let part1 = (numGens, initialState, noteLookup) => {
  let gen = ref(0);
  let state = ref(initialState);
  let offset = ref(0);
  while (gen^ < numGens) {
    gen := gen^ + 1;
    let (newState, offsetDelta) = padStateWithDots(state^);
    state := newState;
    state := createNewState(state^, noteLookup);
    offset := offset^ - (offsetDelta - 2);
  };
  (state^)->countHashes(offset^);
};

input
->parseInput
->(((initialState, noteLookup)) => part1(50000, initialState, noteLookup))
->Js.log;
