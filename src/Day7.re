let testInput = "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
";

let input = Node.Fs.readFileSync("./src/input/day7.txt", `ascii);

type node = {
  next: array(node),
  prev: array(node),
  letter: string,
};

let lineRegex =
  Js.Re.fromString(
    "Step ([A-Z]) must be finished before step ([A-Z]) can begin.",
  );

let lineToTuple = line =>
  line
  ->Js.Re.exec(lineRegex)
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.getWithDefault([||])
  ->(
      a => (
        a[1]->Js.Nullable.toOption->Belt.Option.getExn,
        a[2]->Js.Nullable.toOption->Belt.Option.getExn,
      )
    );

let addNodesToGraph = (graph, (a, b)) =>
  (
    graph->Belt.Map.String.getWithDefault(
      a,
      {next: [||], prev: [||], letter: a},
    ),
    graph->Belt.Map.String.getWithDefault(
      b,
      {next: [||], prev: [||], letter: b},
    ),
  )
  |> (
    ((nodeA, nodeB)) =>
      graph
      ->Belt.Map.String.set(
          a,
          {...nodeA, next: nodeA.next->Belt.Array.concat([|nodeB|])},
        )
      ->Belt.Map.String.set(
          b,
          {...nodeB, prev: nodeB.prev->Belt.Array.concat([|nodeA|])},
        )
  );

let getHeads = graph =>
  graph
  ->Belt.Map.String.toArray
  ->Belt.Array.keep(((_, {prev})) => prev->Array.length < 1)
  ->Belt.Array.map(((_, {letter})) => letter);

let buildGraph = input =>
  (input |> Js.String.trim |> Js.String.split("\n"))
  ->Belt.Array.map(lineToTuple)
  ->Belt.Array.reduce(Belt.Map.String.empty, addNodesToGraph);

let getFirstElligibleLetter = (visited, queue, graph) =>
  queue
  ->Belt.Array.keep(l =>
      graph
      ->Belt.Map.String.getExn(l)
      ->(
          ({prev}) =>
            prev
            ->Belt.Array.map(({letter}) => letter)
            ->Belt.Array.every(letter =>
                visited->Belt.Map.String.getWithDefault(letter, false)
              )
        )
    )
  ->Belt.List.fromArray
  ->Belt.List.head;

let rec traverseGraph =
        (~visited=Belt.Map.String.empty, ~queue=[||], letter, graph) => {
  let newVisited = visited->Belt.Map.String.set(letter, true);
  let newQueue =
    queue
    ->Belt.Array.map(l => (l, true))
    ->Belt.Map.String.fromArray
    ->(
        lookup =>
          graph
          ->Belt.Map.String.getExn(letter)
          ->(({next}) => next->Belt.Array.map(({letter}) => letter))
          ->Belt.Array.keep(l =>
              !newVisited->Belt.Map.String.getWithDefault(l, false)
            )
          ->Belt.Array.reduce(lookup, (lookup', l) =>
              Belt.Map.String.set(lookup', l, true)
            )
          ->Belt.Map.String.toArray
          ->Belt.Array.map(((l1, _)) => l1)
      )
    ->Belt.List.fromArray
    ->Belt.List.sort((a, b) => a > b ? 1 : (-1))
    ->Belt.List.toArray;
  switch (getFirstElligibleLetter(newVisited, newQueue, graph)) {
  | Some(l) =>
    l
    ++ traverseGraph(
         ~visited=newVisited,
         ~queue=newQueue->Belt.Array.keep(l' => l' != l),
         l,
         graph,
       )
  | None => ""
  };
};

let part1 = input =>
  input
  ->buildGraph
  ->(
      graph =>
        getHeads(graph)
        ->(
            letters =>
              letters
              ->Belt.List.fromArray
              ->Belt.List.sort((a, b) => a > b ? 1 : (-1))
          )
        ->(
            letters => (
              letters->Belt.List.toArray,
              Belt.List.headExn(letters),
            )
          )
        ->(
            ((letters, l)) =>
              l
              ++ traverseGraph(
                   ~queue=letters->Belt.Array.keep(l' => l' != l),
                   l,
                   graph,
                 )
          )
    );
