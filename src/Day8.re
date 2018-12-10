let testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";

let input =
  Node.Fs.readFileSync("./src/input/day8.txt", `ascii) |> Js.String.trim;

let parseInput = input =>
  (input |> Js.String.split(" "))->Belt.Array.map(int_of_string);

let metaDataCount = ref(0);

let rec countMetadata = ns =>
  switch (ns) {
  | [numChildren, numMetadata, ...rest] =>
    Belt.List.make(numChildren, 1)
    ->Belt.List.reduce(rest, (ns, _) => countMetadata(ns))
    ->(
        newRest => {
          metaDataCount :=
            metaDataCount^
            + Belt.List.take(newRest, numMetadata)
              ->Belt.Option.getExn
              ->Belt.List.reduce(0, (sum, a) => sum + a);
          newRest->Belt.List.drop(numMetadata)->Belt.Option.getExn;
        }
      )
  | _ => []
  };

let part1 = input =>
  input->parseInput->Belt.List.fromArray->countMetadata->(_ => metaDataCount^);

part1(input)->Js.log;

let rec countMetadata2 = ns =>
  switch (ns) {
  | [numChildren, numMetadata, ...rest] =>
    Belt.List.make(numChildren, 1)
    ->Belt.List.reduce(
        (rest, []),
        ((ns, vs), _) => {
          let (rest, value) = countMetadata2(ns);
          (rest, List.concat([vs, [value]]));
        },
      )
    ->(
        ((newRest, values)) => {
          let value =
            Belt.List.take(newRest, numMetadata)
            ->Belt.Option.getExn
            ->Belt.List.reduce(0, (sum, index) =>
                (
                  numChildren < 1 ?
                    index :
                    values
                    ->Belt.List.get(index - 1)
                    ->Belt.Option.getWithDefault(0)
                )
                + sum
              );
          (newRest->Belt.List.drop(numMetadata)->Belt.Option.getExn, value);
        }
      )
  | _ => ([], 0)
  };

let part2 = input =>
  input
  ->parseInput
  ->Belt.List.fromArray
  ->countMetadata2
  ->(((_, value)) => value);

part2(input)->Js.log;
