let input =
  (
    Node.Fs.readFileSync("./src/input/day2.txt", `ascii)
    |> Js.String.split("\n")
  )
  ->Belt.Array.keep(line => line != "")
  ->Belt.Array.map(line => line |> Js.String.split(""));

let countOccurences = line =>
  line->Belt.Array.reduce(Belt.Map.String.empty, (countByLetter, n) =>
    countByLetter
    ->Belt.Map.String.getWithDefault(n, 0)
    ->(count => countByLetter->Belt.Map.String.set(n, count + 1))
  );

let getLineChecksum = lineOccurenceCount =>
  (
    lineOccurenceCount->Belt.Array.some(((_, count)) => count == 2),
    lineOccurenceCount->Belt.Array.some(((_, count)) => count == 3),
  )
  ->(((has2, has3)) => (has2 ? 1 : 0, has3 ? 1 : 0));

let getChecksum = lines =>
  lines
  ->Belt.Array.map(countOccurences)
  ->Belt.Array.map(Belt.Map.String.toArray)
  ->Belt.Array.map(getLineChecksum)
  ->Belt.Array.reduce((0, 0), ((a, b), (a', b')) => (a + a', b + b'))
  ->(((num2s, num3s)) => num2s * num3s);

/* input->getChecksum->Js.log; */

let getMatchingLines = lines => {
  let matchingLines = ref(None);

  for (i in 0 to Belt.Array.length(lines) - 1) {
    for (j in 0 to Belt.Array.length(lines) - 1) {
      if ((matchingLines^)->Belt.Option.isNone && i != j) {
        let line1 = lines->Belt.Array.getExn(i);
        let line2 = lines->Belt.Array.getExn(j);
        let numNonMatching =
          Belt.Array.zip(line1, line2)
          ->Belt.Array.reduce(0, (numNonMatching, (a, b)) =>
              a != b ? numNonMatching + 1 : numNonMatching
            );
        if (numNonMatching < 2) {
          matchingLines := Some((line1, line2));
        };
      };
    };
  };
  matchingLines^;
};

input
->getMatchingLines
->Belt.Option.map(((a, b)) => {
    (a |> Js.Array.joinWith(""))->Js.log;
    (b |> Js.Array.joinWith(""))->Js.log;
  });
