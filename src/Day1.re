let input =
  (
    Node.Fs.readFileSync("./src/input/day1.txt", `ascii)
    |> Js.String.split("\n")
  )
  ->(a => Belt.Array.slice(a, ~offset=0, ~len=Array.length(a) - 1))
  ->Belt.Array.map(Js.String.replace("+", ""))
  ->Belt.Array.map(int_of_string);

let getRepeatedFreq = list => {
  let freqLookup = ref(Belt.Map.Int.fromArray([|(0, true)|]));
  let maybeFreq = ref(None);
  let sum = ref(0);

  while ((maybeFreq^)->Belt.Option.isNone) {
    let (newSum, newFreqLookup, newMaybeFreq) =
      Belt.Array.reduce(
        list,
        (sum^, freqLookup^, maybeFreq^),
        ((sum, freqs, maybeRepeated), num) =>
        (
          sum + num,
          freqs->Belt.Map.Int.set(sum + num, true),
          maybeRepeated->Belt.Option.isSome ?
            maybeRepeated :
            freqs->Belt.Map.Int.getWithDefault(sum + num, false) ?
              Some(sum + num) : None,
        )
      );
    maybeFreq := newMaybeFreq;
    freqLookup := newFreqLookup;
    sum := newSum;
  };

  (maybeFreq^)->Belt.Option.getExn;
};

getRepeatedFreq(input)->Js.log;
