let input =
  Node.Fs.readFileSync("./src/input/day5.txt", `ascii)
  |> Js.String.replace("\n", "");

let testInput = "dabAcCaCBAcCcaDA";

let shouldCancel = (a, b) =>
  Char.uppercase(a) == a
  && Char.lowercase(b) == b
  && Char.lowercase(a) == b
  || Char.uppercase(b) == b
  && Char.lowercase(a) == a
  && Char.lowercase(b) == a;

let rec merge = (a, b) =>
  (a |> String.length < 1 || b |> String.length < 1)
  || !
       shouldCancel(
         a->String.get((a |> String.length) - 1),
         b->String.get(0),
       ) ?
    a ++ b :
    merge(
      a->String.sub(0, String.length(a) - 1),
      b->String.sub(1, b->String.length - 1),
    );

let rec collapsePolymer = (s: string): string => {
  let len = s |> String.length;
  if (len < 2) {
    s;
  } else {
    let l = s->String.sub(0, len / 2);
    let r = s->String.sub(len / 2, len / 2 + (len mod 2 == 0 ? 0 : 1));
    merge(collapsePolymer(l), collapsePolymer(r));
  };
};

let part1 = input => input->collapsePolymer->String.length;

let rec removeUnit = (a, str) => {
  let newStr =
    str
    |> Js.String.replace(a, "")
    |> Js.String.replace(a |> String.uppercase, "");
  newStr |> String.length == (str |> String.length) ?
    str : removeUnit(a, newStr);
};

let letters = [
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z",
];

let part2 = input =>
  letters
  ->Belt.List.map(l =>
      (removeUnit(l, input) |> collapsePolymer)->String.length
    )
  ->Belt.List.sort((a, b) => a > b ? 1 : (-1))
  ->Belt.List.head;
