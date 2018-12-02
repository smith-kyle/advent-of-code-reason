open Belt;

let input =
  (Node.Fs.readFileSync("src/input/day0.txt", `ascii) |> Js.String.split(""))
  ->Array.keep(char => char != "\n")
  ->Array.map(int_of_string);

let getRepeatedSum = input => {
  let lastNumber = input->Array.getExn(Array.length(input) - 1);

  let (sum, _) =
    Array.reduce(input, (0, lastNumber), ((sum, prev), num) =>
      num == prev ? (sum + num, num) : (sum, num)
    );
  sum;
};

let getHalfwayAround = (xs, n) =>
  xs
  ->Array.length
  ->(len => (n + len / 2) mod len)
  ->(i => Array.getExn(xs, i));

let getHalfwaySum = input =>
  input
  ->Belt.List.fromArray
  ->List.reduceWithIndex(0, (sum, num, i) =>
      num == getHalfwayAround(input, i) ? sum + num : sum
    );

[|1, 1, 2, 2|]->getRepeatedSum->Assert.equal(3);
[|1, 1, 1, 1|]->getRepeatedSum->Assert.equal(4);
[|1, 2, 3, 4|]->getRepeatedSum->Assert.equal(0);
[|9, 1, 2, 1, 2, 1, 2, 9|]->getRepeatedSum->Assert.equal(9);
input->getRepeatedSum->Js.log;
getHalfwayAround([|1, 2, 3, 4|], 2)->Assert.equal(1);
input->getHalfwaySum->Js.log;
