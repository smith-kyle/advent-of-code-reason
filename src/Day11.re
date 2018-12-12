let memoizedReturn = ref(Belt.Map.String.empty);
let getPowerLevel = (serialNumber, (x, y)) => {
  let key = string_of_int(x) ++ "-" ++ string_of_int(y);
  if ((memoizedReturn^)->Belt.Map.String.has(key)) {
    (memoizedReturn^)->Belt.Map.String.getExn(key);
  } else {
    let rackId = x + 10;
    let result =
      (
        (rackId * y + serialNumber)
        * rackId
        |> string_of_int
        |> Js.String.split("")
      )
      ->Belt.Array.reverse
      ->(a => a[2])
      ->int_of_string
      ->(v => v - 5);
    memoizedReturn := Belt.Map.String.set(memoizedReturn^, key, result);
    result;
  };
};

let getTotalPowerLevel = (sideLength, serialNumber, (x, y)) =>
  Belt.Array.makeBy(sideLength, i =>
    Belt.Array.makeBy(sideLength, j => (i, j))
  )
  ->Belt.Array.reduce(0, (sum, array) =>
      array->Belt.Array.reduce(sum, (acc, (x', y')) =>
        acc + getPowerLevel(serialNumber, (x' + x, y' + y))
      )
    );

let get3by3TotalPowerLevel = getTotalPowerLevel(3);

getPowerLevel(57, (122, 79))->Assert.equal(-5);
getPowerLevel(39, (217, 196))->Assert.equal(0);
getPowerLevel(71, (101, 153))->Assert.equal(4);

get3by3TotalPowerLevel(18, (33, 45))->Assert.equal(29);

let getMaxPowerForSquare = (squareSideLength, serialNumber) =>
  Belt.List.makeBy(300 - (squareSideLength - 1), x =>
    Belt.List.makeBy(300 - (squareSideLength - 1), y => (x, y))
  )
  ->Belt.List.flatten
  ->Belt.List.map(point =>
      (getTotalPowerLevel(squareSideLength, serialNumber, point), point)
    )
  ->Belt.List.sort(((a, _), (b, _)) => a > b ? (-1) : 1)
  ->Belt.List.headExn;

let getMaxPower = serialNumber =>
  Belt.List.makeBy(299, i => i + 1)
  ->Belt.List.map(n => {
      Js.log(n);
      let (power, (x, y)) = getMaxPowerForSquare(n, serialNumber);
      Js.log3(power, x, y);
      (power, (x, y));
    })
  ->Belt.List.sort(((a, _), (b, _)) => a > b ? (-1) : 1)
  ->Belt.List.headExn
  ->(((_, point)) => point);

getMaxPower(4455);
getMaxPower(18)->Js.log;
getMaxPowerForSquare(11, 4455)->Js.log;
