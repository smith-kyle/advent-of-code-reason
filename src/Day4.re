let testInput = "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up";

let guardStartReg = "\\[.+\\] Guard #\\d+ begins shift";
let guardSleepReg = "\\[.+\\] falls asleep";
let guardAwakeReg = "\\[.+\\] wakes up";

let getGuardId = line =>
  line
  |> Js.String.split("#")
  |> (a => a[1] |> Js.String.split(" "))
  |> (a => a[0]);

let getTimestamp = line =>
  line
  |> Js.String.split(" ")
  |> (a => a[1] |> Js.String.split(":"))
  |> (a => a[1])
  |> (
    s =>
      s |> Js.String.substrAtMost(~from=0, ~length=Js.String.length(s) - 1)
  )
  |> int_of_string;

let testReg = (regStr, line) =>
  Js.Re.fromString(regStr) |> Js.Re.test(line);
let isGuardStartLine = testReg(guardStartReg);
let isGuardSleepLine = testReg(guardSleepReg);
let isGuardAwakeLine = testReg(guardAwakeReg);

let getGuardSleepIntervals = lines =>
  lines->Belt.Array.reduce(
    ([||], "", 0), ((acc, currentGuardId, lastSleepTime), line) =>
    switch (
      line |> isGuardStartLine,
      line |> isGuardSleepLine,
      line |> isGuardAwakeLine,
    ) {
    | (true, false, false) => (acc, line |> getGuardId, lastSleepTime)
    | (false, true, false) => (acc, currentGuardId, line |> getTimestamp)
    | (false, false, true) => (
        acc->Belt.Array.concat([|
          (currentGuardId, lastSleepTime, line->getTimestamp),
        |]),
        currentGuardId,
        0,
      )
    | _ => (acc, currentGuardId, lastSleepTime)
    }
  )
  |> (((acc, _, _)) => acc);

let getGuardSleepTotalsById = sleepIntervals =>
  sleepIntervals->Belt.Array.reduce(
    Belt.Map.String.empty, (sleepTotalsById, (id, sleepTime, wakeTime)) =>
    sleepTotalsById
    ->Belt.Map.String.getWithDefault(id, 0)
    ->(
        curSum =>
          Belt.Map.String.set(
            sleepTotalsById,
            id,
            curSum + (wakeTime - sleepTime),
          )
      )
  );

let getMaxHourForId = (id, sleepIntervals) =>
  sleepIntervals
  ->Belt.Array.keep(((id', _, _)) => id' == id)
  ->Belt.Array.reduce(
      Belt.Map.Int.empty, (countByHour, (_, sleepTime, wakeTime)) =>
      Belt.Array.makeBy(wakeTime - sleepTime, i => i + sleepTime)
      ->Belt.Array.reduce(countByHour, (countByHour', i) =>
          countByHour'
          ->Belt.Map.Int.getWithDefault(i, 0)
          ->(count => Belt.Map.Int.set(countByHour', i, count + 1))
        )
    )
  ->Belt.Map.Int.toList
  ->Belt.List.sort(((_, a), (_, b)) => a > b ? 1 : (-1))
  ->Belt.List.reverse
  ->Belt.List.headExn;

let part1 = lines => {
  let sleepIntervals = lines |> getGuardSleepIntervals;
  let sleepTotalsById = sleepIntervals |> getGuardSleepTotalsById;
  let sleepTotalsByIdList = sleepTotalsById->Belt.Map.String.toList;
  let sortedSleepTotalsByIdList =
    Belt.List.sort(sleepTotalsByIdList, ((_, a), (_, b)) =>
      a > b ? 1 : (-1)
    )
    ->Belt.List.reverse;
  let mostSleepId =
    Belt.List.head(sortedSleepTotalsByIdList)
    ->Belt.Option.map(((id, _)) => id)
    ->Belt.Option.getWithDefault("-1");
  let (hour, _count) = getMaxHourForId(mostSleepId, sleepIntervals);
  (mostSleepId, hour);
};

let part2 = lines => {
  let sleepIntervals = lines |> getGuardSleepIntervals;
  let ids =
    sleepIntervals
    ->Belt.Array.reduce(Belt.Map.String.empty, (lookup, (id, _, _)) =>
        lookup->Belt.Map.String.set(id, true)
      )
    ->Belt.Map.String.toArray
    ->Belt.Array.map(((id, _)) => id);
  ids
  ->Belt.Array.map(id => (id, getMaxHourForId(id, sleepIntervals)))
  ->Belt.List.fromArray
  ->Belt.List.sort(((_, (_, countA)), (_, (_, countB))) =>
      countA > countB ? 1 : (-1)
    )
  ->Belt.List.reverse
  ->Belt.List.headExn;
};

let input =
  (
    Node.Fs.readFileSync("./src/input/day4.txt", `ascii)
    |> Js.String.split("\n")
  )
  ->Belt.Array.keep(line => line != "")
  ->Belt.List.fromArray
  ->Belt.List.sort((a, b) => a > b ? 1 : (-1))
  ->Belt.List.toArray;

input->part2->Js.log;
