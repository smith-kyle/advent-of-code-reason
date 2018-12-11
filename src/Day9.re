let insert = (array, x, i) => {
  Js.Array.spliceInPlace(~pos=i, ~remove=0, ~add=[|x|], array)->ignore;
  array;
};

let oldInsert = (array, x, i) =>
  Belt.Array.concatMany([|
    Belt.Array.slice(array, ~offset=0, ~len=i),
    [|x|],
    Belt.Array.slice(array, ~offset=i, ~len=array->Array.length - i),
  |]);

let remove = (array, i) => {
  Js.Array.spliceInPlace(~pos=i, ~remove=1, ~add=[||], array)->ignore;
  array;
};

let oldRemove = (array, i) =>
  Belt.Array.concatMany([|
    Belt.Array.slice(array, ~offset=0, ~len=i),
    Belt.Array.slice(array, ~offset=i + 1, ~len=array->Array.length - i),
  |]);

let part1 = (numPlayers, lastMarble) => {
  let curMarble = ref(1);
  let circle = ref(Array.make(2, 0));
  Js.log("Created array");
  circle^[1] = 1;
  let arrayLength = ref(2);
  let curMarbleIndex = ref(1);
  let scoreByPlayer = Belt.Array.make(numPlayers, 0);

  while (curMarble^ < lastMarble) {
    curMarble := curMarble^ + 1;
    if (curMarble^ mod 23 == 0) {
      let curPlayer = curMarble^ mod numPlayers;
      let sevenCounterClockwise =
        curMarbleIndex^ - 7 < 0 ?
          arrayLength^ - (7 - curMarbleIndex^) : curMarbleIndex^ - 7;
      let removedMarble = circle^[sevenCounterClockwise];
      circle := remove(circle^, sevenCounterClockwise);
      arrayLength := arrayLength^ - 1;
      curMarbleIndex := sevenCounterClockwise;
      scoreByPlayer[curPlayer] =
        scoreByPlayer[curPlayer] + curMarble^ + removedMarble;
    } else {
      curMarbleIndex := (curMarbleIndex^ + 2) mod arrayLength^;
      circle := (circle^)->insert(curMarble^, curMarbleIndex^);
      arrayLength := arrayLength^ + 1;
    };
    /* (circle^)->Js.log; */
  };

  scoreByPlayer
  ->Belt.List.fromArray
  ->Belt.List.sort((a, b) => a > b ? (-1) : 1)
  ->Belt.List.headExn;
};

type node = {
  next: ref(option(node)),
  prev: ref(option(node)),
  value: int,
};

module LinkedList = {
  let length = ref(0);
  let make = value => {
    let myNode = ref({next: ref(None), prev: ref(None), value});
    myNode := {next: ref(Some(myNode^)), prev: ref(Some(myNode^)), value};
    length := 1;
    myNode^;
  };

  let addNode = (ll, value) => {
    length := length^ + 1;
    let newNode = {next: ll.next, prev: ref(Some(ll)), value};
    ll.next := Some(newNode);
    newNode;
  };

  let removeNode = ll => {
    length := length^ - 1;
    (ll.prev^)->Belt.Option.map(prev => prev.next := ll.next^)->ignore;
    (ll.next^)->Belt.Option.map(next => next.prev := ll.prev^)->ignore;
  };

  let print = (ll: node) => {
    let i = ref(0);
    let cur = ref(ll);
    while (i^ < length^) {
      Js.log(ll.value);
      cur := (cur^.next^)->Belt.Option.getExn;
      i := i^ + 1;
    };
  };
};

let part2 = (numPlayers, lastMarble) => {};

/* let date1 = Js.Date.now();
   part1(465, 100000)->Js.log;
   let date2 = Js.Date.now();
   (date2 -. date1)->Js.log; */

LinkedList.make(0)
->LinkedList.addNode(1)
->LinkedList.addNode(2)
->LinkedList.print;
