// I dare someone to rewrite this in Reason

const createNode = value => ({
  next: null,
  prev: null,
  value
});

const createLinkedList = value => {
  const node = createNode(value);
  node.next = node;
  node.prev = node;
  return node;
};

const addNode = (node, value) => {
  const newNode = createNode(value);
  node.next.prev = newNode;
  newNode.next = node.next;
  newNode.prev = node;
  node.next = newNode;
  return newNode;
};

const removeNode = node => {
  node.prev.next = node.next;
  node.next.prev = node.prev;
  return node.next;
};

const print = node => {
  let curNode = node.next;
  let result = [node.value];
  while (node !== curNode) {
    result.push(curNode.value);
    curNode = curNode.next;
  }
  console.log(result.join(", "));
};

const part1 = (numPlayers, lastMarble) => {
  let circle = createLinkedList(0);
  const scoreByPlayer = {};
  let i = 0;
  while (i < lastMarble) {
    i += 1;
    if (i % 23 === 0) {
      const curPlayer = i % numPlayers;
      scoreByPlayer[curPlayer] = (scoreByPlayer[curPlayer] || 0) + i;
      for (let i = 0; i < 7; i++) {
        circle = circle.prev;
      }
      scoreByPlayer[curPlayer] = (scoreByPlayer[curPlayer] || 0) + circle.value;
      circle = removeNode(circle);
    } else {
      circle = circle.next;
      circle = addNode(circle, i);
    }
  }
  return Object.keys(scoreByPlayer)
    .map(k => scoreByPlayer[k])
    .reduce((max, v) => (v > max ? v : max), -1);
};

console.log(part1(465, 7149800));
