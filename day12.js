const fs = require('fs');

const toBool = s => s === '#' ? true : false;
const toString = b => b ? '#' : '.';

/* Parse rules into a single function with signature:
 * (boolean, boolean, boolean, boolean, boolean) -> boolean
 *   L         L        C        R         R
 *
 *  each rule looks like "##.## => ."
 */
const parseRules = rules => {
  // Rulemap keys look like '##.##' and values are '#' or '.'
  const ruleMap = rules.reduce(
    (acc, currentValue) => {
      const [leftSide,,rightSide] = currentValue.split(' ');
      acc[leftSide] = rightSide;
      return acc;
    },
    {}
  );

  return (ll, l, c, r, rr) =>
    toBool(
      ruleMap[[ll, l, c, r, rr].map(toString).join('')]
    )
};

/* Parse the initial state into an array. */
const parseInitialState = initialState =>
  initialState.split('').map(toBool);

const advanceGenerations = rules => n => ({leftPots, rightPots}) => {
  for (let i = 0; i < n; i++) {
    const newLeftPots = leftPots.map(
      (currentPot, leftIndex) => {
        const realIndex = (leftIndex * -1) - 1;
        return getIsPlanted(realIndex, leftPots, rightPots, rules);
      }
    );
    // We can add up to two to the left.
    for (let j = leftPots.length; j < leftPots.length + 2; j++) {
      const realIndex = (j * -1) - 1;
      if (getIsPlanted(realIndex, leftPots, rightPots, rules)) {
        newLeftPots.push(true);
      }
    }

    const newRightPots = rightPots.map(
      (currentPot, rightIndex) => {
        const realIndex = rightIndex;
        return getIsPlanted(realIndex, leftPots, rightPots, rules);
      }
    );
    // We can add up to two to the right.
    for (let j = rightPots.length; j < rightPots.length + 2; j++) {
      const realIndex = j;
      if (getIsPlanted(realIndex, leftPots, rightPots, rules)) {
        newRightPots.push(true);
      }
    }
    leftPots = newLeftPots;
    rightPots = newRightPots;
  }

  return { leftPots, rightPots };
};

// refactor this someday
const getIsPlanted = (realIndex, leftPots, rightPots, rules) => {
  if (realIndex < -2) {
    const leftPotIndex = (realIndex * -1) - 1;
    return rules(
      leftPots[leftPotIndex - 2],
      leftPots[leftPotIndex - 1],
      leftPots[leftPotIndex],
      leftPots[leftPotIndex + 1],
      leftPots[leftPotIndex + 2]
    );
  }
  if (realIndex === -2) {
    return rules(
      leftPots[3],
      leftPots[2],
      leftPots[1],
      leftPots[0],
      rightPots[0]
    );
  }
  if (realIndex === -1) {
    return rules(
      leftPots[2],
      leftPots[1],
      leftPots[0],
      rightPots[0],
      rightPots[1]
    );
  }
  if (realIndex === 0) {
    return rules(
      leftPots[1],
      leftPots[0],
      rightPots[0],
      rightPots[1],
      rightPots[2]
    );
  }
  if (realIndex === 1) {
    return rules(
      leftPots[0],
      rightPots[0],
      rightPots[1],
      rightPots[2],
      rightPots[3]
    );
  }
  return rules(
    rightPots[realIndex - 2],
    rightPots[realIndex - 1],
    rightPots[realIndex],
    rightPots[realIndex + 1],
    rightPots[realIndex + 2]
  );
};

const countPlantPots = ({ leftPots, rightPots }) => {
  const leftPotSum = leftPots.reduce(
    (acc, isPlanted, index) => (
      isPlanted
        // leftPots[0]'s real index is -1
        // leftPots[1]'s real index is -2
        ? acc + (index * -1) - 1
        : acc
    ),
    0
  );
  const rightPotSum = rightPots.reduce(
    (acc, isPlanted, index) => (
      isPlanted
        // rightPots[0]'s real index is 0
        // rightPots[1]'s real index is 1
        ? acc + index
        : acc
    ),
    0
  );
  return leftPotSum + rightPotSum;
};

const partOne = () => {
  const inputLines = fs.readFileSync('./input/day12', 'utf-8').split('\n');
  const leftPots = [];
  const rightPots = parseInitialState(inputLines[0].split(': ')[1]);
  const rules = parseRules(inputLines.slice(2));

  console.log(
    countPlantPots(
      advanceGenerations(rules)(20)({leftPots, rightPots})
    )
  );
};

//partOne();

const partTwo = () => {
  const inputLines = fs.readFileSync('./input/day12', 'utf-8').split('\n');
  const leftPots = [];
  const rightPots = parseInitialState(inputLines[0].split(': ')[1]);
  const rules = parseRules(inputLines.slice(2));

  const after1000 = advanceGenerations(rules)(1000)({leftPots, rightPots});
  console.log("After 1000, sum is ", countPlantPots(after1000));

  const after2000 = advanceGenerations(rules)(1000)(after1000);
  console.log("After 2000, sum is ", countPlantPots(after2000));

  const after3000 = advanceGenerations(rules)(1000)(after2000);
  console.log("After 3000, sum is ", countPlantPots(after3000));

  const after4000 = advanceGenerations(rules)(1000)(after3000);
  console.log("After 4000, sum is ", countPlantPots(after4000));

  const after5000 = advanceGenerations(rules)(1000)(after4000);
  console.log("After 5000, sum is ", countPlantPots(after5000));

  console.log("Get remaining sum for 3000", getRemainingSum(3000));
  console.log("Get remaining sum for 4000", getRemainingSum(4000));
  console.log("Get remaining sum for 5000", getRemainingSum(5000));
  // Pattern noticed:
  // after 2000 generations, its 116,454.
  // After 3000 generations, its 173,454.
  // In other words, every 1000th generation adds 56,000
}

partTwo();

// Assume n > 2000, and is divisible by 1000
function getRemainingSum(n) {
  return ((n - 2000) / 1000 * 57000) + 116454;
}

// const leftPots = [];
// const rightPots = parseInitialState('#..#.#..##......###...###');
// const rules = parseRules([
//   '...## => #',
//   '..#.. => #',
//   '.#... => #',
//   '.#.#. => #',
//   '.#.## => #',
//   '.##.. => #',
//   '.#### => #',
//   '#.#.# => #',
//   '#.### => #',
//   '##.#. => #',
//   '##.## => #',
//   '###.. => #',
//   '###.# => #',
//   '####. => #'
// ]);
//
// const newPots = advanceGenerations(rules)(20)({leftPots, rightPots});
// console.log("Left pots: ");
// console.log(newPots.leftPots.map(toString).join(''));
// console.log("Right pots: ");
// console.log(newPots.rightPots.map(toString).join(''));
