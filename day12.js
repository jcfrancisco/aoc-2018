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

/* Parse the initial state into an array. Also add
 * padding to the left and right, so that the final length is
 * 4x the initial length.
 *
 * Each element of the array also contains a `realIndex` value
 * that can be negative.
 * { isPlanted: bool, realIndex: number }
 */
const parseInitialState = initialState => {
  const parsed =
    initialState.split('').map((s, i) => (
      {
        isPlanted: toBool(s),
        realIndex: i
      }
    ));
  const leftSide =
    Array.from({ length: parsed.length }, (_, i) => (
      {
        isPlanted: false,
        realIndex: i - parsed.length
      }
    ));
  const rightSide =
    Array.from({ length: parsed.length }, (_, i) => (
      {
        isPlanted: false,
        realIndex: parsed.length + i
      }
    ));
  return leftSide.concat(parsed).concat(rightSide);
}

const advanceGenerations = rules => n => pots => {
  let advancedPots = [...pots];
  for (let i = 0; i < n; i++) {
    advancedPots = advancedPots.map(
      (currentPot, j) => j - 2 < 0 || j + 2 > advancedPots.length - 1
        // Don't run the rules if we're at the extreme left or extreme right of the pots
        ? currentPot
        : {
            realIndex: currentPot.realIndex,
            isPlanted: rules(
              ...[
                advancedPots[j - 2],
                advancedPots[j - 1],
                currentPot,
                advancedPots[j + 1],
                advancedPots[j + 2]
              ].map(pot => pot.isPlanted)
            )
          }
    );
  }
  return advancedPots;
};

const countPlantPots = pots =>
  pots.reduce(
    (acc, { isPlanted, realIndex }) => isPlanted
      ? acc + realIndex
      : acc,
    0
  );

const partOne = () => {
  const inputLines = fs.readFileSync('./input/day12', 'utf-8').split('\n');
  const initialState = parseInitialState(inputLines[0].split(': ')[1]);
  const rules = parseRules(inputLines.slice(2));

  console.log(
    countPlantPots(
      advanceGenerations(rules)(20)(initialState)
    )
  );
};

partOne();

const partTwo = () => {
  const inputLines = fs.readFileSync('./input/day12', 'utf-8').split('\n');
  const initialState = parseInitialState(inputLines[0].split(': ')[1]);
  const rules = parseRules(inputLines.slice(2));

  console.log(
    countPlantPots(
      advanceGenerations(rules)(50000000000)(initialState)
    )
  );
};

//partTwo();


//const pots = parseInitialState('#..#.#..##......###...###');
//const rules = parseRules([
//  '...## => #',
//  '..#.. => #',
//  '.#... => #',
//  '.#.#. => #',
//  '.#.## => #',
//  '.##.. => #',
//  '.#### => #',
//  '#.#.# => #',
//  '#.### => #',
//  '##.#. => #',
//  '##.## => #',
//  '###.. => #',
//  '###.# => #',
//  '####. => #'
//]);
