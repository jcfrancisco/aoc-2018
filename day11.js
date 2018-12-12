const GRID_SIZE = 300;

function generateGrid(serialNumber) {
  return Array.from(
    { length: GRID_SIZE },
    (_, colIndex) =>
      Array.from(
        { length: GRID_SIZE },
        (_, rowIndex) => calculatePowerLevel(serialNumber, colIndex, rowIndex)
      )
  );
}

function calculatePowerLevel(serialNumber, colIndex, rowIndex) {
  // The problem specifies a grid that's 1-indexed, so we should adjust here
  const adjustedColIndex = colIndex + 1;
  const adjustedRowIndex = rowIndex + 1;

  const rackId = adjustedRowIndex + 10;
  let powerLevel = rackId * adjustedColIndex;
  powerLevel += serialNumber;
  powerLevel *= rackId;
  powerLevel = Math.floor((powerLevel % 1000) / 100);
  powerLevel -= 5;
  return powerLevel;
}

function getMostPowerful(grid) {
  // Cache[y][x][squareSize] = sum for [y][x] with squareSize - 1
  const cache = Array.from(
    { length: GRID_SIZE },
    () =>
      Array.from(
        { length: GRID_SIZE },
        () => []
      )
  );
  let mostPowerful = { x: null, y: null, value: Number.MIN_SAFE_INTEGER, squareSize: null };
  Array.from({ length: 300 }, (_, i) => {
    const squareSize = i + 1;
    const mostPowerfulForSquareSize = getMostPowerfulForSquareSize(grid, squareSize, cache);
    if (mostPowerfulForSquareSize.value > mostPowerful.value) {
      Object.assign(mostPowerful, {
        x: mostPowerfulForSquareSize.x,
        y: mostPowerfulForSquareSize.y,
        value: mostPowerfulForSquareSize.value,
        squareSize
      });
    }
  });
  const { x, y, squareSize } = mostPowerful;
  return { x, y, squareSize };
}

// Returns the coordinates (1-indexed) of the top left cell whose
// squareSize x squareSize block has the highest total power in the grid.
function getMostPowerfulForSquareSize(grid, squareSize, cache) {
  const mostPowerful = { x: null, y: null, value: Number.MIN_SAFE_INTEGER };

  grid.forEach((col, colIndex) => {
    col.forEach((powerLevel, rowIndex) => {
      if (
        colIndex > (GRID_SIZE - squareSize)
        || rowIndex > (GRID_SIZE - squareSize)
      ) {
        return;
      }
      const totalPowerLevel = getTotalPowerLevel(
        colIndex, rowIndex, grid, squareSize, cache
      );
      if (totalPowerLevel > mostPowerful.value) {
        mostPowerful.value = totalPowerLevel;
        mostPowerful.x = rowIndex;
        mostPowerful.y = colIndex;
      }
    });
  });

  const { x, y, value } = mostPowerful;
  return { x: x + 1, y: y + 1, value };
}

/* Total power level for a particular grid item, with a given square size
 *   a b c
 *   d e f
 *   g h i
 */
function getTotalPowerLevel(colIndex, rowIndex, grid, squareSize, cache) {
  if (cache && cache[colIndex][rowIndex][squareSize - 1]) {
    return cache[colIndex][rowIndex][squareSize - 1];
  }
  let sum;

  if (squareSize === 1) {
    sum = grid[colIndex][rowIndex];
  } else if (squareSize % 2 !== 0) {
    // If it's an odd square size, add the last row and the last column,
    // and recursively call on current cell (with squareSize - 1).
    const lastColIndex = squareSize + colIndex - 1;
    const lastRowIndex = squareSize + rowIndex - 1;

    let lastRowSum = 0;
    for (let c = colIndex; c <= lastColIndex; c++) {
      lastRowSum += grid[c][lastRowIndex];
    }

    let lastColSum = 0;
    // Don't look at [lastColIndex][lastRowIndex] here, because
    // that was already accounted for in the prior loop
    //                      👇
    for (let r = rowIndex; r < lastRowIndex; r++) {
      lastColSum += grid[lastColIndex][r];
    }

    sum = lastColSum + lastRowSum + getTotalPowerLevel(colIndex, rowIndex, grid, squareSize - 1, cache);
  } else {
    // If it's an even square size, split into four equal squares and recurse!
    const halfSquareSize = squareSize / 2;
    sum = getTotalPowerLevel(colIndex, rowIndex, grid, halfSquareSize, cache)
      + getTotalPowerLevel(
        colIndex + halfSquareSize, rowIndex, grid, halfSquareSize, cache
      )
      + getTotalPowerLevel(
        colIndex, rowIndex + halfSquareSize, grid, halfSquareSize, cache
      )
      + getTotalPowerLevel(
        colIndex + halfSquareSize, rowIndex + halfSquareSize, grid, halfSquareSize, cache
      );
  }

  if (cache) {
    cache[colIndex][rowIndex][squareSize - 1] = sum;
  }
  return sum;
}

// Testing power level
const exampleCases = [
  { serialNumber: 57, squareSize: 3, x: 122, y: 79, expectedPowerLevel: -5 },
  { serialNumber: 39, squareSize: 3, x: 217, y: 196, expectedPowerLevel: 0 },
  { serialNumber: 71, squareSize: 3, x: 101, y: 153, expectedPowerLevel: 4 },
];
exampleCases.forEach(({ serialNumber, squareSize, x, y, expectedPowerLevel }) => {
  const powerLevel = generateGrid(serialNumber)[y - 1][x - 1];
  if (powerLevel !== expectedPowerLevel) {
    console.log(
      `Grid ${serialNumber} was supposed to have ${expectedPowerLevel}`
      + ` at (${x}, ${y}) but had ${powerLevel} instead.`
    );
  } else {
    console.log(`Grid ${serialNumber} had the correct power level.`);
  }
});

// Testing total power
const exampleCases2 = [
  { serialNumber: 18, squareSize: 3, expectedX: 33, expectedY: 45 },
  { serialNumber: 42, squareSize: 3, expectedX: 21, expectedY: 61 }
];

exampleCases2.forEach(({ serialNumber, squareSize, expectedX, expectedY }) => {
  const { x, y } = getMostPowerfulForSquareSize(generateGrid(serialNumber), squareSize);
  if (x !== expectedX || y !== expectedY) {
    console.log(
      `Grid ${serialNumber} was supposed to be most powerful at `
      + `(${expectedX}, ${expectedY}) but had its most powerful `
      + `at (${x},${y}) instead.`
    );
  } else {
    console.log(`Grid ${serialNumber} had the correct most powerful cell.`);
  }
});

// Testing square size

const exampleCases3 = [
  { serialNumber: 18, expectedX: 90, expectedY: 269, expectedSquareSize: 16 },
  { serialNumber: 42, expectedX: 232, expectedY: 251, expectedSquareSize: 12 }
];

exampleCases3.forEach(({ serialNumber, expectedX, expectedY, expectedSquareSize }) => {
  const { x, y, squareSize } = getMostPowerful(generateGrid(serialNumber));
  if (x !== expectedX || y !== expectedY || squareSize !== expectedSquareSize) {
    console.log(
      `Grid ${serialNumber} was supposed to be most powerful with square size `
      + `${expectedSquareSize}, at point (${expectedX}, ${expectedY}). Instead,`
      + ` its square size was ${squareSize} and most powerful was at `
      + `(${x},${y}) instead.`
    );
  } else {
    console.log(`Grid ${serialNumber} had the correct most powerful cell and square size.`);
  }
});

console.log(JSON.stringify(getMostPowerful(generateGrid(5535))));
