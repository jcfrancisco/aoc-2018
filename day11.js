function generateGrid(serialNumber) {
  return Array.from(
    { length: 300 },
    (_, colIndex) =>
      Array.from(
        { length: 300 },
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

// Returns the coordinates (1-indexed) of the top left cell whose 3x3 block
// has the highest total power in the grid.
function getMostPowerful(grid) {
  const mostPowerful = { x: null, y: null, value: 0 };

  grid.forEach((col, colIndex) => {
    col.forEach((powerLevel, rowIndex) => {
      if (colIndex > 297 || rowIndex > 297) {
        return;
      }
      const topLeft = powerLevel;
      const top = grid[colIndex + 1][rowIndex];
      const topRight = grid[colIndex + 2][rowIndex];
      const left = grid[colIndex][rowIndex + 1];
      const center = grid[colIndex + 1][rowIndex + 1];
      const right = grid[colIndex + 2][rowIndex + 1];
      const bottomLeft = grid[colIndex][rowIndex + 2];
      const bottom = grid[colIndex + 1][rowIndex + 2];
      const bottomRight = grid[colIndex + 2][rowIndex + 2];
      const totalPowerLevel = topLeft + top + topRight + left + center + right
        + bottomLeft + bottom + bottomRight;
      if (totalPowerLevel > mostPowerful.value) {
        mostPowerful.value = totalPowerLevel;
        mostPowerful.x = rowIndex;
        mostPowerful.y = colIndex;
      }
    });
  });

  return { x: mostPowerful.x + 1, y: mostPowerful.y + 1 };
}

// Testing power level
const exampleCases = [
  { serialNumber: 57, x: 122, y: 79, expectedPowerLevel: -5 },
  { serialNumber: 39, x: 217, y: 196, expectedPowerLevel: 0 },
  { serialNumber: 71, x: 101, y: 153, expectedPowerLevel: 4 },
];
exampleCases.forEach(({ serialNumber, x, y, expectedPowerLevel }) => {
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
  { serialNumber: 18, expectedX: 33, expectedY: 45 },
  { serialNumber: 42, expectedX: 21, expectedY: 61 }
];

exampleCases2.forEach(({ serialNumber, expectedX, expectedY }) => {
  const { x, y } = getMostPowerful(generateGrid(serialNumber));
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

console.log(getMostPowerful(generateGrid(5535)));
