const fs = require('fs');
const readline = require('readline');

// Takes the input string and makes a list of points
function parsePoints(inputString) {
  const lines = inputString.split("\n");

  return lines.filter(line => !!line).map(line => {
    if (!line) {
      return;
    }
    const matches = line.match(
      /position=<([^,]*),([^,]*)>.*velocity=<([^,]*),([^,]*)>/
    );
    const [posX, posY, velX, velY] =
      matches.slice(1).map(match => parseInt(match));

    return { posX, posY, velX, velY };
  });
}

// Given a list of points, create a small printable grid.
//
// Each point in the grid corresponds to a (shrinkFactor x shrinkFactor)
// square of the "real" grid (as specified by the point positions).
function toGrid(shrinkFactor, points) {
  const { lowX, lowY, highX, highY } = getHighAndLowValues(points);
  const sizeX = Math.ceil((Math.abs(lowX) + highX + 1) / shrinkFactor);
  const sizeY = Math.ceil((Math.abs(lowY) + highY + 1) / shrinkFactor);
  const grid = Array.from({ length: sizeY }, () => false).map(() =>
    Array.from({ length: sizeX }, () => false)
  );

  points.forEach(({ posX, posY, velX, velY }) => {
    const y = Math.floor((posY + Math.abs(lowY)) / shrinkFactor);
    const x = Math.floor((posX + Math.abs(lowX)) / shrinkFactor);
    grid[y][x] = true;
  });

  return grid;
}

function getHighAndLowValues(points) {
  let lowX = 0, lowY = 0, highX = 0, highY = 0;
  points.forEach(({ posX, posY }) => {
    if (posX < lowX) { lowX = posX };
    if (posX > highX) { highX = posX };
    if (posY < lowY) { lowY = posY };
    if (posY > highY) { highY = posY };
  });
  return { lowX, lowY, highX, highY };
}

// Move points n seconds
function movePoints(n, points) {
  let newPoints = points;
  for (let i = 0; i < n; i++) {
    newPoints = newPoints.map(point => (
      {
        posX: point.posX + point.velX,
        posY: point.posY + point.velY,
        velX: point.velX,
        velY: point.velY
      }
    ));
  }
  return newPoints;
}

function toString(grid) {
  let s = '';
  grid.forEach(col => {
    col.forEach(gridItem => {
      s += !gridItem ? '.' : '#';
    });
    s += '\n';
  });
  return s;
}

const points = parsePoints(fs.readFileSync('./input/day10', 'utf-8'));

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

// Message shows up at: 10079 moves, scaling factor of 1
const askQuestion = () => {
  rl.question("How many moves? ", moves => {
    rl.question("Scaling factor? ", scale => {
      console.log(toString(toGrid(scale, movePoints(moves, points))));

      askQuestion();
    });
  });
}

askQuestion();
