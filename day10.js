const fs = require('fs');

// Takes the input string and makes a 2d grid. each element of the grid includes
// its velocity.
// grid[1][2] : second row, first column.
function toGrid(inputString) {
  const lines = inputString.split("\n");

  // find lowest and highest x and y positions in order to
  // figure out how large of a grid to construct.
  let lowX = 0, lowY = 0, highX = 0, highY = 0;
  const allPoints = lines.filter(line => !!line).map(line => {
    if (!line) {
      return;
    }
    const matches = line.match(
      /position=<([^,]*),([^,]*)>.*velocity=<([^,]*),([^,]*)>/
    );
    const [posX, posY, velX, velY] =
      matches.slice(1).map(match => parseInt(match));

    if (posX < lowX) { lowX = posX; }
    if (posX > highX) { highX = posX; }
    if (posY < lowY) { lowY = posY; }
    if (posY > highY) { highY = posY; }

    return { posX, posY, velX, velY };
  });

  const sizeX = Math.abs(lowX) + highX + 1;
  const sizeY = Math.abs(lowY) + highY + 1;
  const grid = Array.from({ length: sizeY }, () => null).map(() =>
    Array.from({ length: sizeX }, () => null)
  );

  allPoints.forEach(({ posX, posY, velX, velY }) => {
    const y = posY + Math.abs(lowY);
    const x = posX + Math.abs(lowX);
    const newPoint = {velX, velY};
    grid[y][x] = (grid[y][x] || []).concat(newPoint);
  });

  return grid;
}

// Move grid n times
function moveGrid(n, grid) {
  if (n === 0) { return grid; }
  const newGrid = Array.from({ length: grid.length }, () => null).map(() =>
    Array.from({ length: grid[0].length }, () => null)
  );
  grid.forEach((col, colIndex) => {
    col.forEach((points, rowIndex) => {
      if (!points) { return; }

      points.forEach(point => {
        const y = colIndex + point.velY;
        const x = rowIndex + point.velX;
        newGrid[y][x] = (newGrid[y][x] || []).concat([point]);
      });
    });
  });
  return moveGrid(n - 1, newGrid);
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

const exampleInput = `position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>`;

//console.log(toString(moveGrid(3, toGrid(exampleInput))));
//console.log(toString(moveGrid(0, toGrid(fs.readFileSync('./input/day10', 'utf-8')))));
const grid = toGrid(fs.readFileSync('./input/day10', 'utf-8'));
