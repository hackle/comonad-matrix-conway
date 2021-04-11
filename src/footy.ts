const players = [ 'A', 'B', 'C', 'D', 'E' ];
const passes = players.map((currentPlayer, focus, context) => `${currentPlayer} passes the ball to ${context[(focus + 1) % context.length]}`);

console.log(passes.join(', '));