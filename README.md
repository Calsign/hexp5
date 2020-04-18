
Implementation of the board game
[Hex](https://en.wikipedia.org/wiki/Hex_(board_game))
using [p5ml](https://github.com/Calsign/p5ml).
Inspired by an introductory game theory class (ECON 2801 at Cornell).

## Rules

There are two players. The first player has white tiles and the second has
black tiles. Players alternate placing one tile on the board. White's goal is
to connect the two white sides of the board with a continuous chain of white
tiles, while black's goal is to do the same but with black. When this happens,
the game is over and the player that formed a continuous chain is victorious.

One of the interesting features of this game is that it is impossible for there
to be a draw; after enough moves, either white or black will be victorious.

## Controls

Click to place a tile. Use the left and right arrow keys to undo and redo,
respectively.

Type a number and press enter to resize the board. For example, type `1`, `2`,
and then press enter to make a 12 by 12 board. Note that this erases a game
if there was one in progress.

Works like a normal game by default, alternating between white and black tiles,
but for exploratory purposes you can change the placement mode. Press `w` to
place only white tiles, `b` to place only black tiles, `x` to place
obstructions (e.g. creating a hole in the middle of the board), and `g` to
return to a normal alternating game.
