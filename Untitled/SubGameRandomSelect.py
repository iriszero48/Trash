import os
import random

game_list = list(set("""
Rubik's Cube
Maze
Solve the no connection puzzle
Solve a Numbrix puzzle
Solve a Hopido puzzle
Solve a Holy Knight's tour
Solve a Hidato puzzle
Word wheel
Chess player
Minesweeper game
Flipping bits game
Towers of Hanoi
Deal cards for FreeCell
RCRPG
Remote agent
Go Fish
Knight's tour
Hunt_The_Wumpus
CLI-based maze-game
Greed
Word wheel
Hunt The Wumpus
Robots
Black Box
Mastermind
Hexapawn
Nim Game
Snake
Tamagotchi emulator
Rock-paper-scissors
Morpion solitaire
Sokoban
16Puzzle
asteroids
bang
betwixt
blix
boom
boomerang
climb
clocks
colorSort
columns
crackTheLock
fiveOrMore
flappypac
flipIt
halloween
knightsTour
mastermind
mazeClimb
memdots
minesweeper
minicow
ninjaJump
numbers
pairs
panic
pegSolitaire
plusPlus
pong
pudi
reaction
riseRise
rps
spaceInvaders
spikes
square
stairs
steps
tetris
tictactoe
timber
upJump
zenax
13
Add 7
Blocks
Bones
Chain shot
Colors sort
Dice puzzle
FlipFlop
Greed
Hippodrome
Inundo
Lights out
Mines
MobFlux
Move!
Game of Pig
Ping
Pipes
Queens quadrille
Robots
SET Game
Solichess
YANG
15 Puzzle
Arkanoid
Black box
Break the C0D3
Hexapawn
Sector26
Snake
Binary
Blox
Button Game
Cascade II
FloodThis
Funk It!
HiLights
Knight's Tour
Lines
MasterMind
Memory Game
Rivers
Space Invaders
Tower of Hanoi
Tic tac toe
Theseus and the Minotaur
Turn It!
TrafficJam
""".lower().strip().split('\n')))


def get_path(index):
    return f'roll{file_index}'


if __name__ == '__main__':
    file_index = 0
    filepath = get_path(file_index)
    while True:
        if not (os.path.exists(filepath) or os.path.exists(f'{filepath}.select')):
            break
        file_index += 1
        filepath = get_path(file_index)
    with open(filepath, 'w') as file:
        file.write(game_list[random.SystemRandom().randint(0, len(game_list) - 1)])
