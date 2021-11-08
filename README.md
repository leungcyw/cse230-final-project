# CSE 230: Elsa and Olaf
# Introduction

For this project, we would like to implement a spinoff of the popular game [Fireboy and Watergirl](https://www.coolmathgames.com/0-fireboy-and-water-girl-in-the-forest-temple), Elsa and Olaf. In our game, we seek to create a world wherein two characters must collaborate to overcome obstacles, collect tokens, and reach the exit door. We also aim to introduce a fun, new, interactive feature where users can provide a text file as input, which will then be parsed into a unique level.

<img src="https://i.imgur.com/dytigqQ.png" alt="game" width="500"/>

*A screenshot of the original Fireboy and Watergirl game*

# Game Mechanics
The two characters can be controlled using keyboard input--one character will be moved using the up, left, and down arrow keys, while the other will be controlled using the characters "A," "W," and "D."

The primary obstacle of our game is the pools of liquid (where characters can only pass through liquid of the same color as their body). They will also need to enlist the help of platforms that are controlled by levers and buttons to reach the exit door.

The objective of the game is to pass a given level. To pass a level, each character must collect all tokens associated with that character, avoid obstacles, and reach the door corresponding to that character. 

# Goals
In making this application, we would like to create an interactive local two-player game that involves puzzle solving and collaboration to complete. We would also like to allow players to express their creative side by allowing them to design their own levels. 


# Deliverables
- A working interactive game that can be installed through *stack* and played through terminal.
- Example text input for levels that can be parsed and played.
- A comprehensive mapping from ASCII characters to game world elements so the starting state of the game is generated with an input text file.
