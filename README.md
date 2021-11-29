# CSE 230: Elsa and Olaf
Group Members: Eric Ke (ericke8), Christina Leung (leungcyw), Yiming Zhao (yimingnzhao)
# Introduction

For this project, we would like to implement a spinoff of the popular game [Fireboy and Watergirl](https://www.coolmathgames.com/0-fireboy-and-water-girl-in-the-forest-temple), Elsa and Olaf. In our game, we seek to create a world wherein two characters must collaborate to overcome obstacles, collect tokens, and reach the exit door. We also aim to introduce a fun, new, interactive feature where users can provide a text file as input, which will then be parsed into a unique level.

<img src="https://i.imgur.com/dytigqQ.png" alt="game" width="500"/>

*A screenshot of the original Fireboy and Watergirl game*

# Game Mechanics
The two characters can be controlled using keyboard input--one character will be moved using the up, left, right, and down arrow keys, while the other will be controlled using the characters "A," "W," "D," and “S.”

The primary obstacle of our game is the pools of liquid (where characters can only pass through liquid of the same color as their body). They will also need to enlist the help of platforms that are controlled by levers and buttons to reach the exit door.

The objective of the game is to pass a given level. To pass a level, each character must collect all tokens associated with that character, avoid obstacles, and reach the door corresponding to that character. 

# Goals
In making this application, we would like to create an interactive local two-player game that involves puzzle solving and collaboration to complete. We would also like to allow players to express their creative side by allowing them to design their own levels. 


# Deliverables
- A working interactive game that can be installed through *stack* and played through terminal.
- Example text input for levels that can be parsed and played.
- A comprehensive mapping from ASCII characters to game world elements so the starting state of the game is generated with an input text file.

# Application Architecture
Our application can be divided into three main sections: the game environment, the characters, and the user interface. 

The game environment is a data type that includes the data attributes for game objects, such as tokens, lakes, exits, and game state booleans (level failed, level passed). The game environment also contains the characters, which we keep as a separate data type due to the additional interactivity and complexity of the characters. 

The character data type holds information about the current horizontal and vertical positions and velocities of the respective character. We use positions to determine where the character actually appears in the game and velocities are used to determine how character horizontal and vertical positions will change in the next time step. By having a character object, we can use two instances to independently track the movements of both players in the game.

The final key component of our game is the user interface. As part of this component, we define the shape, size, and color of the game’s interface, as well as how the tokens, lakes, exits, and characters appear. We also include keyboard bindings to specific actions as part of the user interface experience. For example, we have arrow keys and WASD to control the two characters, respectively, as well as bindings for quitting and restarting. Finally, we include instructions for how to play the game, and failure/success messages.

# Challenges in Implementation
One of the biggest challenges was learning how to use the Brick and Graphics library since they were completely new to us. Luckily, there were a lot of resources online, such as tutorials or example projects, that were great learning tools. Experimenting with different features of the library was also a great way to familiarize ourselves with the language and explore how to best leverage the libraries according to our application’s needs. In addition, the Hackage documentation of various packages was a huge help in figuring out how to use features such as lenses.

Since we have moving characters in our application, another challenge was making their movements appear as natural as possible such that both players can smoothly control their respective characters. The first challenge in character control is to minimize the reassignment of character data per time step. We expect that character data should change at every time step, but we strive to ensure this reassignment happens at most once per tick. This helps with the fluidity of character movements, allowing for the appearance of both characters moving at once.

Another challenge with character movements is jumping animations. This entailed implementing acceleration when the characters jumped up and ensuring that once in the air, they fell down due to the force of gravity. We were once again able to find resources online on how to achieve this, including tutorials inspired by the character movements in Super Mario. Specifically, instead of dynamically computing the position of the character using uniform acceleration equations, we define and use a precomputed velocity vector that allows us to access the velocity via indices. We were inspired by the ideas presented in the tutorial, and implemented our own version independently for both vertical and horizontal accelerations, which defines how fast our character can move around in the stage.

# Assessing Our Progress
We expect that we will be able to meet our goals before the deadline, although our game might not have every single feature we planned for. We currently have completed:
- Character movements
- Support for two characters
- Collecting tokens for characters
- Lakes that only a specific character can pass through
- Win and Lose conditions

For the next steps, we plan on implementing the following:
- Platforms
- Collision detection (could be done alongside platforms)
- Buttons and/or Levers
- User-inputted levels

# Modifying Our Goals
Instead of including interactive buttons and levers inside of our game, we will first aim to implement only one of the two. This is because buttons and levers both result in an animation of another game object (being a moveable platform), which would iterate through a set number of time steps (i.e. not instantaneous). The difficulty would be to compute collisions while two objects (the moving platform and the character) require movement in the same time step. Because buttons and levers trigger platform movement under different conditions, we would like to first focus on collision logic before focusing on the different conditions for moveable platforms. 
