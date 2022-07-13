# HacMan
A Haskell version of the popular retro game Pac-Man.

The main game play is the same: the player must move Pac-Man around the maze to eat all the pellets while 
avoiding the ghosts. There are also larger pellets that cause the ghosts to become scared and turn 
blue; in this state Pac-Man can eat the ghosts, which gives the player points and sends the ghost 
back to its starting position where it returns to its normal state and colour.

However, there are also some differences. One of the largest changes is that ghosts stay blue until 
the user eats one of them or finishes the level – there’s no time limit for how long the ghosts are 
blue for within one level. If a ghost is eaten, it returns to its starting position like in the original 
game, but all the other ghosts also change back to regular mode rather than remaining as ghosts.

Another difference is that while in Pac-Man, there are 256 levels 
that increase in difficulty by increasing the speed of the ghosts and reducing the amount of 
time they’re blue for after Pac-Man eats a larger pellet, in HacMan there are 4 levels which increase 
in difficulty by adding a ghost. The level a user is on corresponds with the number of ghosts in the 
game. 

Furthermore, the fruit that Pac-Man eats to gain more points haven’t been implemented in HacMan 
and the player’s points are calculated differently. In Pac-Man the player gains points by eating 
pellets and fruit, while in HacMan the user starts on a high number of points which decreases as 
time goes on, so their final score is partly based on how long it took them to finish the level.

The last change is that all the ghosts in HacMan move completely randomly, whereas the ghosts in 
Pac-Man have distinct personalities which show through their movements. In Pac-Man the ghosts 
are programmed to know how to exit the box they start in but due to the fully random movement in 
HacMan, ghosts may spend a few of their first movements trying to exit the box.

The program is split into four modules: the main file, which controls the running of the game; the 
maze file, which contains definitions and functions involving it; the sprite file, which contains 
definitions and functions about the Pac-Man and ghost characters; the display file, which contains 
definitions about the overall game and functions that are used in the other files. 
