# Space Invaders BSL    
A simple version of Space Invaders written in Racket BSL. This program is the final project for UBCx's    HtC1x, the first module of their [Software Development MicroMasters](https://www.edx.org/micromasters/ubcx-software-development) program on edX.
    
## Usage    
    
Download the `space-invaders.rkt` file and either run it through the DrRacket IDE or from the command line with:    
    
```       
$ racket space-invaders.rkt    
```
![space-invaders](https://user-images.githubusercontent.com/67990439/152660114-d87d620b-aebf-41b3-9476-940f2ecdba9e.png)
### Controls

In the game, the tank will constantly move either left or right. To change the movement direction of the tank use the "left" and "right" arrow keys. 
Shoot missiles by pressing the "spacebar".

Invaders will be considered shot if a missile is within a 10px range of the invader's (x,y) coordinate.

The game ends if an invader lands.
            

## Options                                                                                                
At the beginning on the file there are several constants defined that can be fine tuned to change the     gameplay.    
                                                                       
For example, to change the size of the game window, adjust:
```
- WIDTH  (default 300)    
- HEIGHT (default 500)                                                                                                                                               
```
You can also change the movement speed of the tank, the missiles, or the invaders (both their horizontal  and vertical speeds can be adjusted):
```
- INVADER-X-SPEED (default 1.5)    
- INVADER-Y-SPEED (default 1.5)    
- TANK-SPEED      (default 3)     
- MISSILE-SPEED   (default 10)    
```    
Finally, to change the rate at which invaders are spawned, adjust the invate rate variable:
```
- INVADE-RATE (default 5)    
``` 
