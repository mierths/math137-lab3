# Programming Lab 3

In Lab 2, we wrote the core logic/rules of our simulation.
In this lab, we are going to turn that into an interactive program that we can see and control.
I've written most of the program for you, but I've left a few crucial pieces for you to fill in.

Fork this repo to your personal GitHub, then Clone it to your computer to begin.

## First Steps

  1. In your terminal, start `elm reactor`. It'll tell you a web address. Open that web address in your browser, navigate to `src` => `Main.elm`. You should see a black screen with an error message.

  2. Find all the places that say `todo` in `src/Life.elm`, and fill those in with your code from Lab 2. Save your file.

  3. Find all the places that say `todo` in `src/Main.elm`, and fill those in, using the provided libraries. This will be a bit challenging, but I'll help you. You will need to do a little bit of coordinate geometry (with which I'll also help you), so it's a good idea to keep out a pencil and some scratch paper. Save your file.

  4. Once all the `todo`s are filled in, your program should run. Save your files and go back to the browser and try to run `src/Main.elm`. You should see stuff!

## Making Your Program Fast

The program will not be very fast.
The reason is our game board data is stored as a function, but functions are not very efficient at _storing_ data (functions are efficient at _transforming_ data).

We need to convert our game board function into a data structure at each clock tick.
The functions and types needed to do this can be found in `Lib.Memoize`.
This will not require big changes to your program, in fact the change will be quite painless, but it is a bit technical, so I'll help you with this step.

Once done, try your program again in the browser.
It should be much faster, basically for free.

## Making Your Program Interactive

The player will want the ability to pause the simulation.
A good option is to pause/unpause the game via the spacebar, but it's your game, you can choose how the player pauses/unpauses the game.

  1. Add a field to your `State` type called `paused` with type `Bool`.

  2. Modify the `updateGame` function so that it does not apply `nextBoard` if the game is paused.

  3. Modify the `updateGame` function to listen for a key press (spacebar, for example) and toggle the value of `paused` when that key is pressed.

  4. Save your files and try out the new feature in the browser.

## Conjuring Life From The Void

The player will want the ability to choose the status of a cell by clicking on it.

  1. Add a variant to your `Event` type that carries a `Cell`.

  2. Modify your `drawCell` function so that its `onClick` property will trigger your new event.

  3. Modify your `updateGame` function so that when you receive your new event, the status of that cell will be flipped.

  4. Save your files and try them out in the browser.

## All Done

When complete, submit the URL of your fork in Canvas. I haven't decided a due date yet, but this is likely to be about two weeks of work.
