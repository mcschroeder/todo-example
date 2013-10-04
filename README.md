# todo-example in Haskell

todo-example is a showcase of a lightweight web application written in Haskell.

A simple todo list server with a REST API using our own frameworks Welshy and TX, with a simple UI displaying the interaction with the API.

## Why?

todo-example was inspired and resulted from parts of a project done in the lecture 'Industrial Haskell' at [Vienna UT](http://www.informatik.tuwien.ac.at/) by [Michael Schröder](https://twitter.com/schrototo), [Harald Steinlechner](https://github.com/haraldsteinlechner), and [Jürgen Cito](https://twitter.com/citostyle).

Following our presentation of the project, feedback from fellow students was something along the lines of 'Wow, you can write web applications in Haskell? I thought it's only for compilers and other academic stuff'. Our aim is to change this perception of Haskell and showcase a simple working web applicatiion

## Screenshot/Usage

Below you can see a screenshot of what it looks like. You can create new lists of todo items which you can share with the 'Invite-URL'. Everyone who has the link can then add, edit and delete items from the list - updates are made available to everyone in near realtime.

![haskell todo-example Screenshot](screenshot.png)

## Frameworks
### Welshy

A Haskell web framework heavily influenced by the excellent Scotty, which was in turn influenced by Ruby's Sinatra.

Welshy strives to make it easier to do error handling without overly complicating the control flow.

Welshy is on [Hackage](http://hackage.haskell.org/package/welshy)


### TX

TX provides persistent transaction on top of STM. 

TX on [Hackage](http://hackage.haskell.org/package/tx)

## Slides

todo-example was presented as a lightning talk at the [New York Haskell User Group](http://www.meetup.com/NY-Haskell/). The slides are online on [SpeakerDeck](https://speakerdeck.com/citostyle/haskell-todo-example).


