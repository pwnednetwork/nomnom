# nomnom

## Purpose

nomnom is a small package for emacs I recently began developing. It helps you keep track of food. Whenever you load a .fud file it should kick in (if you configured it properly) or you can manually set it by doing M-x nomnom-mode 

You can add another day via nomnom-insert-date (SPC c i if properly configured ).
*TODO: should a date self-append if we don't have any entries in the file for the current date? Is there a reason not to do that?*

## Syntax
For now you can write items like this:

```
<2025-03-30 Sun> 
400 steak
100x2 apple
```

Upon saving nomnom-calculate will be called which will go through all the days in current file and all the items and update the total calorie count next to the date. Like so:

```               
<2025-03-30 Sun>  600
400 steak         
100x2 apple       
```               

## Installation and configuration
See INSTALL.md. For now nomonom is written exclusively in emacs lisp and requires no building.

## TODO
- ability to put grams, ml, or count + item and have the script reference a local database to figure out how many calories it will be. For example:

```
80g cheese
1 slice white bread
```
