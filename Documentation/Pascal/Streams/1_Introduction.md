<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
> This page is currently incomplete.

# [Streams](./0_Contents.md)
## Introduction

## Contents:
* [What are the LKSL Streams for?](#what-are-the-lksl-streams-for)
* [Interfaced Streams](#interfaced-streams)

## What are the LKSL Streams for?
The LKSL provides a set of bespoke *Stream* types, designed specifically to play nicely when being accessed (*read or modified*) from multiple *Threads* at the same time.

For *Memory Streams*, this represents a significant improvement as the same block of memory can be read by as many *Threads* at any one time as you need, but only so long as that block of memory isn't being *modified* at the time.

For *File Streams* (derrived from a *Handle Stream*), only one comsumer can read from or modify it at any one time. This is simply a limitation of the way these kinds of *Stream* work.

The LKSL Streams take care of all of this for you, by enforcing appropriate *Locks* and *State Switches* (where required), eliminating the need for your implementation to handle these issues.

## Interfaced Streams
The *Stream Types* in the LKSL are represented by appropriate *Interfaces*, meaning that you can exploit *Reference Counting* to eliminate the need to manually manage their lifetimes.

The base *Stream Interface Type* is called `ILKStream`, and provides all of the public methods and properties available to *all* of the LKSL *Stream Types*.

> Section in progress (work interrupted) please view again shortly!

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->
