<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
> This page is currently incomplete.

# [Streams](./0_Contents.md)
## Introduction

## Contents:
* [What are the LKSL Streams for?](#what-are-the-lksl-streams-for)
  * [Interfaced Streams](#interfaced-streams)
* [Carets](#carets)
  * [Interfaced Carets](#interfaced-carets)

## What are the LKSL Streams for?
The LKSL provides a set of bespoke *Stream* types, designed specifically to play nicely when being accessed (*read or modified*) from multiple *Threads* at the same time.

For *Memory Streams*, this represents a significant improvement as the same block of memory can be read by as many *Threads* at any one time as you need, but only so long as that block of memory isn't being *modified* at the time.

For *File Streams* (derrived from a *Handle Stream*), only one consumer can read from or modify it at any one time. This is simply a limitation of the way these kinds of *Stream* work. However, multiple *Threads* can make read and/or write requests at any given time, and those requests will simply wait for the current request to be completed.

The LKSL Streams take care of all of this for you, by enforcing appropriate *Locks* and *State Switches* (where required), eliminating the need for your implementation to handle these issues.

**The LKSL Streams are *not* just fancy wrappers sitting on top of `TStream`... they are entirely new Stream implementations.**
Efforts are being taken, however, to make them as analogous of `TStream` and its various descendants as possible.

## Interfaced Streams
The *Stream Types* in the LKSL are represented by appropriate *Interfaces*, meaning that you can exploit *Reference Counting* to eliminate the need to manually manage their lifetimes.

> It's important to note that the implemented *Stream Types* may also extend the base *Interface* with unique properties and methods particular to that *Stream Type*.

The base *Stream Interface Type* is called `ILKStream`, and provides all of the public methods and properties available to *all* of the LKSL *Stream Types*.

The definition of `ILKStream` is currently as follows:

```pascal
  ILKStream = interface
  ['{07F45B12-1DFC-453A-B95C-E00C9F5F4285}']
    function GetSize: Int64;
    procedure SetSize(const ASize: Int64);

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    property Size: Int64 read GetSize write SetSize;
  end;
```

As you would expect, the `Size` property retrieves the current size of the Stream, and (on assignment) will expand or collapse the Stream to the specified size.

## Carets
The most powerful feature of the LSKL *Streams* is that they allow individual *consumers* to request their own *Carets*.

A *Caret* behaves much in the same way as they do in a text or code editor: they provide their own *Position* reference within the *Stream*, and all actions executed against a particular *Caret* take place in the context of that *Position*.

### Interfaced Carets

The *Caret Types* are also *Interfaced*, so you don't need to worry about managing their respective lifetimes.

> It's important to note that the implemented *Stream Caret Types* may also extend the base *Interface* with unique properties and methods particular to that *Stream Caret Type*.

The base *Caret Interface Type* is aclled `ILKStreamCaret`, and provides all of the public methods and properties available to *all* of the LKSL *Stream Caret Types*.

The definition of `ILKStreamCaret` is as follows:

```pascal
  ILKStreamCaret = interface
  ['{D8E849E5-A5A1-4B4F-9AF6-BBD397216C5B}']
    function GetPosition: Int64;
    procedure SetPosition(const APosition: Int64);

    function GetStream: ILKStream;

    function Delete(const ALength: Int64): Int64;
    function Insert(const ABuffer; const ALength: Int64): Int64;
    function MoveBytes(const ACount: Int64; const AOffset: Int64): Int64;
    function Read(var ABuffer; const ALength: Int64): Int64;
    function Write(const ABuffer; const ALength: Int64): Int64;
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;

    property Position: Int64 read GetPosition write SetPosition;
    property Stream: ILKStream read GetStream;
  end;
```

While this interface should be fairly self-explanatory, here is a table describing them in brief:

| Method/Property   | Description                                                                                               |
| ----------------- | --------------------------------------------------------------------------------------------------------- |
| `Delete`          | Removes the given number of Bytes from the current Position (shifting subsequent Bytes to the Left)       |
| `Insert`          | Inserts the given Buffer into the Stream at the current Position (shifting subsequent Bytes to the Right) |
| `MoveBytes`       | Moves the given number of Bytes within the Stream from the current Position by the given Offset           |
| `Read`            | Populates the given Buffer with Bytes of the given number                                                 |
| `Write`           | Writes a given Buffer of a given Size into the Stream from the current Position                           |
| `Seek`            | Adjusts the Position of this Caret (behaves like `TStream.Seek`)                                          |
| `Position`        | Gets/Sets the Position of this Caret within the Stream                                                    |
| `Stream`          | Returns the Stream which owns this Caret                                                                  |

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->
