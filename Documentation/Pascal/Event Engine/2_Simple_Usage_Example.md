<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
# [Event Engine](./0_Contents.md)
## Simple Usage Example

## Contents:
* [What is the Event Engine?](#what-is-the-event-engine)
* [Units to Include](#units-to-include)
* [Defining a custom Event Type](#defining-a-custom-event-type)
    * [Lifetime Control](#important-note-lifetime-control)
* [Defining an Event Listener](#defining-an-event-listener)
* [Defining an Event Thread](#defining-an-event-thread)
    * [What is an Event Thread?](#what-is-an-event-thread)
    * [On to the code!](#on-to-the-code)
    * [Freeing/Finalizing an Event Thread](#important-note-freeingfinalizing-an-event-thread)
* [Preparing and Dispatching an Event](#preparing-and-dispatching-an-event)

### What is the *Event Engine*?
If you haven't done so already, you should first read the [Event Engine Introduction](./1_Introduction.md) article here on the LKSL Documentation.

### Units to Include
For the sake of inlining often-used (but small) routines across virtually every Class Type in the LKSL, you should include a reference to `System.SyncObjs` into the *Uses* section of any unit interacting with the LKSL. If you don't, you'll get compiler warnings informing you that certain methods could not be properly inlined.

For any Unit implementing any class in the Event Engine (*such as defining a new Event Type, Event Listener Type, or Event Thread Type*), you need to include a reference to `LKSL.Events.Main` in the *Uses* section.

> It may also be beneficial (*when defining an Event Thread Type*) to include a reference to `LKSL.Threads.Main` in the *Uses* section. This is, again, for the sake of code inlining.

### Defining a custom Event Type
Event Types inherit from their common Base Type, `TLKEvent`.

> Events should not implement any functionality! They should serve only to contain a collection of Values, to be consumed by any interest *Event Listeners*.

Here's an example of the most simple possible Event Type declaration:

```pascal
type
  TMyEvent = class(TLKEvent)
  private
    FFoo: String;
  public
    constructor Create(const AFoo: String); reintroduce;
    property Foo: String read FFoo;
  end;
```

> Properties of an Event must be **read-only**, to prevent corruption of the Event's State between Listeners/Handlers. This is best achieved by initializing the values of those properties via the Constructor, as will be shown in this example.

We now implement the only required method of `TMyEvent`:

```pascal
{ TMyEvent }

constructor TMyEvent.Create(const AFoo: String);
begin
  inherited Create; // This is important!
  FFoo := AFoo;
end;
```

##### Important Note: Lifetime Control
When your `TLKEvent` descendant is created, it creates an Interfaced `ILKEventHolder` object for itself. This is because the Event Engine needs to operate on Events as raw Class Instances, but it is desirable to be able to "fire and forget" Events in most (*virtually all*) implementations.

Once you dispatch an Event through a Queue or Stack, the Event Engine will pass around the `ILKEventHolder` instance, providing effective *reference counting* for that Event.

Once the reference count for the Event Holder reaches zero (0), the Event and its Holder will both be automatically destroyed.

If, for some reason, you need to persist the Event you're about to dispatch, you need to hold a reference to the `Holder` property of said Event. This will prevent the Event from being destroyed once it has been processed through the Event Engine.

To do this, you would use code similar to the following:

```pascal
var
  MyEvent: TMyEvent;
  MyEventHolder: ILKEventHolder;
begin
  MyEvent := TMyEvent.Create('Bar');
  MyEventHolder := MyEvent.Holder;
  // Dispatch the Event
  // Do SOMETHING ELSE with the Event via MyEventHolder
end; // On return, the Event Holder reference will be automatically nulled, and the Reference Count decremented accordingly.
```

> Note that it is recommended that you do *not* attempt to hold a reference to an Event after it is dispatched. The facility is provided only for *extreme corner cases*. Chances are that if you're needing to hold the reference after dispatch, you need to re-evaluate your approach.

### Defining an Event Listener
Now that we have a defined *Event Type* (`TMyEvent`), we need to define its corresponding *Event Listener Type*, which we shall call `TMyEventListener`:
```pascal
  TMyEventListener = class(TLKEventListener<TMyEvent>);
```
That is all we need to do to define the corresponding *Event Listener Type* for an *Event Type*.

Note that it is not necessary to provide any implementation for an *Event Listener Type*. This is all taken care of thanks to the use of *Generics* (solution contributed by [Uwe Raab](http://www.uweraabe.de/Blog/), thanks Uwe!)

> *Event Listeners* can only be constructed as members of an *Event Thread Instance*. This is because the *Event Thread* architecture provides the foundation necessary to process Events asynchronously (which is, ultimately, the entire point of writing *Event-Driven Systems*).

### Defining an Event Thread
Okay, now that we have an *Event Type* and its corresponding *Event Listener Type* defined, we need to define an *Event Thread Type* to tie everything together.

##### What is an *Event Thread?*
Before we get to the code (*which I'm sure many of you will now skip ahead to, then be left scratching your head until you come back and read this*), I would like to briefly explain what an *Event Thread Type* is.

An *Event Thread Type* is a specialized descendant of `TLKThread`, which expands on `TLKThread`'s High Precision behaviour with the infrastructure necessary to drive the Thread using Events.

This is achieved *without losing the High Precision behaviour of `TLKThread`*, meaning you can still use them to produce Render and Simulation loops (as two examples).

> Note that you must **not** ever create an instance of `TLKEventThread` itself. You can only *inherit* from it to define your own as shown in the example source below.

##### On to the code!
So, with a little insight into what an *Event Thread* actually is, let's define one for our example *Listener*:
```pascal
  TMyEventThread = class(TLKEventThread)
  private
    FMyListener: TMyEventListener;
    procedure DoEvent(const AEvent: TMyEvent);
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;
```

That is the most basic possible definition of an *Event Thread*, so let's take a look at its implementation, shall we?
```pascal
{ TMyEventThread }

procedure TMyEventThread.DoEvent(const AEvent: TMyEvent);
begin
  // This method is called when a "TMyEvent" instance occurs.
  ShowMessage(AEvent.Foo); // This is merely for the sake of example.
  // Remember that this method is being called by the Thread itself, so Synchronize/Message as necessary for the GUI Thread.
end;

procedure TMyEventThread.InitializeListeners;
begin
  FMyListener := TMyEventListener.Create(Self, DoEvent);
end;

procedure TMyEventThread.FinalizeListeners;
begin
  FMyListener.Free;
end;
```
There we have the most simple possible implementation of an *Event Thread*, with a single *Event Listener* to process a single *Event Type*.

On any given `TLKEventThread` descendant type, we can register as many *Listeners* as we want, for as many *Event Types* as we want.

##### Important Note: Freeing/Finalizing an Event Thread:
Assuming that we have a variable defined as follows:
```pascal
var
  MyThread: TMyThread;
```
We safely Free/Finalize the Thread thusly:
```pascal
  MyThread.Free;
```
This will ensure that the *Event Thread* is terminated and finalized *safely*.

> There used to be a method called `Kill` on TLKThread and its descendants, which has now been removed as unnecessary.

### Preparing and Dispatching an Event
We can prepare and dispatch an *Event* from anywhere in our code. We do not need to worry about *Thread Safety*, so we can do this from within any Thread.

Here's the most simple possible example of preparing and dispatching our `TMyEvent` example:
```pascal
  // Through the Queue...
  TMyEvent.Create('Bar').Queue;
  // ... or through the Stack...
  TMyEvent.Create('Bar').Stack;
```
This will create an instance of `TMyEvent`, initialize the value for property `Foo` to the value of "Bar", then Dispatch the instance through the Queue (*or Stack, respectively*).

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->
