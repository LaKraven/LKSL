<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
# Event Engine
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

### What is the _Event Engine_?
If you haven't done so already, you should first read the [Event Engine Introduction](./1_Introduction.md) article here on the LKSL Wiki.

### Units to Include
For the sake of inlining often-used (but small) routines across virtually every Class Type in the LKSL, you should include a reference to `System.SyncObjs` into the _Uses_ section of any unit interacting with the LKSL. If you don't, you'll get compiler warnings informing you that certain methods could not be properly inlined.

For any Unit implementing any class in the Event Engine (_such as defining a new Event Type, Event Listener Type, or Event Thread Type_), you need to include a reference to `LKSL.Events.Main` in the _Uses_ section.

> It may also be beneficial (_when defining an Event Thread Type_) to include a reference to `LKSL.Threads.Main` in the _Uses_ section. This is, again, for the sake of code inlining.

### Defining a custom Event Type
Event Types inherit from their common Base Type, `TLKEvent`.

> Events should not implement any functionality! They should serve only to contain a collection of Values, to be consumed by any interest _Event Listeners_.

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
The above example does not account for the optional parameter on `TLKEvent`'s Constructor, which is defined as:
```pascal
constructor Create(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
```

The parameter `ALifetimeControl` determines whether or not the responsibility of Freeing an instance of your _Event Type_ should be passed along to the Event Engine itself once the _Event Instance_ has been dispatched.

By default, it is presumed that you will want the Event Engine to take responsibility for this, thus the default value is `elcAutomatic`. If you would rather your implementation take control over the lifetime of your `Event Instances`, you will need to provide the constructor of `TLKEvent` with the value `elcManual` for parameter `ALifetimeControl`.

> The _Lifetime Control_ setting for an _Event Instance_ cannot be changed after construction.

### Defining an Event Listener
Now that we have a defined _Event Type_ (`TMyEvent`), we need to define its corresponding _Event Listener Type_, which we shall call `TMyEventListener`:
```pascal
  TMyEventListener = class(TLKEventListener<TMyEvent>);
```
That is all we need to do to define the corresponding _Event Listener Type_ for an _Event Type_.

Note that it is not necessary to provide any implementation for an _Event Listener Type_. This is all taken care of thanks to the use of _Generics_ (solution contributed by [Uwe Raab](http://www.uweraabe.de/Blog/), thanks Uwe!)

> _Event Listeners_ can only be constructed as members of an _Event Thread Instance_. This is because the _Event Thread_ architecture provides the foundation necessary to process Events asynchronously (which is, ultimately, the entire point of writing _Event-Driven Systems_).

### Defining an Event Thread
Okay, now that we have an _Event Type_ and its corresponding _Event Listener Type_ defined, we need to define an _Event Thread Type_ to tie everything together.

##### What is an _Event Thread?_
Before we get to the code (_which I'm sure many of you will now skip ahead to, then be left scratching your head until you come back and read this_), I would like to briefly explain what an _Event Thread Type_ is.

An _Event Thread Type_ is a specialized descendant of `TLKThread`, which expands on `TLKThread`'s High Precision behaviour with the infrastructure necessary to drive the Thread using Events.

This is achieved _without losing the High Precision behaviour of `TLKThread`_, meaning you can still use them to produce Render and Simulation loops (as two examples).

> Note that you must **not** ever create an instance of `TLKEventThread` itself. You can only *inherit* from it to define your own as shown in the example source below.

##### On to the code!
So, with a little insight into what an _Event Thread_ actually is, let's define one for our example _Listener_:
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

That is the most basic possible definition of an _Event Thread_, so let's take a look at its implementation, shall we?
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
There we have the most simple possible implementation of an _Event Thread_, with a single _Event Listener_ to process a single _Event Type_.

On any given `TLKEventThread` descendant type, we can register as many _Listeners_ as we want, for as many _Event Types_ as we want.

##### Important Note: Freeing/Finalizing an Event Thread:
Assuming that we have a variable defined as follows:
```pascal
var
  MyThread: TMyThread;
```
We safely Free/Finalize the Thread thusly:
```pascal
  MyThread.Kill;
```
This will ensure that the _Event Thread_ is terminated and finalized _safely_.

### Preparing and Dispatching an Event
We can prepare and dispatch an _Event_ from anywhere in our code. We do not need to worry about _Thread Safety_, so we can do this from within any Thread.

Here's the most simple possible example of preparing and dispatching our `TMyEvent` example:
```pascal
  // Through the Queue...
  TMyEvent.Create('Bar').Queue;
  // ... or through the Stack...
  TMyEvent.Create('Bar').Stack;
```
This will create an instance of `TMyEvent`, initialize the value for property `Foo` to the value of "Bar", then Dispatch the instance through the Queue (_or Stack, respectively_).

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->