<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
> This page is currently incomplete.

# [Event Engine](./0_Contents.md)
## Introduction

## Contents:
* [What is the *Event Engine*?](#what-is-the-event-engine)
    * [In a nutshell...](#in-a-nutshell)
* [What is an *Event*?](#what-is-an-event)
    * [An *Event* is *not* a *Task*!](#an-event-is-not-a-task)
* [What is an *Event Listener*?](#what-is-an-event-listener)
* [Uncoupled Modular Design](#uncoupled-modular-design)
* [Simple Usage Example](#simple-usage-example)

### What is the *Event Engine*?
#### In a nutshell...
The *LKSL Event Engine* is a system to enable *entirely uncoupled* communication between *Modules* in your system (*which may consist of a single application/service on one physical device, multiple applications/services on the same device, or even multiple applications/services across multiple device... even on different platforms*).

### What is an *Event*?
An *Event* is nothing more than a block of *Information*. The *Event Type* defines the *context* of that information, and the *Event* itself provides the *Information* that is to be consumed by *Listeners* on all relevant *Event Threads*.

If, for example, we want to dispatch an *Event* each time we take a reading from a temperature probe, then the *Event Type* may be something like `TTemperatureReadingEvent`, to specify its *context*.
`TTemperatureReadingEvent` will then (logically) contain a `Temperature` value. This `Temperature` value represents the *Information* contained within this *Event*.

#### An *Event* is *not* a *Task*!
It is very important that we recognize the distinction between an *Event*, and a *Task*.

A *Task* defines *behaviour*, instructing the system to perform a predefined operation where certain *Parameters* may provide *dynamic* information (I.E. information specific to one instance of a *Task*).

An *Event* simply conveys *Information*, and it is down to each *consumer* of that *Information* to determine what it needs to do with that *Information*.

While this distinction may seem subtle at first glance, it is so fundamental that if you approach the *Event Engine* from the perspective of defining *Tasks*, you are not going to produce the results you are after!

### What is an *Event Listener*?
An *Event Listener* is simply an association between an *Event Type*, and a *Method* to be invoked when an *Event* of that *Event Type* is processed.

The *LKSL Event Engine* defines *Event Listeners* as *Objects* because they can provide *conditional behaviour*, can be *dynamically activated and deactivated*, and need to be *safely deactivated* upon their destruction.

This makes the *Event Listeners* in the LKSL more robust and versatile than simple *Callbacks* as employed by most comparable solutions.

### Uncoupled Modular Design
The *Event Engine* is intended to facilitate *uncoupled* communications between separate *Modules*. These *Modules* take the form of **Event Threads**, each *Event Thread* taking responsibility for processing its own *Queue* and *Stack* of *Events*.

> *Module* = **Event Thread**

The idea is that a program consisting of three modules (*ModuleA, ModuleB, and ModuleC respectively*) can exchange information between each of those three modules, *without any of them having any awareness of the existence of the others*.

  * *ModuleA* dispatches an *Event*, the *Event Type* for which *ModuleB* has a registered *Event Listener*.
  * *ModuleC* also has a registered *Event Listener* for the *Event Type* of the instances being dispatched by *ModuleA*.

*ModuleB* and *ModuleC* will both be notified (*independently*) each time *ModuleA* dispatches an *Event* of the pertinent *Event Type*, and both can consume the *Information* contained within that *Event* to perform their own distinct tasks.

> All three modules need to be aware of that particular *Event Type*, but they need not be aware of each other.

Better still, if we decide to extend our application with new functionality based on the *Information* contained in the aforementioned *Event Type*, we could introduce *ModuleD*, again with an *Event Listener* interested in the *Event Type* for which *ModuleA* is dispatching instances. *ModuleD* can then consume this same information in an entirely different way.
Best of all, this new functionality can be introduced without having to make *any* modifications to the existing *Modules*.

### Simple Usage Example
You can learn how to integrate the *LKSL Event Engine* into your systems by reading the [Simple Usage Example](./2_Simple_Usage_Example.md) here on the LKSL Wiki!

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->
