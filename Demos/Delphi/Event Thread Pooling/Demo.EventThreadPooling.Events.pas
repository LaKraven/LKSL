unit Demo.EventThreadPooling.Events;

interface

uses
  System.Classes,
  LKSL.Common.Types,
  LKSL.Events.Main;

type
  { Forward Declarations }
  TLKDemoEvent = class;
  TLKDemoResponseEvent = class;

  TLKDemoEvent = class(TLKEvent)

  end;

  TLKDemoResponseEvent = class(TLKEvent)

  end;

  TLKDemoEventListener = class(TLKEventListener<TLKDemoEvent>);
  TLKDemoResponseEventListener = class(TLKEventListener<TLKDemoResponseEvent>);

implementation

end.
