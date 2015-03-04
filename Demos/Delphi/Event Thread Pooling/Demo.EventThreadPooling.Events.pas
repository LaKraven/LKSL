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
  protected
    {$IFNDEF LKSL_EVENTENGINE_REFCOUNT}
      procedure Clone(const AFromEvent: TLKEvent); override;
    {$ENDIF LKSL_EVENTENGINE_REFCOUNT}
  end;

  TLKDemoResponseEvent = class(TLKEvent)
  protected
    {$IFNDEF LKSL_EVENTENGINE_REFCOUNT}
      procedure Clone(const AFromEvent: TLKEvent); override;
    {$ENDIF LKSL_EVENTENGINE_REFCOUNT}
  end;

  TLKDemoEventListener = class(TLKEventListener<TLKDemoEvent>);
  TLKDemoResponseEventListener = class(TLKEventListener<TLKDemoResponseEvent>);

implementation

{ TLKDemoEvent }

{$IFNDEF LKSL_EVENTENGINE_REFCOUNT}
  procedure TLKDemoEvent.Clone(const AFromEvent: TLKEvent);
  begin

  end;
{$ENDIF LKSL_EVENTENGINE_REFCOUNT}

{ TLKDemoResponseEvent }

{$IFNDEF LKSL_EVENTENGINE_REFCOUNT}
  procedure TLKDemoResponseEvent.Clone(const AFromEvent: TLKEvent);
  begin

  end;
{$ENDIF LKSL_EVENTENGINE_REFCOUNT}

end.
