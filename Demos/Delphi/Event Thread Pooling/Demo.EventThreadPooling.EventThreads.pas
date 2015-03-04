unit Demo.EventThreadPooling.EventThreads;

interface

uses
  System.Classes, System.SyncObjs,
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Events.Main,
  Demo.EventThreadPooling.Events;

type
  TLKDemoEventThread = class(TLKEventThread)
  private
    FEventListener: TLKDemoEventListener;
    procedure DoEvent(const AEvent: TLKDemoEvent);
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;

var
  DemoEventThread: TLKDemoEventThread;

implementation

{ TLKDemoEventThread }

procedure TLKDemoEventThread.DoEvent(const AEvent: TLKDemoEvent);
begin
  TLKDemoResponseEvent.Create.Queue;
end;

procedure TLKDemoEventThread.InitializeListeners;
begin
  FEventListener := TLKDemoEventListener.Create(Self, DoEvent);
end;

procedure TLKDemoEventThread.FinalizeListeners;
begin
  FEventListener.Free;
end;

initialization
  DemoEventThread := TLKDemoEventThread.Create;
finalization
  DemoEventThread.Kill;

end.
