unit Demo.EventThreadPooling.EventThreads;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Events.Main,
  LKSL.Generics.Collections,
  Demo.EventThreadPooling.Events;

type
  TTestEventThread = class(TLKEventThread)
  private
    FPerformanceHistory: TLKList<LKFloat>;
    FListener: TTestEventListener;
    procedure DoEvent(const AEvent: TTestEvent);

    function GetAveragePerformance: LKFloat;
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;

  TTestEventPool = class(TLKEventPool<TTestEventThread>);

var
  TestEventPool: TTestEventPool;

implementation

uses
  LKSL.Math.SIUnits,
  Demo.EventThreadPooling.MainForm;

{ TTestEventThread }

procedure TTestEventThread.DoEvent(const AEvent: TTestEvent);
var
  LDelta: LKFloat;
begin
  LDelta := GetReferenceTime - AEvent.DispatchTime;
  FPerformanceHistory.Add(LDelta);

  SYNCHRONIZE(procedure begin
                Form1.memLog.Lines.BeginUpdate;
                try
                  Form1.memLog.Lines.Add(Format('Average Over %d Runs = %s Seconds [This run = %s Seconds]', [FPerformanceHistory.Count, FormatFloat('#######################.#######################', GetAveragePerformance), FormatFloat('#######################.#######################', LDelta)]));
                finally
                  Form1.memLog.Lines.EndUpdate;
                end;
              end);
end;

procedure TTestEventThread.FinalizeListeners;
begin
  inherited;
  FListener.Free;
  FPerformanceHistory.Free;
end;

function TTestEventThread.GetAveragePerformance: LKFloat;
var
  I: Integer;
  LTotal: LKFloat;
begin
  if FPerformanceHistory.Count > 0 then
  begin
    LTotal := 0;
    for I := 0 to FPerformanceHistory.Count - 1 do
      LTotal := LTotal + FPerformanceHistory[I];

    Result := LTotal / FPerformanceHistory.Count;
  end else
    Result := 0;
end;

procedure TTestEventThread.InitializeListeners;
begin
  inherited;
  FPerformanceHistory := TLKList<LKFloat>.Create;
  FListener := TTestEventListener.Create(Self, DoEvent);
end;

initialization
  TestEventPool := TTestEventPool.Create(TThread.ProcessorCount);
finalization
  TestEventPool.Kill;

end.
