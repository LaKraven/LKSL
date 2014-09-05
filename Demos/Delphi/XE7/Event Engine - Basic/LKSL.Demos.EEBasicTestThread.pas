{
  The Thread in this unit does not perform a looped process, but does not immediately terminate
  as a normal "TThread" would without some looping code to keep it alive.
  Now, you can (and likely will) implement some actual looping routine into the "Tick" method,
  but this particular Thread doesn't need to do any repetative task.

  It essentially sits and waits (taking up no measurable CPU time) to recieve a relevant "Event"
  through the "FListenerGenerateCircle", at which point this Thread generates the Edge Data for
  a shape of a Radius, and with a number of Segments defined by the received Event.

  Yes, the Listener's callback is ACTUALLY EXECUTED ON THIS THREAD!

  "TLKEventThread" is deliberately coded to be a "good citizen", and to yield as much CPU
  time while waiting for Events as possible.

  This means you can construct as many distinct Threads in your Application as you want, and
  they will only occupy CPU time when any relevant Event is received by one or more Listeners
  associated with that Thread.

  Amongst the countless advantages to this programming methodology, YOU CAN FULLY DECOUPLE YOUR CODE!
  ThreadA does not need to have ANY awareness of the existence of ThreadB (or vice-versa).
}
unit LKSL.Demos.EEBasicTestThread;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  LKSL.Threads.Base,
  LKSL.Events.Base,
  LKSL.Demos.EEBasicTestEvents;

type
  { Forward Declarations }
  TCircleGenerator = class(TLKEventThread)
  private
    FListenerGenerateCircle: TEventListenerGenerateCircle;
    procedure GenerateCircle(const AEvent: TEventGenerateCircle);
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  CircleGenerator: TCircleGenerator;

implementation

{ TCircleGenerator }

constructor TCircleGenerator.Create;
begin
  inherited;
  // Create our Listener, remembering to pass "Self" as the constructor parameter so that
  // the Listener executes Events on THIS Thread (rather than the Queue or Stack Threads)
  FListenerGenerateCircle := TEventListenerGenerateCircle.Create(Self);
  FListenerGenerateCircle.OnGenerateCircle := GenerateCircle;
  FListenerGenerateCircle.Subscribe; // This registers the Listener and allows it to receive Events
end;

destructor TCircleGenerator.Destroy;
begin
  // Must not forget to free the Listener when the Thread is destroyed!
  // Every time some memory leaks, the Programming Ninjas kill a <insert your favorite animal here>!
  FListenerGenerateCircle.Free;
  inherited;
end;

procedure TCircleGenerator.GenerateCircle(const AEvent: TEventGenerateCircle);
var
  I: Integer;
  LTheta: Double;
  LVector1, LVector2: TPointF;
  LEdges: TEdges;
  LTimeStart: Double;
begin
  LTimeStart := GetReferenceTime;
  if AEvent.Segments < 3 then
    Exit;
  LTheta := (2.0 * 3.1415926) / AEvent.Segments;
  LVector1 := PointF((AEvent.Radius * Cos(LTheta)) + AEvent.Position.X,
                     (AEvent.Radius * Sin(LTheta)) + AEvent.Position.Y);

  // Calculate remaining Vertices
  for I := 1 to AEvent.Segments do
  begin
    LTheta := (2.0 * 3.1415926 * I) / AEvent.Segments;
    LVector2 := PointF(AEvent.Radius * Cos(LTheta),
                       AEvent.Radius * Sin(LTheta));

    SetLength(LEdges, Length(LEdges) + 1);
    LEdges[High(LEdges)].Vertice1 := LVector1;
    LEdges[High(LEdges)].Vertice2 := LVector2;

    LVector1 := PointF(AEvent.Position.X + LVector2.X, AEvent.Position.Y + LVector2.Y);
  end;

  // Edge back to first Vector
  LTheta := (2.0 * 3.1415926) / AEvent.Segments;
  LVector2 := PointF((AEvent.Radius * Cos(LTheta)) + AEvent.Position.X, (AEvent.Radius * Sin(LTheta)) + AEvent.Position.Y);
  // Add the Edge to the Array
  SetLength(LEdges, Length(LEdges) + 1);
  LEdges[High(LEdges)].Vertice1 := LVector1;
  LEdges[High(LEdges)].Vertice2 := LVector2;

  // Now dispatch a confirmation Event containing the Edges
  QueueEvent(TEventCircleGenerated.Create(LEdges, GetReferenceTime - LTimeStart));
end;

function TCircleGenerator.GetInitialThreadState: TLKThreadState;
begin
  // Because this Thread only operates when it receives a triggering Event, we set
  // its Initial State to "Paused" so that it doesn't constantly suck up CPU cycles!
  Result := tsPaused;
end;

procedure TCircleGenerator.Tick(const ADelta, AStartTime: Double);
begin
  // Do nothing (yet)
end;

initialization
  CircleGenerator := TCircleGenerator.Create;
finalization
  CircleGenerator.Kill;

end.
