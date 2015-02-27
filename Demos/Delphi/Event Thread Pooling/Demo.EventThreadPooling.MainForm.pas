unit Demo.EventThreadPooling.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  LKSL.Common.Types,
  LKSL.Events.Main;

type
  TTestEvent = class(TLKEvent)
  private
    FFoo: String;
  public
    constructor Create(const AFoo: String); reintroduce;
    property Foo: String read FFoo;
  end;

  TTestEventListener = class(TLKEventListener<TTestEvent>);

  TTestEventThread = class(TLKEventThread)
  private
    FListener: TTestEventListener;
    procedure DoEvent(const AEvent: TTestEvent);
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;

  TForm1 = class(TForm)
    memLog: TMemo;
    btnDispatchEvent: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnDispatchEventClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTestThread: TTestEventThread;
//    FResponseListener: TLKDemoResponseEventListener;
//    procedure DoResponseEvent(const AEvent: TLKDemoResponseEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  LKSL.Math.SIUnits, LKSL.Threads.Base;

{ TTestEventThread }

procedure TTestEventThread.DoEvent(const AEvent: TTestEvent);
var
  LDelta: LKFloat;
begin
  LDelta := GetReferenceTime - AEvent.DispatchTime;
  SYNCHRONIZE(procedure begin
                Form1.memLog.Lines.Add(Format('%sms - %s', [FormatFloat('#######################.#######################', SIMagnitudeConvert(LDelta, simOne, simMilli)), AEvent.Foo]));
              end);
//  AEvent.Queue;
end;

procedure TTestEventThread.FinalizeListeners;
begin
  inherited;
  FListener.Free;
end;

procedure TTestEventThread.InitializeListeners;
begin
  inherited;
  FListener := TTestEventListener.Create(Self, DoEvent);
end;

{ TForm1 }

procedure TForm1.btnDispatchEventClick(Sender: TObject);
begin
  TTestEvent.Create('Bar').Queue;
//  TLKDemoEvent.Create.Queue;
//  ShowMessage(Format('%g', [SIMagnitudeConvert(1, simZepto, simAtto)]));
end;

function LeftPad(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result  := S;
  RestLen := Len - Length(s);
  if RestLen < 1 then Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  OOM: Array[TLKSIMagnitude] of TLKSIMagnitude = (simYocto, simZepto, simAtto, simFemto, simPico, simNano, siMmicro, simMilli, simCenti, simDeci, simOne,
               simDeca, simHecto, simKilo, simMega, simGiga, simTera, simPeta, simExa, simZetta, simYotta);
var
  X, Y: TLKSIMagnitude;
  LLine: String;
begin
  LLine := LeftPad('', ' ', 30);
  for X := Low(OOM) to High(OOM) do
    LLine := LLine + LeftPad(LK_UNIT_MAGNITUDE_NAMES_SI[X, unLong], ' ', 30);

  memLog.Lines.Add(LLine);

  for X := Low(OOM) to High(OOM) do
  begin
    LLine := LeftPad(LK_UNIT_MAGNITUDE_NAMES_SI[X, unLong], ' ', 30);
    for Y := Low(OOM) to High(OOM) do
      //LLine := LLine + Format('%n' + #9, [SIUnitConvert(1, X, Y)]);
      LLine := LLine + LeftPad(Format('%s', [FormatFloat('#######################.#######################', SIMagnitudeConvert(1337, X, Y))]), ' ', 30);

    memLog.Lines.Add(LLine);
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FTestThread := TTestEventThread.Create;
end;

destructor TForm1.Destroy;
begin
  FTestThread.Kill;
  inherited;
end;

//procedure TForm1.DoResponseEvent(const AEvent: TLKDemoResponseEvent);
//begin
//  memLog.Lines.Add(Format('%n', [AEvent.DispatchTime]));
//end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  FResponseListener := TLKDemoResponseEventListener.Create(DoResponseEvent);
//  FResponseListener.CallUIThread := True;
//  FResponseListener.Subscribe;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  FResponseListener.Free;
end;

{ TTestEvent }

constructor TTestEvent.Create(const AFoo: String);
begin
  inherited Create;
  FFoo := AFoo;
end;

end.
