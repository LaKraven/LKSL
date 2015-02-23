unit Demo.EventThreadPooling.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Demo.EventThreadPooling.Events,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    memLog: TMemo;
    btnDispatchEvent: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnDispatchEventClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FResponseListener: TLKDemoResponseEventListener;
    procedure DoResponseEvent(const AEvent: TLKDemoResponseEvent);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses LKSL.Math.SIUnits;

procedure TForm1.btnDispatchEventClick(Sender: TObject);
begin
  TLKDemoEvent.Create.Queue;
  ShowMessage(Format('%g', [SIMagnitudeConvert(1, simZepto, simAtto)]));
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

procedure TForm1.DoResponseEvent(const AEvent: TLKDemoResponseEvent);
begin
  memLog.Lines.Add(Format('%n', [AEvent.DispatchTime]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FResponseListener := TLKDemoResponseEventListener.Create(DoResponseEvent);
  FResponseListener.CallUIThread := True;
  FResponseListener.Subscribe;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FResponseListener.Free;
end;

end.
