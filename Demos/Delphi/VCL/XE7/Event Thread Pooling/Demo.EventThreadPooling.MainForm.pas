unit Demo.EventThreadPooling.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Demo.EventThreadPooling.Events;

type
  TForm1 = class(TForm)
    memLog: TMemo;
    btnDispatchEvent: TButton;
    Button1: TButton;
    procedure btnDispatchEventClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private Declarations }
  public
    { Public Declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  LKSL.Streams.Main;

{$R *.dfm}

{ TForm1 }

procedure TForm1.btnDispatchEventClick(Sender: TObject);
begin
  TTestEvent.Create('Bar').Stack;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LStream: ILKStream;
  LCaret: ILKStreamCaret;
  LFoo: Integer;
  LBar: TBytes;
begin
  LStream := TLKMemoryStream.Create;
  LFoo := High(Integer) div 2;
  LCaret := LStream.NewCaret;
  LCaret.Write(LFoo, SizeOf(Integer));
  LCaret.Position := 0;
  SetLength(LBar, LStream.Size);
  LCaret.Read(LBar[0], LStream.Size);
  LCaret.Position := 0;
  LCaret.Delete(3);
  LCaret.Position := 0;
  SetLength(LBar, LStream.Size);
  LCaret.Read(LBar[0], LStream.Size);
  ShowMessage(IntToStr(LBar[0]));
end;

end.
