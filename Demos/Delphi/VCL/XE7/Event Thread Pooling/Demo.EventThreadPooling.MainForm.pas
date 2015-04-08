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
  LStream: TLKMemoryStream;
  LCaret: ILKStreamCaret;
  LFoo, LBar: String;
begin
  LStream := TLKMemoryStream.Create;
  try
    LFoo := 'Hello World';
    LStream.Size := Length(LFoo) * SizeOf(Char);
    LCaret := LStream.NewCaret;
    LCaret.Write(LFoo, Length(LFoo) * SizeOf(Char));
    LCaret.Position := 0;
    SetLength(LBar, Length(LFoo) * SizeOf(Char));
    LCaret.Read(LBar, Length(LFoo) * SizeOf(Char));
//    ShowMessage(LBar);
    LCaret := nil;
  finally
    LStream.Free;
  end;
end;

end.
