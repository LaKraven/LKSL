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
  LStream: ILKMemoryStream;
  LCaret: ILKStreamCaret;
  LFoo: Int64;
  LBar: Integer;
begin
  LStream := TLKMemoryStream.Create;
  LFoo := High(Int64);
  LCaret := LStream.NewCaret;
  LCaret.Write(LFoo, SizeOf(Int64));
  LCaret.Position := 0;
  LCaret.Delete(4);
  LCaret.Position := 0;
  LCaret.Read(LBar, LStream.Size);
  ShowMessage(IntToStr(LBar));
end;

end.
