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
    procedure btnDispatchEventClick(Sender: TObject);
  private
    { Private Declarations }
  public
    { Public Declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.btnDispatchEventClick(Sender: TObject);
begin
  TTestEvent.Create('Bar').Queue;
end;

end.
