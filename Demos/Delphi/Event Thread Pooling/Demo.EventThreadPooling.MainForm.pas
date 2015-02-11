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
    procedure FormCreate(Sender: TObject);
    procedure btnDispatchEventClick(Sender: TObject);
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

procedure TForm1.btnDispatchEventClick(Sender: TObject);
begin
  TLKDemoEvent.Create.Queue;
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
