{
  This Demo uses the "Event Engine" to delegate tasks between the UI and a separete
  "Event-Enabled Thread".

  It is not a terribly practical demo, I admit, but I have annotated the code extensively
  where relevant to (hopefully) explain how to use the Event Engine.

  The key points to take note of:
  1) This Unit has absolutely NO awareness of the worker Thread defined in
     "LKSL.Demos.EEBasicTestThread.pas", and likewise that unit has no awareness of this one.
  2) The UI remains 100% responsive between the time the "Generate Circle" Event is Dispatched,
     and the time it receives the "Circle Generated" Event... essentially Events are processed
     ASYNCHRONOUSLY, and this programming methodology presents many incredible advantages over
     the traditional "request-wait-response" synchronized approach to programming most of us
     have always used.
  3) Because the "Circle Generated" Event Listener defined on the Form (FListenerCircleGenerated)
     interacts with the UI directly, the Listener's "CallUIThread" property is set to "True" to
     ensure that this Listener's Event Call is Synchronized on the main Thread.
  4) The code involved in dispatching Events and constructing Event Listeners has been deliberately
     left fully-inflated, for the sake of breaking the process up into annotated steps.

  NOTE: While a Synchronized call is used in this demo (for the sake of keeping it simple),
        Synchronized calls are NOT recommended! They can (if repeated rapidly enough) lead to
        a deadlock of the UI thread!
}
unit LKSL.Demos.EEBasicMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  LKSL.Demos.EEBasicTestEvents,
  LKSL.Events.Base;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    imgCircle: TImage;
    btnGenerateCircle: TButton;
    lblGenerateTime: TLabel;
    nbRadius: TNumberBox;
    Label1: TLabel;
    Label2: TLabel;
    nbSegments: TNumberBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnGenerateCircleClick(Sender: TObject);
  private
    FBitmap: TBitmap;
    FListenerCircleGenerated: TEventListenerCircleGenerated;
    procedure CircleGenerated(const AEvent: TEventCircleGenerated);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.btnGenerateCircleClick(Sender: TObject);
begin
  btnGenerateCircle.Enabled := False;
  // Construct our "Generate Circle" Event, and Dispatch it through the Event Queue
  QueueEvent(TEventGenerateCircle.Create(PointF(0, 0), nbRadius.Value, Round(nbSegments.Value)));
end;

procedure TfrmMain.CircleGenerated(const AEvent: TEventCircleGenerated);
var
  I: Integer;
  LEdges: TEdges;
begin
  lblGenerateTime.Text := 'Generated in ' + FormatFloat('0.00000000', AEvent.GenerationTime) + ' seconds.';
  LEdges := AEvent.Edges;
  for I := Low(LEdges) to High(LEdges) do
  begin
    LEdges[I].Vertice1 := PointF(LEdges[I].Vertice1.X + (imgCircle.Width / 2), LEdges[I].Vertice1.Y + (imgCircle.Height / 2));
    LEdges[I].Vertice2 := PointF(LEdges[I].Vertice2.X + (imgCircle.Width / 2), LEdges[I].Vertice2.Y + (imgCircle.Height / 2));
  end;

  if FBitmap.Canvas.BeginScene then
  begin
    FBitmap.Canvas.Clear(TAlphaColors.Black);
    FBitmap.Canvas.Stroke.Color := TAlphaColors.Aliceblue;
//    FBitmap.Canvas.StrokeThickness := 1.5;
    for I := Low(LEdges) to High(LEdges) do
      FBitmap.Canvas.DrawLine(LEdges[I].Vertice1, LEdges[I].Vertice2, 1.00);
    FBitmap.Canvas.EndScene;
    imgCircle.Bitmap.Assign(FBitmap);
  end;
  btnGenerateCircle.Enabled := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  // Create a Listener to be executed every time a "Circle Generated" Event is triggered.
  FListenerCircleGenerated := TEventListenerCircleGenerated.Create;
  // We're going to be updating our UI when this Listener executes, so we need to switch on "CallUIThread"
  // to Synchronize the call, otherwise you'll lose irreplacable moments of your life trying to debug your
  // own mistakes... and that sucks!
  FListenerCircleGenerated.CallUIThread := True;
  FListenerCircleGenerated.OnCircleGenerated := CircleGenerated;
  FListenerCircleGenerated.Subscribe;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FListenerCircleGenerated.Free;
  FBitmap.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  FBitmap.SetSize(Round(imgCircle.Width), Round(imgCircle.Height));
  if btnGenerateCircle.Enabled then
    btnGenerateCircleClick(Sender);
end;

end.
