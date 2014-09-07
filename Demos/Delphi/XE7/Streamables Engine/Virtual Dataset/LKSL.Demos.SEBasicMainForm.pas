unit LKSL.Demos.SEBasicMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Text,
  LKSL.Demos.SEBasicStreamables, FMX.EditBox, FMX.NumberBox, LKSL.Demos.SEBasicRecordFrame;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    btnNew: TButton;
    btnSave: TButton;
    btnLoad: TButton;
    Layout2: TLayout;
    Label1: TLabel;
    edTableName: TEdit;
    VertScrollBox1: TVertScrollBox;
    Layout3: TLayout;
    btnAddRecord: TButton;
    odOpenTable: TOpenDialog;
    sdSaveTable: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edTableNameChange(Sender: TObject);
    procedure btnAddRecordClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTable: TStreamableDummyTable;
    FRecords: Array of TframeRecord;
  public
    procedure AddRecord;
    procedure NewTable;
    procedure RefreshTable;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.AddRecord;
begin
  FTable.Lock;
  TStreamableDummyRecord.Create(FTable);
  SetLength(FRecords, FTable.RecordCount);
  FRecords[FTable.RecordCount - 1] := TframeRecord.Create(VertScrollBox1);
  VertScrollBox1.AddObject(FRecords[FTable.RecordCount - 1]);
  FRecords[FTable.RecordCount - 1].SetRecord(FTable.Records[FTable.RecordCount - 1]);
  FRecords[FTable.RecordCount - 1].Name := Format('Record%d', [FTable.RecordCount - 1]);
  FRecords[FTable.RecordCount - 1].Position.Y := (FRecords[FTable.RecordCount - 1].Height + 2) * (FTable.RecordCount);
  FRecords[FTable.RecordCount - 1].Align := TAlignLayout.Top;
  FRecords[FTable.RecordCount - 1].Visible := True;
  FRecords[FTable.RecordCount - 1].OnDelete := RefreshTable;
  FTable.Unlock;
end;

procedure TfrmMain.btnAddRecordClick(Sender: TObject);
begin
  AddRecord;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  if odOpenTable.Execute then
  begin
    if FTable <> nil then
      FTable.Free;
    FTable := TStreamableDummyTable.Create;
    FTable.LoadFromFile(odOpenTable.FileName);
    RefreshTable;
  end;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
  NewTable;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if sdSaveTable.Execute then
    FTable.SaveToFile(sdSaveTable.FileName);
end;

procedure TfrmMain.edTableNameChange(Sender: TObject);
begin
  FTable.Name := edTableName.Text;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FTable := nil;
  NewTable;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FTable.Free;
end;

procedure TfrmMain.NewTable;
begin
  if FTable <> nil then
    FTable.Free;
  FTable := TStreamableDummyTable.Create;
  RefreshTable;
end;

procedure TfrmMain.RefreshTable;
var
  I: Integer;
begin
  FTable.Lock;
  edTableName.Text := FTable.Name;
  for I := High(FRecords) downto Low(FRecords) do
    FRecords[I].Free;

  SetLength(FRecords, FTable.RecordCount);
  for I := 0 to FTable.RecordCount - 1 do
  begin
    FRecords[I] := TframeRecord.Create(VertScrollBox1);
    VertScrollBox1.AddObject(FRecords[I]);
    FRecords[I].SetRecord(FTable.Records[I]);
    FRecords[I].Name := Format('Record%d', [I]);
    FRecords[I].Position.Y := (FRecords[I].Height + 2) * (I + 1);
    FRecords[I].Align := TAlignLayout.Top;
    FRecords[I].Visible := True;
    FRecords[I].OnDelete := RefreshTable;
  end;
  VertScrollBox1.Visible := False;
  VertScrollBox1.Visible := True;
  FTable.Unlock;
end;

end.
