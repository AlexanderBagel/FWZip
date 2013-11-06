////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip - FWZipPerfomance
//  * Purpose   : Тестирование производительности FWZip
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2013.
//  * Version   : 1.0.10
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//  http://www.base2ti.com/
//

unit Unit1;

interface

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FileCtrl, ComCtrls,
  FWZipWriter, FWZipReader, FWZipConsts, Contnrs;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    LabeledEdit2: TLabeledEdit;
    Button2: TButton;
    GroupBox2: TGroupBox;
    LabeledEdit3: TLabeledEdit;
    Button3: TButton;
    CheckBox2: TCheckBox;
    LabeledEdit4: TLabeledEdit;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Label5: TLabel;
    Button5: TButton;
    Button6: TButton;
    Memo1: TMemo;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    InitialHeapSize, MaxHeapSize, AverageHeapSize: Int64;
    TotalGetHeapStatusCount: Integer;
    StopProcess: Boolean;
    procedure OnProgress(Sender: TObject; const FileName: string;
      Percent, TotalPercent: Byte; var Cancel: Boolean;
      ProgressState: TProgressState);
    procedure UpdateMemoryStatus;
    procedure SetEnabledState(Value: Boolean);
    procedure ClearZipData;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('Укажите папку для сжатия', '', Dir) then
    LabeledEdit1.Text := Dir;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I: Integer;
  TotalSize: Int64;
  Heap: THeapStatus;
  TicCount: DWORD;
  Item: TFWZipWriterItem;
  Writer: TFWZipWriter;
begin
  Writer := TFWZipWriter.Create;
  try
    DeleteFile(
      IncludeTrailingPathDelimiter(LabeledEdit1.Text) + 'FWZipTest.zip');
    Writer.AddFolder('', LabeledEdit1.Text, '');
    TotalSize := 0;
    InitialHeapSize := 0;
    for I := 0 to Writer.Count - 1 do
    begin
      Item := Writer[I];
      Inc(TotalSize, Item.Size);
      Inc(InitialHeapSize, SizeOf(TCentralDirectoryFileHeaderEx));
      if LabeledEdit2.Text <> '' then
      begin
        Item.Password := LabeledEdit2.Text;
        Item.NeedDescriptor := True;
      end;
    end;
    Label3.Caption := 'Общее количество элементов: ' + IntToStr(Writer.Count);
    Label4.Caption := 'Общий размер элементов: ' + IntToStr(TotalSize);
    Writer.OnProgress := OnProgress;
    SetEnabledState(False);
    try
      Heap := GetHeapStatus;
      Inc(InitialHeapSize, Heap.Overhead + Heap.TotalAllocated);
      MaxHeapSize := 0;
      AverageHeapSize := 0;
      TotalGetHeapStatusCount := 0;
      StopProcess := False;
      TicCount := GetTickCount;
      Writer.BuildZip(
        IncludeTrailingPathDelimiter(LabeledEdit1.Text) + 'FWZipTest.zip');
      if TotalGetHeapStatusCount = 0 then
        TotalGetHeapStatusCount := 1;
      ShowMessage(Format(
        'Пиковый расход памяти: %d байт' + sLineBreak +
        'Средний расход памяти: %d байт' + sLineBreak +
        'Общее время работы: %d секунд',
        [MaxHeapSize, AverageHeapSize div TotalGetHeapStatusCount,
         (GetTickCount - TicCount) div 1000]));
    finally
      SetEnabledState(True);
    end;
  finally
    Writer.Free;
    ClearZipData;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LabeledEdit3.Text := OpenDialog1.FileName;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  TotalSize: Int64;
  Heap: THeapStatus;
  TicCount: DWORD;
  Path: string;
  Reader: TFWZipReader;
begin
  SetLength(Path, MAX_PATH);
  Path := LabeledEdit3.Text;
  Path := ChangeFileExt(Path, '');
  Reader := TFWZipReader.Create;
  try
    Reader.LoadFromFile(LabeledEdit3.Text);
    TotalSize := 0;
    for I := 0 to Reader.Count - 1 do
      Inc(TotalSize, Reader[I].UncompressedSize);
    Label3.Caption := 'Общее количество элементов: ' + IntToStr(Reader.Count);
    Label4.Caption := 'Общий размер элементов: ' + IntToStr(TotalSize);
    Reader.OnProgress := OnProgress;
    if LabeledEdit4.Text <> '' then
      Reader.PasswordList.Add(LabeledEdit4.Text);
    SetEnabledState(False);
    try
      Heap := GetHeapStatus;
      InitialHeapSize := Heap.Overhead + Heap.TotalAllocated;
      MaxHeapSize := 0;
      AverageHeapSize := 0;
      TotalGetHeapStatusCount := 0;
      StopProcess := False;
      Memo1.Lines.Clear;
      TicCount := GetTickCount;
      if TButton(Sender).Tag = 0 then
        Reader.ExtractAll(Path)
      else
        Reader.Check;
      if TotalGetHeapStatusCount = 0 then
        TotalGetHeapStatusCount := 1;
      ShowMessage(Format(
        'Пиковый расход памяти: %d байт' + sLineBreak +
        'Средний расход памяти: %d байт' + sLineBreak +
        'Общее время работы: %d секунд',
        [MaxHeapSize, AverageHeapSize div TotalGetHeapStatusCount,
         (GetTickCount - TicCount) div 1000]));
    finally
      SetEnabledState(True);
    end;
  finally
    Reader.Free;
    ClearZipData;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  StopProcess := True;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  LabeledEdit2.Enabled := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  LabeledEdit4.Enabled := CheckBox2.Checked;
end;

procedure TForm1.ClearZipData;
begin
  Label1.Caption := 'Текущий расход памяти: 0 байт';
  Label2.Caption := 'Пиковый расход памяти: 0 байт';
  Label3.Caption := 'Общее количество элементов: 0';
  Label4.Caption := 'Общий размер элементов: 0';
  Label5.Caption := '';
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
  Button2.Enabled := DirectoryExists(LabeledEdit1.Text);
end;

procedure TForm1.LabeledEdit3Change(Sender: TObject);
begin
  Button4.Enabled := FileExists(LabeledEdit3.Text);
end;

procedure TForm1.OnProgress(Sender: TObject; const FileName: string; Percent,
  TotalPercent: Byte; var Cancel: Boolean; ProgressState: TProgressState);
const
  p: array [TProgressState] of string = ('psStart', 'psInitialization',
    'psInProgress', 'psFinalization', 'psEnd', 'psException');
begin
  Cancel := StopProcess;
  Label5.Caption := Format('(%d) %s', [Percent, FileName]);
  ProgressBar1.Position := Percent;
  ProgressBar2.Position := TotalPercent;
  Memo1.Lines.Add(Format('%s - %s percent %d total %d',
    [FileName, P[ProgressState], Percent, TotalPercent]));
  UpdateMemoryStatus;
end;

procedure TForm1.SetEnabledState(Value: Boolean);
begin
  Button1.Enabled := Value;
  Button2.Enabled := Value;
  Button3.Enabled := Value;
  Button4.Enabled := Value;
  Button5.Visible := not Value;
  Button6.Enabled := Value;
  LabeledEdit1.Enabled := Value;
  LabeledEdit2.Enabled := Value;
  LabeledEdit3.Enabled := Value;
  LabeledEdit4.Enabled := Value;
  CheckBox1.Enabled := Value;
  CheckBox2.Enabled := Value;
end;

procedure TForm1.UpdateMemoryStatus;
var
  HeapStatus: THeapStatus;
  HeapSize: Int64;
begin
  HeapStatus := GetHeapStatus;
  HeapSize := HeapStatus.Overhead + HeapStatus.TotalAllocated;
  Dec(HeapSize, InitialHeapSize);
  if HeapSize > MaxHeapSize then
    MaxHeapSize := HeapSize;
  Inc(TotalGetHeapStatusCount);
  Inc(AverageHeapSize, HeapSize);
  Label1.Caption := 'Текущий расход памяти: ' + IntToStr(HeapSize) + ' байт';
  Label2.Caption := 'Пиковый расход памяти: ' + IntToStr(MaxHeapSize) + ' байт';
  Application.ProcessMessages;
  Application.ProcessMessages;
end;

end.
