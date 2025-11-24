unit UnitPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.IOUtils, System.Hash, System.Variants,
  System.Generics.Collections, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, DBClient;

type

  TForm1 = class(TForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    Memo1: TMemo;
    btnAtualizar: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
//    function GetAllFiles(const StartDirectory: string): TFileList;
    procedure FillDataSetWithFiles(DataSet: TClientDataSet; const StartDirectory: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UnitFileManager;

procedure TForm1.btnAtualizarClick(Sender: TObject);
var
  vFileManger: TFileManager;
begin
  try
    vFileManger:= TFileManager.Create;
    Screen.Cursor := crHourGlass;

    try
      vFileManger.SynchronizeFolders('C:\ProgramData\Unifar\cache_versoes\4.077.0-0\UniWin', 'C:\Publico\UniWin');
      ShowMessage('Sincronização concluída com sucesso!');
    except
      on E: Exception do
        ShowMessage('Erro durante a sincronização: ' + E.Message);
    end;
  finally
    vFileManger.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FillDataSetWithFiles(ClientDataSet1, 'C:\Publico\UniWin');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SyncList: TFileSyncList;
  SyncInfo: TFileSyncInfo;
  vFileManger: TFileManager;
begin
  try
    Screen.Cursor := crHourGlass;
    vFileManger:= TFileManager.Create;
    try
      SyncList := vFileManger.CompareFolders('C:\ProgramData\Unifar\cache_versoes\4.077.0-0\UniWin', 'C:\Publico\UniWin');

      // Exibir no grid ou lista
      for SyncInfo in SyncList do
      begin
        case SyncInfo.Operation of
          soCopy: Memo1.Lines.Add('COPIAR: ' + SyncInfo.RelativePath);
          soUpdate: Memo1.Lines.Add('ATUALIZAR: ' + SyncInfo.RelativePath);
          soDelete: Memo1.Lines.Add('EXCLUIR: ' + SyncInfo.RelativePath);
        end;
      end;

      ShowMessage(Format('Foram encontradas %d operações necessárias', [Length(SyncList)]));
    except
      on E: Exception do
        ShowMessage('Erro durante a comparação: ' + E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.FillDataSetWithFiles(DataSet: TClientDataSet; const StartDirectory: string);
var
  Files: TFileList;
  FileInfo: TFileInfo;
begin
  // Configurar o ClientDataSet se ainda não estiver configurado
  if not DataSet.Active then
  begin
    DataSet.Close;
    DataSet.FieldDefs.Clear;

    DataSet.FieldDefs.Add('Name', ftString, 255);
    DataSet.FieldDefs.Add('Path', ftString, 1000);
    DataSet.FieldDefs.Add('Size', ftLargeint);
    DataSet.FieldDefs.Add('ModifiedDate', ftDateTime);
    DataSet.FieldDefs.Add('MD5', ftString, 32);

    DataSet.CreateDataSet;
    DataSet.Open;
  end
  else
  begin
    DataSet.EmptyDataSet;
  end;

  // Obter a lista de arquivos
//  Files := GetAllFiles(StartDirectory);

  // Preencher o DataSet
  for FileInfo in Files do
  begin
    DataSet.Append;
    DataSet.FieldByName('Name').AsString := FileInfo.Name;
    DataSet.FieldByName('Path').AsString := FileInfo.Path;
    DataSet.FieldByName('Size').AsLargeInt := FileInfo.Size;
    DataSet.FieldByName('ModifiedDate').AsDateTime := FileInfo.ModifiedDate;
    DataSet.FieldByName('MD5').AsString := FileInfo.MD5;
    DataSet.Post;
  end;

  DataSet.First;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurar o DBGrid
  DBGrid1.DataSource := DataSource1;

  // Configurar colunas do DBGrid (opcional - pode fazer no designer)
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'Name';
    Title.Caption := 'Nome do Arquivo';
    Width := 200;
  end;

  with DBGrid1.Columns.Add do
  begin
    FieldName := 'Path';
    Title.Caption := 'Caminho';
    Width := 300;
  end;

  with DBGrid1.Columns.Add do
  begin
    FieldName := 'Size';
    Title.Caption := 'Tamanho (bytes)';
    Width := 120;
  end;

  with DBGrid1.Columns.Add do
  begin
    FieldName := 'ModifiedDate';
    Title.Caption := 'Modificado em';
    Width := 150;
  end;

  with DBGrid1.Columns.Add do
  begin
    FieldName := 'MD5';
    Title.Caption := 'MD5';
    Width := 250;
  end;
end;


//function TForm1.GetAllFiles(const StartDirectory: string): TFileList;
//var
//  Files: TArray<string>;
//  Directories: TArray<string>;
//  i, j: Integer;
//  CurrentFile: TFileInfo;
//  SubFiles: TFileList;
//  TempFiles: TArray<string>;
//begin
//  if not TDirectory.Exists(StartDirectory) then
//    Exit(nil);
//
//  // Busca todos os arquivos no diretório atual
//  Files := TDirectory.GetFiles(StartDirectory);
//
//  // Filtra os arquivos, removendo .log e .fbd
//  j := 0;
//  SetLength(TempFiles, Length(Files));
//  for i := 0 to High(Files) do
//  begin
//    if not ShouldIgnoreFile(Files[i]) then
//    begin
//      TempFiles[j] := Files[i];
//      Inc(j);
//    end
//    else
//      Memo1.Lines.Add('Arquivo ignorado: ' + TPath.GetFileNameWithoutExtension(Files[i]));
//  end;
//  SetLength(TempFiles, j);
//  Files := TempFiles;
//
//  SetLength(Result, Length(Files));
//
//  for i := 0 to High(Files) do
//  begin
//    try
//      CurrentFile.Name := TPath.GetFileName(Files[i]);
//      CurrentFile.Path := Files[i];
//      CurrentFile.Size := TFile.GetSize(Files[i]);
//      CurrentFile.ModifiedDate := TFile.GetLastWriteTime(Files[i]);
//      CurrentFile.MD5 := CalculateFileMD5(Files[i]);
//      Result[i] := CurrentFile;
//    except
//      on E: Exception do
//      begin
//        CurrentFile.Name := TPath.GetFileName(Files[i]);
//        CurrentFile.Path := Files[i];
//        CurrentFile.Size := -1;
//        CurrentFile.ModifiedDate := 0;
//        CurrentFile.MD5 := 'Error: ' + E.Message;
//        Result[i] := CurrentFile;
//      end;
//    end;
//  end;
//
//  // Busca recursivamente nas subpastas
//  Directories := TDirectory.GetDirectories(StartDirectory);
//  for i := 0 to High(Directories) do
//  begin
//    try
//      SubFiles := GetAllFiles(Directories[i]);
//      Result := Result + SubFiles;
//    except
//      Continue;
//    end;
//  end;
//end;


end.
