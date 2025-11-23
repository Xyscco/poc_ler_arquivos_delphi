unit UnitPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.IOUtils, System.Hash, System.Variants,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, DBClient;

type

  TFileInfo = record
    Name: string;
    Path: string;
    Size: Int64;
    ModifiedDate: TDateTime;
    MD5: string;
  end;

  TFileList = TArray<TFileInfo>;

  TForm1 = class(TForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetAllFiles(const StartDirectory: string): TFileList;
    function CalculateFileMD5(const FilePath: string): string;
    procedure FillDataSetWithFiles(DataSet: TClientDataSet; const StartDirectory: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FillDataSetWithFiles(ClientDataSet1, 'F:\Emuladores\Packs jogos traduzidos');
end;

function TForm1.CalculateFileMD5(const FilePath: string): string;
var
  MD5: THashMD5;
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FilePath) then
    Exit;

  try
    Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      MD5 := THashMD5.Create;
      Result := MD5.GetHashString(Stream);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      Result := 'Error: ' + E.Message;
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
  Files := GetAllFiles(StartDirectory);

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

function TForm1.GetAllFiles(const StartDirectory: string): TFileList;
var
  Files: TArray<string>;
  Directories: TArray<string>;
  i: Integer;
  CurrentFile: TFileInfo;
  SubFiles: TFileList;
begin
  // Verifica se o diretório existe
  if not TDirectory.Exists(StartDirectory) then
    Exit(nil);

  // Busca todos os arquivos no diretório atual
  Files := TDirectory.GetFiles(StartDirectory);
  SetLength(Result, Length(Files));

  for i := 0 to High(Files) do
  begin
    try
      CurrentFile.Name := TPath.GetFileName(Files[i]);
      CurrentFile.Path := Files[i];
      CurrentFile.Size := TFile.GetSize(Files[i]);
      CurrentFile.ModifiedDate := TFile.GetLastWriteTime(Files[i]);
      CurrentFile.MD5 := CalculateFileMD5(Files[i]);
      Result[i] := CurrentFile;
    except
      on E: Exception do
      begin
        // Em caso de erro, preenche com valores padrão
        CurrentFile.Name := TPath.GetFileName(Files[i]);
        CurrentFile.Path := Files[i];
        CurrentFile.Size := -1;
        CurrentFile.ModifiedDate := 0;
        CurrentFile.MD5 := 'Error calculating MD5: ' + E.Message;
        Result[i] := CurrentFile;
      end;
    end;
  end;

  // Busca recursivamente nas subpastas
  Directories := TDirectory.GetDirectories(StartDirectory);
  for i := 0 to High(Directories) do
  begin
    try
      SubFiles := GetAllFiles(Directories[i]); // Chamada recursiva
      Result := Result + SubFiles; // Concatena arrays
    except
      on E: Exception do
        // Continua processando outras pastas mesmo se uma falhar
        Continue;
    end;
  end;
end;

end.
