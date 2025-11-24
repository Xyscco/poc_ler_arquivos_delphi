unit UnitFileManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.IOUtils, System.Hash, System.Variants,
  System.Generics.Collections, System.Types;

type

  TSyncOperation = (soCopy, soUpdate, soDelete, soNone);

  TFileSyncInfo = record
    FileName: string;
    RelativePath: string;
    Operation: TSyncOperation;
    SourcePath: string;
    TargetPath: string;
    SourceMD5: string;
    TargetMD5: string;
  end;

  TFileSyncList = TArray<TFileSyncInfo>;

  TFileInfo = record
    Name: string;
    Path: string;
    Size: Int64;
    ModifiedDate: TDateTime;
    MD5: string;
  end;

  TFileList = TArray<TFileInfo>;

  TFileManager = Class
    private
      function CalculateFileMD5(const FilePath: string): string;
      function ShouldIgnoreFile(const FileName: string): Boolean;
      function GetRelativePath(const FullPath, BaseDir: string): string;
      function GetAllFilesRecursive(const BaseDir: string): TArray<string>;
    public
      procedure SynchronizeFolders(const SourceDir, TargetDir: string);
      function CompareFolders(const SourceDir, TargetDir: string): TFileSyncList;
    protected
  End;



implementation

{ TFileManager }

function TFileManager.CalculateFileMD5(const FilePath: string): string;
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

function TFileManager.CompareFolders(const SourceDir, TargetDir: string): TFileSyncList;
var
  SourceFiles, TargetFiles: TStringDynArray;
  SourceDict, TargetDict: TDictionary<string, string>;
  RelativePath, SourceMD5, TargetMD5: string;
  i: Integer;
  SyncInfo: TFileSyncInfo;
  Keys: TArray<string>;
begin
  Result := nil;

  if not TDirectory.Exists(SourceDir) then
    raise Exception.Create('Diretório fonte não existe: ' + SourceDir);

  if not TDirectory.Exists(TargetDir) then
    raise Exception.Create('Diretório destino não existe: ' + TargetDir);

  // Obter todos os arquivos (recursivamente)
  SourceFiles := GetAllFilesRecursive(SourceDir);
  TargetFiles := GetAllFilesRecursive(TargetDir);

  // Criar dicionários para acesso rápido
  SourceDict := TDictionary<string, string>.Create;
  TargetDict := TDictionary<string, string>.Create;
  try
    // Popular dicionário de origem
    for i := 0 to High(SourceFiles) do
    begin
      try
        RelativePath := GetRelativePath(SourceFiles[i], SourceDir);
        if RelativePath <> '' then
          SourceDict.AddOrSetValue(RelativePath, SourceFiles[i]);
      except
        on E: Exception do
{$IFDEF DEBUG}
          Writeln('Erro ao processar arquivo fonte: ' + SourceFiles[i] + ' - ' + E.Message);
{$ENDIF}
      end;
    end;

    // Popular dicionário de destino
    for i := 0 to High(TargetFiles) do
    begin
      try
        RelativePath := GetRelativePath(TargetFiles[i], TargetDir);
        if RelativePath <> '' then
          TargetDict.AddOrSetValue(RelativePath, TargetFiles[i]);
      except
        on E: Exception do
{$IFDEF DEBUG}
          Writeln('Erro ao processar arquivo destino: ' + TargetFiles[i] + ' - ' + E.Message);
{$ENDIF}
      end;
    end;

    // Obter chaves para evitar problemas de modificação durante iteração
    Keys := SourceDict.Keys.ToArray;

    // Processar arquivos que existem apenas na origem (COPIA)
    for RelativePath in Keys do
    begin
      if not TargetDict.ContainsKey(RelativePath) then
      begin
        SyncInfo.FileName := TPath.GetFileName(SourceDict[RelativePath]);
        SyncInfo.RelativePath := RelativePath;
        SyncInfo.Operation := soCopy;
        SyncInfo.SourcePath := SourceDict[RelativePath];
        SyncInfo.TargetPath := IncludeTrailingPathDelimiter(TargetDir) + RelativePath;

        try
          SyncInfo.SourceMD5 := CalculateFileMD5(SyncInfo.SourcePath);
        except
          SyncInfo.SourceMD5 := 'Error calculating MD5';
        end;

        SyncInfo.TargetMD5 := '';

        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := SyncInfo;
      end;
    end;

    // Processar arquivos que existem em ambas as pastas (COMPARAÇÃO MD5)
    for RelativePath in Keys do
    begin
      if TargetDict.ContainsKey(RelativePath) then
      begin
        try
          SourceMD5 := CalculateFileMD5(SourceDict[RelativePath]);
          TargetMD5 := CalculateFileMD5(TargetDict[RelativePath]);
        except
          on E: Exception do
          begin
            SourceMD5 := 'Error';
            TargetMD5 := 'Error';
{$IFDEF DEBUG}
            Writeln('Erro ao calcular MD5 para: ' + RelativePath + ' - ' + E.Message);
{$ENDIF}
          end;
        end;

        if SourceMD5 <> TargetMD5 then
        begin
          SyncInfo.FileName := TPath.GetFileName(SourceDict[RelativePath]);
          SyncInfo.RelativePath := RelativePath;
          SyncInfo.Operation := soUpdate;
          SyncInfo.SourcePath := SourceDict[RelativePath];
          SyncInfo.TargetPath := TargetDict[RelativePath];
          SyncInfo.SourceMD5 := SourceMD5;
          SyncInfo.TargetMD5 := TargetMD5;

          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := SyncInfo;
        end;
      end;
    end;

    // Processar arquivos que existem apenas no destino (EXCLUSÃO)
    Keys := TargetDict.Keys.ToArray;
    for RelativePath in Keys do
    begin
      if not SourceDict.ContainsKey(RelativePath) then
      begin
        SyncInfo.FileName := TPath.GetFileName(TargetDict[RelativePath]);
        SyncInfo.RelativePath := RelativePath;
        SyncInfo.Operation := soDelete;
        SyncInfo.SourcePath := '';
        SyncInfo.TargetPath := TargetDict[RelativePath];
        SyncInfo.SourceMD5 := '';

        try
          SyncInfo.TargetMD5 := CalculateFileMD5(SyncInfo.TargetPath);
        except
          SyncInfo.TargetMD5 := 'Error calculating MD5';
        end;

        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := SyncInfo;
      end;
    end;

  finally
    SourceDict.Free;
    TargetDict.Free;
  end;
end;

function TFileManager.GetAllFilesRecursive(const BaseDir: string): TArray<string>;
var
  List: TList<string>;

  procedure RecursiveAdd(const Dir: string);
  var
    Files: TStringDynArray;
    Directories: TStringDynArray;
    i: Integer;
    FileName: string;
  begin
    if not TDirectory.Exists(Dir) then
      Exit;

    try
      // Busca arquivos no diretório atual (filtrados)
      Files := TDirectory.GetFiles(Dir);
      for FileName in Files do
      begin
        if not ShouldIgnoreFile(FileName) then
          List.Add(FileName);
      end;

      // Busca recursivamente nas subpastas
      Directories := TDirectory.GetDirectories(Dir);
      for i := 0 to High(Directories) do
      begin
        RecursiveAdd(Directories[i]);
      end;
    except
      on E: Exception do
      begin
        // Log do erro, mas continua processando
{$IFDEF DEBUG}
        Writeln('Erro ao acessar diretório ' + Dir + ': ' + E.Message);
{$ENDIF}
      end;
    end;
  end;

begin
  List := TList<string>.Create;
  try
    RecursiveAdd(BaseDir);
    Result := List.ToArray;
  finally
    List.Free;
  end;

end;

function TFileManager.GetRelativePath(const FullPath, BaseDir: string): string;
var
  Base: string;
begin
  Base := IncludeTrailingPathDelimiter(BaseDir);
  if Pos(Base, FullPath) = 1 then
    Result := Copy(FullPath, Length(Base) + 1, Length(FullPath))
  else
    Result := FullPath;
end;

function TFileManager.ShouldIgnoreFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(TPath.GetExtension(FileName));
  Result := (Ext = '.log') or (Ext = '.fdb') or (Ext = '.zip') or (Ext = '.fbk');
end;

procedure TFileManager.SynchronizeFolders(const SourceDir, TargetDir: string);
var
  SyncList: TFileSyncList;
  SyncInfo: TFileSyncInfo;
  TargetDirPath: string;
begin
  // Obter lista de operações necessárias
  SyncList := CompareFolders(SourceDir, TargetDir);

  // Executar as operações
  for SyncInfo in SyncList do
  begin
    try
      case SyncInfo.Operation of
        soCopy:
          begin
            // Garantir que o diretório de destino existe
            TargetDirPath := ExtractFilePath(SyncInfo.TargetPath);
            if not TDirectory.Exists(TargetDirPath) then
              ForceDirectories(TargetDirPath);

            TFile.Copy(SyncInfo.SourcePath, SyncInfo.TargetPath, False);
{$IFDEF DEBUG}
            Writeln('COPIADO: ' + SyncInfo.RelativePath);
{$ENDIF}
          end;

        soUpdate:
          begin
            // Fazer backup do arquivo antigo se necessário
            if TFile.Exists(SyncInfo.TargetPath + '.bak') then
              TFile.Delete(SyncInfo.TargetPath + '.bak');

            if TFile.Exists(SyncInfo.TargetPath) then
              TFile.Copy(SyncInfo.TargetPath, SyncInfo.TargetPath + '.bak', False);

            // Substituir arquivo
            TFile.Copy(SyncInfo.SourcePath, SyncInfo.TargetPath, True);

            // Apaga arquivo após copia do mais atual
            TFile.Delete(SyncInfo.TargetPath + '.bak');
{$IFDEF DEBUG}
            Writeln('ATUALIZADO: ' + SyncInfo.RelativePath);
{$ENDIF}
          end;

        soDelete:
          begin
            if TFile.Exists(SyncInfo.TargetPath) then
            begin
              TFile.Delete(SyncInfo.TargetPath);
{$IFDEF DEBUG}
              Writeln('EXCLUÍDO: ' + SyncInfo.RelativePath);
{$ENDIF}
            end;
          end;
      end;
    except
      on E: Exception do
        Writeln('Erro ao executar operação em ' + SyncInfo.RelativePath + ': ' + E.Message);
    end;
  end;
end;

end.
