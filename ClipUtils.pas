unit ClipUtils;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Clipbrd, System.Generics.Collections,
  Registry, System.StrUtils;

type
  TClipboardItem = record
    TimeStamp: TDateTime;
    Content: string;
    Format: UINT;
    constructor Create(AContent: string; AFormat: UINT);
  end;

  TClipboardManager = class
  private
    FHistory: TList<TClipboardItem>;
    FLastClipboardContent: string;
    FLastClipboardFormat: UINT;
    FEnabled: Boolean;
    FMaxHistoryItems: Integer;
    FAutoSave: Boolean;
    FAutoStart: Boolean;
    FStartMinimized: Boolean;
    FRegistry: TRegistry;
    procedure SetMaxHistoryItems(const Value: Integer);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetAutoStart(Enabled: Boolean);
    function IsAutoStartEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckClipboard;
    procedure AddToHistory(const AContent: string; AFormat: UINT);
    procedure ClearHistory;
    procedure SaveHistoryToFile(const AFileName: string);
    procedure LoadHistoryFromFile(const AFileName: string);
    function GetHistoryAsText: string;
    function SearchHistory(const ASearchText: string; AWholeWord: Boolean = False;
      AStartsWith: Boolean = False): TArray<TClipboardItem>;
    
    property History: TList<TClipboardItem> read FHistory;
    property Enabled: Boolean read FEnabled write FEnabled;
    property MaxHistoryItems: Integer read FMaxHistoryItems write SetMaxHistoryItems;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property AutoStart: Boolean read FAutoStart write FAutoStart;
    property StartMinimized: Boolean read FStartMinimized write FStartMinimized;
  end;

implementation

const
  REG_KEY = 'Software\ClipboardManager';
  APP_NAME = 'ClipboardManager';

{ TClipboardItem }

constructor TClipboardItem.Create(AContent: string; AFormat: UINT);
begin
  TimeStamp := Now;
  Content := AContent;
  Format := AFormat;
end;

{ TClipboardManager }

constructor TClipboardManager.Create;
begin
  FHistory := TList<TClipboardItem>.Create;
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  
  // Инициализация значений по умолчанию
  FEnabled := True;
  FMaxHistoryItems := 100;
  FAutoSave := False;
  FAutoStart := False;
  FStartMinimized := False;
  
  // Загружаем настройки
  LoadSettings;
end;

destructor TClipboardManager.Destroy;
begin
  if FAutoSave then
    SaveHistoryToFile('clipboard_history.txt');
    
  SaveSettings;
  FHistory.Free;
  FRegistry.Free;
  inherited;
end;

procedure TClipboardManager.LoadSettings;
begin
  if FRegistry.OpenKey(REG_KEY, True) then
  begin
    try
      if FRegistry.ValueExists('Enabled') then
        FEnabled := FRegistry.ReadBool('Enabled');
        
      if FRegistry.ValueExists('AutoSave') then
        FAutoSave := FRegistry.ReadBool('AutoSave');
        
      if FRegistry.ValueExists('MaxItems') then
        FMaxHistoryItems := FRegistry.ReadInteger('MaxItems');
        
      if FRegistry.ValueExists('AutoStart') then
        FAutoStart := FRegistry.ReadBool('AutoStart');
        
      if FRegistry.ValueExists('StartMinimized') then
        FStartMinimized := FRegistry.ReadBool('StartMinimized');
    finally
      FRegistry.CloseKey;
    end;
  end;
  
  // Проверяем автозагрузку в реестре
  FAutoStart := IsAutoStartEnabled;
end;

procedure TClipboardManager.SaveSettings;
begin
  if FRegistry.OpenKey(REG_KEY, True) then
  begin
    try
      FRegistry.WriteBool('Enabled', FEnabled);
      FRegistry.WriteBool('AutoSave', FAutoSave);
      FRegistry.WriteInteger('MaxItems', FMaxHistoryItems);
      FRegistry.WriteBool('AutoStart', FAutoStart);
      FRegistry.WriteBool('StartMinimized', FStartMinimized);
      
      // Обновляем автозагрузку
      SetAutoStart(FAutoStart);
    finally
      FRegistry.CloseKey;
    end;
  end;
end;

procedure TClipboardManager.SetAutoStart(Enabled: Boolean);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False) then
    begin
      if Enabled then
        Reg.WriteString(APP_NAME, '"' + ParamStr(0) + '" /minimized')
      else
        Reg.DeleteValue(APP_NAME);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TClipboardManager.IsAutoStartEnabled: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False) then
    begin
      Result := Reg.ValueExists(APP_NAME);
      Reg.CloseKey;
    end
    else
      Result := False;
  finally
    Reg.Free;
  end;
end;

procedure TClipboardManager.CheckClipboard;
var
  ClipContent: string;
begin
  if not FEnabled then Exit;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      ClipContent := Clipboard.AsText;
      
      // Проверяем, изменилось ли содержимое буфера
      if (ClipContent <> FLastClipboardContent) or 
         (CF_TEXT <> FLastClipboardFormat) then
      begin
        AddToHistory(ClipContent, CF_TEXT);
        FLastClipboardContent := ClipContent;
        FLastClipboardFormat := CF_TEXT;
      end;
    end
    else if Clipboard.HasFormat(CF_BITMAP) and 
            ((FLastClipboardContent = '') or (CF_BITMAP <> FLastClipboardFormat)) then
    begin
      AddToHistory('[Bitmap Image]', CF_BITMAP);
      FLastClipboardContent := '[Bitmap Image]';
      FLastClipboardFormat := CF_BITMAP;
    end;
    // Можно добавить обработку других форматов (CF_UNICODETEXT, CF_HDROP и т.д.)
  except
    on E: Exception do
      OutputDebugString(PChar('Clipboard error: ' + E.Message));
  end;
end;

procedure TClipboardManager.AddToHistory(const AContent: string; AFormat: UINT);
begin
  if AContent.Trim = '' then Exit;

  // Ограничиваем количество элементов в истории
  if FHistory.Count >= FMaxHistoryItems then
    FHistory.Delete(0);

  FHistory.Add(TClipboardItem.Create(AContent, AFormat));
end;

procedure TClipboardManager.ClearHistory;
begin
  FHistory.Clear;
  FLastClipboardContent := '';
  FLastClipboardFormat := 0;
end;

procedure TClipboardManager.SaveHistoryToFile(const AFileName: string);
var
  SL: TStringList;
  Item: TClipboardItem;
begin
  SL := TStringList.Create;
  try
    for Item in FHistory do
      SL.Add(Format('%s [%s] %s', 
        [DateTimeToStr(Item.TimeStamp), 
         Format('%.8x', [Item.Format]), 
         Item.Content]));

    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

procedure TClipboardManager.LoadHistoryFromFile(const AFileName: string);
var
  SL: TStringList;
  i: Integer;
  Parts: TArray<string>;
  ItemTime: TDateTime;
  ItemFormat: UINT;
  ItemContent: string;
begin
  if not FileExists(AFileName) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    FHistory.Clear;
    
    for i := 0 to SL.Count - 1 do
    begin
      // Формат строки: "дата [формат] содержимое"
      Parts := SL[i].Split([' [', '] ']);
      if Length(Parts) >= 3 then
      begin
        if TryStrToDateTime(Parts[0], ItemTime) then
        begin
          ItemFormat := StrToInt('$' + Parts[1]);
          ItemContent := SL[i].Substring(
            Pos('] ', SL[i]) + 2);
            
          FHistory.Add(TClipboardItem.Create(ItemContent, ItemFormat));
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TClipboardManager.GetHistoryAsText: string;
var
  SB: TStringBuilder;
  Item: TClipboardItem;
begin
  SB := TStringBuilder.Create;
  try
    for Item in FHistory do
    begin
      SB.AppendLine(Format('%s [%s]', 
        [DateTimeToStr(Item.TimeStamp), 
         Format('%.8x', [Item.Format])]));
      SB.AppendLine(Item.Content);
      SB.AppendLine('------------------------');
    end;
    
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TClipboardManager.SearchHistory(const ASearchText: string; 
  AWholeWord: Boolean = False; AStartsWith: Boolean = False): TArray<TClipboardItem>;
var
  I: Integer;
  LowerSearch: string;
  Item: TClipboardItem;
  FoundItems: TList<TClipboardItem>;
begin
  LowerSearch := LowerCase(ASearchText);
  FoundItems := TList<TClipboardItem>.Create;
  try
    for I := 0 to FHistory.Count - 1 do
    begin
      Item := FHistory[I];
      
      if AStartsWith then
      begin
        if LowerCase(Item.Content).StartsWith(LowerSearch) then
          FoundItems.Add(Item);
      end
      else if AWholeWord then
      begin
        if LowerCase(Item.Content) = LowerSearch then
          FoundItems.Add(Item);
      end
      else
      begin
        if Pos(LowerSearch, LowerCase(Item.Content)) > 0 then
          FoundItems.Add(Item);
      end;
    end;
    
    Result := FoundItems.ToArray;
  finally
    FoundItems.Free;
  end;
end;

procedure TClipboardManager.SetMaxHistoryItems(const Value: Integer);
begin
  FMaxHistoryItems := Value;
  // Удаляем лишние элементы, если новый лимит меньше текущего количества
  while FHistory.Count > FMaxHistoryItems do
    FHistory.Delete(0);
end;

end.