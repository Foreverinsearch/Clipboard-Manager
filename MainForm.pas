unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ClipUtils, Vcl.Menus, Registry, ShellAPI, SettingsForm, HistoryForm;

type
  TFormMain = class(TForm)
    btnShowHistory: TButton;
    btnClearHistory: TButton;
    btnSettings: TButton;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    miShow: TMenuItem;
    miExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowHistoryClick(Sender: TObject);
    procedure btnClearHistoryClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  private
    FClipManager: TClipboardManager;
    FStartMinimized: Boolean;
    FFormSettings: TFormSettings;
    FFormHistory: TFormHistory;
    procedure MinimizeToTray;
    procedure RestoreFromTray;
    // Удалено объявление UpdateClipboardHistory, так как оно больше не используется
  public
    property ClipManager: TClipboardManager read FClipManager;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
begin
  FClipManager := TClipboardManager.Create;
  
  FFormSettings := nil;
  FFormHistory := nil;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\ClipboardManager', True) then
    begin
      try
        if Reg.ValueExists('StartMinimized') then
          FStartMinimized := Reg.ReadBool('StartMinimized')
        else
          FStartMinimized := False;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
  
  if (ParamCount > 0) and (LowerCase(ParamStr(1)) = '/minimized') then
    FStartMinimized := True;
  
  if FClipManager.AutoSave and FileExists('clipboard_history.txt') then
    FClipManager.LoadHistoryFromFile('clipboard_history.txt');
    
  Caption := 'Clipboard Manager v1.0';
  
  if FStartMinimized then
  begin
    MinimizeToTray;
    Hide;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FFormSettings) then
    FFormSettings.Free;
    
  if Assigned(FFormHistory) then
    FFormHistory.Free;
    
  FClipManager.Free;
end;

procedure TFormMain.btnSettingsClick(Sender: TObject);
begin
  if not Assigned(FFormSettings) then
    FFormSettings := TFormSettings.Create(Application);
    
  FFormSettings.SetClipManager(FClipManager);
  
  if FFormSettings.ShowModal = mrOk then
  begin
    // Настройки сохраняются автоматически
  end;
end;

procedure TFormMain.btnShowHistoryClick(Sender: TObject);
begin
  if not Assigned(FFormHistory) then
    FFormHistory := TFormHistory.Create(Application);
    
  FFormHistory.UpdateHistoryList(FClipManager.History);
  FFormHistory.Show;
end;

procedure TFormMain.btnClearHistoryClick(Sender: TObject);
begin
  if MessageDlg('Очистить всю историю буфера обмена?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FClipManager.ClearHistory;
    if Assigned(FFormHistory) then
      FFormHistory.UpdateHistoryList(FClipManager.History);
    ShowMessage('История буфера обмена очищена');
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  FClipManager.CheckClipboard;
  if Assigned(FFormHistory) and FFormHistory.Visible then
    FFormHistory.UpdateHistoryList(FClipManager.History);
end;

procedure TFormMain.MinimizeToTray;
begin
  TrayIcon1.Visible := True;
  ShowWindow(Handle, SW_HIDE);
end;

procedure TFormMain.RestoreFromTray;
begin
  TrayIcon1.Visible := False;
  ShowWindow(Handle, SW_SHOW);
  BringToFront;
end;

procedure TFormMain.TrayIcon1DblClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TFormMain.miShowClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if TrayIcon1.Visible then
  begin
    CanClose := False;
    MinimizeToTray;
  end;
end;

end.