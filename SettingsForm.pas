unit SettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  ClipUtils, Registry;

type
  TFormSettings = class(TForm)
    chkEnabled: TCheckBox;
    chkAutoSave: TCheckBox;
    seMaxItems: TSpinEdit;
    Label1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    chkAutoStart: TCheckBox;
    chkStartMinimized: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FClipManager: TClipboardManager;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetAutoStart(Enabled: Boolean);
  public
    procedure SetClipManager(AManager: TClipboardManager);
  end;

implementation

{$R *.dfm}

const
  REG_KEY = 'Software\ClipboardManager';
  APP_NAME = 'ClipboardManager';

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFormSettings.FormDestroy(Sender: TObject);
begin
  // Не освобождаем FClipManager - он принадлежит MainForm
end;

procedure TFormSettings.SetClipManager(AManager: TClipboardManager);
begin
  FClipManager := AManager;
  if Assigned(FClipManager) then
  begin
    chkEnabled.Checked := FClipManager.Enabled;
    chkAutoSave.Checked := FClipManager.AutoSave;
    seMaxItems.Value := FClipManager.MaxHistoryItems;
    chkAutoStart.Checked := FClipManager.AutoStart;
    chkStartMinimized.Checked := FClipManager.StartMinimized;
  end;
end;

procedure TFormSettings.LoadSettings;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_KEY, False) then
    begin
      try
        if Reg.ValueExists('Enabled') then 
          chkEnabled.Checked := Reg.ReadBool('Enabled');
        if Reg.ValueExists('AutoSave') then
          chkAutoSave.Checked := Reg.ReadBool('AutoSave');
        if Reg.ValueExists('MaxItems') then
          seMaxItems.Value := Reg.ReadInteger('MaxItems');
        if Reg.ValueExists('AutoStart') then
          chkAutoStart.Checked := Reg.ReadBool('AutoStart');
        if Reg.ValueExists('StartMinimized') then
          chkStartMinimized.Checked := Reg.ReadBool('StartMinimized');
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormSettings.SaveSettings;
var
  Reg: TRegistry;
begin
  if not Assigned(FClipManager) then Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_KEY, True) then
    begin
      try
        Reg.WriteBool('Enabled', chkEnabled.Checked);
        Reg.WriteBool('AutoSave', chkAutoSave.Checked);
        Reg.WriteInteger('MaxItems', seMaxItems.Value);
        Reg.WriteBool('AutoStart', chkAutoStart.Checked);
        Reg.WriteBool('StartMinimized', chkStartMinimized.Checked);
      finally
        Reg.CloseKey;
      end;
    end;

    // Обновляем автозагрузку
    SetAutoStart(chkAutoStart.Checked);
    
    // Обновляем менеджер
    FClipManager.Enabled := chkEnabled.Checked;
    FClipManager.AutoSave := chkAutoSave.Checked;
    FClipManager.MaxHistoryItems := seMaxItems.Value;
    FClipManager.AutoStart := chkAutoStart.Checked;
    FClipManager.StartMinimized := chkStartMinimized.Checked;
  finally
    Reg.Free;
  end;
end;

procedure TFormSettings.SetAutoStart(Enabled: Boolean);
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
        if Reg.ValueExists(APP_NAME) then
          Reg.DeleteValue(APP_NAME);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormSettings.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

end.