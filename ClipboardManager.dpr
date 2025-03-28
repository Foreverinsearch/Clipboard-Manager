program ClipboardManager;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  HistoryForm in 'HistoryForm.pas' {FormHistory},
  SettingsForm in 'SettingsForm.pas' {FormSettings},
  ClipUtils in 'ClipUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
