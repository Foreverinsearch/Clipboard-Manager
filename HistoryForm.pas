unit HistoryForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, 
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Clipbrd, 
  System.Generics.Collections, ClipUtils;

type
  TFormHistory = class(TForm)
    lbHistory: TListBox;
    btnCopy: TButton;
    btnClose: TButton;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure lbHistoryDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateHistoryList(History: TList<TClipboardItem>);
  end;

var
  FormHistory: TFormHistory;

implementation

{$R *.dfm}

uses MainForm;

procedure TFormHistory.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHistory.btnCopyClick(Sender: TObject);
begin
  if lbHistory.ItemIndex >= 0 then
    Clipboard.AsText := lbHistory.Items[lbHistory.ItemIndex];
end;

procedure TFormHistory.btnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    lbHistory.Items.SaveToFile(SaveDialog1.FileName);
end;

procedure TFormHistory.lbHistoryDblClick(Sender: TObject);
begin
  btnCopy.Click;
end;

procedure TFormHistory.UpdateHistoryList(History: TList<TClipboardItem>);
var
  Item: TClipboardItem;
begin
  lbHistory.Clear;
  
  for Item in History do
    lbHistory.Items.Add(Format('%s: %s', 
      [DateTimeToStr(Item.TimeStamp), 
       Item.Content.Substring(0, 100)])); // Показываем первые 100 символов
end;

end.