unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  Smart.TimerQueue;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // 단 한번만 실행되는 타이머
  TTimerQueue.New(
    'Run Once Timer',
    0,
    procedure(ATimer: PTimer)
    begin
      ShowMessage(ATimer.Name);
    end
  );

  // 매 10초마다 실행되는 타이머
  TTimerQueue.New(
    'Run every 10 sec Timer',
    10000,
    procedure(ATimer: PTimer)
    begin
      ShowMessage(ATimer.Name);
    end
  );
end;

end.
