program TimerQueue;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form3},
  Smart.TimerQueue in 'Smart.TimerQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
