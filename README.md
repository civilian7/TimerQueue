# TimerQueue
Windows의 TimerQueue를 사용하여 타이머를 손쉽게 사용할 수 있다.

사용 예

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
