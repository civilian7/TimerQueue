unit Smart.TimerQueue;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Winapi.Windows;

type
  PTimer = ^TTimer;
  TTimer = record
    Handle: THandle;
    Interval: Cardinal;
    IsRunning: Boolean;
    Name: string;
    Proc: TProc<PTimer>;
  end;

  TTimerQueue = class
  strict private
    const
      DEFAULT_DUE_TIME = 1000;
    type
      TTimerList = class(TDictionary<string, PTimer>)
      protected
        procedure ValueNotify(const Value: PTimer; Action: TCollectionNotification); override;
      end;
    class var
      FDueTime: Integer;
      FLocker: TCriticalSection;
      FTimerList: TTimerList;
      FTimerQueueHandle: THandle;

    class constructor Create;
    class destructor Destroy;

    class function  GetCount: Integer; static;
    class function  GetTimer(const AName: string): PTimer; static;
    class function  InternalNewTimer(ATimer: PTimer): PTimer; static;
  protected
    class procedure Execute(ATimer: PTimer); static;
  public
    class procedure Clear; static;
    class function  New(const AName: string; const AInterval: Cardinal; AProc: TProc<PTimer>): PTimer; static;
    class procedure Remove(ATimer: PTimer); overload; static;
    class procedure Remove(const AName: string); overload; static;
    class procedure SetInterval(ATimer: PTimer; AInterval: Cardinal); static;

    class property Count: Integer read GetCount;
    class property DueTime: Integer read FDueTime write FDueTime;
    class property Handle: THandle read FTimerQueueHandle;
    class property Timer[const AName: string]: PTimer read GetTimer; default;
  end;

implementation

{ Callback }

procedure WaitOrTimerCallback(ATimer: PTimer; TimerOrWaitFired: ByteBool); stdcall;
begin
  if not Assigned(ATimer) then
    Exit;

  TTimerQueue.Execute(ATimer);
end;

{ TTimerQueue.TTimerList }

procedure TTimerQueue.TTimerList.ValueNotify(const Value: PTimer; Action: TCollectionNotification);
begin
  inherited ValueNotify(Value, Action);

  if (Action = cnRemoved) and Assigned(Value) then
    System.Dispose(Value);
end;

{ TTimerQueue }

class constructor TTimerQueue.Create;
begin
  FDueTime := DEFAULT_DUE_TIME;
  FTimerQueueHandle := CreateTimerQueue();
  FTimerList := TTimerList.Create;
  FLocker := TCriticalSection.Create;
end;

class destructor TTimerQueue.Destroy;
begin
  DeleteTimerQueueEx(FTimerQueueHandle, 0);
  FTimerQueueHandle := INVALID_HANDLE_VALUE;

  Clear;
  FTimerList.Free;
  FLocker.Free;
end;

class procedure TTimerQueue.Clear;
begin
  FLocker.Enter;
  try
    FTimerList.Clear;
  finally
    FLocker.Leave;
  end;
end;

class procedure TTimerQueue.Execute(ATimer: PTimer);
begin
  if ATimer.IsRunning then
    Exit;

  if Assigned(ATimer.Proc) then
  begin
    ATimer.IsRunning := True;
    ATimer.Proc(ATimer);
    ATimer.IsRunning := False;
    if (ATimer.Interval = 0) then
      TTimerQueue.Remove(ATimer);
  end;
end;

class function TTimerQueue.GetCount: Integer;
begin
  FLocker.Enter;
  try
    Result := FTimerList.Count;
  finally
    FLocker.Leave;
  end;
end;

class function TTimerQueue.GetTimer(const AName: string): PTimer;
begin
  FTimerList.TryGetValue(UpperCase(AName), Result);
end;

class function TTimerQueue.InternalNewTimer(ATimer: PTimer): PTimer;
begin
  if not Assigned(ATimer) then
    Exit(nil);

  if CreateTimerQueueTimer(ATimer.Handle, FTimerQueueHandle, @WaitOrTimerCallback, ATimer, FDueTime, ATimer.Interval, 0) then
  begin
    FLocker.Enter;
    try
      FTimerList.AddOrSetValue(ATimer.Name, ATimer);
    finally
      FLocker.Leave;
    end;
    Result := ATimer;
  end
  else
  begin
    System.Dispose(ATimer);
    Result := nil;
  end;
end;

class function TTimerQueue.New(const AName: string; const AInterval: Cardinal; AProc: TProc<PTimer>): PTimer;
var
  LTimer: PTimer;
begin
  System.New(LTimer);
  LTimer.Name := UpperCase(AName);
  LTimer.Interval := AInterval;
  LTimer.IsRunning := False;
  LTimer.Proc := AProc;

  Result := InternalNewTimer(LTimer);
end;

class procedure TTimerQueue.Remove(const AName: string);
var
  LTimer: PTimer;
begin
  FLocker.Enter;
  try
    if FTimerList.TryGetValue(UpperCase(AName), LTimer) then
      Remove(LTimer);
  finally
    FLocker.Leave;
  end;
end;

class procedure TTimerQueue.Remove(ATimer: PTimer);
begin
  if not Assigned(ATimer) then
    Exit;

  FLocker.Enter;
  try
    DeleteTimerQueueTimer(FTimerQueueHandle, ATimer.Handle, 0);
    FTimerList.Remove(ATimer^.Name);
  finally
    FLocker.Leave;
  end;
end;

class procedure TTimerQueue.SetInterval(ATimer: PTimer; AInterval: Cardinal);
begin
  if not Assigned(ATimer) then
    Exit;

  FLocker.Enter;
  try
    ChangeTimerQueueTimer(FTimerQueueHandle, ATimer.Handle, 0, AInterval);
    ATimer.Interval := AInterval;
  finally
    FLocker.Leave;
  end;
end;

end.
