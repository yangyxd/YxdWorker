unit Unit1;

interface

uses
  YxdWorker, SyncObjs, CommCtrl,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    FLogs: TStrings;       // 存放日志内容
    FAutoScroll: Boolean;  // 是否自动滚动
    FLogsIsDel: Boolean;   // 是否已经删除日志
    FLogRef: Integer;      // 状态计数器
    FLocker: TCriticalSection;

    FTestRef: Integer;
  public
    { Public declarations }
    procedure UpdateList();
    procedure DoDataChange();
    procedure DoWriteLog(Sender: TObject; const Log: string);
    procedure OnDataChange(AJob: PJob);

    procedure DoTest(AJob: PJob);
    procedure Log(const Text: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Workers.Post(DoTest, nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FLocker.Enter;
  FLogs.Clear;
  FLogsIsDel := True;
  FLocker.Leave;
  DoDataChange();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Log('写一行日志');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Workers.Clear(DoTest, nil);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FLocker.Enter;
  FAutoScroll := CheckBox1.Checked;
  FLocker.Leave;
end;

procedure TForm1.DoDataChange;
begin
  // FLogRef 很关键，决定什么时候真正更新显示。
  // 多线程写日志时，FLogRef > 1，那么也只更新一次。
  if InterlockedIncrement(FLogRef) = 1 then
    // 延时50ms更新。此值设定的越大，列表更新的越慢。
    // 不延时的话，界面刷新太快，占用资源会比较大
    Workers.Post(OnDataChange, nil, True, 50)
  else
    InterlockedDecrement(FLogRef);
end;

procedure TForm1.DoTest(AJob: PJob);
var
  I, M: Integer;
begin
  M := 0;
  while (not AJob.IsTerminated) and (M < 500) do begin
    Inc(M);
    I := InterlockedIncrement(FTestRef);
    Log(Format('日志内容。工作者：%d. (%d)', [AJob.Handle, I]));
    Sleep(10);
  end;
end;

procedure TForm1.DoWriteLog(Sender: TObject; const Log: string);
var
  I: Integer;
begin
  if Assigned(FLogs) and (Assigned(Self)) then begin
    FLocker.Enter;
    // 大于10万行时，删除前面的1万行
    if FLogs.Count > 100000 then begin
      FLogsIsDel := True;
      for I := 10000 downto 0 do
        FLogs.Delete(I);  
    end;
    // 添加当前日志内容
    FLogs.Add('[' + FormatDateTime('hh:mm:ss.zzz', Now) + '] ' + Log);
    FLocker.Leave;
    // 产生一个变更通知
    //DoDataChange();
  end;
end;

// 初始化
procedure TForm1.FormCreate(Sender: TObject);
begin
  FLogsIsDel := False;
  FLogRef := 0;
  FTestRef := 0;
  FLogs := TStringList.Create();
  FLocker := TCriticalSection.Create;
  FAutoScroll := CheckBox1.Checked;
  // 将最大工作者数量设置大一些。因为太少的话，工作者不够用，没有时间来显示日志了。
  //Workers.MaxWorkers := 512;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Workers.Clear(Self);
  FreeAndNil(FLogs);
  FreeAndNil(FLocker);
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
begin
  FLocker.Enter;
  if Assigned(FLogs) and (Item.Index < FLogs.Count) then
    Item.Caption := FLogs[Item.Index];
  FLocker.Leave;
end;

procedure TForm1.Log(const Text: string);
begin
  if Assigned(Self) then    
    DoWriteLog(Self, Text);
end;

procedure TForm1.OnDataChange(AJob: PJob);
begin
  if Assigned(Self) and Assigned(FLogs) then begin
    UpdateList;
    InterlockedDecrement(FLogRef);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Assigned(Workers) then Exit;  
  Label1.Caption := Format('CPU: %d, Workers: %d,%d, Busy: %d, Idle: %d, Count: %d. '
    + 'JOB: Simple: %d, Repeat: %d, Signal: %d',
    [Workers.CPUNum, Workers.MinWorkers, Workers.MaxWorkers,
      Workers.BusyWorkerCount, Workers.IdleWorkerCount,
      Workers.WorkerCount, Workers.SimpleJobCount, Workers.RepeatJobCount,
      Workers.SignalJobCount]);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  UpdateList;
end;

procedure TForm1.UpdateList;
begin
  if Assigned(ListView1) and (ListView1.HandleAllocated = True) then begin
    ListView_SetItemCountEx(ListView1.Handle, FLogs.Count,
      LVSICF_NOINVALIDATEALL or LVSICF_NOSCROLL); // 修改列表项的数量，并不改变滚动条位置
    if FAutoScroll then
      SendMessage(ListView1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    if FLogsIsDel then begin
      FLogsIsDel := False;
      ListView1.Invalidate;
    end;
  end;
end;

end.
