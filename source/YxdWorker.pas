{*******************************************************}
{                                                       }
{       YxdWorker 后台工作者管理库                      }
{                                                       }
{       版权所有 (C) 2013 - 2015      YangYxd           }
{                                                       }
{*******************************************************}
{
 --------------------------------------------------------------------
  说明
 --------------------------------------------------------------------
  YxdWorker 基于QDAC项目的QWorker ，并且绝大部分代码来自于此，
  感谢swish和他的QWorker，QDAC。YxdWorker 版权归swish, YangYxd所有
  QWorker来自QDAC项目，版权归swish(QQ:109867294)所有
  QDAC官方群：250530692

 --------------------------------------------------------------------
  更新记录
 --------------------------------------------------------------------

 2017.09.25 ver 1.1.10
 --------------------------------------------------------------------
  * 同步 QWorker 更新JobGroup的问题
  
 2017.07.28 ver 1.1.9
 --------------------------------------------------------------------
  - 兼容 D7
  
 2016.09.06 ver 1.1.8
 --------------------------------------------------------------------
  - 修复使用匿名作业处理函数时出错的bug

 2016.03.17 ver 1.1.7
 --------------------------------------------------------------------
  - 同步更新QWorker已经发现并修复的BUG, 更加稳定
  
 2015.04.14 ver 1.1.6
 --------------------------------------------------------------------
  - TJob增加Handle属性，可以在作业中查看自己的Handle. 比如定时作业中,
    可以用Handle来取消任务.
  - 其它细小修改

 2015.04.03 ver 1.1.5
 --------------------------------------------------------------------
  - 修正了 GetTimeTick 函数溢出造成定时作业调度失败的问题 (同步QWorker)
  - 修正了 TQRepeatJobs.DoTimeCompare 比较时间时算术溢出，造成特定应用
    环境下出错的问题 (同步QWorker)
    
 2015.01.29 ver 1.1.4
 --------------------------------------------------------------------
  - TJobGroup.Cancel增加是否等待正在运行的作业结束参数
  - 修正了 TSimpleJobs.Clear 如果第一个就满足需要时算法逻辑出错的
    问题(KEN)

 2014.11.11 ver 1.1.3
 --------------------------------------------------------------------
  - 修复在Android平台中存在的Bug
  - 修改 TJobHandle 为 NativeInt，在64位时使用64位整数

 2014.11.08 ver 1.1.2
 --------------------------------------------------------------------
  - 将重复定时作业Data为自动释放时，由完成一次以后就释放改为直到作业
    被取消时才释放
  - 修正了Job.Assign时，忘记增加引用计数的问题

 2014.11.08 ver 1.1.1
 --------------------------------------------------------------------
  - 修改作业投寄 Post 的返回值为 TJobHandle 型， 用来唯一标记一项作
    业，可以在需要时调用Clear(句柄值)来清除相应的作业 (恢宏)
  - 将并行 For 移入 TYxdWorkers 中，直接使用 Workers.&For 即可
  - 增加 Job.ExData 扩展数据，可以使用 NewExData 初始化。
  - 修复定时循环作业且Data为自动释放时，完成一次以后Data已经被释放，
    但在下次触发时仍然存在这个Data的Bug.
  - 增加条件编译开关 AutoFreeJobExData, 启用时强制释放作业的ExData，
    不管FreeType是什么。

 2014.10.14 ver 1.1.0
 --------------------------------------------------------------------
  - 增加GetWorkerState方法，输出各工作者状态
  - 开启 Use_DebugHelper 编译开关时，需要引用 YxdMapFile，可查看作业
    的函数名称
  - 稳定版，修复所有已知BUG

 2014.09.29 ver 1.0.9
 --------------------------------------------------------------------
  - 增加并行 For， 访问方式为TForJobs.For(...) (同步QWorker)
  - 加入后台对CPU利用率的检查，在CPU占用率较低时且有需要立即处理的作
    业时，启动新工作者 (同步QWorker)
  - 修正了未达到工作者线程上限，但已创建的工作者都在工作中时可能造成
    的延迟问题  (同步QWorker)
  - 修改TQJobProc/TQJobProcA/TQJobProcG的写法，以便更方便阅读 (同步
    QWorker)
  - 修复所有已知BUG

 2014.09.10 ver 1.0.8
 --------------------------------------------------------------------
  - 修复TimeToDelay函数错误

 2014.09.05 ver 1.0.7
 --------------------------------------------------------------------
  - 修复JobGroup不能并行执行的BUG
  - 修改HasJobRunning函数，解决JobGroup执行时，由于每个Job运行时间很
    长导致Celar失败的问题。

 2014.08.30 ver 1.0.6
 --------------------------------------------------------------------
  - 修复开启定时作业后，线程回收机制失效BUG
  - 解决FBusyCount计数器相关的BUG
  - 提升稳定性，测速时速度下降，总体来说提升了性能

 2014.08.25 ver 1.0.5
 --------------------------------------------------------------------
  - 作业附加的Data释放方式新增jdfFreeAsC1~jdfFreeAsC6以便上层自己管理
    Data成员数据的自动释放
  - 优化FreeJob, FreeJobData
    
 2014.08.23 ver 1.0.4
 --------------------------------------------------------------------
  - 解决Busy计数器BUG

 2014.08.22 ver 1.0.3
 --------------------------------------------------------------------
  - 解决JobGroup超时和Cancel的问题，解决某些原因引起测速很慢的问题
  - 提取合并部分代码，减少体积

 2014.08.16 ver 1.0.2
 --------------------------------------------------------------------
  - 改进长时间任务处理方式 ，TSimpleJobs增加 FLongFirst，FLongLast 专
    门应对长时间任务，解决长时间任务导致Clear失败BUG
  - 同步QWorker修改TQJobGroup.AfterDone改为除了在完成时，在中断或超时
    时仍然触发
  - 同步QWorker增加TQJobGroup.Run函数加入超时设置，超过指定的时间如果
    仍未执行完成，则中止后续执行
  - 同步QWorker增加TQJobGroup.Cancel函数用于取消未执行的作业执行

 2014.08.16 ver 1.0.1
 --------------------------------------------------------------------
  - 增加 FOnErrorNotify通知事件，以便使用者可以记录相关日志
  - 将原QWorker中的Delay，At，Post合并为Post方法。
  - 将原QWorker中的时间精度由0.1ms调整为1ms.
  - 将原QWorker中TJobHelper的功能直接放入TJob中，以便在D2007中还能保
    持良好的语法提示
  - 将原QWorker中Worker类设置Flags相关功能改为GetValue,SetValue，减小
    单元大小
  - 对JobGroup的Add功能增加参数AFreeType, 并默认AInMainThread=False
  - 提取合并部分代码，减少单元大小
  - 删除Job中的Owner字段

 --------------------------------------------------------------------
}
unit YxdWorker;

{.$DEFINE WORKER_SIMPLE_LOCK} // 是否使用原子自旋锁?
{$DEFINE USE_ATOMIC}          // 是否启用原子函数(不启用时，使用YxdHash单元中的相关函数)
{$DEFINE SAVE_WORDER_TIME}    // 记录工作者开始和最近工作时间
{$IFNDEF NEXTGEN}
{$DEFINE Use_DebugHelper}     // 是否使用调试助手(可查看各工作者执行的作业名称)
{$ENDIF}
{$DEFINE AutoFreeJobExData}   // 是否自动认别Job的ExData，并强制为自动释放

{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE USEINLINE}
{$IFEND}

interface

uses
  {$IFDEF UNICODE}Generics.Collections, {$ENDIF}
  {$IFDEF NEXTGEN}Fmx.Forms, System.Diagnostics, {$ENDIF}
  {$IFDEF POSIX}Posix.Base, Posix.Unistd, Posix.Pthread, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, Messages, TlHelp32, Activex, {$ENDIF}
  {$IFDEF Use_DebugHelper}YxdMapFile, {$ENDIF}
  YxdRBTree, YxdHash, SysUtils, Classes, Types, SyncObjs;

const
  JOB_RUN_ONCE = $0001;         // 作业只运行一次
  JOB_IN_MAINTHREAD = $0002;    // 作业只能在主线程中运行
  JOB_MAX_WORKERS = $0004;      // 尽可能多的开启可能的工作者线程来处理作业，暂不支持
  JOB_LONGTIME = $0008;         // 作业需要很长的时间才能完成，以便调度程序减少它对其它作业的影响
  JOB_SIGNAL_WAKEUP = $0010;    // 作业根据信号需要唤醒
  JOB_TERMINATED = $0020;       // 作业不需要继续进行，可以结束了
  JOB_GROUPED = $0040;          // 当前作业是作业组的一员
  JOB_ANONPROC = $0080;         // 当前作业过程是匿名函数
  JOB_BY_PLAN = $001000;        // 作业的Interval是一个TPlanMask的掩码值
  JOB_DATA_OWNER = $0F00;       // 作业是Data成员的所有者

  JOB_HANDLE_SIMPLE_MASK = $00;
  JOB_HANDLE_REPEAT_MASK = $01;
  JOB_HANDLE_SIGNAL_MASK = $02;
  JOB_HANDLE_PLAN_MASK   = $03;

  WORKER_ISBUSY = $0001;          // 工作者忙碌
  WORKER_PROCESSLONG = $0002;     // 当前处理的一个长时间作业
  WORKER_COM_INITED = $0004;      // 工作者已初始化为支持COM的状态(仅限Windows)
  WORKER_LOOKUP = $0008;          // 工作者正在查找作业
  WORKER_EXECUTING = $0010;       // 工作者正在执行作业
  WORKER_EXECUTED = $0020;        // 工作者已经完成作业
  WORKER_FIRING = $0040;          // 工作者正在被解雇
  WORKER_RUNNING = $0080;         // 工作者线程已经开始运行
  WORKER_CLEANING = $0100;        // 工作者线程正在清理作业

const
  WAITJOB_TIMEOUT = 15000;      // 工作者等待作业超时时间 (15秒)

const
  WOSecond = 1000;             // 1s
  WOMinute = 60000;            // 60s/1min
  WOHour = 3600000;            // 3600s/60min/1hour
  WODay = Int64(86400000);     // 1天
  
{$IFNDEF UNICODE}
type
  TThreadId = Cardinal;
{$ENDIF}

type
  /// <summary>作业空闲原因，内部使用</summary>
  TWorkerIdleReason = (
    irTimeout,                  // 工作者已经等待超时，可以被释放
    irNoJob                     // 没有需要处理的作业，此时工作者会进行WAITJOB_TIMEOUT释放
                                // 等待状态，如果在WAITJOB_TIMEOUT内有新作业进来，则工作者
                                // 会被唤醒，否则超时后会被释放
  );

type
  /// <summary>作业结束时如何处理Data成员</summary>
  TJobDataFreeType = (
    jdfFreeByUser,              // 用户管理对象的释放
    jdfFreeAsObject,            // 附加的是一个TObject继承的对象，作业完成时会调用FreeObject释放
    jdfFreeAsRecord,            // 附加的是一个Record对象，作业完成时会调用Dispose释放
    jdfFreeAsInterface,         // 附加的是一个接口对象，添加时会增加计数，作业完成时会减少计数
    jdfFreeAsC1,                // 用户自行指定的释放方法1
    jdfFreeAsC2,
    jdfFreeAsC3,
    jdfFreeAsC4,
    jdfFreeAsC5,
    jdfFreeAsC6
  );

type
  TJobBase = class;
  TJobGroup = class;
  TJobExtData = class;
  TForJobs = class;
  TSimpleJobs = class;
  TRepeatJobs = class;
  TYXDWorker = class;
  TYXDWorkers = class;
  {$IFNDEF UNICODE}
  IntPtr = Integer;
  {$ENDIF}
  PJob = ^TJob;

  TJobHandle = NativeUInt;

  // 作业处理回调函数
  TJobProc = procedure(AJob: PJob) of object;
  PJobProc = ^TJobProc;
  TJobProcG = procedure(AJob: PJob);
  {$IFDEF UNICODE}
  TJobProcA = reference to procedure(AJob: PJob);
  {$ENDIF}
  TForJobProc = procedure(ALoopMgr: TForJobs; AJob: PJob; AIndex: NativeInt) of object;
  PForJobProc = ^TForJobProc;
  TForJobProcG = procedure(ALoopMgr: TForJobs; AJob: PJob; AIndex: NativeInt);
  {$IFDEF UNICODE}
  TForJobProcA = reference to procedure(ALoopMgr: TForJobs; AJob: PJob; AIndex: NativeInt);
  {$ENDIF}

  TExtFreeEvent = procedure(var AData: Pointer) of object;
  {$IFDEF UNICODE}
  TExtFreeEventA = reference to procedure(var AData: Pointer);
  {$ENDIF}

  TWorkerWaitParam = record
    WaitType: Byte;
    Data: Pointer;
    case Integer of
      0:
        (Bound: Pointer); // 按对象清除
      1:
        (WorkerProc: TMethod);
      2:
        (SourceJob: PJob);
      3:
        (Group: Pointer);
  end;

  // 信号的内部定义
  PSignal = ^TSignal;
  TSignal = packed record
    Id: Integer;    // 信号的索引
    Fired: Integer; // 信号已触发次数
    Name: string;   // 信号的名称
    First: PJob;    // 首个作业
  end;

  TJobMethod = record
  case Integer of
    0:
      (Proc: {$IFNDEF NEXTGEN}TJobProc{$ELSE}Pointer{$ENDIF});
    1:
      (ProcG: TJobProcG);
    2:
      (ProcA: Pointer);
    3:
      (ForProc: {$IFNDEF NEXTGEN}TForJobProc{$ELSE}Pointer{$ENDIF});
    4:
      (ForProcG: TForJobProcG);
    5:
      (ForProcA: Pointer);
    6:
      (Code: Pointer; Data: Pointer);
  end;

  {$IFNDEF USEINLINE}
  TJobExData = record
    case Integer of
      0:
        (
          SignalId: Integer;  // 信号编码
          Source: PJob;       // 源作业地址
          RefCount: PInteger; // 源数据
        );
      1:
        (
          Interval: Int64;    // 运行时间间隔，单位为1ms，实际精度受不同操作系统限制+8B
          FirstDelay: Int64;  // 首次运行延迟，单位为1ms，默认为0
        );
      2:
        (
          Group: Pointer;     // 分组作业支持
        );
  end;
  {$ENDIF}

  TJob = {$IFNDEF USEINLINE}object{$ELSE}record{$ENDIF}
  private
    function GetAvgTime: Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetElapseTime: Int64; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetValue(Index: Integer): Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure SetValue(Index: Integer; const Value: Boolean); {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsTerminated: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure SetIsTerminated(const Value: Boolean); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure SetFreeType(const Value: TJobDataFreeType); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure AfterRun(AUsedTime: Int64);
    procedure UpdateNextTime;
    function GetFreeType: TJobDataFreeType; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsCustomFree: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsInterfaceOwner: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsObjectOwner: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsRecordOwner: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetExtData: TJobExtData; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetHandle: TJobHandle;

    {$IFNDEF USEINLINE}
    function GetFirstDelay: Int64;
    function GetGroup: Pointer;
    function GetInterval: Int64;
    function GetRefCount: PInteger;
    function GetSignalId: Integer;
    function GetSource: PJob;
    procedure SetFirstDelay(const Value: Int64);
    procedure SetGroup(const Value: Pointer);
    procedure SetInterval(const Value: Int64);
    procedure SetRefCount(const Value: PInteger);
    procedure SetSignalId(const Value: Integer);
    procedure SetSource(const Value: PJob);
    {$ENDIF}
  public
    procedure Create(AProc: TJobProc);
    /// <summary>值拷贝函数</summary>
    /// <remarks>Worker/Next/Source不会复制并会被置空，Owner不会被复制</remarks>
    procedure Assign(const ASource: PJob);
    /// <summary>重置内容，以便为从队列中弹出做准备</summary>
    procedure Reset; {$IFDEF USEINLINE}inline;{$ENDIF}
    /// <summary>公开下线程对象的同步方法，但更推荐投寄异步作业到主线程中处理</summary>
    procedure Synchronize(AMethod: TThreadMethod); {$IFDEF USEINLINE}inline;{$ENDIF}

    {$IFNDEF USEINLINE}
    property SignalId: Integer read GetSignalId write SetSignalId;
    property Source: PJob read GetSource write SetSource;
    property RefCount: PInteger read GetRefCount write SetRefCount;

    property Interval: Int64 read GetInterval write SetInterval;
    property FirstDelay: Int64 read GetFirstDelay write SetFirstDelay;

    property Group: Pointer read GetGroup write SetGroup;
    {$ENDIF}

    /// <summary>平均每次运行时间，单位为1ms</summary>
    property AvgTime: Integer read GetAvgTime;
    /// <summmary>本次已运行时间，单位为1ms</summary>
    property ElapseTime: Int64 read GetElapseTime;
    /// <summary>释放类型</summary>
    property FreeType: TJobDataFreeType read GetFreeType write SetFreeType;

    /// <summary>是否只运行一次，投递作业时自动设置</summary>
    property Runonce: Boolean index JOB_RUN_ONCE read GetValue;
    /// <summary>是否要求在主线程执行作业，实际效果比Windows的PostMessage相似</summary>
    property InMainThread: Boolean index JOB_IN_MAINTHREAD read GetValue;
    /// <summary>是否是一个运行时间比较长的作业，用Workers.LongtimeWork设置</summary>
    property IsLongtimeJob: Boolean index JOB_LONGTIME read GetValue;
    /// <summary>是否是一个信号触发的作业</summary>
    property IsSignalWakeup: Boolean index JOB_SIGNAL_WAKEUP read GetValue;
    /// <summary>是否是分组作业的成员</summary>
    property IsGrouped: Boolean index JOB_GROUPED read GetValue;
    /// <summary>是否要求结束当前作业</summary>
    property IsTerminated: Boolean read GetIsTerminated write SetIsTerminated;
    /// <summary>判断作业是否拥有Data数据成员</summary>
    property IsDataOwner: Boolean index JOB_DATA_OWNER read GetValue;
    /// <summary>判断作业处理过程是否是一个匿名函数</summary>
    property IsAnonWorkerProc: Boolean index JOB_ANONPROC read GetValue write SetValue;
    /// <summary>作业是由一个计划任务触发</summary>
    property IsByPlan: Boolean index JOB_BY_PLAN read GetValue write SetValue;

    /// <summary>判断作业的Data指向的是一个对象且要求作业完成时自动释放</summary>
    property IsObjectOwner: Boolean read GetIsObjectOwner;
    /// <summary>判断作业的Data指向的是一个记录且要求作业完成时自动释放</summary>
    property IsRecordOwner: Boolean read GetIsRecordOwner;
    /// <summary>判断作业的Data指向的是一个接口且要求作业完成时自动释放</summary>
    property IsInterfaceOwner: Boolean read GetIsInterfaceOwner;
    /// <summary>判断作业的Data是否是由用户所指定的方法自动释放</summary>
    property IsCustomFree: Boolean read GetIsCustomFree;
    /// <summary>扩展的作业处理过程数据</summary>
    property ExtData: TJobExtData read GetExtData;
    property Handle: TJobHandle read GetHandle;
  public
    FirstTime: Int64;       // 作业第一次开始时间
    StartTime: Int64;       // 本次作业开始时间,8B
    PushTime: Int64;        // 入队时间
    PopTime: Int64;         // 出队时间
    NextTime: Int64;        // 下一次运行的时间,+8B=16B
    WorkerProc: TJobMethod; // 作业处理函数+8/16B
    Owner: TJobBase;        // 作业所隶属的队列
    Next: PJob;             // 下一个结点
    Worker: TYXDWorker;     // 当前作业工作者
    Runs: Integer;          // 已经运行的次数+4B
    MinUsedTime: Integer;   // 最小运行时间+4B
    TotalUsedTime: Integer; // 运行总计花费的时间，TotalUsedTime/Runs可以得出平均执行时间+4B
    MaxUsedTime: Integer;   // 最大运行时间+4B
    Flags: Integer;         // 作业标志位+4B
    Data: Pointer;          // 附加数据内容
    {$IFNDEF USEINLINE}
    ExData: TJobExData;
    {$ELSE}
    case Integer of
      0:
        (
          SignalId: Integer;  // 信号编码
          Source: PJob;       // 源作业地址
          RefCount: PInteger; // 源数据
        );
      1:
        (
          Interval: Int64;    // 运行时间间隔，单位为1ms，实际精度受不同操作系统限制+8B
          FirstDelay: Int64;  // 首次运行延迟，单位为1ms，默认为0
        );
      2:
        (
          Group: Pointer;     // 分组作业支持
        );
    {$ENDIF}
  end;

  // 作业队列对象的基类，提供基础的接口封装
  TJobBase = class(TObject)
  protected
    FOwner: TYXDWorkers;
    function InternalPush(AJob: PJob): Boolean; virtual; abstract;
    function InternalPop: PJob; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetEmpty: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
  public
    constructor Create(AOwner: TYXDWorkers); virtual;
    destructor Destroy; override;
    // 投寄一个作业 (外部不应尝试直接投寄任务到队列，其由Workers的相应函数内部调用。)
    function Push(AJob: PJob): Boolean; 
    // 弹出一个作业
    function Pop: PJob; {$IFDEF USEINLINE}inline;{$ENDIF}
    // 空所有作业
    procedure Clear; overload; virtual;
    // 清空指定的作业
    function Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer; overload; virtual; abstract;
    // 清空一个对象关联的所有作业
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer; overload; virtual; abstract;
    // 根据句柄清除一个作业对象
    function Clear(AHandle: TJobHandle): Boolean; overload; virtual; abstract;
    /// 不可靠警告：Count和Empty值仅是一个参考，在多线程环境下可能并不保证下一句代码执行时，会一致
    property Empty: Boolean read GetEmpty; // 当前队列是否为空
    property Count: Integer read GetCount; // 当前队列元素数量
  end;

  {$IFDEF WORKER_SIMPLE_LOCK}
  // 一个基于位锁的简单锁定对象，使用原子函数置位
  TSimpleLock = class
  private
    FFlags: Integer;
  public
    constructor Create;
    procedure Enter; inline;
    procedure Leave; inline;
  end;
  {$ELSE}
  TSimpleLock = class(TCriticalSection)
  end;
  {$ENDIF}

  TJobErrorSource = (jesExecute, jesFreeData, jesWaitDone, jesAfterDone);

  TForLoopIndexType = {$IF RTLVersion>=26}NativeInt{$ELSE}Integer{$IFEND};

  /// <summary>
  /// 并行For支持
  /// </summary>
  TForJobs = class
  private
    FOwner: TYXDWorkers;
    FStartIndex, FStopIndex, FIterator: TForLoopIndexType;
    FBreaked: Integer;
    FEvent: TEvent;
    FWorkerCount: Integer;
    FWorkJob: PJob;
    procedure DoJob(AJob: PJob);
    procedure Start;
    function Wait(AMsgWait: Boolean): TWaitResult;
    function GetBreaked: Boolean;
    function GetRuns: Cardinal; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetTotalTime: Cardinal; {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetAvgTime: Cardinal; {$IFDEF USEINLINE}inline;{$ENDIF}
  public
    constructor Create(const AStartIndex, AStopIndex: TForLoopIndexType;
      AData: Pointer; AFreeType: TJobDataFreeType); overload;
    constructor Create(AOwner: TYXDWorkers; const AStartIndex, AStopIndex: TForLoopIndexType;
      AData: Pointer; AFreeType: TJobDataFreeType); overload;
    destructor Destroy; override;
    procedure BreakIt;
    property StartIndex: TForLoopIndexType read FStartIndex;
    property StopIndex: TForLoopIndexType read FStopIndex;
    property Breaked: Boolean read GetBreaked;
    property Runs: Cardinal read GetRuns;
    property TotalTime: Cardinal read GetTotalTime;
    property AvgTime: Cardinal read GetAvgTime;
  end;
  
  /// <summary>
  /// 工作者线程使用单向链表管理，而不是进行排序检索是因为对于工作者数量有限，额外
  /// 的处理反而不会直接最简单的循环直接有效
  /// </summary>
  TYXDWorker = class(TThread)
  private
    FOwner: TYXDWorkers;
    FEvent: TEvent;
    FFlags: Integer;
    FTimeout: Integer;
    FFireDelay: Integer;
    FTerminatingJob: PJob;
    FPending: Boolean; // 已经计划作业
    FProcessed: Cardinal;
    {$IFDEF SAVE_WORDER_TIME}
    FStartTime: Int64;
    FLastExecTime: Int64;
    {$ENDIF}
    FLastActiveTime: Int64;
    function GetValue(Index: Integer): Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure SetValue(Index: Integer; const Value: Boolean); {$IFDEF USEINLINE}inline;{$ENDIF}
    function GetIsIdle: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    function WaitSignal(ATimeout: Integer; AByRepeatJob: Boolean): TWaitResult; {$IFDEF USEINLINE}inline;{$ENDIF}
  protected
    FActiveJob: PJob;
    // 之所以不直接使用FActiveJob的相关方法，是因为保证外部可以线程安全的访问这两个成员
    FActiveJobProc: TJobProc;
    FActiveJobData: Pointer;
    FActiveJobSource: PJob;
    FActiveJobGroup: TJobGroup;
    FActiveJobFlags: Integer;
    procedure Execute; override;
    procedure FireInMainThread;
    procedure DoJob(AJob: PJob);
  public
    constructor Create(AOwner: TYXDWorkers); overload;
    destructor Destroy; override;
    procedure ComNeeded(AInitFlags: Cardinal = 0);
    // 判断COM是否已经初始化为支持COM
    property ComInitialized: Boolean index WORKER_COM_INITED read GetValue;
    // 判断当前是否处于长时间作业处理过程中
    property InLongtimeJob: Boolean index WORKER_PROCESSLONG read GetValue;
    // 判断当前是否空闲
    property IsIdle: Boolean read GetIsIdle;
    // 判断当前是否忙碌
    property IsBusy: Boolean index WORKER_ISBUSY read GetValue;
    property IsLookuping: Boolean index WORKER_LOOKUP read GetValue;
    property IsExecuting: Boolean index WORKER_EXECUTING read GetValue;
    property IsExecuted: Boolean index WORKER_EXECUTED read GetValue;
    property IsFiring: Boolean index WORKER_FIRING read GetValue;
    property IsRunning: Boolean index WORKER_RUNNING read GetValue;
    {$IFDEF SAVE_WORDER_TIME}
    // 工作者出生时间
    property StartTime: Int64 read FStartTime;
    // 工作者最后一次工作时间
    property LastExecTime: Int64 read FLastExecTime;
    {$ENDIF}
  end;

  // 工作者错误通知事件
  TWorkerErrorNotify = procedure(AJob: PJob; E: Exception; ErrSource: TJobErrorSource) of object;
  // 自定义数据释放事件
  TCustomFreeDataEvent = procedure(ASender: TYXDWorkers; AFreeType: TJobDataFreeType; var AData: Pointer) of object;

  PWorkerStateItem = ^TWorkerStateItem;
  TWorkerStateItem = {$IFNDEF USEINLINE}object{$ELSE}packed record{$ENDIF}
  private
    Flags: Integer;               // 状态标志
    function GetValue(Index: Integer): Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
  public
    Handle: Cardinal;             // 线程句柄
    ActiveJobFlags: Integer;      // 正在执行的作业标志位
    ActiveJobElapseTime: Int64;   // 正在执行的作业已经运行的时间 (ms)
    ActiveJobProc: TJobProc;      // 正在执行的作业过程
    TerminatingJobProc: TJobProc; // 正在中止的作业过程
    {$IFDEF SAVE_WORDER_TIME}
    StartTime: Int64;
    LastExecTime: Int64;
    {$ENDIF}
    // 判断COM是否已经初始化为支持COM
    property ComInitialized: Boolean index WORKER_COM_INITED read GetValue;
    // 判断当前是否处于长时间作业处理过程中
    property InLongtimeJob: Boolean index WORKER_PROCESSLONG read GetValue;
    // 判断当前是否忙碌
    property IsBusy: Boolean index WORKER_ISBUSY read GetValue;
    property IsLookuping: Boolean index WORKER_LOOKUP read GetValue;
    property IsExecuting: Boolean index WORKER_EXECUTING read GetValue;
    property IsExecuted: Boolean index WORKER_EXECUTED read GetValue;
    property IsFiring: Boolean index WORKER_FIRING read GetValue;
    property IsRunning: Boolean index WORKER_RUNNING read GetValue;
  end;
  TWorkerStateList = array of TWorkerStateItem;

  /// <summary>
  /// 工作者管理对象，用来管理工作者和作业
  /// </summary>
  TYXDWorkers = class(TObject)
  private
    FWorkers: array of TYXDWorker;
    FWorkerCount: Integer;
    FDisableCount: Integer;
    FBusyCount: Integer;
    FFiringWorkerCount: Integer;
    FMinWorkers: Integer;
    FMaxWorkers: Integer;
    FMaxSignalId: Integer;
    FLongTimeWorkers: Integer;    // 记录下长时间作业中的工作者，这种任务长时间不释放资源，可能会造成其它任务无法及时响应
    FMaxLongtimeWorkers: Integer; // 允许最多同时执行的长时间任务数，不允许超过MaxWorkers的一半
    FFireTimeout: Integer;
    FTerminating: Boolean;
    FCPUNum: Integer;
    FLocker: TCriticalSection;
    FSimpleJobs: TSimpleJobs;
    FRepeatJobs: TRepeatJobs;
    FSignalJobs: TYXDHashTable;

    FStaticThread: TThread;
    
    FOnError: TWorkerErrorNotify;
    FOnCustomFreeData: TCustomFreeDataEvent;
    {$IFDEF MSWINDOWS}
    FMainWorker: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
    {$ENDIF}
    function GetEnabled: Boolean;
    function PostWaitJob(AJob: PJob; ASignalId: Integer): TJobHandle;
    function ClearSignalJobs(ASource: PJob): Integer;
    function ClearJobs(AObject: Pointer; AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer;
    function ClearWaitJobs(ASignalId: Integer; const ASignalName: string): Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetMaxLongtimeWorkers(const Value: Integer);
    procedure SetMaxWorkers(const Value: Integer);
    procedure SetMinWorkers(const Value: Integer);
    procedure EnableWorkers;
    procedure DisableWorkers;
    procedure ClearWorkers();
    procedure FreeJob(AJob: PJob);
    procedure FreeJobData(AData: Pointer; AFreeType: TJobDataFreeType);
    procedure SetFireTimeout(const Value: Integer);
    function GetSimpleJobCount(): Integer;
    function GetRepeatJobCount(): Integer;
    function GetSignalJobCount(): Integer;
    function GetIdleWorkerCount: Integer;
  protected
    function Popup: PJob;
    function Post(AJob: PJob): TJobHandle; overload;
    //function LookupIdleWorker: Boolean;
    function LookupIdleWorker(AFromSimple: Boolean = True): Boolean;
    function SignalIdByName(const AName: string): Integer;
    procedure SignalWorkDone(AJob: PJob; AUsedTime: Int64);
    procedure WorkerTerminate(AWorker: TYXDWorker);
    procedure WaitRunningDone(const AParam: TWorkerWaitParam);
    procedure FireSignalJob(ASignal: PSignal; AData: Pointer; AFreeType: TJobDataFreeType);
    procedure DoJobFree(ATable: TObject; AHash: THashType; AData: Pointer);
    procedure DoCustomFreeData(AFreeType: TJobDataFreeType; var AData: Pointer);
    procedure NewWorkerNeeded;
    procedure WorkerTimeout(AWorker: TYXDWorker); {$IFDEF USEINLINE}inline;{$ENDIF}
    function CreateWorker(ASuspended: Boolean): TYXDWorker;
    function IsAutoFreeType(AJob: PJob): Boolean;
  public
    constructor Create(AMinWorkers: Integer = 2); overload;
    destructor Destroy; override;

    // 获取工作者状态信息
    function GetWorkerState: TWorkerStateList;

    // 获取Job池大小
    class function JobPoolCount(): Integer;
    // 获取实例
    class function GetInstance: TYXDWorkers;
    
    // 获取一个作业执行过程的描述信息
    {$IFDEF Use_DebugHelper}
    class function GetJobPorcInfo(const AJobProc: TJobMethod): string; overload;
    class function GetJobPorcInfo(const AJobProc: TJobProc): string; overload;
    {$ENDIF}
    
    // 清除所有作业
    procedure Clear; overload;
    /// <summary>清除一个对象相关的所有作业</summary>
    /// <param name="AObject">要释放的作业处理过程关联对象</param>
    /// <param name="AMaxTimes">最多清除的数量，如果<0，则全清</param>
    /// <returns>返回实际清除的作业数量</returns>
    /// <remarks>一个对象如果计划了作业，则在自己释放前应调用本函数以清除关联的作业，
    /// 否则，未完成的作业可能会触发异常。</remarks>
    function Clear(AObject: Pointer; AMaxTimes: Integer = -1): Integer; overload;
    /// <summary>清除所有投寄的指定过程作业</summary>
    /// <param name="AProc">要清除的作业执行过程</param>
    /// <param name="AData">要清除的作业附加数据指针地址，如果值为nil，
    /// 则清除所有的相关过程，否则，只清除附加数据地址一致的过程</param>
    /// <param name="AMaxTimes">最多清除的数量，如果<0，则全清</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer = -1): Integer; overload;
    /// <summary>清除指定信号关联的所有作业</summary>
    /// <param name="ASingalId">要清除的信号名称</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(const ASignalName: string): Integer; overload;
    /// <summary>清除指定信号关联的所有作业</summary>
    /// <param name="ASingalId">要清除的信号ID</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(ASignalId: Integer): Integer; overload;
    /// <summary>清除指定句柄对应的作业</summary>
    /// <param name="AHandle">要清除的作业句柄</param>
    procedure Clear(AHandle: TJobHandle); overload;

    /// <summary>投寄一个作业</summary>
    /// <param name="AJobProc">要定时执行的作业过程</param>
    /// <param name="ADelay">第一次执行前延迟时间，小于等于0则立即执行</param>
    /// <param name="AInterval">后续重复作业间隔，如果小于等于0，则作业只执行一次</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function Post(AJobProc: TJobProc; AData: Pointer; ARunInMainThread: Boolean = False;
      const ADelay: Int64 = 0; const AInterval: Int64 = 0; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    function Post(AJobProc: TJobProcG; AData: Pointer; ARunInMainThread: Boolean = False;
      const ADelay: Int64 = 0; const AInterval: Int64 = 0; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$IFDEF UNICODE}
    function Post(AJobProc: TJobProcA; AData: Pointer; ARunInMainThread: Boolean = False;
      const ADelay: Int64 = 0; const AInterval: Int64 = 0; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$ENDIF}

    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AJobProc">要定时执行的作业过程</param>
    /// <param name="ATime">执行时间，只要时间部分，日期忽略</param>
    /// <param name="AInterval">后续重复作业间隔，如果小于等于0，则作业只执行一次</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function Post(AJobProc: TJobProc; AData: Pointer; const ATime: TDateTime; const AInterval: Int64 = 0;
      ARunInMainThread: Boolean = False; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    function Post(AJobProc: TJobProcG; AData: Pointer; const ATime: TDateTime; const AInterval: Int64 = 0;
      ARunInMainThread: Boolean = False; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$IFDEF UNICODE}
    function Post(AJobProc: TJobProcA; AData: Pointer; const ATime: TDateTime; const AInterval: Int64 = 0;
      ARunInMainThread: Boolean = False; AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$ENDIF}

    /// <summary>投寄一个后台长时间执行的作业</summary>
    /// <param name="AJobProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    /// <remarks>长时间作业强制在后台线程中执行，而不允许投递到主线程中执行</remarks>
    function PostLongJob(AJobProc: TJobProc; AData: Pointer;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    function PostLongJob(AJobProc: TJobProcG; AData: Pointer;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$IFDEF UNICODE}
    function PostLongJob(AJobProc: TJobProcA; AData: Pointer;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TJobHandle; overload;
    {$ENDIF}

    /// <summary>投寄一个等待信号才开始的作业</summary>
    /// <param name="AJobProc">要执行的作业过程</param>
    /// <param name="ASignalId">等待的信号编码，该编码由RegisterSignal函数返回</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function PostWait(AJobProc: TJobProc; ASignalId: Integer;
      ARunInMainThread: Boolean = False): TJobHandle; overload;
    function PostWait(AJobProc: TJobProcG; ASignalId: Integer;
      ARunInMainThread: Boolean = False): TJobHandle; overload;
    {$IFDEF UNICODE}
    function PostWait(AJobProc: TJobProcA; ASignalId: Integer;
      ARunInMainThread: Boolean = False): TJobHandle; overload;
    {$ENDIF}

    /// <summary>触发一个信号</summary>
    /// <param name="AId">信号编码，由RegisterSignal返回</param>
    /// <param name="AData">附加给作业的用户数据指针地址</param>
    /// <remarks>触发一个信号后，Workers会触发所有已注册的信号关联处理过程的执行</remarks>
    procedure SendSignal(AId: Integer; AData: Pointer = nil; AFreeType: TJobDataFreeType = jdfFreeByUser); overload;
    /// <summary>按名称触发一个信号</summary>
    /// <param name="AName">信号名称</param>
    /// <param name="AData">附加给作业的用户数据指针地址</param>
    /// <remarks>触发一个信号后，Workers会触发所有已注册的信号关联处理过程的执行</remarks>
    procedure SendSignal(const AName: string; AData: Pointer = nil; AFreeType: TJobDataFreeType = jdfFreeByUser); overload;

    /// <summary>注册一个信号</summary>
    /// <param name="AName">信号名称</param>
    /// <remarks>
    /// 1.重复注册同一名称的信号将返回同一个编码
    /// 2.信号一旦注册，则只有程序退出时才会自动释放
    /// </remarks>
    function RegisterSignal(const AName: string): Integer;

    /// <summary>
    /// 并行For运算 (注意 AWorkerProc 与普通 Post 作业不同)
    /// </summary>
    {$IFDEF USEINLINE}
    function &For(const AStartIndex, AStopIndex: TForLoopIndexType;
      AWorkerProc: TForJobProc; AData: Pointer = nil; AMsgWait: Boolean = False;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TWaitResult; overload;
    {$ENDIF}
    {$IFDEF UNICODE}
    function &For(const AStartIndex, AStopIndex: TForLoopIndexType;
      AWorkerProc: TForJobProcA; AData: Pointer = nil; AMsgWait: Boolean = False;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TWaitResult; overload;
    {$ENDIF}
    {$IFDEF USEINLINE}
    function &For(const AStartIndex, AStopIndex: TForLoopIndexType;
      AWorkerProc: TForJobProcG; AData: Pointer = nil; AMsgWait: Boolean = False;
      AFreeType: TJobDataFreeType = jdfFreeByUser): TWaitResult; overload;
    {$ENDIF}


    // 最大允许工作者数量，不能小于2
    property MaxWorkers: Integer read FMaxWorkers write SetMaxWorkers;
    // 最小工作者数量，不能小于2
    property MinWorkers: Integer read FMinWorkers write SetMinWorkers;
    // 大允许的长时间作业工作者数量，等价于允许开始的长时间作业数量
    property MaxLongtimeWorkers: Integer read FMaxLongtimeWorkers write SetMaxLongtimeWorkers;
    // 是否允许开始作业，如果为false，则投寄的作业都不会被执行，直到恢复为True
    // (Enabled为False时已经运行的作业将仍然运行，它只影响尚未执行的作来)
    property Enabled: Boolean read GetEnabled write SetEnabled;
    // 是否正在释放TQWorkers对象自身
    property Terminating: Boolean read FTerminating;
    // 当前系统CPU数量
    property CPUNum: Integer read FCPUNum;
    // 繁忙的工作者数量
    property BusyWorkerCount: Integer read FBusyCount;
    // 当前空闲工作者数量
    property IdleWorkerCount: Integer read GetIdleWorkerCount;
    // 当前工作者数量
    property WorkerCount: Integer read FWorkerCount;

    property SimpleJobCount: Integer read GetSimpleJobCount;
    property RepeatJobCount: Integer read GetRepeatJobCount;
    property SignalJobCount: Integer read GetSignalJobCount;

    // 默认解雇工作者的超时时间
    property FireTimeout: Integer read FFireTimeout write SetFireTimeout default WAITJOB_TIMEOUT;
    // 工作者错误回调通知事件
    property OnError: TWorkerErrorNotify read FOnError write FOnError;
    // 用户指定的作业的Data对象释放方式
    property OnCustomFreeData: TCustomFreeDataEvent read FOnCustomFreeData write FOnCustomFreeData;
  end;

  /// <summary>
  /// 用于管理简单的异步调用，没有触发时间要求的作业
  /// </summary>
  TSimpleJobs = class(TJobBase)
  private
    FFirst, FLast: PJob;
    FCount: Integer;
    FLocker: TSimpleLock;
    function ClearJobs(AObject: Pointer; AProc: TJobProc; AData: Pointer;
      AMaxTimes: Integer; AHandle: TJobHandle = 0): Integer;
  protected
    function InternalPush(AJob: PJob): Boolean; override;
    function InternalPop: PJob; override;
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TYXDWorkers); override;
    destructor Destroy; override;
    procedure Clear; overload; override;
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer; overload; override;
    function Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer; overload; override;
    function Clear(AHandle: TJobHandle): Boolean; overload; override;
  end;

  /// <summary>
  /// 用于管理计划型任务，需要在指定的时间点触发
  /// </summary>
  TRepeatJobs = class(TJobBase)
  private
    FLocker: TCriticalSection;
    FFirstFireTime: Int64;
    procedure SetFirstFireTime(Value: Int64); {$IFDEF USEINLINE}inline;{$ENDIF}
    function ClearJobs(AObject: Pointer; AProc: TJobProc; AData: Pointer;
      AMaxTimes: Integer; AHandle: TJobHandle = 0): Integer;
    procedure AfterJobRun(AJob: PJob; AUsedTime: Int64);
  protected
    FItems: TRBTree;
    function InternalPush(AJob: PJob): Boolean; override;
    function InternalPop: PJob; override;
    function DoTimeCompare(P1, P2: Pointer): Integer;
    procedure DoJobDelete(ATree: TRBTree; ANode: PRBNode);
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TYXDWorkers); override;
    destructor Destroy; override;
    procedure Clear; override;
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer; overload; override;
    function Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer; overload; override;
    function Clear(AHandle: TJobHandle): Boolean; overload; override;
  end;
  
  {$IFDEF UNICODE}
  TJobItemList = TList<PJob>;
  {$ELSE}
  TJobItemList = TList;
  {$ENDIF}

  /// <summary>
  /// 作业组，放在一起顺序执行或乱序执行，可以使用 WaitFor 等待全部完成
  /// </summary>
  TJobGroup = class(TObject)
  private
    FOwner: TYXDWorkers;
    FLocker: TSimpleLock;
    FEvent: TEvent;           // 事件，用于等待作业完成
    FByOrder: Boolean;
    FWaitResult: TWaitResult;
    FAfterDone: TNotifyEvent; // 作业完成事件通知
    FTimeoutCheck: Boolean;   // 是否检查作业超时
    FTag: Pointer;
    function GetCount: Integer;
  protected
    FItems: TJobItemList;     // 作业列表
    FRuns: Integer;           // 已经运行的数量
    FPosted: Integer;         // 已经提交执行的数量
    FCanceled: Integer;       // 已经取消的作业数量
    FPrepareCount: Integer;   // 准备计数
    procedure DoJobExecuted(AJob: PJob);
    procedure DoJobsTimeout(AJob: PJob);
    procedure DoAfterDone;
  public
    constructor Create(AByOrder: Boolean = False); overload;
    constructor Create(AOwner: TYXDWorkers; AByOrder: Boolean = False); overload;
    destructor Destroy; override;
    // 取消未完成的作业
    procedure Cancel(AWaitRunningDone: Boolean = True);
    // 准备添加作业，实际增加内部计数器
    procedure Prepare;
    // 减少内部计数器，如果计数器减为0，则开始实际执行作业
    procedure Run(ATimeout: Cardinal = INFINITE);
    // 添加一个作业过程，如果准备内部计数器为0，则直接执行，否则只添加到列表
    function Add(AProc: TJobProc; AData: Pointer; AInMainThread: Boolean = False;
      AFreeType: TJobDataFreeType = jdfFreeByUser): Boolean;
    // 等待作业完成，ATimeout为最长等待时间
    function WaitFor(ATimeout: Cardinal = INFINITE): TWaitResult;
    // 等待作业完成，ATimeout为最长等待时间，不同的是MsgWaitFor不阻塞消息处理
    function MsgWaitFor(ATimeout: Cardinal = INFINITE): TWaitResult;
    // 未完成的作业数量
    property Count: Integer read GetCount;
    // 全部作业执行完成时触发的回调事件
    property AfterDone: TNotifyEvent read FAfterDone write FAfterDone;
    // 是否是按顺序执行(即必需等待上一个作业完成后才执行下一个)
    property ByOrder: Boolean read FByOrder;
    // 已执行完成的作业数量
    property Runs: Integer read FRuns;
    property Tag: Pointer read FTag write FTag;
  end;

  /// <summary>
  /// 扩展作业数据
  /// </summary>
  TJobExtData = class
  private
    function GetAsBoolean: Boolean;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: string;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);
  protected
    FData: Pointer;
    FOnFree: TExtFreeEvent;
    {$IFDEF UNICODE}
    FOnFreeA: TExtFreeEventA;
    {$ENDIF}
    procedure DoFreeAsString(var AData: Pointer);
    {$IFNDEF NEXTGEN}{$IFDEF UNICODE}
    procedure DoFreeAsAnsiString(var AData: Pointer);
    {$ENDIF}{$ENDIF}
    {$IFNDEF UNICODE}
    procedure DoFreeAsWideString(var AData: Pointer);
    {$ENDIF}
    procedure DoSimpleTypeFree(var AData: Pointer);
    {$IFDEF AutoFreeJobExData}
    procedure DoAddToExDataMap;
    {$ENDIF}
  public
    constructor Create(AData: Pointer; AOnFree: TExtFreeEvent); overload;
    constructor Create(AOnFree: TExtFreeEvent); overload;
    {$IFDEF UNICODE}
    constructor Create(AData: Pointer; AOnFree: TExtFreeEventA); overload;
    constructor Create(AOnFree: TExtFreeEventA); overload;
    {$ENDIF}
    constructor Create(const Value: Int64); overload;
    constructor Create(const Value: Integer); overload;
    constructor Create(const Value: Boolean); overload;
    constructor Create(const Value: Double); overload;
    constructor CreateAsDateTime(const Value: TDateTime); overload;
    constructor Create(const Value: string); overload;
    {$IFNDEF UNICODE}
    constructor Create(const Value: WideString); overload;
    {$ENDIF}
    {$IFNDEF NEXTGEN}{$IFDEF UNICODE}
    constructor Create(const Value: AnsiString); overload;
    {$ENDIF}{$ENDIF}
    destructor Destroy; override;

    function SetFreeEvent(AOnFree: TExtFreeEvent): TJobExtData; overload;
    {$IFDEF UNICODE}
    function SetFreeEvent(AOnFree: TExtFreeEventA): TJobExtData; overload;
    {$ENDIF}

    property Data: Pointer read FData;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Double read GetAsDouble write SetAsDouble;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

var
  Workers: TYXDWorkers = nil;  // 需要时初始化，也可以自己定义，允许多个

// 返回值的时间精度为1ms
function GetTimestamp: Int64;
// 获取CPU数量
function GetCPUCount: Integer;
// 将全局的作业处理函数转换为TJobProc类型，以便正常调度使用
function MakeJobProc(const AProc: TJobProcG): TJobProc; {$IFDEF USEINLINE}inline;{$ENDIF}
// 设置线程运行的CPU
procedure SetThreadCPU(AHandle: THandle; ACpuNo: Integer); {$IFDEF USEINLINE}inline;{$ENDIF}
{$IFDEF USE_ATOMIC}
{$IF RTLVersion<26}
// 为与D2007兼容, 原子操作函数
function AtomicCmpExchange(var Target: Integer; Value, Comparand: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
function AtomicExchange(var Target: Integer; Value: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
function AtomicIncrement(var Target: Integer; const Value: Integer = 1): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
function AtomicDecrement(var Target: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
{$IFEND}
{$IFDEF WORKER_SIMPLE_LOCK}
// 原子操作函数
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
{$ENDIF}
{$ENDIF}

function ToJobProc(const AMethod: TJobMethod): TJobProc; {$IFDEF USEINLINE}inline;{$ENDIF}

// 创建一个扩展数据作为作业的参数
function NewExData(const Value: string): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
{$IFNDEF UNICODE}
function NewExData(const Value: WideString): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
{$ENDIF}
{$IFNDEF NEXTGEN}{$IFDEF UNICODE}
function NewExData(const Value: AnsiString): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
{$ENDIF}{$ENDIF}
function NewExData(const Value: Int64): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
function NewExData(const Value: Cardinal): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
function NewExData(const Value: Double): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
function NewExData(const Value: Boolean): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
function NewExData(const Value: Byte): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
function NewExDataAsTime(const Value: TDateTime): TJobExtData; overload; {$IFDEF USEINLINE}inline;{$ENDIF}
// 消息等待，直到事件被触发
function MsgWaitForEvent(AEvent: TEvent; ATimeout: Cardinal): TWaitResult;
// 检测一个线程是否存在
function ThreadExists(AThreadId: TThreadId; AProcessId: DWORD = 0): Boolean;

implementation

resourcestring
  SNotSupportNow = '当前尚未支持功能 %s';
  SNotInitWorkers = '当前尚未初始化有工作者管理对象 TYXDWorkers';
  STooFewWorkers = '指定的最小工作者数量太少(必需大于等于1)。';
  STooManyLongtimeWorker = '不能允许太多长时间作业线程(最多允许工作者一半)。';
  SBadWaitDoneParam = '未知的等待正在执行作业完成方式:%d';
  SUnsupportPlatform = '%s 当前在本平台不受支持。';

{$IFNDEF UNICODE}
const
  wrIOCompletion = TWaitResult(4);
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TGetTickCount64 = function: Int64;
  TGetSystemTimes = function(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
{$ENDIF MSWINDOWS}
type
  TJobPool = class
  protected
    FFirst: PJob;
    FCount: Integer;
    FSize: Integer;
    FLocker: TSimpleLock;
  public
    constructor Create(AMaxSize: Integer);
    destructor Destroy; override;
    procedure Push(AJob: PJob);
    function Pop: PJob;
    property Count: Integer read FCount;
    property Size: Integer read FSize write FSize;
  end;     

type
  {$IF RTLVersion<24}
  TSystemTimes = record
    IdleTime, UserTime, KernelTime, NiceTime: UInt64;
  end;
  {$IFEND <XE5}
  TStaticThread = class(TThread)
  protected
    FOwner: TYXDWorkers;
    FEvent: TEvent;
    FLastTimes: {$IF RTLVersion>=25}TThread.{$IFEND >=XE5}TSystemTimes;
    procedure Execute; override;
  public
    constructor Create(AOwner: TYXDWorkers; CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    procedure CheckNeeded;
  end;

var
  JobPool: TJobPool;
  {$IFDEF AutoFreeJobExData}
  ExDataMap: TIntHash;
  {$ENDIF}
  _CPUCount: Integer;
  {$IFDEF NEXTGEN}
  _Watch: TStopWatch;
  {$ELSE}
  GetTickCount64: TGetTickCount64;
  WinGetSystemTimes: TGetSystemTimes;
  _StartCounter: Int64;
  _PerfFreq: Int64;
  {$ENDIF}
  {$IFDEF __BORLANDC}
  procedure FreeAsCDelete(AData: Pointer); external;
  procedure FreeAsCDeleteArray(AData: Pointer); external;
  {$ENDIF}

function GetTimestamp: Int64;
begin
  {$IFDEF MSWINDOWS}
  if _PerfFreq > 0 then begin
    QueryPerformanceCounter(Result);
    Result := Trunc((Result - _StartCounter) / _PerfFreq * 1000);
  end else if Assigned(GetTickCount64) then
    Result := (GetTickCount64 - _StartCounter)
  else
    Result := (GetTickCount - _StartCounter)
  {$ELSE}
  Result := _Watch.Elapsed.Ticks div 10000;
  {$ENDIF}
end; 

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
  if _CPUCount = 0 then begin
  {$IFDEF MSWINDOWS}
    GetSystemInfo(si);
    Result := si.dwNumberOfProcessors;
  {$ELSE}// Linux,MacOS,iOS,Andriod{POSIX}
  {$IFDEF POSIX}
    Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSE}// 不认识的操作系统，CPU数默认为1
    Result := 1;
  {$ENDIF !POSIX}
  {$ENDIF !MSWINDOWS}
  end else
    Result := _CPUCount;
end;

function MakeJobProc(const AProc: TJobProcG): TJobProc;
begin
  TMethod(Result).Data := nil;
  TMethod(Result).Code := @AProc;
end;

function SameWorkerProc(const P1: TJobMethod; const P2: TJobProc): Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  Result := (P1.Code = TMethod(P2).Code) and (P1.Data = TMethod(P2).Data);
end;

procedure SetThreadCPU(AHandle: THandle; ACpuNo: Integer);
begin
  {$IFDEF MSWINDOWS}
  SetThreadIdealProcessor(AHandle, ACpuNo);
  {$ELSE}
  // Linux/Andriod/iOS暂时忽略,XE6未引入sched_setaffinity定义
  {$ENDIF}
end;

{$IFDEF USE_ATOMIC}
// 兼容2007版的原子操作接口
{$IF RTLVersion<26}
function AtomicCmpExchange(var Target: Integer; Value: Integer; Comparand: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
{$IFDEF USEINLINE}
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
var
  LTarget: Pointer absolute Target;
  LValue: Pointer absolute Value;
  LComparand: Pointer absolute Comparand;
begin
  Result := Integer(InterlockedCompareExchange(LTarget, LValue, LComparand));
{$ENDIF}
end;

function AtomicIncrement(var Target: Integer; const Value: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if Value = 1 then
    Result := InterlockedIncrement(Target)
  else if Value = -1 then
    Result := InterlockedDecrement(Target)
  else
    Result := InterlockedExchangeAdd(Target, Value);
  {$ELSE}
  if Value = 1 then
    Result := TInterlocked.Increment(Target)
  else if Value = -1 then
    Result := TInterlocked.Decrement(Target)
  else
    Result := TInterlocked.Add(Target, Value);
  {$ENDIF}
end;

function AtomicDecrement(var Target: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  Result := InterlockedDecrement(Target);
end;

function AtomicExchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := InterlockedExchange(Target, Value);
end;
{$IFEND <XE5}

{$IFDEF WORKER_SIMPLE_LOCK}
// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result and AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result or AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;
{$ENDIF}
{$ENDIF}

procedure ThreadYield; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  try
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
  except end;
end;

function ThreadExists(AThreadId: TThreadId; AProcessId: DWORD): Boolean;
  {$IFDEF MSWINDOWS}
  function WinThreadExists: Boolean;
  var
    ASnapshot: THandle;
    AEntry: TThreadEntry32;
  begin
    Result := False;
    ASnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if ASnapshot = INVALID_HANDLE_VALUE then
      Exit;
    try
      AEntry.dwSize := SizeOf(TThreadEntry32);
      if Thread32First(ASnapshot, AEntry) then begin
        if AProcessId = 0 then
          AProcessId := GetCurrentProcessId;
        repeat
          if ((AEntry.th32OwnerProcessID = AProcessId) or
            (AProcessId = $FFFFFFFF)) and (AEntry.th32ThreadID = AThreadId) then
          begin
            Result := True;
            Break;
          end;
        until not Thread32Next(ASnapshot, AEntry);
      end;
    finally
      CloseHandle(ASnapshot);
    end;
  end;
  {$ENDIF}
  {$IFDEF POSIX}
  // Linux的进程与其线程之间的关系存在于/proc/进程ID/task目录下，每一个为一个线程
  function IsChildThread: Boolean;
  var
    sr: TSearchRec;
    AId: Integer;
  begin
    Result := False;
    if FindFirst('/proc/' + IntToStr(AProcessId) + '/task/*', faAnyFile, sr) = 0 then begin
      try
        repeat
          if TryStrToInt(sr.Name, AId) then begin
            if TThreadId(AId) = AThreadId then
              Result := True;
          end;
        until FindNext(sr) <> 0;
      finally
        FindClose(sr);
      end;
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF POSIX}
  Result := pthread_kill(pthread_t(AThreadId), 0) = 0;
  if Result and (AProcessId <> $FFFFFFFF) then begin
    /// 目前未找到合适的方法得到真正的线程ID，pthread_self得到的是一个指针
    { if AProcessId = 0 then
      AProcessId := getpid;
      Result :=IsChildThread; }
  end;
  {$ELSE}
  Result := WinThreadExists;
  {$ENDIF}
end;

procedure ProcessAppMessage;
{$IFDEF MSWINDOWS}
var
  AMsg: MSG;
{$ENDIF}
begin
  FillChar(AMsg, SizeOf(AMsg), 0);
  {$IFDEF MSWINDOWS}
  while PeekMessage(AMsg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(AMsg);
    DispatchMessage(AMsg);
  end;
  {$ELSE}
  Application.ProcessMessages;
  {$ENDIF}
end;

function MsgWaitForEvent(AEvent: TEvent; ATimeout: Cardinal): TWaitResult;
var
  T: Cardinal;
{$IFDEF MSWINDOWS}
  AHandles: array [0 .. 0] of THandle;
  rc: DWORD;
{$ENDIF}
begin
  if GetCurrentThreadId <> MainThreadId then
    Result := AEvent.WaitFor(ATimeout)
  else begin
    {$IFDEF MSWINDOWS}
    Result := wrTimeout;
    AHandles[0] := AEvent.Handle;
    repeat
      T := GetTickCount;
      rc := MsgWaitForMultipleObjects(1, AHandles[0], False, ATimeout, QS_ALLINPUT);
      if rc = WAIT_OBJECT_0 + 1 then begin
        ProcessAppMessage;
        T := GetTickCount - T;
        if ATimeout > T then
          Dec(ATimeout, T)
        else begin
          Result := wrTimeout;
          Break;
        end;
      end else begin
        case rc of
          WAIT_ABANDONED:
            Result := wrAbandoned;
          WAIT_OBJECT_0:
            Result := wrSignaled;
          WAIT_TIMEOUT:
            Result := wrTimeout;
          WAIT_FAILED:
            Result := wrError;
          WAIT_IO_COMPLETION:
            Result := wrIOCompletion;
        end;
        Break;
      end;
    until False;
    {$ELSE}
    repeat
      // 每隔10毫秒检查一下是否有消息需要处理，有则处理，无则进入下一个等待
      T := GetTimestamp;
      ProcessAppMessage;
      Result := AEvent.WaitFor(10);
      if Result = wrTimeout then begin
        T := (GetTimestamp - T) div 10;
        if ATimeout > T then
          Dec(ATimeout, T)
        else
          Break;
      end else
        Break;
    until False;
    {$ENDIF}
  end;
end;

{ TJobPool }

constructor TJobPool.Create(AMaxSize: Integer);
begin
  FCount := 0;
  FSize := AMaxSize;
  FLocker := TSimpleLock.Create;
end;

destructor TJobPool.Destroy;
var
  AJob: PJob;
begin
  FLocker.Enter;
  try
    while FFirst <> nil do begin
      AJob := FFirst.Next;
      Dispose(FFirst);
      FFirst := AJob;
    end;
  finally
    FLocker.Free;
  end;
  inherited;
end;

function TJobPool.Pop: PJob;
begin
  FLocker.Enter;
  Result := FFirst;
  if Result <> nil then begin
    FFirst := Result.Next;
    Dec(FCount);
  end;
  FLocker.Leave;
  if Result = nil then
    GetMem(Result, SizeOf(TJob));
  Result.Reset;
end;

procedure TJobPool.Push(AJob: PJob);
var
  ADoFree: Boolean;
begin
  {$IFDEF UNICODE}
  if AJob.IsAnonWorkerProc then
    TJobProcA(AJob.WorkerProc.ProcA) := nil{$IFNDEF NEXTGEN}; {$ENDIF}
  {$ENDIF}{$IFDEF NEXTGEN}
  else
    PJobProc(@AJob.WorkerProc)^ := nil;
  {$ENDIF} 
  FLocker.Enter;
  ADoFree := (FCount = FSize);
  if not ADoFree then begin
    AJob.Next := FFirst;
    FFirst := AJob;
    Inc(FCount);
  end;
  FLocker.Leave;
  if ADoFree then
    FreeMem(AJob);
end;


{ TStaticThread }

procedure TStaticThread.CheckNeeded;
begin
  if Assigned(Self) then FEvent.SetEvent;
end;

constructor TStaticThread.Create(AOwner: TYXDWorkers; CreateSuspended: Boolean);
begin
  FOwner := AOwner;
  FEvent := TEvent.Create(nil, False, False, '');
  inherited Create(CreateSuspended);
  {$IFDEF MSWINDOWS}
  Priority := tpIdle;
  {$ENDIF}
end;

destructor TStaticThread.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

procedure TStaticThread.Execute;
var
  ATimeout: Cardinal;

  // 计算末1秒的CPU占用率，如果低于60%且有未处理的作业，则启动更多的工作者来完成作业
  function LastCpuUsage: Integer;
  {$IFDEF MSWINDOWS}
  var
    CurSystemTimes: TSystemTimes;
    Usage, Idle: UInt64;
  {$ENDIF}
  begin
    {$IFDEF MSWINDOWS}
    Result := 0;
    if WinGetSystemTimes(PFileTime(@CurSystemTimes.IdleTime)^,
      PFileTime(@CurSystemTimes.KernelTime)^, PFileTime(@CurSystemTimes.UserTime)^)
    then begin
      Usage := (CurSystemTimes.UserTime - FLastTimes.UserTime) +
        (CurSystemTimes.KernelTime - FLastTimes.KernelTime) +
        (CurSystemTimes.NiceTime - FLastTimes.NiceTime);
      Idle := CurSystemTimes.IdleTime - FLastTimes.IdleTime;
      if Usage > Idle then
        Result := (Usage - Idle) * 100 div Usage;
      FLastTimes := CurSystemTimes;
    end;
    {$ELSE}
    Result := TThread.GetCPUUsage(FLastTimes);
    {$ENDIF}
  end;

begin
  {$IFDEF MSWINDOWS}
  {$IF RTLVersion>=21}
  NameThreadForDebugging('StaticThread');
  {$IFEND >=2010}
  if Assigned(WinGetSystemTimes) then // Win2000/XP<SP2该函数未定义，不能使用
    ATimeout := 1000
  else
    ATimeout := INFINITE;
  {$ELSE}
  ATimeout := 1000;
  {$ENDIF}
  try
    while not Terminated do begin
      case FEvent.WaitFor(ATimeout) of
        wrSignaled:
          if Assigned(FOwner) and (not FOwner.Terminating) and (FOwner.IdleWorkerCount = 0) then
            FOwner.LookupIdleWorker(False);
        wrTimeout:
          if Assigned(FOwner) and (not FOwner.Terminating) and (Assigned(FOwner.FSimpleJobs)) and
            (FOwner.FSimpleJobs.Count > 0) and (LastCpuUsage < 60) and
            (FOwner.IdleWorkerCount = 0) then
            FOwner.LookupIdleWorker;
      end;
    end;
  finally
    FOwner.FStaticThread := nil;
  end;
end;

{ TJob }

procedure TJob.AfterRun(AUsedTime: Int64);
begin
  Inc(Runs);
  if AUsedTime > 0 then begin
    Inc(TotalUsedTime, AUsedTime);
    if MinUsedTime = 0 then
      MinUsedTime := AUsedTime
    else if MinUsedTime > AUsedTime then
      MinUsedTime := AUsedTime;
    if MaxUsedTime = 0 then
      MaxUsedTime := AUsedTime
    else if MaxUsedTime < AUsedTime then
      MaxUsedTime := AUsedTime;
  end;
end;

procedure TJob.Assign(const ASource: PJob);
begin
  Self := ASource^;
  {$IFDEF UNICODE}
  if IsAnonWorkerProc then begin
    WorkerProc.ProcA := nil;
    IUnknown(WorkerProc.ProcA)._AddRef;
  end;
  {$ENDIF}
  // 下面三个成员不拷贝
  Worker := nil;
  Next := nil;
  {$IFDEF USEINLINE}
  Source := nil;
  {$ELSE}
  ExData.Source := nil;
  {$ENDIF}
end;

function TJob.GetAvgTime: Integer;
begin
  if Runs > 0 then
    Result := TotalUsedTime div Runs
  else
    Result := 0;
end;

function TJob.GetElapseTime: Int64;
begin
  Result := GetTimestamp - StartTime;
end;

function TJob.GetExtData: TJobExtData;
begin
  Result := Data;
end;

function TJob.GetHandle: TJobHandle;
var
  AMask: TJobHandle;
begin
  if IsSignalWakeup then
    AMask := JOB_HANDLE_SIGNAL_MASK
  else if IsByPlan then
    AMask := JOB_HANDLE_PLAN_MASK
  else if (FirstDelay <> 0) or (not Runonce) then
    AMask := JOB_HANDLE_REPEAT_MASK
  else
    AMask := JOB_HANDLE_SIMPLE_MASK;
  if Assigned(Source) then
    Result := TJobHandle(Source) or AMask
  else
    Result := TJobHandle(@Self) or AMask;
end;

{$IFNDEF USEINLINE}
function TJob.GetFirstDelay: Int64;
begin
  Result := ExData.FirstDelay;
end;

function TJob.GetGroup: Pointer;
begin
  Result := ExData.Group;
end;

function TJob.GetInterval: Int64;
begin
  Result := ExData.Interval;
end;

function TJob.GetRefCount: PInteger;
begin
  Result := ExData.RefCount;
end;

function TJob.GetSignalId: Integer;
begin
  Result := ExData.SignalId;
end;

function TJob.GetSource: PJob;
begin
  Result := ExData.Source;
end;

procedure TJob.SetFirstDelay(const Value: Int64);
begin
  ExData.FirstDelay := Value;
end;

procedure TJob.SetGroup(const Value: Pointer);
begin
  ExData.Group := Value;
end;

procedure TJob.SetInterval(const Value: Int64);
begin
  ExData.Interval := Value;
end;

procedure TJob.SetRefCount(const Value: PInteger);
begin
  ExData.RefCount := Value;
end;

procedure TJob.SetSignalId(const Value: Integer);
begin
  ExData.SignalId := Value;
end;

procedure TJob.SetSource(const Value: PJob);
begin
  ExData.Source := Value;
end;
{$ENDIF}

function TJob.GetFreeType: TJobDataFreeType;
begin
  Result := TJobDataFreeType((Flags shr 8) and $0F);
end;

procedure TJob.SetFreeType(const Value: TJobDataFreeType);
begin
  Flags := (Flags and (not JOB_DATA_OWNER)) or (Integer(Value) shl 8);
end;

function TJob.GetIsCustomFree: Boolean;
begin
  Result := FreeType in [jdfFreeAsC1..jdfFreeAsC6];
end;

function TJob.GetIsInterfaceOwner: Boolean;
begin
  Result := FreeType = jdfFreeAsInterface;
end;

function TJob.GetIsObjectOwner: Boolean;
begin
  Result := FreeType = jdfFreeAsObject;
end;

function TJob.GetIsRecordOwner: Boolean;
begin
  Result := FreeType = jdfFreeAsRecord
end;

function TJob.GetIsTerminated: Boolean;
begin
  if Assigned(Worker) and Assigned(Worker.FOwner) then
    Result := Worker.FOwner.Terminating or Worker.Terminated or
      ((Flags and JOB_TERMINATED) <> 0) or (Worker.FTerminatingJob = @Self)
  else
    Result := (Flags and JOB_TERMINATED) <> 0;
end;

function TJob.GetValue(Index: Integer): Boolean;
begin
  Result := (Flags and Index) <> 0;
end;

procedure TJob.Create(AProc: TJobProc);
begin
  {$IFDEF NEXTGEN}
  PJobProc(@WorkerProc)^ := AProc;
  {$ELSE}
  WorkerProc.Proc := AProc;
  {$ENDIF}
  SetValue(JOB_RUN_ONCE, True);
end;

procedure TJob.Reset;
begin
  FillChar(Self, SizeOf(TJob), 0);
end;

procedure TJob.SetIsTerminated(const Value: Boolean);
begin
  SetValue(JOB_TERMINATED, Value);
end;

procedure TJob.SetValue(Index: Integer; const Value: Boolean);
begin
  if Value then
    Flags := (Flags or Index)
  else
    Flags := (Flags and (not Index));
end;

procedure TJob.Synchronize(AMethod: TThreadMethod);
begin
  if GetCurrentThreadId = MainThreadId then
    AMethod
  else
    Worker.Synchronize(AMethod);
end;

procedure TJob.UpdateNextTime;
begin
  if (Runs = 0) and (FirstDelay <> 0) then
    NextTime := PushTime + FirstDelay
  else if Interval <> 0 then begin
    if NextTime = 0 then
      NextTime := GetTimestamp + Interval
    else
      Inc(NextTime, Interval);
  end else
    NextTime := GetTimestamp;
end;

{ TSimpleLock }

{$IFDEF WORKER_SIMPLE_LOCK}
constructor TSimpleLock.Create;
begin
  inherited;
  FFlags := 0;
end;

procedure TSimpleLock.Enter;
begin
  while (AtomicOr(FFlags, $01) and $01) <> 0 do begin
  {$IFDEF MSWINDOWS}
    SwitchToThread;
  {$ELSE}
    TThread.Yield;
  {$ENDIF}
  end;
end;

procedure TSimpleLock.Leave;
begin
  AtomicAnd(FFlags, Integer($FFFFFFFE));
end;
{$ENDIF}  

{ TJobBase }

procedure TJobBase.Clear;
var
  AItem: PJob;
begin
  while True do begin
    AItem := Pop;
    if AItem <> nil then
      FOwner.FreeJob(AItem)
    else
      Break;
  end;
end;

constructor TJobBase.Create(AOwner: TYXDWorkers);
begin
  FOwner := AOwner;
end;

destructor TJobBase.Destroy;
begin
  Clear;
  inherited;
end;

function TJobBase.GetEmpty: Boolean;
begin
  Result := (Count = 0);
end;

function TJobBase.Pop: PJob;
begin
  Result := InternalPop;
  if Result <> nil then begin
    Result.PopTime := GetTimestamp;
    Result.Next := nil;
  end;
end;

function TJobBase.Push(AJob: PJob): Boolean;
begin
  AJob.Owner := Self;
  AJob.PushTime := GetTimestamp;
  Result := InternalPush(AJob);
  if not Result then begin
    AJob.Next := nil;
    FOwner.FreeJob(AJob);
  end;
end;

{ TSimpleJobs }

function TSimpleJobs.Clear(AHandle: TJobHandle): Boolean;
begin
  if AHandle <> 0 then
    Result := ClearJobs(nil, nil, nil, -1, AHandle and (not $03)) > 0
  else
    Result := False;
end;

function TSimpleJobs.ClearJobs(AObject: Pointer; AProc: TJobProc;
  AData: Pointer; AMaxTimes: Integer; AHandle: TJobHandle): Integer;

  function PopAll: PJob;
  begin
    FLocker.Enter;
    Result := FFirst;
    FFirst := nil;
    FLast := nil;
    FCount := 0;
    FLocker.Leave;
  end;

  procedure Repush(ANewFirst: PJob);
  var
    ALast: PJob;
    ACount: Integer;
  begin
    if ANewFirst <> nil then begin
      ALast := ANewFirst;
      ACount := 0;
      while ALast.Next <> nil do begin
        ALast := ALast.Next;
        Inc(ACount);
      end;
      FLocker.Enter;
      ALast.Next := FFirst;
      FFirst := ANewFirst;
      if FLast = nil then
        FLast := ALast;
      Inc(FCount, ACount);
      FLocker.Leave;
    end;
  end;

var
  AFirst, AJob, APrior, ANext: PJob;
  b: Boolean;
begin
  AJob := PopAll();

  Result := 0;
  APrior := nil;
  AFirst := nil;
  while (AJob <> nil) and (AMaxTimes <> 0) do begin
    ANext := AJob.Next;
    if AObject <> nil then
      b := AJob.WorkerProc.Data = AObject
    else if AHandle > 0 then
      b := TJobHandle(AJob) = AHandle
    else
      b := SameWorkerProc(AJob.WorkerProc, AProc) and ((AJob.Data = AData) or (AData = nil) or (AData = Pointer(-1)));
    if b then begin
      if APrior <> nil then
        APrior.Next := ANext
      else //首个
        AFirst := ANext;
      AJob.Next := nil;
      FOwner.FreeJob(AJob);
      Dec(AMaxTimes);
      Inc(Result);
      if TJobHandle(AJob) = AHandle then
        Break;
    end else begin
      if AFirst = nil then
        AFirst := AJob;
      APrior := AJob;
    end;
    AJob := ANext;
  end;

  Repush(AFirst);
end;

procedure TSimpleJobs.Clear;
var
  AFirst: PJob;
begin
  FLocker.Enter;
  AFirst := FFirst;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
  FLocker.Leave;
  FOwner.FreeJob(AFirst);
end;

function TSimpleJobs.Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(nil, AProc, AData, AMaxTimes);
end;

function TSimpleJobs.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(AObject, nil, nil, AMaxTimes);
end;

constructor TSimpleJobs.Create(AOwner: TYXDWorkers);
begin
  inherited Create(AOwner);
  FLocker := TSimpleLock.Create;
end;

destructor TSimpleJobs.Destroy;
begin
  inherited;
  FLocker.Free;
end;

function TSimpleJobs.GetCount: Integer;
begin
  Result := FCount;
end;

function TSimpleJobs.InternalPop: PJob;
begin
  FLocker.Enter;
  Result := FFirst;
  if Result <> nil then begin
    FFirst := Result.Next;
    if FFirst = nil then
      FLast := nil;
    Dec(FCount);
  end;
  FLocker.Leave;
end;

function TSimpleJobs.InternalPush(AJob: PJob): Boolean;
begin
  FLocker.Enter;
  if FLast = nil then
    FFirst := AJob
  else
    FLast.Next := AJob;
  FLast := AJob;
  Inc(FCount);
  FLocker.Leave;
  Result := true;
end;

{ TRepeatJobs }

procedure TRepeatJobs.AfterJobRun(AJob: PJob; AUsedTime: Int64);
var
  ANode: PRBNode;

  function UpdateSource: Boolean;
  var
    ATemp, APrior: PJob;
  begin
    Result := False;
    ATemp := ANode.Data;
    APrior := nil;
    while ATemp <> nil do begin
      if ATemp = AJob.Source then begin
        if AJob.IsTerminated then begin
          if APrior <> nil then
            APrior.Next := ATemp.Next
          else
            ANode.Data := ATemp.Next;
          ATemp.Next := nil;
          FOwner.FreeJob(ATemp);
          if ANode.Data = nil then
            FItems.Delete(ANode);
        end else
          ATemp.AfterRun(AUsedTime);
        Result := True;
        Break;
      end;
      APrior := ATemp;
      ATemp := ATemp.Next;
    end;
  end;

begin
  FLocker.Enter;
  try
    ANode := FItems.Find(AJob);
    if ANode <> nil then begin
      if UpdateSource then
        Exit;
    end;
    ANode := FItems.First;
    while ANode <> nil do begin
      if UpdateSource then
        Break;
      ANode := ANode.Next;
    end;
  finally
    FLocker.Leave;
  end;
end;

function TRepeatJobs.ClearJobs(AObject: Pointer; AProc: TJobProc;
  AData: Pointer; AMaxTimes: Integer; AHandle: TJobHandle): Integer;
var
  ANode, ANext: PRBNode;
  APriorJob, AJob, ANextJob: PJob;
  ACanDelete, B: Boolean;
begin
  Result := 0;   // 现在清空重复的计划作业
  FLocker.Enter;
  try
    ANode := FItems.First;
    while (ANode <> nil) and (AMaxTimes <> 0) do begin
      ANext := ANode.Next;
      AJob := ANode.Data;
      ACanDelete := True;
      APriorJob := nil;
      while AJob <> nil do begin
        ANextJob := AJob.Next;
        if AObject <> nil then
          B := AJob.WorkerProc.Data = AObject
        else if AHandle > 0 then
          B := TJobHandle(AJob) = AHandle
        else
          B := SameWorkerProc(AJob.WorkerProc, AProc) and ((AData = nil) or (AData = Pointer(-1)) or (AData = AJob.Data));
        if B then begin
          if Assigned(APriorJob) then
            APriorJob.Next := ANextJob
          else
            ANode.Data := ANextJob;
          AJob.Next := nil;
          FOwner.FreeJob(AJob);
          Dec(AMaxTimes);
          Inc(Result);
          if TJobHandle(AJob) = AHandle then
            Break;
        end else begin
          APriorJob := AJob;
          ACanDelete := False;
        end;
        AJob := ANextJob;
      end;
      if AObject = nil then
        ACanDelete := ANode.Data = nil;
      if ACanDelete then
        FItems.Delete(ANode);
      ANode := ANext;
    end;
    if FItems.Count > 0 then
      SetFirstFireTime(PJob(FItems.First.Data).NextTime)
    else
      SetFirstFireTime(0);
  finally
    FLocker.Leave;
  end;
end;

function TRepeatJobs.Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(nil, AProc, AData, AMaxTimes);
end;

function TRepeatJobs.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(AObject, nil, nil, AMaxTimes);
end;

procedure TRepeatJobs.Clear;
begin
  FLocker.Enter;
  try
    FItems.Clear;
  finally
    FLocker.Leave;
  end;
end;

constructor TRepeatJobs.Create(AOwner: TYXDWorkers);
begin
  inherited Create(AOwner);
  FLocker := TCriticalSection.Create;
  FItems := TRBTree.Create(DoTimeCompare);
  FItems.OnDelete := DoJobDelete;
end;

destructor TRepeatJobs.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
  FreeAndNil(FLocker);
end;

procedure TRepeatJobs.DoJobDelete(ATree: TRBTree; ANode: PRBNode);
begin
  FOwner.FreeJob(ANode.Data);
end;

function TRepeatJobs.DoTimeCompare(P1, P2: Pointer): Integer;
var
  ATemp: Int64;
begin
  ATemp := PJob(P1).NextTime - PJob(P2).NextTime;
  if ATemp < 0 then
    Result := -1
  else if ATemp > 0 then
    Result := 1
  else
    Result := 0;
end;

function TRepeatJobs.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRepeatJobs.InternalPop: PJob;
var
  ANode: PRBNode;
  ATick: Int64;
  AJob: PJob;
begin
  Result := nil;
  if FItems.Count = 0 then Exit;
  FLocker.Enter;
  try
    if FItems.Count > 0 then begin
      ATick := GetTimestamp;
      ANode := FItems.First;
      AJob := ANode.Data;
      if AJob.NextTime <= ATick then begin
        if AJob.Next <> nil then // 如果没有更多需要执行的作业，则删除结点，否则指向下一个
          ANode.Data := AJob.Next
        else begin
          ANode.Data := nil;
          FItems.Delete(ANode);
          ANode := FItems.First;
          if ANode <> nil then
            SetFirstFireTime(PJob(ANode.Data).NextTime)
          else // 没有计划作业了，不需要了
            SetFirstFireTime(0);            
        end;
        if AJob.Runonce then
          Result := AJob
        else begin
          AJob.Next := nil;  // yangyxd 2014.09.12
          AJob.PopTime := ATick;
          Inc(AJob.NextTime, AJob.Interval);
          Result := JobPool.Pop;
          Result.Assign(AJob);
          Result.Source := AJob;
          Result.Next := nil;
          // 重新插入作业
          ANode := FItems.Find(AJob);
          if ANode = nil then begin
            FItems.Insert(AJob); 
            SetFirstFireTime(PJob(FItems.First.Data).NextTime);
          end else begin// 如果已经存在同一时刻的作业，则自己挂接到其它作业头部
            AJob.Next := PJob(ANode.Data);
            ANode.Data := AJob; // 首个作业改为自己
          end;           
        end;
      end;
    end else
      SetFirstFireTime(0);
  finally
    FLocker.Leave;
  end;
end;

function TRepeatJobs.InternalPush(AJob: PJob): Boolean;
var
  ANode: PRBNode;
begin
  AJob.UpdateNextTime;  // 计算作业的下次执行时间
  FLocker.Enter;
  try
    ANode := FItems.Find(AJob);
    if ANode = nil then begin
      FItems.Insert(AJob);
      SetFirstFireTime(PJob(FItems.First.Data).NextTime);
    end else begin // 如果已经存在同一时刻的作业，则自己挂接到其它作业头部
      AJob.Next := PJob(ANode.Data);
      ANode.Data := AJob; // 首个作业改为自己
    end;
  finally
    FLocker.Leave;
  end;
  Result := True;
end;

procedure TRepeatJobs.SetFirstFireTime(Value: Int64);
begin
  FFirstFireTime := Value;
end;

function TRepeatJobs.Clear(AHandle: TJobHandle): Boolean;
begin
  if AHandle <> 0 then
    Result := ClearJobs(nil, nil, nil, -1, AHandle and (not $03)) > 0
  else
    Result := False;
end;

{ TYXDWorker }

procedure TYXDWorker.ComNeeded(AInitFlags: Cardinal);
begin
  {$IFDEF MSWINDOWS}
  if not ComInitialized then begin
    if AInitFlags = 0 then
      CoInitialize(nil)
    else
      CoInitializeEx(nil, AInitFlags);
    SetValue(WORKER_COM_INITED, True);
  end;
  {$ENDIF MSWINDOWS}
end;

constructor TYXDWorker.Create(AOwner: TYXDWorkers);
begin
  inherited Create(True);
  FOwner := AOwner;
  FTimeout := 1000;
  {$IFDEF SAVE_WORDER_TIME}
  FStartTime := GetTimestamp;
  FLastExecTime := 0;
  {$ENDIF}
  FEvent := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := True;
end;

destructor TYXDWorker.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

procedure TYXDWorker.DoJob(AJob: PJob);
begin
  {$IFDEF SAVE_WORDER_TIME}
  FLastExecTime := 0;
  {$ENDIF}
  {$IFDEF UNICODE}
  if AJob.IsAnonWorkerProc then
    TJobProcA(AJob.WorkerProc.ProcA)(AJob)
  else
  {$ENDIF}
  begin
    if AJob.WorkerProc.Data <> nil then
      {$IFDEF NEXTGEN}
      PJobProc(@AJob.WorkerProc)^(AJob)
      {$ELSE}
      AJob.WorkerProc.Proc(AJob)
      {$ENDIF}
    else
      AJob.WorkerProc.ProcG(AJob);
  end;
  {$IFDEF SAVE_WORDER_TIME}
  FLastExecTime := GetTimestamp;
  {$ENDIF}
end;

function TYXDWorker.WaitSignal(ATimeout: Integer; AByRepeatJob: Boolean): TWaitResult;
var
  T: Int64;
begin
  if ATimeout > 1 then begin
    T := GetTimestamp;
    if ATimeout > FOwner.FFireTimeout + FFireDelay - FTimeout then
      ATimeout := FOwner.FFireTimeout + FFireDelay - FTimeout;
    Result := FEvent.WaitFor(ATimeout);
    T := GetTimestamp - T;
    if Result = wrTimeout then begin
      Inc(FTimeout, T div 10);
      if AByRepeatJob then
        Result := wrSignaled;
    end;
  end else
    Result := wrSignaled;
end;

procedure TYXDWorker.Execute;
var
  wr: TWaitResult;
  {$IFDEF MSWINDOWS}
  SyncEvent: TEvent;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  SyncEvent := TEvent.Create(nil, False, False, '');
  {$IFDEF UNICODE}
  NameThreadForDebugging('YXDWorker');
  {$ENDIF}
  {$ENDIF}
  try
    SetValue(WORKER_RUNNING, true);
    FLastActiveTime := GetTimestamp;
    FFireDelay := Random(FOwner.FFireTimeout shr 1);

    while not(Terminated or FOwner.FTerminating) do begin

      SetValue(WORKER_CLEANING, False);
      if FOwner.Enabled then begin
        if (FOwner.FSimpleJobs.FFirst <> nil) then begin
          wr := WaitSignal(0, False)
        end else if (FOwner.FRepeatJobs.FFirstFireTime <> 0) then begin
          wr := WaitSignal(FOwner.FRepeatJobs.FFirstFireTime - GetTimestamp, True)
        end else
          wr := WaitSignal(FOwner.FFireTimeout, False);
      end else
        wr := WaitSignal(FOwner.FFireTimeout, False);

      if Terminated or FOwner.FTerminating then
        Break;

      if wr = wrSignaled then begin          
        if FOwner.FTerminating then
          Break;


        FPending := False;
        if (FOwner.WorkerCount - AtomicIncrement(FOwner.FBusyCount) = 0) and
          (FOwner.WorkerCount < FOwner.MaxWorkers) then
          FOwner.NewWorkerNeeded;

        repeat
          SetValue(WORKER_LOOKUP, True);
          FActiveJob := FOwner.Popup;
          SetValue(WORKER_LOOKUP, False);
          if FActiveJob <> nil then begin
            SetValue(WORKER_ISBUSY, True);
            FTimeout := 0;
            FLastActiveTime := FActiveJob.PopTime;
            FActiveJob.Worker := Self;
            FActiveJobProc := ToJobProc(FActiveJob^.WorkerProc);

            // 为Clear(AObject)准备判断，以避免FActiveJob线程不安全
            FActiveJobData := FActiveJob.Data;
            if FActiveJob.IsSignalWakeup then
              FActiveJobSource := FActiveJob.Source
            else
              FActiveJobSource := nil;
            if FActiveJob.IsGrouped then
              FActiveJobGroup := FActiveJob.Group
            else
              FActiveJobGroup := nil;
            FActiveJobFlags := FActiveJob.Flags;
            if FActiveJob.StartTime = 0 then begin
              FActiveJob.StartTime := FLastActiveTime;
              FActiveJob.FirstTime := FActiveJob.StartTime;
            end else
              FActiveJob.StartTime := FLastActiveTime;

            try
              FFlags := (FFlags or WORKER_EXECUTING) and (not WORKER_LOOKUP);
              if FActiveJob.InMainThread then begin
                {$IFDEF MSWINDOWS}
                if PostMessage(FOwner.FMainWorker, WM_APP, WPARAM(FActiveJob), LPARAM(SyncEvent)) then
                  SyncEvent.WaitFor(INFINITE)
                {$ELSE}
                Synchronize(Self, FireInMainThread)
                {$ENDIF}
              end else
                DoJob(FActiveJob);
            except
              on E: Exception do
                if Assigned(FOwner.FOnError) then
                  FOwner.FOnError(FActiveJob, E, jesExecute);
            end;
            
            Inc(FProcessed);
            SetValue(WORKER_CLEANING, True);
            FActiveJob.Worker := nil;
            if not FActiveJob.Runonce then begin
              FOwner.FRepeatJobs.AfterJobRun(FActiveJob, GetTimestamp - FActiveJob.StartTime);
              FActiveJob.Data := nil;
            end else begin
              if FActiveJob.IsSignalWakeup then
                FOwner.SignalWorkDone(FActiveJob, GetTimestamp - FActiveJob.StartTime)
              else if FActiveJob.IsLongtimeJob then
                AtomicDecrement(FOwner.FLongTimeWorkers)
              else if FActiveJob.IsGrouped then
                FActiveJobGroup.DoJobExecuted(FActiveJob);
            end;

            if Assigned(FActiveJob) then                       
              FOwner.FreeJob(FActiveJob);
            FActiveJobProc := nil;
            FActiveJobSource := nil;
            FActiveJobFlags := 0;
            FActiveJobGroup := nil;
            FTerminatingJob := nil;
            FFlags := FFlags and (not WORKER_EXECUTING);

          end else
            FFlags := FFlags and (not WORKER_LOOKUP);
            
        until (FActiveJob = nil) or Terminated or FOwner.FTerminating or
          (not FOwner.Enabled);

        SetValue(WORKER_ISBUSY, False);
        AtomicDecrement(FOwner.FBusyCount);
        //readYield;
      end;
      if (FTimeout >= FOwner.FireTimeout + FFireDelay) then begin
        // 加一个随机的2秒延迟，以避免同时释放
        FOwner.WorkerTimeout(Self);
        if not IsFiring then
          FTimeout := 0;
      end;
    end;
  finally
    SetValue(WORKER_RUNNING, False);
    {$IFDEF MSWINDOWS}
    FreeAndNil(SyncEvent);
    if ComInitialized then
      CoUninitialize;
    {$ENDIF}
    //OutputDebugString(PChar('Worker '+IntToStr(ThreadID)+' Done'));
    FOwner.WorkerTerminate(Self);
  end;
end;

procedure TYXDWorker.FireInMainThread;
begin
  DoJob(FActiveJob);
end;

function TYXDWorker.GetIsIdle: Boolean;
begin
  Result := not IsBusy;
end;

function TYXDWorker.GetValue(Index: Integer): Boolean;
begin
  Result := (FFlags and Index) <> 0;
end;

procedure TYXDWorker.SetValue(Index: Integer; const Value: Boolean);
begin
  if Value then
    FFlags := (FFlags or Index)
  else
    FFlags := (FFlags and (not Index));
end;

{ TYXDWorkers }

function TYXDWorkers.Clear(const ASignalName: string): Integer;
begin
  Result := ClearWaitJobs(0, ASignalName);
end;

function TYXDWorkers.Clear(ASignalId: Integer): Integer;
begin
  Result := ClearWaitJobs(ASignalId, '');
end;

{$IFDEF USEINLINE}
function TYXDWorkers.&For(const AStartIndex, AStopIndex: TForLoopIndexType;
  AWorkerProc: TForJobProc; AData: Pointer; AMsgWait: Boolean;
  AFreeType: TJobDataFreeType): TWaitResult;
var
  AInst: TForJobs;
begin
  AInst := TForJobs.Create(Self, AStartIndex, AStopIndex, AData, AFreeType);
  try
    PForJobProc(@AInst.FWorkJob.WorkerProc)^ := AWorkerProc;
    AInst.Start;
    Result := AInst.Wait(AMsgWait);
  finally
    FreeAndNil(AInst);
  end;
end;
{$ENDIF}

{$IFDEF UNICODE}
function TYXDWorkers.&For(const AStartIndex, AStopIndex: TForLoopIndexType;
  AWorkerProc: TForJobProcA; AData: Pointer; AMsgWait: Boolean;
  AFreeType: TJobDataFreeType): TWaitResult;
var
  AInst: TForJobs;
begin
  AInst := TForJobs.Create(Self, AStartIndex, AStopIndex, AData, AFreeType);
  try
    TForJobProcA(AInst.FWorkJob.WorkerProc.ForProcA) := AWorkerProc;
    AInst.FWorkJob.IsAnonWorkerProc := True;
    AInst.Start;
    Result := AInst.Wait(AMsgWait);
  finally
    FreeAndNil(AInst);
  end;
end;
{$ENDIF}

{$IFDEF USEINLINE}
function TYXDWorkers.&For(const AStartIndex, AStopIndex: TForLoopIndexType;
  AWorkerProc: TForJobProcG; AData: Pointer; AMsgWait: Boolean;
  AFreeType: TJobDataFreeType): TWaitResult;
var
  AInst: TForJobs;
begin
  AInst := TForJobs.Create(Self, AStartIndex, AStopIndex, AData, AFreeType);
  try
    AInst.FWorkJob.WorkerProc.ForProcG := AWorkerProc;
    AInst.Start;
    Result := AInst.Wait(AMsgWait);
  finally
    FreeAndNil(AInst);
  end;
end;
{$ENDIF}

procedure TYXDWorkers.Clear(AHandle: TJobHandle);
var
  AInstance: PJob;
  AWaitParam: TWorkerWaitParam;
  Wait: Boolean;

  function RemoveSignalJob: PJob;
  var
    i: Integer;
    AJob, ANext, APrior: PJob;
    AList: PHashList;
    ASignal: PSignal;
  begin
    Result := nil;
    FLocker.Enter;
    try
      for i := 0 to FSignalJobs.BucketCount - 1 do begin
        AList := FSignalJobs.Buckets[i];
        if AList <> nil then begin
          ASignal := AList.Data;
          if ASignal.First <> nil then begin
            AJob := ASignal.First;
            APrior := nil;
            while AJob <> nil do begin
              ANext := AJob.Next;
              if AJob = AInstance then begin
                if ASignal.First = AJob then
                  ASignal.First := ANext;
                if Assigned(APrior) then
                  APrior.Next := ANext;
                AJob.Next := nil;
                Result := AJob;
                Exit;
              end else
                APrior := AJob;
              AJob := ANext;
            end;
          end;
        end;
      end;
    finally
      FLocker.Leave;
    end;
  end;

  function ClearSignalJob: Boolean;
  var
    AJob: PJob;
  begin
    AJob := RemoveSignalJob;
    if Assigned(AJob) then
      ClearSignalJobs(AJob);
    Result := AJob <> nil;
  end;

begin
  if AHandle = 0 then Exit;
  AInstance := Pointer(AHandle and (not $03));
  case AHandle and $03 of
    0: Wait := FSimpleJobs.Clear(AHandle); // SimpleJobs
    1: Wait := FRepeatJobs.Clear(AHandle); // RepeatJobs
    2: Wait := ClearSignalJob();
    else
      Exit;
  end;
  if not Wait then Exit;
  FillChar(AWaitParam, SizeOf(TWorkerWaitParam), 0);
  AWaitParam.SourceJob := AInstance;
  if (AHandle and $03) = 0 then
    AWaitParam.WaitType := 4
  else
    AWaitParam.WaitType := 2;
  WaitRunningDone(AWaitParam);
end;

function TYXDWorkers.Clear(AProc: TJobProc; AData: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(nil, AProc, AData, AMaxTimes);
end;

function TYXDWorkers.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
begin
  Result := ClearJobs(AObject, nil, nil, AMaxTimes);
end;

procedure TYXDWorkers.Clear;
var
  i: Integer;
  AParam: TWorkerWaitParam;
  ASignal: PSignal;
begin
  DisableWorkers; // 避免工作者取得新的作业
  try
    FSimpleJobs.Clear;
    FRepeatJobs.Clear;
    FLocker.Enter;
    try
      for i := 0 to FSignalJobs.BucketCount - 1 do begin
        if Assigned(FSignalJobs.Buckets[i]) then begin
          ASignal := FSignalJobs.Buckets[i].Data;
          FreeJob(ASignal.First);
          ASignal.First := nil;
        end;
      end;
    finally
      FLocker.Leave;
    end;
    AParam.WaitType := $FF;
    WaitRunningDone(AParam);
  finally
    EnableWorkers;
  end;
end;

function TYXDWorkers.ClearJobs(AObject: Pointer; AProc: TJobProc;
  AData: Pointer; AMaxTimes: Integer): Integer;
var
  ACleared: Integer;
  AWaitParam: TWorkerWaitParam;

  function ClearSignalJobs(IsClearObject: Boolean): Integer;
  var
    i: Integer;
    AJob, ANext, APrior: PJob;
    AList: PHashList;
    ASignal: PSignal;
    B: Boolean;
  begin
    Result := 0;
    FLocker.Enter;
    try
      for i := 0 to FSignalJobs.BucketCount - 1 do begin
        AList := FSignalJobs.Buckets[i];
        if AList <> nil then begin
          ASignal := AList.Data;
          if ASignal.First <> nil then begin
            AJob := ASignal.First;
            APrior := nil;
            while (AJob <> nil) and (AMaxTimes <> 0) do begin
              ANext := AJob.Next;
              if IsClearObject then
                B := AJob.WorkerProc.Data = AObject
              else
                B := SameWorkerProc(AJob.WorkerProc, AProc) and ((AData = nil) or (AData = Pointer(-1)) or (AJob.Data = AData));
              if B then begin
                if ASignal.First = AJob then
                  ASignal.First := ANext;
                if Assigned(APrior) then
                  APrior.Next := ANext;
                AJob.Next := nil;
                FreeJob(AJob);
                Dec(AMaxTimes);
                Inc(Result);
              end else
                APrior := AJob;
              AJob := ANext;
            end;
            if AMaxTimes = 0 then
              Break;
          end;
        end;
      end;
    finally
      FLocker.Leave;
    end;
  end;

begin
  Result := 0;
  if Self <> nil then begin
    ACleared := FSimpleJobs.ClearJobs(AObject, AProc, AData, AMaxTimes);
    Inc(Result, ACleared);
    Dec(AMaxTimes, ACleared);
    if AMaxTimes <> 0 then begin
      ACleared := FRepeatJobs.ClearJobs(AObject, AProc, AData, AMaxTimes);
      Dec(AMaxTimes, ACleared);
      Inc(Result, ACleared);
      if AMaxTimes <> 0 then begin
        ACleared := ClearSignalJobs(AObject <> nil);
        Inc(Result, ACleared);
        if AMaxTimes <> 0 then begin
          if AObject <> nil then begin
            AWaitParam.WaitType := 0;
            AWaitParam.Bound := AObject;
          end else begin
            AWaitParam.WaitType := 1;
            AWaitParam.Data := AData;
            AWaitParam.WorkerProc := TMethod(AProc);
          end;
          WaitRunningDone(AWaitParam);
        end;
      end;
    end;
  end;
end;

function TYXDWorkers.ClearWaitJobs(ASignalId: Integer; const ASignalName: string): Integer;
var
  i: Integer;
  ASignal: PSignal;
  AJob: PJob;
  B: Boolean;
begin
  Result := 0;
  AJob := nil;
  FLocker.Enter;
  try
    for i := 0 to FSignalJobs.BucketCount - 1 do begin
      if FSignalJobs.Buckets[i] <> nil then begin
        ASignal := FSignalJobs.Buckets[i].Data;
        if ASignalId > 0 then
          B := ASignal.Id = ASignalId
        else
          B := ASignal.Name = ASignalName;
        if B then begin
          AJob := ASignal.First;
          ASignal.First := nil;
          Break;
        end;
      end;
    end;
  finally
    FLocker.Leave;
  end;
  if AJob <> nil then
    Result := ClearSignalJobs(AJob)
end;

function TYXDWorkers.ClearSignalJobs(ASource: PJob): Integer;
var
  ACount: Integer;
  AFirst, ALast, APrior, ANext: PJob;
  AWaitParam: TWorkerWaitParam;
begin
  Result := 0;
  AFirst := nil;
  APrior := nil;
  FSimpleJobs.FLocker.Enter;
  try
    ALast := FSimpleJobs.FFirst;
    ACount := FSimpleJobs.Count;
    FSimpleJobs.FFirst := nil;
    FSimpleJobs.FLast := nil;
    FSimpleJobs.FCount := 0;
  finally
    FSimpleJobs.FLocker.Leave;
  end;

  while ALast <> nil do begin
    if (ALast.IsSignalWakeup) and (ALast.Source = ASource) then begin
      ANext := ALast.Next;
      ALast.Next := nil;
      FreeJob(ALast);
      ALast := ANext;
      if APrior <> nil then
        APrior.Next := ANext;
      Dec(ACount);
      Inc(Result);
    end else begin
      if AFirst = nil then
        AFirst := ALast;
      APrior := ALast;
      ALast := ALast.Next;
    end;
  end;

  if ACount > 0 then begin
    FSimpleJobs.FLocker.Enter;
    try
      APrior.Next := FSimpleJobs.FFirst;
      FSimpleJobs.FFirst := AFirst;
      Inc(FSimpleJobs.FCount, ACount);
      if FSimpleJobs.FLast = nil then
        FSimpleJobs.FLast := APrior;
    finally
      FSimpleJobs.FLocker.Leave;
    end;
  end;
  AWaitParam.WaitType := 2;
  AWaitParam.SourceJob := ASource;
  WaitRunningDone(AWaitParam);
  FreeJob(ASource);
end;

procedure TYXDWorkers.ClearWorkers();
var
  i: Integer;
  AInMainThread: Boolean;

  function WorkerExists: Boolean;
  var
    J: Integer;
  begin
    Result := False;
    FLocker.Enter;
    try
      J := FWorkerCount - 1;
      while J >= 0 do begin
        if ThreadExists(FWorkers[J].ThreadID) then begin
          Result := true;
          Break;
        end;
        Dec(J);
      end;
    finally
      FLocker.Leave;
    end;
  end;

var
  T: Int64;
begin
  FTerminating := True;
  FLocker.Enter;
  try
    FRepeatJobs.SetFirstFireTime(0);
    for i := 0 to FWorkerCount - 1 do
      FWorkers[i].FEvent.SetEvent;
  finally
    FLocker.Leave;
  end;
  AInMainThread := GetCurrentThreadId = MainThreadId;
  T := GetTimestamp;
  while (FWorkerCount > 0) and WorkerExists do begin
    if AInMainThread then
      ProcessAppMessage;
    if GetTimestamp - T > 35000 then
      Break;
    Sleep(30);
  end;
  for i := 0 to FWorkerCount - 1 do begin
    if FWorkers[i] <> nil then
      FreeAndNil(FWorkers[i]);
  end;
  FWorkerCount := 0;
end;

constructor TYXDWorkers.Create(AMinWorkers: Integer);
var
  i: Integer;
begin
  FBusyCount := 0;
  FFireTimeout := WAITJOB_TIMEOUT;
  FSimpleJobs := TSimpleJobs.Create(Self);
  FRepeatJobs := TRepeatJobs.Create(Self);
  FSignalJobs := TYXDHashTable.Create(1361);
  FSignalJobs.OnDelete := DoJobFree;
  FSignalJobs.AutoSize := True;
  FLocker := TCriticalSection.Create;

  FCPUNum := GetCPUCount;
  if AMinWorkers < 1 then
    FMinWorkers := 2
  else
    FMinWorkers := AMinWorkers; // 最少工作者为2个
  FMaxWorkers := (FCPUNum shl 1) + 1;
  if FMaxWorkers <= FMinWorkers then
    FMaxWorkers := (FMinWorkers shl 1) + 1;
  FTerminating := False;

  // 创建默认工作者
  FDisableCount := 0;
  FMaxSignalId := 0;
  FWorkerCount := 0;
  SetLength(FWorkers, FMaxWorkers + 1);
  for i := 0 to FMinWorkers - 1 do 
    FWorkers[i] := CreateWorker(True);
  for i := 0 to FMinWorkers - 1 do begin
    FWorkers[i].FEvent.SetEvent;
    FWorkers[i].Suspended := False;
  end;
  FMaxLongtimeWorkers := (FMaxWorkers shr 1);
  {$IFDEF MSWINDOWS}
  FMainWorker := AllocateHWnd(DoMainThreadWork);
  {$ENDIF}
  FStaticThread := TStaticThread.Create(Self, True);
  FStaticThread.Suspended := False;
end;

function TYXDWorkers.CreateWorker(ASuspended: Boolean): TYXDWorker;
begin
  if FWorkerCount < FMaxWorkers then begin
    Result := TYXDWorker.Create(Self);
    FWorkers[FWorkerCount] := Result;
    {$IFDEF MSWINDOWS}
    SetThreadCPU(Result.Handle, FWorkerCount mod FCPUNum);
    {$ELSE}
    SetThreadCPU(Result.ThreadId, FWorkerCount mod FCPUNum);
    {$ENDIF}
    Inc(FWorkerCount);
    if not ASuspended then begin
      Result.FPending := true;
      Result.FEvent.SetEvent;
      Result.Suspended := False;
    end;
  end else
    Result := nil;
end;

destructor TYXDWorkers.Destroy;
var
  T: Int64;
  AStaticThreadId: TThreadId;
  AInMainThread: Boolean;
begin
  ClearWorkers();
  FLocker.Enter;
  try
    FreeAndNil(FSimpleJobs);
    FreeAndNil(FRepeatJobs);
    FreeAndNil(FSignalJobs);
  finally
    FLocker.Free;
  end;
  {$IFDEF MSWINDOWS}
  DeallocateHWnd(FMainWorker);
  {$ENDIF}
  AStaticThreadId := FStaticThread.ThreadId;
  FStaticThread.FreeOnTerminate := True;
  FStaticThread.Terminate;
  TStaticThread(FStaticThread).FEvent.SetEvent;
  ThreadYield;
  T := GetTimestamp;
  AInMainThread := GetCurrentThreadId = MainThreadId;
  while Assigned(FStaticThread) and (ThreadExists(AStaticThreadId)) and
    (GetTimestamp - T < 6000) do
  begin
    if AInMainThread then
      ProcessAppMessage;
    Sleep(200);
  end;
  try
    if Assigned(FStaticThread) then
      FreeAndNil(FStaticThread);
  except
    {$IFNDEF NEXTGEN}OutputDebugString(PChar(Exception(ExceptObject).Message));{$ENDIF}
  end;
  inherited;
end;

procedure TYXDWorkers.DisableWorkers;
begin
  AtomicIncrement(FDisableCount);
end;

procedure TYXDWorkers.DoCustomFreeData(AFreeType: TJobDataFreeType;
  var AData: Pointer);
begin
  if Assigned(FOnCustomFreeData) then
    FOnCustomFreeData(Self, AFreeType, AData);
end;

procedure TYXDWorkers.DoJobFree(ATable: TObject; AHash: THashType; AData: Pointer);
var
  ASignal: PSignal;
begin
  ASignal := AData;
  if ASignal.First <> nil then
    FreeJob(ASignal.First);
  Dispose(ASignal);
end;

{$IFDEF MSWINDOWS}
procedure TYXDWorkers.DoMainThreadWork(var AMsg: TMessage);
var
  AJob: PJob;
begin
  if AMsg.MSG = WM_APP then begin
    AJob := PJob(AMsg.WPARAM);
    try
      AJob.Worker.DoJob(AJob);
    except
      on E: Exception do begin
        if Assigned(FOnError) then
          FOnError(AJob, E, jesExecute);
      end;
    end;
    if AMsg.LPARAM <> 0 then
      TEvent(AMsg.LPARAM).SetEvent;
  end else
    AMsg.Result := DefWindowProc(FMainWorker, AMsg.MSG, AMsg.WPARAM, AMsg.LPARAM);
end;
{$ENDIF}

procedure TYXDWorkers.EnableWorkers;
var
  ANeedCount: Integer;
begin
  if AtomicDecrement(FDisableCount) = 0 then begin
    if (FSimpleJobs.Count > 0) or (FRepeatJobs.Count > 0) then begin
      ANeedCount := FSimpleJobs.Count + FRepeatJobs.Count;
      while ANeedCount > 0 do begin
        if not LookupIdleWorker then
          Break;
        Dec(ANeedCount);
      end;
    end;
  end;
end;

procedure InitJobFreeType(AOwner: TYXDWorkers; AJob: PJob; AData: Pointer; AFreeType: TJobDataFreeType); {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  if AData <> nil then begin
    AJob.Flags := AJob.Flags or (Integer(AFreeType) shl 8);
    if AFreeType = jdfFreeAsInterface then
      IUnknown(AData)._AddRef;
  end;
end;

procedure TYXDWorkers.FireSignalJob(ASignal: PSignal; AData: Pointer;
  AFreeType: TJobDataFreeType);
var
  AJob, ACopy: PJob;
  ACount: PInteger;
begin
  Inc(ASignal.Fired);
  if AData <> nil then begin
    New(ACount);
    ACount^ := 1; //初始值
  end else
    ACount := nil;
  AJob := ASignal.First;
  while AJob <> nil do begin
    ACopy := JobPool.Pop;
    ACopy.Assign(AJob);
    ACopy.SetValue(JOB_RUN_ONCE, True);
    ACopy.Source := AJob;
    ACopy.Data := AData;
    InitJobFreeType(Self, ACopy, AData, AFreeType);
    if ACount <> nil then begin
      AtomicIncrement(ACount^);
      ACopy.RefCount := ACount;
    end;
    FSimpleJobs.Push(ACopy);
    AJob := AJob.Next;
  end;
  if AData <> nil then begin
    if AtomicDecrement(ACount^) = 0 then begin
      Dispose(ACount);
      FreeJobData(AData, AFreeType);
    end;
  end;
end;

procedure TYXDWorkers.FreeJob(AJob: PJob);
var
  ANext: PJob;
  AFreeData: Boolean;
begin
  while AJob <> nil do begin
    ANext := AJob.Next;
    if AJob.Data <> nil then begin
      if AJob.IsSignalWakeup then begin
        AFreeData := AtomicDecrement(AJob.RefCount^) = 0;
        if AFreeData then
          Dispose(AJob.RefCount);
      end else
        AFreeData := AJob.IsDataOwner;
      {$IFDEF AutoFreeJobExData}
      if ExDataMap.Remove(Cardinal(AJob.Data)) then
        FreeAndNil(TObject(AJob.Data))
      else
      {$ENDIF}
      if AFreeData then begin
        FreeJobData(AJob.Data, AJob.FreeType);
        AJob.Data := nil;
      end;
    end;
    JobPool.Push(AJob);
    AJob := ANext;
  end;
end;

procedure TYXDWorkers.FreeJobData(AData: Pointer; AFreeType: TJobDataFreeType);
begin
  if AData = nil then Exit;
  try
    case AFreeType of
      jdfFreeAsObject:
        FreeAndNil(TObject(AData));
      jdfFreeAsRecord:
        Dispose(AData);
      jdfFreeAsInterface:
        IUnknown(AData)._Release
    else
      DoCustomFreeData(AFreeType, AData);
    end;
  except
    on E: Exception do
      if Assigned(FOnError) then FOnError(nil, E, jesFreeData);
  end;
end;

function TYXDWorkers.GetEnabled: Boolean;
begin
  Result := (FDisableCount = 0);
end;

function TYXDWorkers.GetIdleWorkerCount: Integer;
begin
  Result := FWorkerCount - FBusyCount;
end;

class function TYXDWorkers.GetInstance: TYXDWorkers;
begin
  if not Assigned(Workers) then
    Workers := TYXDWorkers.Create();
  Result := Workers;
end;

{$IFDEF Use_DebugHelper}
class function TYXDWorkers.GetJobPorcInfo(const AJobProc: TJobProc): string;
begin
  if Assigned(AJobProc) then begin
    Result := Format('%s,%s', [IntToHex(Integer(@AJobProc), 8),
      GetDebugMapDesc(TMethod(AJobProc).Code)]);
  end else
    Result := '-';
end;
{$ENDIF}

{$IFDEF Use_DebugHelper}
class function TYXDWorkers.GetJobPorcInfo(const AJobProc: TJobMethod): string;
begin
  if Assigned(AJobProc.Proc) then begin
    Result := Format('%s,%s', [IntToHex(Integer(AJobProc.Code), 8),
      GetDebugMapDesc(AJobProc.Code)]);
  end else
    Result := '-';
end;
{$ENDIF}

function TYXDWorkers.GetWorkerState: TWorkerStateList;
var
  I, J: Integer;
begin
  J := 0;
  FLocker.Enter;
  try
    SetLength(Result, Length(FWorkers));
    for I := 0 to High(Result) do
      if Assigned(FWorkers[i]) and (FWorkers[i].{$IFDEF NEXTGEN}ThreadID{$ELSE}Handle{$ENDIF} > 0) then begin
        Result[j].Handle := FWorkers[i].{$IFDEF NEXTGEN}ThreadID{$ELSE}Handle{$ENDIF};
        Result[j].Flags := FWorkers[i].FFlags;
        if FWorkers[i].FActiveJob <> nil then begin
          Result[j].ActiveJobFlags := FWorkers[i].FActiveJob.Flags;
          Result[j].ActiveJobElapseTime := FWorkers[i].FActiveJob.ElapseTime;
        end else begin
          Result[j].ActiveJobFlags := 0;
          Result[j].ActiveJobElapseTime := 0;
        end;
        Result[j].ActiveJobProc := FWorkers[i].FActiveJobProc;
        if FWorkers[i].FTerminatingJob <> nil then
          Result[j].TerminatingJobProc := ToJobProc(FWorkers[i].FTerminatingJob.WorkerProc)
        else
          Result[j].TerminatingJobProc := nil;
        {$IFDEF SAVE_WORDER_TIME}
        Result[j].StartTime := FWorkers[i].FStartTime;
        Result[j].LastExecTime := FWorkers[i].LastExecTime;
        {$ENDIF}
        Inc(J);
      end;
  finally
    FLocker.Leave;
  end;
  SetLength(Result, J);
end;

function TYXDWorkers.IsAutoFreeType(AJob: PJob): Boolean;
begin
  Result := (AJob <> nil) and
    (AJob.Data <> nil) and
    (AJob.FreeType <> jdfFreeByUser) and
    ((not AJob.IsCustomFree) or Assigned(FOnCustomFreeData));
end;

class function TYXDWorkers.JobPoolCount: Integer;
begin
  Result := JobPool.Count;
end;

function TYXDWorkers.LookupIdleWorker(AFromSimple: Boolean): Boolean;
var
  AWorker: TYXDWorker;
  APasscount, APendingCount: Integer;

  procedure InternalLookupWorker;
  var I: Integer;
  begin
    I := 0;
    FLocker.Enter;
    try
      while I < FWorkerCount do begin
        if (FWorkers[I].IsIdle) and (FWorkers[I].IsRunning) and (not FWorkers[I].IsFiring) then
        begin
          if AWorker = nil then begin
            if not FWorkers[I].FPending then begin
              AWorker := FWorkers[I];
              AWorker.FPending := True;
              AWorker.FEvent.SetEvent;
              Break;
            end else
              Inc(APendingCount);
          end;
        end;
        Inc(I);
      end;
      if FWorkerCount = MaxWorkers then
        // 如果已经到最大工作者，就不必重试了
        APasscount := -1
      else if (AWorker = nil) and (APendingCount = 0) then begin
        // 未找到且没有启动中的工作者，则尝试创建新的
        // OutputDebugString(PChar(Format('Pending %d,Passcount %d',
        // [APendingCount, APasscount])));
        AWorker := CreateWorker(False);
      end;
    finally
      FLocker.Leave;
    end;
  end;

begin
  Result := False;
  if (FBusyCount >= FMaxWorkers) or ((FDisableCount <> 0) or FTerminating) then
    Exit;

  AWorker := nil;
  APasscount := 0;
  repeat
    APendingCount := 0;
    // 如果有正在解雇的工作者，那么等待完成
    while FFiringWorkerCount > 0 do
      ThreadYield;
    InternalLookupWorker;
    if (AWorker = nil) and (APendingCount > 0) then begin
      // 如果没能分配工作者并且有未启动完成的工作者，则切换掉线程的时间片，然后再尝试检查
      ThreadYield;
      Inc(APasscount);
    end;
  until (APasscount < 0) or (AWorker <> nil);
  Result := AWorker <> nil;
  if Result then
    ThreadYield;
end;  

function TYXDWorkers.Popup: PJob;
begin
  Result := FSimpleJobs.Pop;
  if Result = nil then
    Result := FRepeatJobs.Pop;
end;

procedure TYXDWorkers.NewWorkerNeeded;
begin
  TStaticThread(FStaticThread).CheckNeeded;
end;

procedure InitJob(AJob: PJob; AData: Pointer;
  ARunInMainThread: Boolean; const ADelay, AInterval: Int64); {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  AJob.Data := AData;
  if AInterval > 0 then begin
    AJob.Interval := AInterval;
    AJob.SetValue(JOB_RUN_ONCE, False);
  end else
    AJob.SetValue(JOB_RUN_ONCE, True);
  AJob.FirstDelay := ADelay;
  AJob.SetValue(JOB_IN_MAINTHREAD, ARunInMainThread);
end;

function TYXDWorkers.Post(AJobProc: TJobProc; AData: Pointer;
  ARunInMainThread: Boolean; const ADelay, AInterval: Int64;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  {$IFDEF NEXTGEN}
  PJobProc(@AJob.WorkerProc)^ := AJobProc;
  {$ELSE}
  AJob.WorkerProc.Proc := AJobProc;
  {$ENDIF}
  InitJob(AJob, AData, ARunInMainThread, ADelay, AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  Result := Post(AJob);
end;

function TYXDWorkers.Post(AJobProc: TJobProcG; AData: Pointer;
  ARunInMainThread: Boolean; const ADelay, AInterval: Int64;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  {$IFDEF NEXTGEN}
  PJobProc(@AJob.WorkerProc)^ := MakeJobProc(AJobProc);
  {$ELSE}
  AJob.WorkerProc.Proc := MakeJobProc(AJobProc);
  {$ENDIF}
  InitJob(AJob, AData, ARunInMainThread, ADelay, AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  Result := Post(AJob);
end;

{$IFDEF UNICODE}
function TYXDWorkers.Post(AJobProc: TJobProcA; AData: Pointer;
  ARunInMainThread: Boolean; const ADelay, AInterval: Int64;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  TJobProcA(AJob.WorkerProc.ProcA) := AJobProc;
  InitJob(AJob, AData, ARunInMainThread, ADelay, AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  AJob.IsAnonWorkerProc := True;
  Result := Post(AJob);
end;
{$ENDIF}

function TimeToDelay(const ATime: TDateTime): Int64; {$IFDEF USEINLINE}inline;{$ENDIF}
var
  ANow, ATemp: TDateTime;
begin
  ANow := Now;
  ANow := ANow - Trunc(ANow); // ATime我们只要时间部分，日期忽略
  ATemp := ATime - Trunc(ATime);
  if ANow > ATemp then // 好吧，今天的点已经过了，算明天
    Result := Trunc(((1 - ANow) + ATemp) * WODay) // 延迟的时间，单位为1ms
  else
    Result := Trunc((ATemp - ANow) * WODay);
end;

function TYXDWorkers.Post(AJobProc: TJobProcG; AData: Pointer;
  const ATime: TDateTime; const AInterval: Int64; ARunInMainThread: Boolean;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  {$IFDEF NEXTGEN}
  PJobProc(@AJob.WorkerProc)^ := MakeJobProc(AJobProc);
  {$ELSE}
  AJob.WorkerProc.Proc := MakeJobProc(AJobProc);
  {$ENDIF}
  InitJob(AJob, AData, ARunInMainThread, TimeToDelay(ATime), AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  Result := Post(AJob);
end;

function TYXDWorkers.Post(AJobProc: TJobProc; AData: Pointer;
  const ATime: TDateTime; const AInterval: Int64; ARunInMainThread: Boolean;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  {$IFDEF NEXTGEN}
  PJobProc(@AJob.WorkerProc)^ := AJobProc;
  {$ELSE}
  AJob.WorkerProc.Proc := AJobProc;
  {$ENDIF}
  InitJob(AJob, AData, ARunInMainThread, TimeToDelay(ATime), AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  Result := Post(AJob);
end;

{$IFDEF UNICODE}
function TYXDWorkers.Post(AJobProc: TJobProcA; AData: Pointer;
  const ATime: TDateTime; const AInterval: Int64; ARunInMainThread: Boolean;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  TJobProcA(AJob.WorkerProc.ProcA) := AJobProc;
  InitJob(AJob, AData, ARunInMainThread, TimeToDelay(ATime), AInterval);
  InitJobFreeType(Self, AJob, AData, AFreeType);
  AJob.IsAnonWorkerProc := True;
  Result := Post(AJob);
end;
{$ENDIF}

procedure InitLogJob(AJob: PJob; AData: Pointer); {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  AJob.Data := AData;
  AJob.SetValue(JOB_LONGTIME, True);
  AJob.SetValue(JOB_RUN_ONCE, True); // 长作业只运行一次
end;

function TYXDWorkers.PostLongJob(AJobProc: TJobProc; AData: Pointer;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  if AtomicIncrement(FLongTimeWorkers) <= FMaxLongtimeWorkers then begin
    AJob := JobPool.Pop;
    {$IFDEF NEXTGEN}
    PJobProc(@AJob.WorkerProc)^ := AJobProc;
    {$ELSE}
    AJob.WorkerProc.Proc := AJobProc;
    {$ENDIF}
    InitLogJob(AJob, AData);
    InitJobFreeType(self, AJob, AData, AFreeType);
    Result := Post(AJob);
  end else begin // 长期作业数已经达到极限
    AtomicDecrement(FLongTimeWorkers);
    Result := 0;
  end;
end;

function TYXDWorkers.PostLongJob(AJobProc: TJobProcG; AData: Pointer;
  AFreeType: TJobDataFreeType): TJobHandle;
begin
  Result := PostLongJob(MakeJobProc(AJobProc), AData, AFreeType);
end;

{$IFDEF UNICODE}
function TYXDWorkers.PostLongJob(AJobProc: TJobProcA; AData: Pointer;
  AFreeType: TJobDataFreeType): TJobHandle;
var
  AJob: PJob;
begin
  if AtomicIncrement(FLongTimeWorkers) <= FMaxLongtimeWorkers then begin
    AJob := JobPool.Pop;
    TJobProcA(AJob.WorkerProc.ProcA) := AJobProc;
    InitLogJob(AJob, AData);
    InitJobFreeType(self, AJob, AData, AFreeType);
    AJob.IsAnonWorkerProc := True;
    Result := Post(AJob);
  end else begin // 长期作业数已经达到极限
    AtomicDecrement(FLongTimeWorkers);
    Result := 0;
  end;
end;
{$ENDIF}

procedure InitWaitJob(AJob: PJob; ASignalId: Integer; ARunInMainThread: Boolean); {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  AJob.Data := nil;
  AJob.SignalId := ASignalId;
  AJob.PushTime := GetTimestamp;
  AJob.SetValue(JOB_SIGNAL_WAKEUP, True);
  AJob.SetValue(JOB_IN_MAINTHREAD, ARunInMainThread);
end;

function TYXDWorkers.PostWaitJob(AJob: PJob; ASignalId: Integer): TJobHandle;
var
  ASignal: PSignal;
begin
  Result := 0;
  FLocker.Enter;
  try
    ASignal := FSignalJobs.FindFirstData(ASignalId);
    if ASignal <> nil then begin
      AJob.Next := ASignal.First;
      ASignal.First := AJob;
      Result := TJobHandle(AJob) + $02;
    end;
  finally
    FLocker.Leave;
    if Result = 0 then
      JobPool.Push(AJob);
  end;
end;

function TYXDWorkers.PostWait(AJobProc: TJobProc; ASignalId: Integer;
  ARunInMainThread: Boolean): TJobHandle;
var
  AJob: PJob;
begin
  if not FTerminating then begin
    AJob := JobPool.Pop;
    {$IFDEF NEXTGEN}
    PJobProc(@AJob.WorkerProc)^ := AJobProc;
    {$ELSE}
    AJob.WorkerProc.Proc := AJobProc;
    {$ENDIF}
    InitWaitJob(AJob, ASignalId, ARunInMainThread);
    Result := PostWaitJob(AJob, ASignalId);
  end else
    Result := 0;
end;

function TYXDWorkers.PostWait(AJobProc: TJobProcG; ASignalId: Integer;
  ARunInMainThread: Boolean): TJobHandle;
begin
  Result := PostWait(MakeJobProc(AJobProc), ASignalId, ARunInMainThread);
end;

{$IFDEF UNICODE}
function TYXDWorkers.PostWait(AJobProc: TJobProcA; ASignalId: Integer;
  ARunInMainThread: Boolean): TJobHandle;
var
  AJob: PJob;
begin
  if not FTerminating then begin
    AJob := JobPool.Pop;
    TJobProcA(AJob.WorkerProc.ProcA) := AJobProc;
    InitWaitJob(AJob, ASignalId, ARunInMainThread);
    AJob.IsAnonWorkerProc := True;
    Result := PostWaitJob(AJob, ASignalId);
  end else
    Result := 0;
end;
{$ENDIF}

function TYXDWorkers.Post(AJob: PJob): TJobHandle;
begin
  Result := 0;
  if (not FTerminating) and (Assigned(AJob.WorkerProc.Proc)
    {$IFDEF UNICODE} or Assigned(AJob.WorkerProc.ProcA){$ENDIF}) then
  begin
    if AJob.Runonce and (AJob.FirstDelay = 0) then begin
      if FSimpleJobs.Push(AJob) then begin
        Result := TJobHandle(AJob);
        LookupIdleWorker(True);
      end;
    end else if FRepeatJobs.Push(AJob) then begin
      Result := TJobHandle(AJob) or JOB_HANDLE_REPEAT_MASK;
      LookupIdleWorker(False);
    end;
  end else begin
    AJob.Next := nil;
    FreeJob(AJob);
  end;
end;

procedure TYXDWorkers.SendSignal(AId: Integer; AData: Pointer;
  AFreeType: TJobDataFreeType);
var
  AFound: Boolean;
  ASignal: PSignal;
begin
  AFound := False;
  if FTerminating then Exit;
  FLocker.Enter;
  try
    ASignal := FSignalJobs.FindFirstData(AId);
    if ASignal <> nil then begin
      AFound := True;
      FireSignalJob(ASignal, AData, AFreeType);
    end;
  finally
    FLocker.Leave;
  end;
  if AFound then
    LookupIdleWorker(True)
  else
    FreeJobData(AData, AFreeType);
end;

procedure TYXDWorkers.SendSignal(const AName: string; AData: Pointer;
  AFreeType: TJobDataFreeType);
var
  i: Integer;
  ASignal: PSignal;
  AFound: Boolean;
begin
  AFound := False;
  if Length(AName) = 0 then Exit;
  FLocker.Enter;
  try
    for i := 0 to FSignalJobs.BucketCount - 1 do begin
      if FSignalJobs.Buckets[i] <> nil then begin
        ASignal := FSignalJobs.Buckets[i].Data;
        if (Length(ASignal.Name) = Length(AName)) and (ASignal.Name = AName) then begin
          AFound := True;
          FireSignalJob(ASignal, AData, AFreeType);
          Break;
        end;
      end;
    end;
  finally
    FLocker.Leave;
  end;
  if AFound then
    LookupIdleWorker(True)
  else
    FreeJobData(AData, AFreeType);
end;

procedure TYXDWorkers.SetEnabled(const Value: Boolean);
begin
  if Value then
    EnableWorkers
  else
    DisableWorkers;
end;

procedure TYXDWorkers.SetFireTimeout(const Value: Integer);
begin
  if Value <= 0 then
    FFireTimeout := MaxInt
  else
    FFireTimeout := Value;
end;

procedure TYXDWorkers.SetMaxLongtimeWorkers(const Value: Integer);
begin
  if FMaxLongtimeWorkers <> Value then begin
    if Value > (MaxWorkers shr 1) then // 长时间运行的作业不能大于最大工作线程的一半
      raise Exception.Create(STooManyLongtimeWorker);
    FMaxLongtimeWorkers := Value;
  end;
end;

procedure TYXDWorkers.SetMaxWorkers(const Value: Integer);
var
  ATemp, AMaxLong: Integer;
begin
  if (Value >= 2) and (FMaxWorkers <> Value) then begin
    AtomicExchange(ATemp, FLongTimeWorkers);
    AtomicExchange(FLongTimeWorkers, 0); // 强制置0，防止有新入的长时间作业
    AMaxLong := Value shr 1;
    FLocker.Enter;
    try
      if FLongTimeWorkers < AMaxLong then begin // 已经进行的长时间作业数小于一半的工作者
        if ATemp < AMaxLong then
          AMaxLong := ATemp;                    // 长时间作业最大值使用更改之前已经存在的长时间作业数量
        if FMaxWorkers > Value then begin
          FMaxWorkers := Value;
          SetLength(FWorkers, Value + 1);
        end else begin
          FMaxWorkers := Value;
          SetLength(FWorkers, Value + 1);
        end;
      end;
    finally
      FLocker.Leave;
      AtomicExchange(FLongTimeWorkers, AMaxLong);
    end;
  end;
end;

procedure TYXDWorkers.SetMinWorkers(const Value: Integer);
begin
  if FMinWorkers <> Value then begin
    if Value < 1 then
      raise Exception.Create(STooFewWorkers);
    FMinWorkers := Value;
  end;
end;

function TYXDWorkers.SignalIdByName(const AName: string): Integer;
var
  i, j: Integer;
  ASignal: PSignal;
begin
  Result := -1;
  j := Length(AName);
  if j < 1 then Exit;
  for i := 0 to FSignalJobs.BucketCount - 1 do begin
    if FSignalJobs.Buckets[i] <> nil then begin
      ASignal := FSignalJobs.Buckets[i].Data;
      if (Length(ASignal.Name) = j) and (ASignal.Name = AName) then begin
        Result := ASignal.Id;
        Exit;
      end;
    end;
  end;
end;

procedure TYXDWorkers.SignalWorkDone(AJob: PJob; AUsedTime: Int64);
var
  ASignal: PSignal;
  ATemp, APrior: PJob;
begin
  APrior := nil;
  FLocker.Enter;
  try
    ASignal := FSignalJobs.FindFirstData(AJob.SignalId);
    ATemp := ASignal.First;
    while ATemp <> nil do begin
      if ATemp = AJob.Source then begin
        if AJob.IsTerminated then begin
          if APrior <> nil then
            APrior.Next := ATemp.Next
          else
            ASignal.First := ATemp.Next;
          ATemp.Next := nil;
          FreeJob(ATemp);
        end else begin
          Inc(ATemp.Runs);  // 更新信号作业的统计信息
          if AUsedTime > 0 then begin
            if ATemp.MinUsedTime = 0 then
              ATemp.MinUsedTime := AUsedTime
            else if AUsedTime < ATemp.MinUsedTime then
              ATemp.MinUsedTime := AUsedTime;
            if ATemp.MaxUsedTime = 0 then
              ATemp.MaxUsedTime := AUsedTime
            else if AUsedTime > ATemp.MaxUsedTime then
              ATemp.MaxUsedTime := AUsedTime;
            Break;
          end;
        end;
      end;
      APrior := ATemp;
      ATemp := ATemp.Next;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TYXDWorkers.WaitRunningDone(const AParam: TWorkerWaitParam);
var
  AInMainThread: Boolean;

  function HasJobRunning: Boolean;
  var
    i: Integer;
    AJob: PJob;
  begin
    Result := False;
    DisableWorkers;
    FLocker.Enter;
    try
      for i := 0 to FWorkerCount - 1 do begin
        if FWorkers[i].IsLookuping then begin// 还未就绪，所以在下次查询
          Result := True;
          Break;
          //Continue;
        end else if FWorkers[i].IsExecuting then begin
          AJob := FWorkers[i].FActiveJob;
          case AParam.WaitType of
            0: // ByObject
              Result := TMethod(FWorkers[i].FActiveJobProc).Data = AParam.Bound;
            1: // ByData
              Result := (TMethod(FWorkers[i].FActiveJobProc).Code = TMethod(AParam.WorkerProc).Code) and
                (TMethod(FWorkers[i].FActiveJobProc).Data = TMethod(AParam.WorkerProc).Data) and
                ((AParam.Data = nil) or (AParam.Data = Pointer(-1)) or
                (FWorkers[i].FActiveJobData = AParam.Data));
            2: // BySignalSource
              Result := (FWorkers[i].FActiveJobSource = AParam.SourceJob);
            3: // ByGroup
              Result := (FWorkers[i].FActiveJobGroup = AParam.Group);
            $FF: // 所有
              Result := True;
          else 
            begin
              if Assigned(FOnError) then
                FOnError(AJob, Exception.CreateFmt(SBadWaitDoneParam, [AParam.WaitType]), jesWaitDone)
              else
                raise Exception.CreateFmt(SBadWaitDoneParam, [AParam.WaitType]);
            end;
          end;
          if Result then
            FWorkers[i].FTerminatingJob := AJob;
        end;
      end;
    finally
      FLocker.Leave;
      EnableWorkers;
    end;
  end;

begin
  AInMainThread := GetCurrentThreadId = MainThreadId;
  while True do begin
    if HasJobRunning then begin
      if AInMainThread then
        // 如果是在主线程中清理，由于作业可能在主线程执行，可能已经投寄尚未执行，所以必需让其能够执行
        ProcessAppMessage;
      Sleep(20);
    end else // 没找到
      Break;
  end;
end;

procedure TYXDWorkers.WorkerTerminate(AWorker: TYXDWorker);
var
  i, J: Integer;
begin
  FLocker.Enter;
  try
    Dec(FWorkerCount);
    if AWorker.IsFiring then
      AtomicDecrement(FFiringWorkerCount);
    if FWorkerCount = 0 then
      FWorkers[0] := nil
    else begin
      for i := 0 to FWorkerCount do begin
        if AWorker = FWorkers[i] then begin
          for J := i to FWorkerCount do
            FWorkers[J] := FWorkers[J + 1];
          Break;
        end;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TYXDWorkers.WorkerTimeout(AWorker: TYXDWorker);
var
  AWorkers: Integer;
begin
  AWorkers := FWorkerCount - AtomicIncrement(FFiringWorkerCount);
  if (AWorkers < FMinWorkers) or (AWorkers = BusyWorkerCount) then // 至少保留1个空闲
    AtomicDecrement(FFiringWorkerCount)
  else begin
    AWorker.SetValue(WORKER_FIRING, true);
    AWorker.Terminate;
  end;
end;

function TYXDWorkers.RegisterSignal(const AName: string): Integer;
var
  ASignal: PSignal;
begin
  FLocker.Enter;
  try
    Result := SignalIdByName(AName);
    if Result < 0 then begin
      Inc(FMaxSignalId);
      New(ASignal);
      ASignal.Id := FMaxSignalId;
      ASignal.Fired := 0;
      ASignal.Name := AName;
      ASignal.First := nil;
      FSignalJobs.Add(ASignal, ASignal.Id);
      Result := ASignal.Id;
    end;
  finally
    FLocker.Leave;
  end;
end;

function TYXDWorkers.GetSimpleJobCount(): Integer;
begin
  if Assigned(FSimpleJobs) then
    Result := FSimpleJobs.Count
  else
    Result := 0;
end;

function TYXDWorkers.GetRepeatJobCount(): Integer;
begin
  if Assigned(FRepeatJobs) then
    Result := FRepeatJobs.Count
  else
    Result := 0;
end;

function TYXDWorkers.GetSignalJobCount(): Integer;
begin
  if Assigned(FSignalJobs) then
    Result := FSignalJobs.Count
  else
    Result := 0;
end;

{ TJobGroup }

function TJobGroup.Add(AProc: TJobProc; AData: Pointer;
  AInMainThread: Boolean; AFreeType: TJobDataFreeType): Boolean;
var
  AJob: PJob;
begin
  AJob := JobPool.Pop;
  AJob.Group := Self;
  {$IFDEF NEXTGEN}
  PJobProc(@AJob.WorkerProc)^ := AProc;
  {$ELSE}
  AJob.WorkerProc.Proc := AProc;
  {$ENDIF}
  AJob.Data := AData;
  AJob.SetValue(JOB_RUN_ONCE, True);
  AJob.SetValue(JOB_GROUPED, True);
  AJob.SetValue(JOB_IN_MAINTHREAD, AInMainThread);
  InitJobFreeType(FOwner, AJob, AData, AFreeType);
  FLocker.Enter;
  try
    FWaitResult := wrIOCompletion;
    if FPrepareCount > 0 then begin // 正在添加项目，加到列表中，等待Run
      FItems.Add(AJob);
      Result := True;
    end else begin
      if ByOrder then begin // 按顺序
        Result := True;
        FItems.Add(AJob);
        if FItems.Count = 0 then
          Result := FOwner.Post(AJob) <> 0;
      end else begin
        Result := FOwner.Post(AJob) <> 0;
        if Result then
          FItems.Add(AJob);
      end;
      if Result then
        AtomicIncrement(FPosted);
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TJobGroup.Cancel(AWaitRunningDone: Boolean);
var
  I: Integer;
  AJob: PJob;
  AWaitParam: TWorkerWaitParam;
begin
  FLocker.Enter;
  try
    AtomicExchange(FCanceled, 0);
    if FByOrder then begin
      I := 0;
      while I < FItems.Count do begin
        AJob := FItems[i];
        if AJob.PopTime = 0 then begin
          FOwner.FreeJob(AJob);
          FItems.Delete(I);
          AtomicIncrement(FCanceled);
        end else
          Inc(I);
      end;
    end;
    FItems.Clear;
  finally
    FLocker.Leave;
  end;
  if (FPosted <> 0) then begin
    I := Workers.FSimpleJobs.Clear(Self, MaxInt);
    if I > 0 then begin
      AtomicIncrement(FPosted, -I);
      AtomicIncrement(FCanceled, I);
    end;
    if AWaitRunningDone then begin
      AWaitParam.WaitType := 3;
      AWaitParam.Group := Self;
      Workers.WaitRunningDone(AWaitParam);
    end;
  end;
  if FPosted = 0 then begin
    if FCanceled > 0 then
      FWaitResult := wrAbandoned;
    FEvent.SetEvent;
  end;
end;

constructor TJobGroup.Create(AOwner: TYXDWorkers; AByOrder: Boolean);
begin
  if Assigned(AOwner) then
    FOwner := AOwner
  else
    FOwner := Workers;
  if (not Assigned(FOwner)) then
    raise Exception.Create(SNotInitWorkers);
  FEvent := TEvent.Create(nil, False, False, '');
  FLocker := TSimpleLock.Create;
  FByOrder := AByOrder;
  FItems := TJobItemList.Create;
end;

constructor TJobGroup.Create(AByOrder: Boolean);
begin
  Create(nil, AByOrder);
end;

destructor TJobGroup.Destroy;
var
  i: Integer;
begin
  Cancel;
  if FTimeoutCheck then
    FOwner.Clear(Self, 1);
  FLocker.Enter;
  try
    if FItems.Count > 0 then begin
      FWaitResult := wrAbandoned;
      FEvent.SetEvent;
      for i := 0 to FItems.Count - 1 do begin
        if PJob(FItems[i]).PushTime <> 0 then
          JobPool.Push(FItems[i]);
      end;
      FItems.Clear;
    end;
  finally
    FLocker.Leave;
  end;
  FreeAndNil(FLocker);
  FreeAndNil(FEvent);
  FreeAndNil(FItems);
  inherited;
end;

procedure TJobGroup.DoAfterDone;
begin
  if Assigned(FAfterDone) then begin
    try
      FAfterDone(Self);
    except
      if Assigned(FOwner.FOnError) then
        FOwner.FOnError(nil, Exception(ExceptObject), jesAfterDone);
    end;
  end;
end;

procedure TJobGroup.DoJobExecuted(AJob: PJob);
var
  i: Integer;
  AIsDone: Boolean;
begin
  AtomicIncrement(FRuns);
  if FWaitResult = wrIOCompletion then begin
    AIsDone := False;
    FLocker.Enter;
    try
      i := FItems.IndexOf(AJob);
      if i <> -1 then begin
        FItems.Delete(i);
        if FItems.Count = 0 then begin
          AIsDone := true;
          FWaitResult := wrSignaled;          
          AtomicExchange(FPosted, 0);
        end else if ByOrder then begin
          if FOwner.Post(FItems[0]) = 0 then begin
            AtomicDecrement(FPosted);
            FItems.Delete(0); // 投寄失败时，Post自动释放了作业
            FWaitResult := wrAbandoned;
            AIsDone := True;
          end;
        end else if (FOwner.FMaxWorkers > 0) and (FPosted <= FOwner.FMaxWorkers) and (FPosted <= FItems.Count) then begin
          if FOwner.Post(FItems[FPosted-1]) = 0 then begin
            AtomicDecrement(FPosted);
            FItems.Delete(0); // 投寄失败时，Post自动释放了作业
            FWaitResult := wrAbandoned;
            AIsDone := True;
          end
        end else
          AtomicDecrement(FPosted);
      end else begin
        AIsDone := (FItems.Count = 0) and (AtomicDecrement(FPosted) = 0);
        if AIsDone then begin
          if FCanceled = 0 then
            FWaitResult := wrSignaled
          else begin
            FWaitResult := wrAbandoned;
            AtomicExchange(FCanceled, 0);
          end;
        end;
      end;
    finally
      FLocker.Leave;
    end;
    if AIsDone then begin
      FEvent.SetEvent;
      DoAfterDone;
    end;
  end;
end;

procedure TJobGroup.DoJobsTimeout(AJob: PJob);
begin
  FTimeoutCheck := False;
  Cancel;
  if FWaitResult = wrIOCompletion then begin
    FWaitResult := wrTimeout;
    FEvent.SetEvent;
    DoAfterDone;
  end;
end;

function TJobGroup.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJobGroup.MsgWaitFor(ATimeout: Cardinal): TWaitResult;
var
  AEmpty: Boolean;
begin
  Result := FWaitResult;
  if GetCurrentThreadId <> MainThreadId then
    Result := WaitFor(ATimeout)
  else begin
    FLocker.Enter;
    try
      AEmpty := FItems.Count = 0;
      if AEmpty then
        Result := wrSignaled;
    finally
      FLocker.Leave;
    end;
    if Result = wrIOCompletion then begin
      if MsgWaitForEvent(FEvent, ATimeout) = wrSignaled then
        Result := FWaitResult;
      if Result = wrIOCompletion then begin
        Cancel;
        if Result = wrIOCompletion then
          Result := wrTimeout;
      end;
      if FTimeoutCheck then
        Workers.Clear(Self);
      if Result = wrTimeout then
        DoAfterDone;
    end else if AEmpty then
      DoAfterDone;
  end;
end;

procedure TJobGroup.Prepare;
begin
  AtomicIncrement(FPrepareCount);
end;

procedure TJobGroup.Run(ATimeout: Cardinal = INFINITE);
var
  i: Integer;
  AJob: PJob;
begin
  if AtomicDecrement(FPrepareCount) = 0 then begin
    if ATimeout <> INFINITE then begin
      FTimeoutCheck := True;
      FOwner.Post(DoJobsTimeout, nil, False, ATimeout);
    end;
    FLocker.Enter;
    try
      if FItems.Count = 0 then
        FWaitResult := wrSignaled
      else begin
        FWaitResult := wrIOCompletion;
        if ByOrder then begin
          AJob := FItems[0];
          if (AJob.PushTime = 0) then begin
            if Workers.Post(AJob) = 0 then
              FWaitResult := wrAbandoned
            else
              AtomicIncrement(FPosted);
          end;
        end else begin
          for i := 0 to FItems.Count - 1 do begin
            AJob := FItems[i];
            if AJob.PushTime = 0 then begin
              if FOwner.Post(AJob) = 0 then begin
                FWaitResult := wrAbandoned;
                Break;
              end else
                AtomicIncrement(FPosted);
            end;
          end;
        end;
      end;
    finally
      FLocker.Leave;
    end;
    if FWaitResult <> wrIOCompletion then begin
      DoAfterDone;
      FEvent.SetEvent;
    end;
  end;
end;

function TJobGroup.WaitFor(ATimeout: Cardinal): TWaitResult;
var
  AEmpty: Boolean;
begin
  Result := FWaitResult;
  FLocker.Enter;
  try
    AEmpty := FItems.Count = 0;
    if AEmpty then
      Result := wrSignaled;
  finally
    FLocker.Leave;
  end;
  if Result = wrIOCompletion then begin
    if FEvent.WaitFor(ATimeout) = wrSignaled then
      Result := FWaitResult
    else begin
      Result := wrTimeout;
      Cancel;
    end;
    if Result = wrTimeout then
      DoAfterDone;
  end;
  if FTimeoutCheck then
    FOwner.Clear;
  if AEmpty then
    DoAfterDone;
end;

{ TWorkerStateItem }

function TWorkerStateItem.GetValue(Index: Integer): Boolean;
begin
  Result := (Flags and Index) <> 0;
end;

{ TJobMethod }

function ToJobProc(const AMethod: TJobMethod): TJobProc;
begin
  {$IFDEF NEXTGEN}
  Result := PJobProc(@AMethod)^;
  {$ELSE} 
  Result := AMethod.Proc; 
  {$ENDIF}
end;

{ TForJobs }

procedure TForJobs.BreakIt;
begin
  AtomicExchange(FBreaked, 1);
end;

constructor TForJobs.Create(const AStartIndex, AStopIndex: TForLoopIndexType;
  AData: Pointer; AFreeType: TJobDataFreeType);
begin
  Create(nil, AStartIndex, AStopIndex, AData, AFreeType);
end;

constructor TForJobs.Create(AOwner: TYXDWorkers; const AStartIndex,
  AStopIndex: TForLoopIndexType; AData: Pointer; AFreeType: TJobDataFreeType);
var
  ACount: TForLoopIndexType;
begin
  if Assigned(AOwner) then
    FOwner := AOwner
  else
    FOwner := Workers;
  if (not Assigned(FOwner)) then
    raise Exception.Create(SNotInitWorkers);
  FIterator := AStartIndex - 1;
  FStartIndex := AStartIndex;
  FStopIndex := AStopIndex;
  FWorkerCount := FOwner.FCPUNum shl 1;
  ACount := (AStopIndex - AStartIndex) + 1;
  if FWorkerCount > ACount then
    FWorkerCount := ACount;
  FWorkJob := JobPool.Pop;
  FWorkJob.Data := AData;
  if AData <> nil then begin
    FWorkJob.Flags := FWorkJob.Flags or (Integer(AFreeType) shl 8);
    if AFreeType = jdfFreeAsInterface then
      IUnknown(AData)._AddRef;
  end;
  FWorkJob.SetValue(JOB_RUN_ONCE, True);
  FWorkJob.SetValue(JOB_IN_MAINTHREAD, False);
  {$IFDEF USEINLINE}
  FEvent := TEvent.Create();
  {$ELSE}
  FEvent := TEvent.Create(nil, True, False, '');
  {$ENDIF}
end;

destructor TForJobs.Destroy;
begin
  FOwner.FreeJob(FWorkJob);
  FreeAndNil(FEvent);
  inherited;
end;

procedure TForJobs.DoJob(AJob: PJob);
var
  i: NativeInt;
begin
  try
    repeat
      i := AtomicIncrement(FIterator);
      if i <= StopIndex then begin
        {$IFDEF UNICODE}
        if FWorkJob.IsAnonWorkerProc then
          TForJobProcA(FWorkJob.WorkerProc.ForProcA)(Self, FWorkJob, i)
        else
        {$ENDIF}
          if FWorkJob.WorkerProc.Data = nil then
            FWorkJob.WorkerProc.ForProcG(Self, FWorkJob, i)
          else
            PForJobProc(@FWorkJob.WorkerProc)^(Self, FWorkJob, i);
          AtomicIncrement(FWorkJob.Runs);
      end else
        Break;
    until (FIterator > StopIndex) or (FBreaked <> 0) or (AJob.IsTerminated);
  except
    on E: Exception do
      if Assigned(FOwner.OnError) then
        FOwner.OnError(FWorkJob, E, jesExecute); 
  end;
  if AJob.IsTerminated then
    BreakIt;
  if AtomicDecrement(FWorkerCount) = 0 then
    FEvent.SetEvent;
end;

function TForJobs.GetAvgTime: Cardinal;
begin
  if Runs > 0 then
    Result := TotalTime div Runs
  else
    Result := 0;
end;

function TForJobs.GetBreaked: Boolean;
begin
  Result := FBreaked <> 0;
end;

function TForJobs.GetRuns: Cardinal;
begin
  Result := FWorkJob.Runs;
end;

function TForJobs.GetTotalTime: Cardinal;
begin
  Result := FWorkJob.TotalUsedTime;
end;

procedure TForJobs.Start;
var
  i: Integer;
begin
  FWorkJob.StartTime := GetTimestamp;
  FOwner.DisableWorkers;
  for i := 0 to FWorkerCount - 1 do
    FOwner.Post(DoJob, nil);
  FOwner.EnableWorkers;
end;

function TForJobs.Wait(AMsgWait: Boolean): TWaitResult;
begin
  if FWorkerCount > 0 then begin
    if AMsgWait then
      Result := MsgWaitForEvent(FEvent, INFINITE)
    else
      Result := FEvent.WaitFor(INFINITE);
    if FBreaked <> 0 then
      Result := wrAbandoned;
  end else
    Result := wrSignaled;
  FWorkJob.TotalUsedTime := GetTimestamp - FWorkJob.StartTime;
end;

{ TJobExtData }

{$IFDEF UNICODE}
constructor TJobExtData.Create(AData: Pointer; AOnFree: TExtFreeEventA);
begin
  FData := AData;
  FOnFreeA := AOnFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(AOnFree: TExtFreeEventA);
begin
  FOnFreeA := AOnFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;
{$ENDIF}

constructor TJobExtData.Create(AData: Pointer; AOnFree: TExtFreeEvent);
begin
  FData := AData;
  FOnFree := AOnFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(AOnFree: TExtFreeEvent);
begin
  FOnFree := AOnFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(const Value: Int64);
begin
  {$IFDEF CPUX64}
  FData := Pointer(Value);
  {$ELSE}
  GetMem(FData, SizeOf(Value));
  PInt64(FData)^ := Value;
  FOnFree := DoSimpleTypeFree;
  {$ENDIF}
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(const Value: Double);
begin
  GetMem(FData, SizeOf(Value));
  PDouble(FData)^ := Value;
  FOnFree := DoSimpleTypeFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(const Value: string);
var
  P: PString;
begin
  New(P);
  P^ := Value;
  Create(P, DoFreeAsString);
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(const Value: Integer);
begin
  FData := Pointer(Value);
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.Create(const Value: Boolean);
begin
  FData := Pointer(Integer(Value));
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

constructor TJobExtData.CreateAsDateTime(const Value: TDateTime);
begin
  GetMem(FData, SizeOf(Value));
  PDateTime(FData)^ := Value;
  FOnFree := DoSimpleTypeFree;
  {$IFDEF AutoFreeJobExData}DoAddToExDataMap;{$ENDIF}
end;

destructor TJobExtData.Destroy;
begin
  {$IFDEF AutoFreeJobExData}ExDataMap.Remove(Cardinal(Self));{$ENDIF}
  if Assigned(FData) then begin
    {$IFDEF UNICODE}
    if Assigned(FOnFreeA) then
      FOnFreeA(FData);
    {$ENDIF}
    if Assigned(FOnFree) then
      FOnFree(FData);
  end;
  inherited;
end;

{$IFNDEF UNICODE}
procedure TJobExtData.DoFreeAsWideString(var AData: Pointer);
begin
  if (AData <> nil) then begin
    Dispose(PWideString(AData));
    AData := nil;
  end;
end;
{$ENDIF}

{$IFDEF AutoFreeJobExData}
procedure TJobExtData.DoAddToExDataMap;
begin
  ExDataMap.Add(THashType(Self), NativeInt(1));
end;
{$ENDIF}

{$IFNDEF NEXTGEN}{$IFDEF UNICODE}
procedure TJobExtData.DoFreeAsAnsiString(var AData: Pointer);
begin
  if (AData <> nil) then begin
    Dispose(PAnsiString(AData));
    AData := nil;
  end;
end;
{$ENDIF}{$ENDIF}

procedure TJobExtData.DoFreeAsString(var AData: Pointer);
begin
  if (AData <> nil) then begin
    Dispose(PString(AData));
    AData := nil;
  end;
end;

procedure TJobExtData.DoSimpleTypeFree(var AData: Pointer);
begin
  if AData <> nil then begin
    FreeMem(AData);
    AData := nil;
  end;
end;

function TJobExtData.GetAsBoolean: Boolean;
begin
  Result := (FData <> nil) and (Boolean(Integer(FData)));
end;

function TJobExtData.GetAsDateTime: TDateTime;
begin
  Result := PDateTime(FData)^;
end;

function TJobExtData.GetAsDouble: Double;
begin
  Result := PDouble(FData)^;
end;

function TJobExtData.GetAsInt64: Int64;
begin
  Result := PInt64(FData)^;
end;

function TJobExtData.GetAsInteger: Integer;
begin
  Result := Integer(FData);
end;

function TJobExtData.GetAsString: string;
begin
  Result := PString(FData)^;
end;

procedure TJobExtData.SetAsBoolean(const Value: Boolean);
begin
  FData := Pointer(Integer(Value));
end;

procedure TJobExtData.SetAsDateTime(const Value: TDateTime);
begin
  PDateTime(FData)^ := Value;
end;

procedure TJobExtData.SetAsDouble(const Value: Double);
begin
  PDouble(FData)^ := Value;
end;

procedure TJobExtData.SetAsInt64(const Value: Int64);
begin
  {$IFDEF CPUX64}
  FData := Pointer(Value);
  {$ELSE}
  PInt64(FData)^ := Value;
  {$ENDIF}
end;

procedure TJobExtData.SetAsInteger(const Value: Integer);
begin
  FData := Pointer(Value);
end;

procedure TJobExtData.SetAsString(const Value: string);
begin
  PString(FData)^ := Value;
end;

function TJobExtData.SetFreeEvent(AOnFree: TExtFreeEvent): TJobExtData;
begin
  FOnFree := AOnFree;
  Result := Self;
end;

{$IFDEF UNICODE}
function TJobExtData.SetFreeEvent(AOnFree: TExtFreeEventA): TJobExtData;
begin
  FOnFreeA := AOnFree;
  Result := Self;
end;
{$ENDIF}

{$IFNDEF UNICODE}
constructor TJobExtData.Create(const Value: WideString);
var
  P: PWideString;
begin
  New(P);
  P^ := Value;
  Create(P, DoFreeAsWideString);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}{$IFDEF UNICODE}
constructor TJobExtData.Create(const Value: AnsiString);
var
  P: PAnsiString;
begin
  New(P);
  P^ := Value;
  Create(P, DoFreeAsAnsiString);
end;
{$ENDIF}{$ENDIF}

function NewExData(const Value: string): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

{$IFNDEF UNICODE}
function NewExData(const Value: WideString): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}{$IFDEF UNICODE}
function NewExData(const Value: AnsiString): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;
{$ENDIF}{$ENDIF}

function NewExData(const Value: Int64): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

function NewExData(const Value: Cardinal): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

function NewExData(const Value: Double): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

function NewExData(const Value: Boolean): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

function NewExData(const Value: Byte): TJobExtData;
begin
  Result := TJobExtData.Create(Value);
end;

function NewExDataAsTime(const Value: TDateTime): TJobExtData;
begin
  Result := TJobExtData.CreateAsDateTime(Value);
end;

initialization
  _CPUCount := GetCPUCount;
  {$IFNDEF NEXTGEN}
  GetTickCount64 := GetProcAddress(GetModuleHandle(kernel32), 'GetTickCount64');
  WinGetSystemTimes := GetProcAddress(GetModuleHandle(kernel32), 'GetSystemTimes');
  if not QueryPerformanceFrequency(_PerfFreq) then begin
    _PerfFreq := -1;
    if Assigned(GetTickCount64) then
      _StartCounter := GetTickCount64
    else
      _StartCounter := GetTickCount;
  end else
    QueryPerformanceCounter(_StartCounter);
  {$ELSE}
    _Watch := TStopWatch.Create;
    _Watch.Start;
  {$ENDIF}

  JobPool := TJobPool.Create(1024);
  {$IFDEF AutoFreeJobExData}
  ExDataMap := TIntHash.Create(9973);
  {$ENDIF}
  Workers := TYXDWorkers.Create;

finalization
  try
    if Assigned(Workers) then
      FreeAndNil(Workers);
  except
    {$IFNDEF NEXTGEN}OutputDebugString(PChar(Exception(ExceptObject).Message));{$ENDIF}
  end;
  if Assigned(JobPool) then FreeAndNil(JobPool);
  {$IFDEF AutoFreeJobExData}
  if Assigned(ExDataMap) then FreeAndNil(ExDataMap);
  {$ENDIF}

end.

