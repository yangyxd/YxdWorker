{*******************************************************}
{                                                       }
{       MapFile 文件解析单元                            }
{                                                       }
{       版权所有 (C) 2014 YangYxd                       }
{                                                       }
{*******************************************************}

unit YxdMapFile;

(*
  使用本单元请先打开工程选项，启用详细的MapFile输出开关。生成的map文件应与
  Exe文件在同一个目录。
*)

interface

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE USEINLINE}
{$IFEND}

{.$DEFINE TEST}  // 显示测试信息

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}SysUtils, Classes, SyncObjs, TlHelp32, PsAPI;

type
  TMapItem = {$IFNDEF USEINLINE}object{$ELSE}record{$ENDIF}
  private
    function GetName: string;
  public
    Addr: Pointer;    // 代码地址
    Line: Integer;    // 行号
    Position: Integer;// 名称开始位置
    Len: Integer;     // 名称长度
    IsImport: Boolean;// 是否是导入函数
    property Name: string read GetName;
  end;
  PMapItem = ^TMapItem;

  {$IFNDEF USEINLINE}
  ULONG_PTR = Cardinal;
  DWORD_PTR = ULONG_PTR;
  ULONGLONG = UInt64;
  {$ENDIF}

type
  TLineItem = packed record
    Addr: Pointer;
    Line: Integer;
  end;
  PLineItem = ^TLineItem;

{$IFDEF MSWINDOWS}
type
  TImportItem = record
    Addr: Pointer;
    Name: string;
  end;
  PImportItem = ^TImportItem;
  TImportData = packed record
    FileName: string;
    FOffset: Cardinal;
    Size: Cardinal;
    List: array of TImportItem;
  end;
  PImportData = ^TImportData;
  TImportDataList = array of TImportData;
  TImportLineItem = packed record
    Addr: Pointer;
    Data: PImportData;
    Item: PImportItem;
  end;
  PImportLineItem = ^TImportLineItem;
{$ENDIF}

type
  TPList = array of Pointer;
  PPList = ^TPList;

type
  TYxdMapFile = class(TObject)
  private
    {$IFDEF UNICODE}
    FStr: string;
    {$ELSE}
    FStream: TMemoryStream;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    FImports: TImportDataList;
    FImportList: array of TImportLineItem;
    FSortimports: TPList;
    {$ENDIF}
    FValue: PChar;
    FSize: Integer;
    FPLSize: Integer;
    FBaseAddr: Cardinal;
    FCodeSize: Cardinal;
    FMap: array of TMapItem;    // 记录CODE信息
    FRow: array of TLineItem;   // 记录地址对应的源码行号
    FSortMap: TPList;
    FSortRow: TPList;
  protected
    procedure GrowRow; virtual;
    procedure GrowMap; virtual;
    procedure InitMap();
    procedure QuickSort(List: PPList; L, R: Integer);
    function InternalFind(List: PPList; const Addr: Pointer;
      var AIndex: Integer): Boolean;
    function IndexOf(List: PPList; Addr: Pointer): Integer;
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadFromFile(const FName: string);
    {$IFDEF MSWINDOWS}
    procedure LoadDefault;
    function FindImportList(const Addr: Pointer): PImportLineItem;
    {$ENDIF}
    function GetDesc(const Addr: Pointer): string;
    function GetMap(const Addr: Pointer): PMapItem;
    property BaseAddr: Cardinal read FBaseAddr;
  end;

const
  //
  // options that are set/returned by SymSetOptions() & SymGetOptions()
  // these are used as a mask
  //
  SYMOPT_CASE_INSENSITIVE = $00000001;
  SYMOPT_UNDNAME = $00000002;
  SYMOPT_DEFERRED_LOADS = $00000004;
  SYMOPT_NO_CPP = $00000008;
  SYMOPT_LOAD_LINES = $00000010;
  SYMOPT_OMAP_FIND_NEAREST = $00000020;
  SYMOPT_DEBUG = $80000000;
  MAX_SYMNAME_SIZE = 1024;

type
  {$A4}
  PIMAGEHLP_SYMBOL = ^IMAGEHLP_SYMBOL;
  ULONG = Cardinal;
  ULONG64 = Int64;
  IMAGEHLP_SYMBOL = record
    SizeOfStruct: ULONG;
    TypeIndex: ULONG;
    Reserved: array [0 .. 1] of ULONG64;
    Index: ULONG;
    Size: ULONG;
    ModBase: ULONG64;
    Flags: ULONG;
    Value: ULONG64;
    Address: ULONG64;
    Registers: ULONG;
    Scope: ULONG;
    Tag: ULONG;
    NameLen: ULONG;
    MaxNameLen: ULONG;
    Name: array [0 .. 1] of Char;
  end;

  LPADDRESS = ^Address;
  Address = record
    Offset: DWORD;
    Segment: WORD;
    Mode: DWORD;
  end;
  ADDRESS_MODE = (AddrMode1616, AddrMode1632, AddrModeReal, AddrModeFlat);
  PKDHELP = ^KDHELP; 
  KDHELP = record
    Thread: DWORD;
    ThCallbackStack: DWORD;
    NextCallback: DWORD;
    FramePointer: DWORD;
    KiCallUserMode: DWORD;
    KeUserCallbackDispatcher: DWORD;
    SystemRangeStart: DWORD;
    ThCallbackBStore: DWORD;
    Reserved: array [0 .. 7] of DWORD;
  end;
  LPSTACKFRAME = ^STACKFRAME;
  STACKFRAME = record
    AddrPC: Address; // program counter
    AddrReturn: Address; // return address
    AddrFrame: Address; // frame pointer
    AddrStack: Address; // stack pointer
    FuncTableEntry: Pointer; // pointer to pdata/fpo or NULL
    Params: array [0 .. 3] of DWORD; // possible arguments to the function
    bFar: LONGBOOL; // WOW far call
    bVirtual: LONGBOOL; // is this a virtual frame?
    Reserved: array [0 .. 2] of DWORD;
    KDHELP: KDHELP;
    AddrBStore: Address; // backing store pointer
  end;
  TStackFrames = array of STACKFRAME;
  PIMAGEHLP_LINE = ^IMAGEHLP_LINE;
  IMAGEHLP_LINE = record
    SizeOfStruct: DWORD; // set to sizeof(IMAGEHLP_LINE)
    Key: Pointer; // internal
    LineNumber: DWORD; // line number in file
    FileName: PChar; // full filename
    Address: DWORD; // first instruction of line
  end;
  {$A-}
  
const
  WCT_MAX_NODE_COUNT       = 16;
  WCT_OBJNAME_LENGTH       = 128;
  WCT_ASYNC_OPEN_FLAG      = $01;
  WCTP_OPEN_ALL_FLAGS      = WCT_ASYNC_OPEN_FLAG;
  WCT_OUT_OF_PROC_FLAG     = $01;
  WCT_OUT_OF_PROC_COM_FLAG = $02;
  WCT_OUT_OF_PROC_CS_FLAG  = $04;
  WCT_NETWORK_IO_FLAG      = $08;
  WCTP_GETINFO_ALL_FLAGS = WCT_OUT_OF_PROC_FLAG or WCT_OUT_OF_PROC_COM_FLAG or
    WCT_OUT_OF_PROC_CS_FLAG;

  THREAD_GET_CONTEXT       = $0008;
  THREAD_QUERY_INFORMATION = $0040;
  THREAD_ALL_ACCESS        = $1FFFFF;

const
  ObjectTypes: array [0 .. 10] of String = ('CriticalSection', 'SendMessage',
    'Mutex', 'Alpc', 'Com', 'ThreadWait', 'ProcWait', 'Thread', 'ComActivation',
    'Unknown', 'Max');
type
  THREAD_BASIC_INFORMATION = record
    ExitStatus: Cardinal;
    TebBaseAddress: Pointer;
    ProcessId: THandle;
    ThreadId: THandle;
    AffinityMask: ULONG;
    Priority: LongInt;
    BasePriority: LongInt;
  end;
  TNtQueryInformationThread = function(ThreadHandle: THandle;
    ThreadInformationClass: Cardinal; ThreadInformation: Pointer;
    ThreadInformationLength: ULONG; ReturnLength: PULONG): Cardinal; stdcall;

type
  TThreadState = (thd_Ready {就绪}, thd_Running {运行中}, thd_Waiting {等待中},
    thd_Blocking {阻塞}, thd_Terminate {中止});
  TThreadInfoEx = record
    ThreadID: THandle;
    BasePri: Longint;
    Handle: THandle;
    State: TThreadState;
    ExitCode: DWORD;
    EIP: DWORD;
    ErrMsg: string;
    LockMsg: string;
    Stack: string;
  end;
  PThreadInfoEx = ^TThreadInfoEx;
  TThreadInfoExList = array of TThreadInfoEx;

// 代码地址转换为详细信息
function GetDebugMapDesc(Addr: Pointer): string;
// 代码地址转换为Map信息
function GetDebugMap(Addr: Pointer): PMapItem;
{$IFDEF MSWINDOWS}
// 获取指定线程堆栈并根据EIP转换为Map信息
function GetThreadMap(hThread: THandle): PMapItem;
// 获取指定线程堆栈信息
function GetThreadStack(hThread: THandle): TContext;
// 获取指定线程调试信息
function GetThreadMapDest(hThread: THandle): string;
{$ENDIF}

function StackByThreadHandle(AThread: THandle; const SLine: string = SLineBreak): string; overload;
function StackByThreadHandle(AThread: THandle; var EIP: Cardinal; const SLine: string = SLineBreak): string; overload;
function DebugHelperExists: Boolean;
function GetThreadStacks(AThreadHandle: THandle): TStackFrames;
function GetFunctionInfo(Addr: Pointer): String;
// 获取指定进程句柄的线程信息
function GetProcessThreadInfo(AProcId: THandle; const LineChar: string = SLineBreak;
  CheckCycle: Boolean = False): TThreadInfoExList;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: Boolean;
  dwThreadId: Cardinal): THandle; stdcall;

var
  NtQueryInformationThread: TNtQueryInformationThread;
  FMapData: TYxdMapFile;

implementation

type
  TCurrentThreadStackHelper = class(TThread)
  protected
    FTargetThread: THandle;
    FStacks: string;
    procedure Execute; override;
  public
    constructor Create(ATargetThread: THandle); overload;
    destructor Destroy; override;
    property Stacks: string read FStacks;
  end;
  
type
  WCT_OBJECT_TYPE = (WctCriticalSectionType = 1, WctSendMessageType,
    WctMutexType, WctAlpcType, WctComType, WctThreadWaitType,
    WctProcessWaitType, WctThreadType, WctComActivationType, WctUnknownType,
    WctSocketIoType, WctSmbIoType, WctMaxType);

  WCT_OBJECT_STATUS = (WctStatusNoAccess = 1, // ACCESS_DENIED for this object
    WctStatusRunning, // Thread status
    WctStatusBlocked, // Thread status
    WctStatusPidOnly, // Thread status
    WctStatusPidOnlyRpcss, // Thread status
    WctStatusOwned, // Dispatcher object status
    WctStatusNotOwned, // Dispatcher object status
    WctStatusAbandoned, // Dispatcher object status
    WctStatusUnknown, // All objects
    WctStatusError, // All objects
    WctStatusMax);

  TLockObject = record
    ObjectName: array [0 .. WCT_OBJNAME_LENGTH - 1] of WideChar;
    Timeout: Int64; // Not implemented in v1
    Alertable: Boolean; // Not implemented in v1
  end;

  TThreadObject = record
    ProcessId, ThreadId, WaitTime, ContextSwitches: DWORD;
  end;

  WAITCHAIN_NODE_INFO = record
    ObjectType: WCT_OBJECT_TYPE;
    ObjectStatus: WCT_OBJECT_STATUS;
    case Integer of
      1:
        (LockObject: TLockObject);
      2:
        (ThreadObject: TThreadObject);
  end;

  TWaitChainNodeInfo = WAITCHAIN_NODE_INFO;
  PWaitChainNodeInfo = ^TWaitChainNodeInfo;
  TWaitChainCallback = procedure(WctHandle: THandle; Context: DWORD_PTR;
    CallbackStatus: DWORD; NodeCount: PDWord;
    NodeInfoArrary: PWaitChainNodeInfo; IsCycle: PBool); stdcall;
    
  PREAD_PROCESS_MEMORY_ROUTINE = function(hProcess: THandle;
    lpBaseAddress: DWORD; lpBuffer: Pointer; nSize: DWORD;
    var lpNumberOfBytesRead: DWORD): Boolean; stdcall;  
  PFUNCTION_TABLE_ACCESS_ROUTINE = function(hProcess: THandle; AddrBase: DWORD)
    : Pointer; stdcall;
  PGET_MODULE_BASE_ROUTINE = function(hProcess: THandle; Address: DWORD)
    : DWORD; stdcall;
  PTRANSLATE_ADDRESS_ROUTINE = function(hProcess, hThread: THandle;
    lpaddr: LPADDRESS): DWORD; stdcall;

  TSymLoadModule = function(hProcess, hFile: THandle;
    ImageName, ModuleName: PChar; BaseOfDll, SizeOfDll: DWORD): DWORD; stdcall;
  TSymGetSymFromAddr = function(hProcess: THandle; dwAddr: DWORD;
    var dwDisplacement: DWORD; pSymbol: PIMAGEHLP_SYMBOL): Boolean; stdcall;
  TSymSetOptions = function(SymOptions: DWORD): DWORD; stdcall;
  TSymInitialize = function(hProcess: THandle; UserSearchPath: PChar;
    fInvadeProcess: Boolean): Boolean; stdcall;
  TSymCleanup = function(hProcess: THandle): Boolean; stdcall;
  TStackWalk = function(MachineType: DWORD; hProcess, hThread: THandle;
    STACKFRAME: LPSTACKFRAME; ContextRecord: Pointer;
    ReadMemoryRoutine: PREAD_PROCESS_MEMORY_ROUTINE;
    FunctionTableAccessRoutine: PFUNCTION_TABLE_ACCESS_ROUTINE;
    GetModuleBaseRoutine: PGET_MODULE_BASE_ROUTINE;
    TranslateAddress: PTRANSLATE_ADDRESS_ROUTINE): Integer; stdcall;
  TSymFunctionTableAccess = function(hProcess: THandle; AddrBase: DWORD)
    : Pointer; stdcall;
  TSymGetModuleBase = function(hProcess: THandle; Address: DWORD)
    : DWORD; stdcall;
  TSymGetLineFromAddr = function(hProcess: THandle; dwAddr: DWORD;
    var dwDisplacement: DWORD; Line: PIMAGEHLP_LINE): Boolean; stdcall;

  TOpenThreadWaitChainSession = function(Flags: DWORD;
    callback: TWaitChainCallback): THandle; stdcall;
  TCloseThreadWaitChainSession = procedure(WctHandle: THandle); stdcall;
  TGetThreadWaitChain = function(WctHandle: THandle; Context: DWORD_PTR;
    Flags, ThreadId: DWORD; NodeCount: LPDWORD;
    NodeInfoArray: PWaitChainNodeInfo; IsCycle: PBool): BOOL; stdcall;
  PCoGetCallState = function(P1: Integer; P2: PCardinal): HRESULT; stdcall;
  PCoGetActivationState = function(AId: TGuid; P1: DWORD; P2: PCardinal)
    : HRESULT; stdcall;
  TRegisterWaitChainCOMCallback = procedure(CallStateCallback: PCoGetCallState;
    ActivationStateCallback: PCoGetActivationState); stdcall;

  {$IF RTLVersion<22}
  TThreadId = Longword;
  {$IFEND}
  TThreadCheckEvent = procedure(WctHandle: THandle; AThreadId: TThreadId;
    ATag: Pointer; var ADeadFound: Boolean);
var
  GetThreadWaitChain: TGetThreadWaitChain;
  OpenThreadWaitChainSession: TOpenThreadWaitChainSession;
  CloseThreadWaitChainSession: TCloseThreadWaitChainSession;

var
  WinSymLoadModule: TSymLoadModule;
  WinSymGetSymFromAddr: TSymGetSymFromAddr;
  WinSymSetOptions: TSymSetOptions;
  WinSymInitialize: TSymInitialize;
  WinStackWalk: TStackWalk;
  WinSymFunctionTableAccess: TSymFunctionTableAccess;
  WinSymGetModuleBase: TSymGetModuleBase;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: Boolean;
  dwThreadId: Cardinal): THandle; stdcall; external kernel32 name 'OpenThread';
  
{$IFNDEF UNICODE}
type
  IntPtr = Integer;
{$ENDIF}
{$IFDEF NEXTGEN}
type
  AnsiChar = Byte;
  PAnsiChar = ^AnsiChar;
  PDWORD = ^Cardinal;
 {$ENDIF}
var
  hDLL: HModule = 0;
  {$IFDEF MSWINDOWS}
  EIP: Cardinal;
  {$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF WIN64}
procedure GetEIP(); stdcall;
asm
  pop eax;
  mov EIP,eax;
  push eax;
end;
{$ENDIF}
{$ENDIF}

function DebugHelperExists: Boolean;
begin
  Result := hDLL <> 0;
end;

function GetDebugMapDesc(Addr: Pointer): string;
begin
  Result := FMapData.GetDesc(Addr);
end;

function GetDebugMap(Addr: Pointer): PMapItem;
begin
  Result := FMapData.GetMap(Addr);
end;

{$IFDEF MSWINDOWS}
function GetThreadStack(hThread: THandle): TContext;
begin
  FillChar(Result, sizeof(Result), 0);
  {$IFNDEF WIN64}
  if hThread <= 0 then Exit;
  if (GetCurrentThread = hThread) then begin
    GetEIP;
    Result.Eip := EIP;
  end else begin
    Result.ContextFlags := CONTEXT_FULL or CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;
    Suspendthread(hThread);
    getthreadcontext(hThread, Result);
    Resumethread(hThread);
  end;
  {$ENDIF}
end;

function GetThreadMap(hThread: THandle): PMapItem;
{$IFDEF WIN64}
begin Result := nil;
{$ELSE}
var
  C: TContext;
begin
  C := GetThreadStack(hThread);
  if C.Eip > 0 then
    Result := GetDebugMap(Pointer(C.Eip))
  else
    Result := nil;
{$ENDIF}
end;

function GetThreadMapDest(hThread: THandle): string;
{$IFDEF WIN64}
begin Result := '';
{$ELSE}
var
  C: TContext;
begin
  C := GetThreadStack(hThread);
  if C.Eip > 0 then
    Result := GetDebugMapDesc(Pointer(C.Eip))
  else
    Result := '';
{$ENDIF}
end;
{$ENDIF}

function LoadProc(AName: String): FARPROC;
var
  ARealName: String;
begin
  {$IFDEF UNICODE}
  ARealName := AName + 'W';
  {$ELSE}
  ARealName := AName + 'A';
  {$ENDIF}
  Result := GetProcAddress(hDLL, PChar(ARealName));
  if not Assigned(Result) then
    Result := GetProcAddress(hDLL, PChar(AName));
  if not Assigned(Result) then begin
    {$IFDEF UNICODE}
    ARealName := AName + 'W64';
    {$ELSE}
    ARealName := AName + '64';
    {$ENDIF}
    Result := GetProcAddress(hDLL, PChar(ARealName));
  end;
end;

function GetObjectTypes(const Index: Integer): string;
begin
  if (Index < 0) or (Index > High(ObjectTypes)) then
    Result := ''
  else
    Result := ObjectTypes[index];
end;

procedure PorcGetThreadWaitChain(hWct: THandle; var Item: TThreadInfoEx;
  LineChar: string; CheckCycle: Boolean);
{$IFDEF WIN64}
begin
{$ELSE}
var
  NodeInfoArray: array [0 .. WCT_MAX_NODE_COUNT] of WAITCHAIN_NODE_INFO;
  J, Count: DWORD;
  IsCycle: Boolean;
  C: TContext;
begin
  Count := WCT_MAX_NODE_COUNT;
  if (CheckCycle) and (not GetThreadWaitChain(hWct, 0, WCTP_GETINFO_ALL_FLAGS, Item.ThreadID,
    @Count, @NodeInfoArray[0], @IsCycle)) then
  begin
    Item.ErrMsg := SysErrorMessage(GetLastError);
  end else begin
    if IsCycle then begin
      Item.State := thd_Waiting;
      if Count > WCT_MAX_NODE_COUNT then
        Count := WCT_MAX_NODE_COUNT;
      for J := 0 to Count - 1 do begin
        case NodeInfoArray[J].ObjectType of
          WctThreadType:
            begin
              if NodeInfoArray[J].ObjectStatus = WctStatusBlocked then
                Item.State := thd_Blocking;
              Item.Stack := StackByThreadHandle(Item.Handle, Item.EIP, LineChar);
            end;
        else
          begin
            Item.LockMsg := Item.LockMsg +
              GetObjectTypes(Integer(NodeInfoArray[J].ObjectType));
            if NodeInfoArray[J].LockObject.ObjectName[0] <> #0 then
              Item.LockMsg := Item.LockMsg + '-' + NodeInfoArray[J].LockObject.ObjectName;
            if NodeInfoArray[J].ObjectStatus = WctStatusAbandoned then
              Item.LockMsg := Item.LockMsg + ' (已中断) ' + LineChar
            else
              Item.LockMsg := Item.LockMsg + ' (等待中) ' + LineChar;
          end;
        end;
      end;
    end else begin
      Item.State := thd_Running;
      C := GetThreadStack(Item.Handle);
      if C.Eip > 0 then begin
        Item.EIP := C.Eip;
        Item.Stack := GetDebugMapDesc(Pointer(C.Eip));
      end;
    end;
  end;
{$ENDIF}
end;

function GetProcessThreadInfo(AProcId: THandle; const LineChar: string;
  CheckCycle: Boolean): TThreadInfoExList;
{$IFDEF WIN64}begin
{$ELSE}
var
  hSnapshot: THandle;
  AEntry: THREADENTRY32;
  AExcludeThread, AMainID: THandle;
  WctHandle: THandle;
  I: Integer;
  AExitCode: Cardinal;
begin
  if AProcId = 0 then Exit;
  if Assigned(OpenThreadWaitChainSession) then
    WctHandle := OpenThreadWaitChainSession(0, nil)
  else
    WctHandle := 0;
  I := 0;
  AMainID := MainThreadID;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, AProcId);
  try
    if hSnapshot = 0 then Exit;
    AExcludeThread := GetCurrentThreadId();
    AEntry.dwSize := SizeOf(THREADENTRY32);
    if Thread32First(hSnapshot, AEntry) then begin
      repeat
        if (AEntry.th32OwnerProcessID = AProcId) then begin
          if Length(Result) <= I then
            SetLength(Result, I div 64 * 64 + 64);
          Result[I].ThreadID := AEntry.th32ThreadID;
          Result[i].BasePri := AEntry.tpBasePri;
          Result[I].ErrMsg := '';
          Result[I].LockMsg := '';
          Result[I].Stack := '-';
          if AEntry.th32ThreadID <> AExcludeThread then begin
            Result[I].Handle := OpenThread(THREAD_ALL_ACCESS, False, AEntry.th32ThreadID);
            if Result[I].Handle <> 0 then begin
              GetExitCodeThread(Result[I].Handle, AExitCode);
              Result[i].ExitCode := AExitCode;
              if (Result[i].ExitCode = STILL_ACTIVE) and (WctHandle <> 0) then
                PorcGetThreadWaitChain(WctHandle, Result[I], LineChar, CheckCycle);
              CloseHandle(Result[I].Handle);
            end;
          end else begin
            GetEIP;
            Result[i].EIP := EIP;
            Result[i].State := thd_Running;
            Result[i].Handle := GetCurrentThread;
            Result[i].ExitCode := STILL_ACTIVE;
            Result[I].Stack := GetDebugMapDesc(Pointer(Result[i].EIP)) + '(当前线程)';
          end;
          if AMainID = AEntry.th32ThreadID then
            Result[i].Stack := Result[i].Stack + '(主线程)';
          Inc(I);
        end;
      until not Thread32Next(hSnapshot, AEntry);
    end;
  finally
    CloseHandle(hSnapshot);
    if WctHandle <> 0 then
      CloseThreadWaitChainSession(WctHandle);
    SetLength(Result, I);
  end;
{$ENDIF}
end;

function GetThreadStacks(AThreadHandle: THandle): TStackFrames;
var
  AFrame: STACKFRAME;
  AContext: PContext;
  pMem: Pointer;
  {$IFNDEF CPU_X64}
  I: Integer;
  {$ENDIF}

  function GetNextStackFrame(var ANextFrame: STACKFRAME): Boolean;
  begin
    Result := WinStackWalk(IMAGE_FILE_MACHINE_I386, GetCurrentProcess(),
      AThreadHandle, @AFrame, AContext, nil, WinSymFunctionTableAccess,
      WinSymGetModuleBase, nil) <> 0;
    if Result then
      ANextFrame := AFrame;
  end;

begin
  {$IFDEF CPUX64}
  SetLength(Result, 0);
  Exit;
  {$ENDIF}
  if not DebugHelperExists then begin
    SetLength(Result, 0);
    Exit;
  end;
  GetMem(pMem, SizeOf(TContext) + 15);
  if (IntPtr(pMem) and $F) <> 0 then
    AContext := Pointer(((IntPtr(pMem) shr 4) + 1) shl 4)
  else
    AContext := pMem;
  SetLength(Result, 0);
  SuspendThread(AThreadHandle);
  try
    AContext.ContextFlags := CONTEXT_CONTROL;
    if GetThreadContext(AThreadHandle, AContext^) then begin
      FillChar(AFrame, SizeOf(AFrame), 0);
      {$IFNDEF CPUX64}
      AFrame.AddrPC.Offset := AContext.Eip;
      AFrame.AddrPC.Mode := DWORD(AddrModeFlat);
      AFrame.AddrStack.Offset := AContext.Esp;
      AFrame.AddrStack.Mode := DWORD(AddrModeFlat);
      AFrame.AddrFrame.Offset := AContext.Ebp;
      AFrame.AddrFrame.Mode := DWORD(AddrModeFlat);
      SetLength(Result, MAX_SYMNAME_SIZE);
      I := 0;
      while (I < MAX_SYMNAME_SIZE) and GetNextStackFrame(Result[I]) do
        Inc(I);
      SetLength(Result, I);
      {$ELSE}
      SetLength(Result, 0);
      {$ENDIF}
    end;
  finally
    ResumeThread(AThreadHandle);
    FreeMem(pMem);
  end;
end;

function IdByThreadHandle(AHandle: THandle): DWORD;
var
  AInfo: THREAD_BASIC_INFORMATION;
  L: ULONG;
begin
  FillChar(AInfo, SizeOf(THREAD_BASIC_INFORMATION), 0);
  NtQueryInformationThread(AHandle, 0, @AInfo,
    SizeOf(THREAD_BASIC_INFORMATION), @L);
  Result := AInfo.ThreadId;
end;

function StackByThreadHandle(AThread: THandle; const SLine: string): string;
var EIP: Cardinal;
begin
  Result := StackByThreadHandle(AThread, EIP, SLine);
end;

function StackByThreadHandle(AThread: THandle; var EIP: Cardinal; const SLine: string): string;
var
  AFrames: TStackFrames;
  AInfo: PMapItem;
  I, C: Integer;

  function DumpCurrentStack: String;
  var
    AHelper: TCurrentThreadStackHelper;
    AHandle: THandle;
    p: PChar;
  begin
    AHandle := OpenThread(THREAD_ALL_ACCESS, FALSE, GetCurrentThreadId);
    try
      AHelper := TCurrentThreadStackHelper.Create(AHandle);
      AHelper.WaitFor;
    finally
      CloseHandle(AHandle);
      Result := AHelper.Stacks;
      FreeAndNil(AHelper);
    end;
    if Length(Result) > 0 then begin
      p := PChar(Result);
      I := Pos('StackByThreadHandle', Result);
      Inc(p, I);
      while (p^ <> #0) do begin
        if p^ = #10 then begin
          Inc(p);
          Result := p;
          Break;
        end else
          Inc(p);
      end;
    end;
  end;

  function FormatLocation: string;
  begin
    Result := IntToHex(NativeInt(AInfo.Addr), SizeOf(NativeInt)) + ' ' + AInfo.Name;
    if (AInfo.Line <> 0) then
      Result := Result + '(' + IntToStr(AInfo.Line) + ')';
  end;

begin
  if GetCurrentThreadId = IdByThreadHandle(AThread) then begin
    Result := DumpCurrentStack;
    Exit;
  end;
  Result := '';
  AFrames := GetThreadStacks(AThread);
  C := High(AFrames);
  if C > 20 then
    C := 20;
  if C > 0 then
    EIP := AFrames[i].AddrPC.Offset - 6;
  for I := Low(AFrames) to C do begin
    AInfo := FMapData.GetMap(Pointer(AFrames[I].AddrPC.Offset - 6));
    if AInfo <> nil then
      Result := Result + FormatLocation + SLINE
    else
      Result := Result + GetFunctionInfo(Pointer(AFrames[I].AddrPC.Offset)) + SLINE;
  end;
end;

function GetFunctionInfo(Addr: Pointer): String;
var
  sName: array [0 .. $FF] of Char;
  mbi: MEMORY_BASIC_INFORMATION;

  procedure LoadSym(Addr: Pointer);
  var
    sPath: array [0 .. 4095] of Char;
    mbi: MEMORY_BASIC_INFORMATION;
  begin
    FillChar(mbi, SizeOf(MEMORY_BASIC_INFORMATION), 0);
    VirtualQuery(Addr, mbi, SizeOf(mbi));
    GetModuleFileName(Cardinal(mbi.AllocationBase), sPath, MAX_PATH);
    WinSymLoadModule(GetCurrentProcess(), 0, sPath, nil,
      DWORD(mbi.AllocationBase), 0);
  end;

  function GetFunctionName: String;
  var
    dwDisplacement: DWORD;
    buffer: array [0 .. 4095] of BYTE;
    pSymbol: PIMAGEHLP_SYMBOL;
  begin
    Result := '';
    FillChar(buffer, 4096, 0);
    pSymbol := PIMAGEHLP_SYMBOL(@buffer);
    pSymbol.SizeOfStruct := SizeOf(IMAGEHLP_SYMBOL);
    pSymbol.MaxNameLen := SizeOf(buffer) - SizeOf(IMAGEHLP_SYMBOL) + 1;
    LoadSym(Addr);
    if (WinSymGetSymFromAddr(GetCurrentProcess(), DWORD(Addr), dwDisplacement,
      pSymbol)) then begin
      if (pSymbol.Flags and $00000800) <> 0 then
        Result := PChar(@pSymbol.Name);
    end;
  end;

begin
  if (VirtualQuery(Addr, mbi, SizeOf(mbi)) = 0) or (mbi.State <> MEM_COMMIT) then
    Exit;
  if (GetModuleFileName(HModule(mbi.AllocationBase), @sName, 256) = 0) then
    Exit;
  Result := GetFunctionName;
  if Length(Result) > 0 then
    Result := ExtractFileName(sName) + '.' + Result
  else
    Result := ExtractFileName(sName) + '.' + IntToHex(IntPtr(Addr),
      SizeOf(Pointer) shl 1);
end;

function EnablePrivilege(PrivName: string; bEnable: Boolean): Boolean;
var
  TP: PTokenPrivileges;
  Dummy: Cardinal;
  hToken: THandle;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    GetMem(TP, SizeOf(DWORD) + SizeOf(TLUIDAndAttributes));
    try
      TP.PrivilegeCount := 1;
      if LookupPrivilegeValue(nil, PChar(PrivName), TP.Privileges[0].Luid) then begin
        if bEnable then
          TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
        else
          TP.Privileges[0].Attributes := 0;
        Result := AdjustTokenPrivileges(hToken, False, TP^, SizeOf(TP), nil, Dummy);
      end else
        Result := False;
    finally
      FreeMem(TP);
      CloseHandle(hToken);
    end;
  end;
end;

function SkipLine(var P: PChar): Integer; {$IFDEF USEINLINE}inline;{$ENDIF}
var
  ps: PChar;
begin
  ps := p;
  while p^ <> #0 do begin
    if p^ = #10 then begin
      Inc(p);
      Break;
    end else
      Inc(p);
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function IsSpace(const c: PChar; ASpaceSize: PInteger): Boolean; //inline;
begin
  Result := (c^=#9) or (c^=#10) or (c^=#13) or (c^=#32){$IFDEF UNICODE} or (c^=#$3000){$ENDIF};
  if Result and (ASpaceSize <> nil) then
    ASpaceSize^ := 1
end;

function SkipSpace(var p: PChar): Integer;
var
  ps: PChar;
  L: Integer;
begin
  ps := p;
  while p^<>#0 do begin
    if IsSpace(p, @L) then
      Inc(p, L)
    else
      Break;
  end;
  Result:= IntPtr(p) - IntPtr(ps);
end;

function PCharToStr(p: PChar; len: Integer): string; {$IFDEF USEINLINE}inline;{$ENDIF}
begin
  if (len > 0) and (p <> nil) then begin
    SetLength(Result, len);
    Move(p^, Result[1], len{$IFDEF UNICODE} shl 1{$ENDIF})
  end else if (p <> nil) then begin
    len := Integer(p);
    while PChar(len)^ <> #0 do
      Inc(len, Sizeof(Char));
    len := PChar(len) - p;
    if len > 0 then begin
      SetLength(Result, len);
      Move(p^, Result[1], len{$IFDEF UNICODE} shl 1{$ENDIF});
    end;
  end else
    Result := '';
end;

{$IFDEF UNICODE}
function AnsiDecode(p: PAnsiChar; l:Integer): string;
var
  ps: PAnsiChar;
{$IFNDEF MSWINDOWS}
  ABytes:TBytes;
{$ENDIF}
begin
  if l<=0 then begin
    ps := p;
    while ps^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do Inc(ps);
    l:=IntPtr(ps)-IntPtr(p);
  end;
  if l>0 then begin
    {$IFDEF MSWINDOWS}
    System.SetLength(Result, MultiByteToWideChar(CP_ACP,0,PAnsiChar(p),l,nil,0));
    MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p),l,PWideChar(Result),Length(Result));
    {$ELSE}
    System.SetLength(ABytes, l);
    Move(p^, PByte(@ABytes[0])^, l);
    Result := TEncoding.ANSI.GetString(ABytes);
    {$ENDIF}
  end else
    System.SetLength(Result,0);
end;

function LoadTextW(AStream: TStream): string;
var
  ASize: Integer;
  ABuffer: TBytes;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize>0 then begin
    SetLength(ABuffer, ASize);
    AStream.ReadBuffer((@ABuffer[0])^, ASize);
    Result := AnsiDecode(@ABuffer[0], ASize)
  end else
    Result := '';
end;
{$ENDIF}

const
  Convert: array[0..255] of Integer =
    (
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
     );

function HexToIntDef(const S: pchar; Len: Integer; def: Integer = 0): integer;
var
  I: Integer;
  v: Integer;
begin
  Result := 0;
  for I := 0 to len-1 do begin
    V := Convert[ord(s[i])];
    if V<0 then begin
      Result := def;
      Exit;
    end;
    result := (result * 16) or V;
  end;
end;

function PCharToIntDef(const S: pchar; Len: Integer; def: Integer = 0): integer; overload;
var
  I: Integer;
  v: Integer;
begin
  Result := 0;
  for I := 0 to len-1 do begin
    V := Convert[ord(s[i])];
    if V < 0 then begin
      Result := def;
      Exit;
    end;
    result := (result * 10) + V;
  end;
end;

{ TYxdMapFile }

constructor TYxdMapFile.Create;
var
  FName: string;
  {$IFDEF TEST}
  T: Cardinal;
  {$ENDIF}
  {$IFDEF UNICODE}
  //S: TMemoryStream;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  try
    LoadDefault();
  except
  end;
  {$ENDIF}
  {$IFDEF UNICODE}
  FStr := '';
  {$ELSE}
  FStream := nil;
  {$ENDIF}
  FName := ChangeFileExt(ParamStr(0), '.map');
  LoadFromFile(FName);
  // 加载本地符号表
end;

destructor TYxdMapFile.Destroy;
begin
  {$IFNDEF UNICODE}
  FreeAndNil(FStream);
  {$ENDIF}
  inherited;
end;

{$IFDEF MSWINDOWS}
function TYxdMapFile.FindImportList(const Addr: Pointer): PImportLineItem;
var
  I: Integer;
begin
  if InternalFind(@FSortimports, Addr, I) then
    Result := FSortimports[I]
  else begin
    if I > 0 then     
      Result := FSortimports[I-1]
    else
      Result := nil;
  end;
end;
{$ENDIF}

function TYxdMapFile.GetDesc(const Addr: Pointer): string;
var
  Item: PMapItem;
  {$IFDEF MSWINDOWS}
  IItem: PImportLineItem;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if (Cardinal(Addr) >= FBaseAddr) and (Cardinal(Addr) <= FBaseAddr + Cardinal(FCodeSize)) then begin
  {$ENDIF}
    Item := GetMap(Addr);
    if Item <> nil then begin
      if Item.Line > 0 then
        Result := Item.Name + ' (Line: ' + IntToStr(Item.Line) + ')'
      else
        Result := Item.Name;
    end else
      Result := '';
  {$IFDEF MSWINDOWS}
  end else begin
    IItem := FindImportList(Addr);
    if IItem <> nil then begin
      if IItem.Item = nil then
        Result := ExtractFileName(IItem.Data.FileName) + ',' + IntToHex(Cardinal(Addr), 8)
      else
        Result := IntToHex(Cardinal(Addr), 8) + ': ' + IItem.Item.Name;
    end else
      Result := '';  
  end;
  {$ENDIF}
end;

function TYxdMapFile.GetMap(const Addr: Pointer): PMapItem;
var
  I: Integer;
begin
  I := IndexOf(@FSortMap, Pointer(Cardinal(Addr) - FBaseAddr));
  if I >= 0 then begin
    Result := PMapItem(FSortMap[I]);
  end else
    Result := nil;
end;

procedure TYxdMapFile.GrowMap;
begin
  SetLength(FMap, Length(FMap) + 20000);
end;

procedure TYxdMapFile.GrowRow;
begin
  SetLength(FRow, Length(FRow) + 50000);
end;

function TYxdMapFile.IndexOf(List: PPList; Addr: Pointer): Integer;
var
  C: Integer;
begin
  if not InternalFind(List, Addr, Result) then begin
    C := Length(List^);
    if C > 0 then begin
      if (Result >= 0) then begin
        Dec(Result)
      end else
        Result := C - 1;
    end else
      Result := -1;
  end;
end;

procedure TYxdMapFile.InitMap();
var
  I, J, K, M, N: Integer;
  P, PC, PL, PB: PChar;
begin
  P := FValue;

  // 获取Code信息开始位置
  I := Pos('Publics by Name', FValue);
  if I < 0 then Exit;
  PC := P;
  Inc(PC, I);
  if SkipLine(PC) < 1 then Exit;

  // 获取基址和代码段大小
  I := Pos('0001', FValue);
  FBaseAddr := $401000;
  FCodeSize := 0;
  if I > 0 then begin
    PB := P;
    Inc(PB, I);
    Inc(PB, 5);
    if PB > PC then Exit;
    FBaseAddr := HexToIntDef(PB, 8, FBaseAddr);
    Inc(PB, 8);
    SkipSpace(PB);
    FCodeSize := HexToIntDef(PB, 8, 0);
  end;
  if FCodeSize = 0 then Exit;

  // 获取行号信息开始位置
  I := Pos('Line numbers', FValue);
  if I < 0 then Exit;
  PL := P;
  Inc(PL, I);
  if SkipLine(PL) < 1 then Exit;

  SkipSpace(PC);
  SkipSpace(PL);
  FPLSize := Length(FValue) - (PL - FValue);

  // 解析行号与地址的对应关系
  J := Length(FRow);
  if Length(FRow) < 100000 then   
    SetLength(FRow, 100000);
  PB := PL;
  M := High(FRow);
  while PL^ <> #0 do begin
    if (PDWORD(PL)^ = PDWORD(PChar('Line'))^) then begin
      if SkipLine(PL) < 1 then
        Break;
      SkipSpace(PL);
    end else begin
      P := PL;
      while (PL^ <> ' ') and (PL - P < 16) do
        Inc(PL);
      K := PCharToIntDef(P, PL - P, 0);
      if K > 0 then begin
        if PCharToIntDef(PL + 1, 4, 0) = 1 then begin
          if M < J then begin
            GrowRow;
            M := High(FRow);
          end;
          FRow[j].Line := K;
          FRow[j].Addr := Pointer(HexToIntDef(PL + 6, 8, 0));
          Inc(J);
        end;
        Inc(PL, 14);
      end else
        Break;
      SkipSpace(PL);
    end;
  end;
  SetLength(FRow, J);
  SetLength(FSortRow, J);
  for i := 0 to J - 1 do
    FSortRow[i] := @FRow[i];
  if J > 1 then
    QuickSort(@FSortRow, 0, J - 1);

  // 解析Code
  J := Length(FMap);
  GrowMap;
  M := High(FMap);
  I := Pos('Publics by Value', FValue);
  if (I > 0) then begin
    PL := FValue;
    Inc(PL, I);
    if PL < PB then
      PB := PL;
  end;
  while (PC < PB) and (PC^ <> #0) do begin
    if PCharToIntDef(PC, 4, 0) <> 1 then begin
      Inc(PC, 13);
      if SkipLine(PC) < 1 then
        Break
      else
        SkipSpace(PC);
      Continue;
    end;
    Inc(PC, 5);
    if M < J then begin
      GrowMap;
      M := High(FMap);
    end;
    FMap[J].Addr := Pointer(HexToIntDef(PC, 8, 0));
    Inc(PC, 8);
    SkipSpace(PC);
    FMap[J].Position := PC - FValue;
    P := PC;
    if SkipLine(PC) < 1 then
      FMap[J].Len := FSize - FMap[J].Position
    else begin
      FMap[J].Len := PC - P - 1;
      SkipSpace(PC);
    end;
    N := IndexOf(@FSortRow, FMap[J].Addr);
    if N >= 0 then
      FMap[J].Line := PLineItem(FSortRow[N]).Line
    else
      FMap[J].Line := 0;
    Inc(J);
  end;
  SetLength(FMap, J);
  SetLength(FSortMap, J);
  for i := 0 to J - 1 do
    FSortMap[i] := @FMap[i];
  if J > 1 then
    QuickSort(@FSortMap, 0, J - 1);
end;

function TYxdMapFile.InternalFind(List: PPList; const Addr: Pointer;
  var AIndex: Integer): Boolean;
var
  L, H, I: Integer;
  V: Pointer;
begin
  Result := False;
  L := 0;
  H := High(List^);
  while L <= H do begin
    I := (L + H) shr 1;
    V := PLineItem(List^[I]).Addr;
    if Cardinal(V) < Cardinal(Addr) then
      L := I + 1
    else begin
      H := I - 1;
      if Cardinal(V) = Cardinal(Addr) then
        Result := True;
    end;
  end;
  AIndex := L
end;

{$IFDEF MSWINDOWS}
type
  _IMAGE_THUNK_DATA64 = record
    case Byte of
      0:
        (ForwarderString: ULONGLONG); // PBYTE
      1:
        (_Function: ULONGLONG); // PDWORD Function -> _Function
      2:
        (Ordinal: ULONGLONG);
      3:
        (AddressOfData: ULONGLONG); // PIMAGE_IMPORT_BY_NAME
  end;

  IMAGE_THUNK_DATA64 = _IMAGE_THUNK_DATA64;
  TImageThunkData64 = _IMAGE_THUNK_DATA64;
  PIMAGE_THUNK_DATA64 = ^_IMAGE_THUNK_DATA64;
  ImageThunkData64 = ^_IMAGE_THUNK_DATA64;

  _IMAGE_THUNK_DATA32 = record
    case Byte of
      0:
        (ForwarderString: DWORD); // PBYTE
      1:
        (_Function: DWORD); // PDWORD Function -> _Function
      2:
        (Ordinal: DWORD);
      3:
        (AddressOfData: DWORD); // PIMAGE_IMPORT_BY_NAME
  end;
  IMAGE_THUNK_DATA32 = _IMAGE_THUNK_DATA32;
  TImageThunkData32 = _IMAGE_THUNK_DATA32;
  PIMAGE_THUNK_DATA32 = ^_IMAGE_THUNK_DATA32;
  PImageThunkData32 = ^_IMAGE_THUNK_DATA32;
  IMAGE_THUNK_DATA = IMAGE_THUNK_DATA32;
  PIMAGE_THUNK_DATA = PIMAGE_THUNK_DATA32;
  _IMAGE_OPTIONAL_HEADER64 = record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    { NT additional fields. }
    ImageBase: ULONGLONG;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: ULONGLONG;
    SizeOfStackCommit: ULONGLONG;
    SizeOfHeapReserve: ULONGLONG;
    SizeOfHeapCommit: ULONGLONG;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array [0 .. IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1]
      of TImageDataDirectory;
  end;

  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;
  PImageNtHeaders64 = ^TImageNtHeaders64;

  _IMAGE_NT_HEADERS64 = record
    Signature: DWORD;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;

  TImageNtHeaders64 = _IMAGE_NT_HEADERS64;
  IMAGE_NT_HEADERS64 = _IMAGE_NT_HEADERS64;

  _IMAGE_IMPORT_BY_NAME = record
    Hint: Word;
    Name: array [0 .. 0] of Byte;
  end;
  TImageNtHeaders32 = TImageNtHeaders;
  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  TPEImageHeader = record
    case Integer of
      1:
        (PE32: TImageNtHeaders32);
      2:
        (PE64: TImageNtHeaders64);
  end;
  _IMAGE_IMPORT_DESCRIPTOR = record
    case Byte of
      0:
        (Characteristics: DWORD); // 0 for terminating null import descriptor
      1:
        (OriginalFirstThunk: DWORD;
          // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
          TimeDateStamp: DWORD; // 0 if not bound,
          // -1 if bound, and real date\time stamp
          // in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
          // O.W. date/time stamp of DLL bound to (Old BIND)

          ForwarderChain: DWORD; // -1 if no forwarders
          Name: DWORD;
          FirstThunk: DWORD);
    // RVA to IAT (if bound this IAT has actual addresses)
  end;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  PIMAGE_IMPORT_DESCRIPTOR = ^_IMAGE_IMPORT_DESCRIPTOR;
  PImageImportDescriptor = ^_IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_BY_NAME = _IMAGE_IMPORT_BY_NAME;
  TImageImportByName = _IMAGE_IMPORT_BY_NAME;
  PIMAGE_IMPORT_BY_NAME = ^_IMAGE_IMPORT_BY_NAME;
  PImageImportByName = ^_IMAGE_IMPORT_BY_NAME;
  
const
  IMAGE_FILE_MACHINE_AMD64 = $8664; { AMD64 (K8) }
  
procedure TYxdMapFile.LoadDefault;

  function IsPEFile(AFile: TFileStream; var APEHdr: TPEImageHeader): Boolean;
  var
    ADosHdr: TImageDosHeader;
  begin
    AFile.ReadBuffer(ADosHdr, SizeOf(TImageDosHeader));
    if (ADosHdr.e_magic = IMAGE_DOS_SIGNATURE) and (ADosHdr._lfanew <> 0) then begin
      AFile.Position := ADosHdr._lfanew;
      AFile.ReadBuffer(APEHdr, SizeOf(TImageNtHeaders32));
      Result := (APEHdr.PE32.Signature = IMAGE_NT_SIGNATURE) and
        ((APEHdr.PE32.FileHeader.Machine = IMAGE_FILE_MACHINE_I386) or
        (APEHdr.PE32.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64));
      if APEHdr.PE32.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64 then
        // 64位则读取PE头部剩下的字节
        AFile.ReadBuffer(PByte(IntPtr(@APEHdr) + SizeOf(TImageNtHeaders32))^,
          SizeOf(TImageNtHeaders64) - SizeOf(TImageNtHeaders32));
    end
  else
    Result := False;
  end;

  procedure ReadImports(AFile: TFileStream; var Item: TImportData;
    AImportVAddr: DWORD; ASections: Word);
  var
    J: Integer;
    
    procedure AddItem(const Name: string; Addr: Pointer);
    begin
      if J > High(Item.List) then
        SetLength(Item.List, Length(Item.List) div 512 * 512 + 512);
      Item.List[J].Name := Name;
      Item.List[J].Addr := Addr;
    end;

  var
    ADLL, AName: string;
    I: Word;
    AHdr: TImageSectionHeader;
    AImport: PImageImportDescriptor;
    AMem: Pointer;
    AOffset: Integer;
    ALastPos: Int64;
    AImportEnd: IntPtr;
    ABuffer: array [0 .. 519] of Byte;
    AThunk: IMAGE_THUNK_DATA;
    AModule: THandle;
  begin
    J := 0;
    for I := 0 to ASections - 1 do begin
      AFile.ReadBuffer(AHdr, SizeOf(TImageSectionHeader));
      if AHdr.VirtualAddress = AImportVAddr then begin
        // 找到了导入小节
        AFile.Position := AHdr.PointerToRawData;
        GetMem(AMem, AHdr.SizeOfRawData);
        try
          AFile.ReadBuffer(AMem^, AHdr.SizeOfRawData);
          AImport := AMem;
          AImportEnd := IntPtr(AMem) + IntPtr(AHdr.SizeOfRawData);
          AOffset := AHdr.PointerToRawData - AHdr.VirtualAddress;
          while (IntPtr(AImport) < AImportEnd) and (AImport.Name <> 0) do begin
            AFile.Position := Int64(AImport.Name) + AOffset;
            AFile.Read(ABuffer[0], 520);
            ADLL := PCharToStr(@ABuffer[0], -1);
            AModule := GetModuleHandle(PChar(ADLL));
            if AModule <> 0 then begin
              ADLL := ExtractFileName(ADLL);
              AFile.Position := Int64(AImport.FirstThunk) + AOffset;
              try
                repeat
                  AFile.ReadBuffer(AThunk, SizeOf(IMAGE_THUNK_DATA));
                  ALastPos := AFile.Position;
                  if AThunk.Ordinal <> 0 then begin
                    AFile.Position := Int64(AThunk.Ordinal) + AOffset;
                    AFile.Read(ABuffer[0], 520);
                    if PImageImportByName(@ABuffer[0]).Hint = 0 then begin
                      AName := PCharToStr(@PImageImportByName(@ABuffer[0]).Name[0], -1);
                      AddItem(ADLL + ', ' + AName, GetProcAddress(AModule, PAnsiChar(AnsiString(AName))));
                      Inc(J);
                      AFile.Position := ALastPos;
                    end;
                  end;
                until AThunk.Ordinal = 0;
              except
              end;
            end; 
            AImport := Pointer(IntPtr(AImport) + SizeOf(TImageImportDescriptor));
          end;
        finally
          FreeMem(AMem);
        end;
        Break;
      end;
    end;
    SetLength(Item.List, J);
  end;

  function LoadPEImports(AFile: TFileStream; var Item: TImportData; AdjustOffset: Boolean): Boolean;
  var
    AHdr: TPEImageHeader;
  begin
    Result := IsPEFile(AFile, AHdr);
    if not Result then
      Exit;
    // 调整模块的加载地址区间
    if AHdr.PE32.FileHeader.Machine = IMAGE_FILE_MACHINE_AMD64 then begin
      if AdjustOffset then begin
        if Item.FOffset = 0 then
          Item.FOffset := AHdr.PE64.OptionalHeader.ImageBase +
            AHdr.PE64.OptionalHeader.BaseOfCode
        else
          Inc(Item.FOffset, AHdr.PE64.OptionalHeader.BaseOfCode);
        end;
      Item.Size := AHdr.PE64.OptionalHeader.SizeOfImage;
      ReadImports(AFile, Item,
        AHdr.PE64.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT]
        .VirtualAddress, AHdr.PE64.FileHeader.NumberOfSections);
    end else begin
      if AdjustOffset then begin
        if Item.FOffset = 0 then
          Item.FOffset := AHdr.PE32.OptionalHeader.ImageBase +
            AHdr.PE32.OptionalHeader.BaseOfCode
        else
          Inc(Item.FOffset, AHdr.PE32.OptionalHeader.BaseOfCode);
      end;
      Item.Size := AHdr.PE32.OptionalHeader.SizeOfImage;
      ReadImports(AFile, Item,
        AHdr.PE32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress,
        AHdr.PE32.FileHeader.NumberOfSections);
    end;
  end;

  function LoadDLLAddresses(var Item: TImportData): Boolean;
  var
    AFile: TFileStream;
  begin
    AFile := TFileStream.Create(Item.FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadPEImports(AFile, Item, False);
    finally
      AFile.Free;
    end;
  end;

  procedure LoadImports(var AList: TImportDataList);
  var
    AHandles: array of THandle;
    ACount: Cardinal;
    I, J: Integer;
    AFile: TFileStream;
  begin
    ACount := 0;
    J := 0;
    // 枚举已加载动态链接库导入函数
    EnumProcessModules(GetCurrentProcess, nil, 0, ACount);
    SetLength(AHandles, ACount);
    if EnumProcessModules(GetCurrentProcess, @AHandles[0], ACount, ACount) then begin
      SetLength(AList, ACount + 1);
      for I := 0 to ACount - 1 do begin
        if (AHandles[I] <> 0) and (AHandles[I] <> MainInstance) then begin
          AList[J].FileName := GetModuleName(AHandles[I]);
          AList[J].FOffset := AHandles[I];
          LoadDLLAddresses(AList[J]);
          Inc(J);
        end;
      end;
      AFile := TFileStream.Create(GetModuleName(MainInstance), fmOpenRead or fmShareDenyWrite);
      try
        LoadPEImports(AFile, AList[J], False);
      finally
        FreeAndNil(AFile);
      end;
    end;
  end;

var
  I, J, K: Integer;
begin
  LoadImports(FImports);
  K := 0;
  SetLength(FImportList, 2048);
  for I := 0 to Length(FImports) - 1 do begin
    if K > High(FImportList) then
      SetLength(FImportList, Length(FImportList) + 1024);
    FImportList[K].Addr := Pointer(FImports[I].FOffset);
    FImportList[K].Data := @FImports[I];
    FImportList[K].Item := nil;
    Inc(K);
    for J := 0 to Length(FImports[I].List) - 1 do begin
      if K > High(FImportList) then
        SetLength(FImportList, Length(FImportList) + 1024);
      FImportList[K].Addr := FImports[I].List[J].Addr;
      FImportList[K].Data := @FImports[I];
      FImportList[K].Item := @FImports[I].List[J];
      Inc(K);
    end;
  end;
  SetLength(FSortimports, K);
  for I := 0 to K - 1 do
    FSortimports[I] := @FImportList[I];
  QuickSort(@FSortimports, 0, K-1);
end;

{$ENDIF}

procedure TYxdMapFile.LoadFromFile(const FName: string);
{$IFDEF TEST}
var
  T: Cardinal;
  {$IFDEF UNICODE}
  S: TMemoryStream;
  {$ENDIF}
{$ELSE}
{$IFDEF UNICODE}
var S: TMemoryStream;
{$ENDIF}
{$ENDIF}
begin
  if FileExists(FName) then begin
    {$IFDEF UNICODE}
    S := TMemoryStream.Create;
    try
      S.LoadFromFile(FName);
      FStr := LoadTextW(S);
      FValue := PChar(FStr);
      FSize := Length(FStr);
    finally
      S.Free;
    end;
    {$ELSE}
    FStream := TMemoryStream.Create;
    FStream.LoadFromFile(FName);
    FValue := FStream.Memory;
    FSize := FStream.Size;
    {$ENDIF}
    {$IFDEF TEST}T := GetTickCount;{$ENDIF}
    SetLength(FMap, 0);
    SetLength(FRow, 0);
    SetLength(FSortMap, 0);
    SetLength(FSortRow, 0);
    InitMap();
    {$IFDEF TEST}
    T := GetTickCount - T;
    MessageBox(0, PChar(Format('加载 Map 用时 %d ms. 共 %d 条数据。', [T, Length(FMap)])), 'MapFile', 64);
    {$ENDIF}
  end else
    FValue := '';
end;

procedure TYxdMapFile.QuickSort(List: PPList; L, R: Integer);
var
  I, J, P: Integer;
  tmp: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Cardinal(PLineItem(List^[I]).Addr) < Cardinal(PLineItem(List^[P]).Addr) do
        Inc(I);
      while Cardinal(PLineItem(List^[J]).Addr) > Cardinal(PLineItem(List^[P]).Addr) do
        Dec(J);
      if I <= J then begin
        if I <> J then begin
          tmp := List^[i];
          List^[i] := List^[J];
          List^[J] := tmp;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(List, L, J);
    L := I;
  until I >= R;
end;

{ TMapItem }

function TMapItem.GetName: string;
begin
  Result := Trim(PCharToStr(FMapData.FValue + Position, Len));
end;

{ TCurrentThreadStackHelper }

constructor TCurrentThreadStackHelper.Create(ATargetThread: THandle);
begin
  FTargetThread := ATargetThread;
  inherited Create(False);
end;

destructor TCurrentThreadStackHelper.Destroy;
begin
  inherited;
end;

procedure TCurrentThreadStackHelper.Execute;
begin
  FStacks := StackByThreadHandle(FTargetThread);
end;

initialization
  {$IFDEF MSWINDOWS}
  EnablePrivilege('SeDebugPrivilege', True);
  NtQueryInformationThread := GetProcAddress(GetModuleHandle('ntdll.dll'),
    'NtQueryInformationThread');
  OpenThreadWaitChainSession := GetProcAddress(GetModuleHandle(Advapi32),
    'OpenThreadWaitChainSession');
  CloseThreadWaitChainSession := GetProcAddress(GetModuleHandle(Advapi32),
    'CloseThreadWaitChainSession');
  GetThreadWaitChain := GetProcAddress(GetModuleHandle(Advapi32),
    'GetThreadWaitChain');
  hDLL := LoadLibrary('dbghelp.dll');
  {$ENDIF}
  if hDLL <> 0 then begin
    WinSymLoadModule := LoadProc('SymLoadModule');
    WinSymGetSymFromAddr := LoadProc('SymGetSymFromAddr');
    WinSymSetOptions := LoadProc('SymSetOptions');
    WinSymInitialize := LoadProc('SymInitialize');
    WinStackWalk := LoadProc('StackWalk');
    WinSymFunctionTableAccess := LoadProc('SymFunctionTableAccess');
    WinSymGetModuleBase := LoadProc('SymGetModuleBase');
    WinSymSetOptions(SYMOPT_UNDNAME or SYMOPT_DEFERRED_LOADS or
      SYMOPT_LOAD_LINES);
    WinSymInitialize(GetCurrentProcess(), nil, True);
    WinSymSetOptions(SYMOPT_UNDNAME or SYMOPT_LOAD_LINES);
  end else begin
    WinSymLoadModule := nil;
    WinSymGetSymFromAddr := nil;
    WinStackWalk := nil;
    WinSymFunctionTableAccess := nil;
    WinSymGetModuleBase := nil;
  end;
  FMapData := TYxdMapFile.Create;

finalization
  FreeAndNil(FMapData);

end.
