{*******************************************************}
{                                                       }
{       YxdHash    哈希表，哈希函数                     }
{                                                       }
{       版权所有 (C) 2013      YangYxd                  }
{                                                       }
{*******************************************************}
{
 --------------------------------------------------------------------
  说明
 --------------------------------------------------------------------
  YxdHash代码来自 swish 的 Qrbtree，感谢swish和他的qrbtree
  YxdHash版本归 swish 和 YangYxd所有，保留一切权利
  Qrbtree来自QDAC项目，版权归swish(QQ:109867294)所有
  QDAC官方群：250530692

 --------------------------------------------------------------------
  更新记录
 --------------------------------------------------------------------

 2015.06.29 ver 1.0.10
 --------------------------------------------------------------------
  - 将 MemPool 移入 YxdMemPool 单元中

 2015.04.22 ver 1.0.9
 --------------------------------------------------------------------
  - 将 TYXDHashMapChainTable 改名为 TYXDHashMapLinkTable

 2015.03.30 ver 1.0.8
 --------------------------------------------------------------------
  - 修改 TYXDHashMapChainTable: Add时允许添加nil数据
  
 2014.11.17 ver 1.0.7
 --------------------------------------------------------------------

 2014.11.08 ver 1.0.6
 --------------------------------------------------------------------
  - 增加 TLinkedList 类，方便双向链表操作

 2014.10.11 ver 1.0.5
 --------------------------------------------------------------------
  - 增加 TStringHash 类，将IniFiles单元的移植过来的

 2014.10.11 ver 1.0.4
 --------------------------------------------------------------------
  - 修复TYXDHashMapChainTable增删时双向链表错乱的BUG(重要).
 
 2014.10.10 ver 1.0.3
 --------------------------------------------------------------------
  - 增加YxdMemPool，使用内存池.
  - 增加TYXDHashMapTable
  - 增加TYXDHashMapList
  - 增加TYXDHashMapChainTable

 2014.08.27 ver 1.0.2
 --------------------------------------------------------------------
  - 优化了一下Insert略微提速.
  
 2014.08.15 ver 1.0.1
 --------------------------------------------------------------------
  - 此单元做为Hash操作的基础库.
  - 将原QRBTree中的RBNote改为record型  
 --------------------------------------------------------------------
}

unit YxdHash;

interface

{$DEFINE USE_MEMPOOL}         // 是否使用内存池
{$DEFINE USE_ATOMIC}          // 是否启用原子操作函数
{.$DEFINE AUTORESIE}           // 哈希表是否自动调整桶大小

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  YxdMemPool,
  SysUtils, Classes, Types, SyncObjs;

type
  Number = Cardinal;

type
  /// 桶内元素的哈希值列表
  THashType = Cardinal;
  PPHashList = ^PHashList;
  PHashList = ^THashList;
  THashList = packed record
    Next: PHashList;  // 下一元素
    Data: Pointer;    // 附加数据成员
    Hash: THashType;  // 当前元素哈希值，记录以便重新分配桶时不需要再次外部计算
    procedure Reset; inline;
  end;
  THashArray = array of PHashList;

type
  PHashValue = ^THashValue;
  THashValue = packed record
    Size: Cardinal;       // 数据大小
    Data: Pointer;        // 数据指针
    function AsString: string;
    procedure Clear;
  end;

type
  PHashMapValue = ^THashMapValue;
  THashMapValue = packed record
    Value: THashValue;      // 数据
    IsStrKey: WordBool;     // 是否是字符串 Key
    Key: string;            // 字符串 Key
    function GetNumKey: Number; inline;
    procedure SetNumKey(const Value: Number); inline;
  end;

type
  PHashMapList = ^THashMapList;
  THashMapList = packed record
    Next: PHashList;      // 下一元素
    Data: PHashMapValue;  // 附加数据成员
    Hash: THashType;      // 当前元素哈希值，记录以便重新分配桶时不需要再次外部计算
  end;

type
  PHashMapLinkItem = ^THashMapLinkItem;
  THashMapLinkItem = packed record
    Next: PHashMapLinkItem;
    Prev: PHashMapLinkItem;
    Value: PHashMapValue;
  end;

type
  /// <summary>比较函数</summary>
  /// <param name='P1'>第一个要比较的参数</param>
  /// <param name='P2'>第二个要比较的参数</param>
  /// <returns> 如果P1<P2，返回小于0的值，如果P1>P2返回大于0的值，如果相等，返回0</returns>
  TYXDCompare = function (P1, P2:Pointer): Integer of object;

type
  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDHashDeleteNotify = procedure (ATable: TObject; AHash: THashType; AData: Pointer) of object;

type
  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: Integer;
  end;

type
  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDStrHashItemFreeNotify = procedure (Item: PHashItem) of object;

  TStringHash = class
  private
    Buckets: array of PHashItem;
    FLocker: TCriticalSection;
    FOnFreeItem: TYXDStrHashItemFreeNotify;
  protected
    function Find(const Key: string): PPHashItem;
  public
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: Integer);
    procedure AddOrUpdate(const Key: string; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: Integer): Boolean;
    function ValueOf(const Key: string): Integer;
    function Exists(const Key: string): Boolean;  
    property OnFreeItem: TYXDStrHashItemFreeNotify read FOnFreeItem write FOnFreeItem;
  end;

type
  PPIntHashItem = ^PIntHashItem;
  PIntHashItem = ^TIntHashItem;
  TIntHashItem = record
    Next: PIntHashItem;
    Key: THashType;
    Value: Integer;
  end;

  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDIntHashItemFreeNotify = procedure (Item: PIntHashItem) of object;

  TIntHash = class
  private
    Buckets: array of PIntHashItem;
    FLocker: TCriticalSection;
    FOnFreeItem: TYXDIntHashItemFreeNotify;
  protected
    function Find(const Key: THashType): PPIntHashItem;
  public
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;
    procedure Add(const Key: THashType; Value: Integer);
    procedure AddOrUpdate(const Key: THashType; Value: Integer);
    procedure Clear;
    function Remove(const Key: THashType): Boolean;
    function Modify(const Key: THashType; Value: Integer): Boolean;
    function ValueOf(const Key: THashType): Integer;
    function Exists(const Key: THashType): Boolean;
    property OnFreeItem: TYXDIntHashItemFreeNotify read FOnFreeItem write FOnFreeItem;
  end;

type
  /// <summary>
  /// 哈希表, 用于存贮一些用于查询的散列数据
  /// </summary>
  TYXDHashTable = class(TObject)
  private
    FPool: TMemPool;
    procedure SetAutoSize(const Value: Boolean);
    procedure FreeBucket(var ABucket: PHashList); virtual;
    function GetMemSize: Int64; virtual;
  protected
    FCount: Integer;
    FBuckets: THashArray;
    FOnDelete: TYXDHashDeleteNotify;
    FOnCompare: TYXDCompare;
    FAutoSize : Boolean; 
    procedure DoDelete(AHash: THashType; AData:Pointer); virtual;
    function GetBuckets(AIndex: Integer): PHashList; inline;
    function GetBucketCount: Integer; inline;
    function Compare(Data1, Data2: Pointer; var AResult: Integer): Boolean; inline;
  public
    ///构造函数，以桶数量为参数，后期可以调用Resize调整
    constructor Create(ASize: Integer); overload; virtual;
    ///构造函数
    constructor Create; overload;
    destructor Destroy;override;
    procedure Clear; virtual;
    procedure DeleteBucket(Index: Integer);
    procedure ReSize(ASize: Cardinal);
    procedure Add(AData: Pointer; AHash: THashType);
    // 找出哈希值为AHash的所有HashList，需要自己释放返回的HashList
    function Find(AHash: THashType): PHashList; overload;
    function Find(AData: Pointer; AHash: THashType): Pointer; overload;
    function FindFirstData(AHash: THashType): Pointer;
    function FindFirst(AHash: THashType): PHashList; inline;
    function FindNext(AList: PHashList): PHashList; inline;
    procedure FreeHashList(AList: PHashList);
    function Exists(AData: Pointer; AHash: THashType):Boolean;
    procedure Delete(AData: Pointer; AHash: THashType);
    procedure Update(AData: Pointer; AOldHash, ANewHash: THashType);
    // 元素个数
    property Count: Integer read FCount;
    // 桶数量
    property BucketCount: Integer read GetBucketCount;
    // 桶列表
    property Buckets[AIndex:Integer]: PHashList read GetBuckets;default;
    // 比较函数
    property OnCompare:TYXDCompare read FOnCompare write FOnCompare;
    // 删除事件通知
    property OnDelete: TYXDHashDeleteNotify read FOnDelete write FOnDelete;
    // 是否自动调整桶大小
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    // 内存占用大小
    property MemSize: Int64 read GetMemSize;
  end;

type
  /// <summary>
  /// 以字符串为 Key 的Hash表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可快速删除数据
  ///   3. 可快速添加数据
  ///   4. 无索引，只能通过桶来遍列每一个数据
  /// </summary>
  TYXDHashMapTable = class(TYXDHashTable)
  private
    FListPool: TMemPool;
    procedure FreeBucket(var ABucket: PHashList); override;
    function GetMemSize: Int64; override;
  protected
    procedure DoAdd(ABucket: PHashMapList); virtual;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Add(const Key: string; AData: PHashValue); overload;
    procedure Add(const Key: Number; AData: PHashValue); overload;
    procedure Add(const Key: string; AData: Integer); overload;
    procedure Add(const Key: Number; AData: Integer); overload;
    procedure Clear; override;
    function Exists(const Key: string): Boolean; overload; inline;
    function Exists(const Key: Number): Boolean; overload; inline;
    function Find(const Key: string): PHashMapValue; overload;
    function Find(const Key: Number): PHashMapValue; overload;
    function FindList(const Key: string): PPHashList; overload;
    function FindList(const Key: Number): PPHashList; overload;
    function Update(const Key: string; Value: PHashValue): Boolean; overload;
    function Update(const Key: Number; Value: PHashValue): Boolean; overload;
    function Remove(const Key: string): Boolean; overload;
    function Remove(const Key: Number): Boolean; overload;
    function Remove(const P: PHashMapValue): Boolean; overload;
    function ValueOf(const Key: string): PHashValue; overload;
    function ValueOf(const Key: Number): PHashValue; overload;
  end;

type
  TYXDHashMapListBase = class(TYXDHashMapTable)
  private
    function GetItem(Index: Integer): PHashMapValue; virtual; abstract;
  public
    property Items[Index: Integer]: PHashMapValue read GetItem;
  end;

type
  /// <summary>
  /// 以字符串为Key，带索引的 Hash 列表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可快速使用 Index 访问遍列数据
  ///   3. 可通过Index删除数据。删除速度较慢
  ///   4. 可快速添加数据
  /// </summary>
  TYXDHashMapList = class(TYXDHashMapListBase)
  private
    FList: TList;
    function GetItem(Index: Integer): PHashMapValue; override;
  protected
    procedure DoAdd(ABucket: PHashMapList); override;
    procedure DoDelete(AHash: THashType; AData:Pointer); override;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
  end;

type
  /// <summary>
  /// 以字符串为Key，带双向链表索引的 Hash 链表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可使用 Index 访问每一个数据（速度慢，建议使用链表方式遍列）
  ///   3. 可快速删除数据
  ///   4. 可快速添加数据
  /// </summary>
  TYXDHashMapLinkTable = class;

  TYXDHashMapLinkTableEnumerator = class
  private
    FItem: PHashMapLinkItem;
  public
    constructor Create(AList: TYXDHashMapLinkTable);
    function GetCurrent: PHashMapLinkItem; inline;
    function MoveNext: Boolean;
    property Current: PHashMapLinkItem read GetCurrent;
  end;

  TYXDHashMapLinkTable = class(TYXDHashMapListBase)
  private
    FFirst: PHashMapLinkItem;
    FLast: PHashMapLinkItem;
    ListBuckets: THashArray;
    FLinkHashPool: TMemPool;
    function GetItem(Index: Integer): PHashMapValue; override;
    function GetMemSize: Int64; override;
    function FindLinkItem(AData: Pointer; isDelete: Boolean): PHashMapLinkItem;
    procedure FreeLinkList;
    function GetLast: PHashMapValue;
  protected
    procedure DoAdd(ABucket: PHashMapList); override;
    procedure DoDelete(AHash: THashType; AData:Pointer); override;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function GetEnumerator: TYXDHashMapLinkTableEnumerator;   
    property First: PHashMapLinkItem read FFirst;
    property Last: PHashMapLinkItem read FLast;
    property LastValue: PHashMapValue read GetLast;
  end;

  // 保留旧的名称，以向下兼容
  TYXDHashMapChainTable = TYXDHashMapLinkTable;

type
  PRBNode = ^TRBNode;
  PPRBNode = ^PRBNode;
  TRBNode = packed record
  private
    FParentColor: IntPtr;
    function GetParent: PRBNode;
    procedure SetParent(const Value: PRBNode);
    function RedParent: PRBNode; inline;
    procedure SetBlack; inline;
  public
    Left: PRBNode;  // 左结点
    Right: PRBNode; // 右结点
    Data: Pointer;  // 附加数据成员
  public
    procedure Free;
    procedure Assign(src: PRBNode);
    // 重置为空结点，调用后IsEmpty将返回true
    procedure Clear;
    // 下一后根次序结点，对应于rb_next_postorder函数
    function NextPostOrder: PRBNode;
    // 下一个节点
    function Next: PRBNode;
    // 前一个结点
    function Prior: PRBNode;
    // 是否为空
    function IsEmpty: Boolean; inline;
    // 是否是黑结点
    function IsBlack: Boolean; inline;
    // 是否为红结点
    function IsRed: Boolean; inline;
    // 最深的最左结点
    function LeftDeepest: PRBNode;
    // 设置父结点和颜色
    procedure SetParentAndColor(AParent: PRBNode; AColor:Integer); inline;
    // 父结点
    property Parent: PRBNode read GetParent write SetParent;
  end;

type
  TRBTree = class;
  TRBCompare = TYXDCompare;
  /// <summary>删除结点通知事件，在删除一个树结点时触发</summary>
  /// <param name="ASender">触发事件的红黑树对象</param>
  /// <param name="ANode">要删除的结点</param>
  TRBDeleteNotify = procedure (ASender: TRBTree; ANode: PRBNode) of object;
  // 下面三个事件我真没想到啥时需要，原Linux代码中触发了，我也就保留了
  TRBRotateNotify = procedure (ASender: TRBTree; AOld, ANew: PRBNode) of object;
  TRBPropagateNotify = procedure (ASender: TRBTree; ANode, AStop: PRBNode) of object;
  TRBCopyNotify = TRBRotateNotify;

  /// <summary>
  /// 红黑树Delphi对象封装 (源自swish基于Linux 3.14.4内核红黑树实现)
  /// </summary>
  TRBTree = class
  private
    function GetIsEmpty: Boolean; inline;
  protected
    FRoot: PRBNode;
    FCount: Integer;
    FRBMempool: TMemPool;
    FOnCompare: TYXDCompare;
    FOnDelete: TRBDeleteNotify;
    FOnRotate: TRBRotateNotify;
    FOnCopy: TRBCopyNotify;
    FOnPropagate: TRBPropagateNotify;
    function EraseAugmented(node: PRBNode): PRBNode; inline;
    procedure RotateSetParents(AOld, ANew: PRBNode; color: Integer);inline;
    procedure EraseColor(AParent: PRBNode); inline;
    procedure ChangeChild(AOld, ANew, parent: PRBNode); inline;
    procedure DoCopy(node1, node2: PRBNode); inline;
    procedure DoPropagate(node1, node2: PRBNode); inline;
    procedure InsertColor(AChild: PRBNode); inline;
    procedure InsertNode(node: PRBNode); inline;
    procedure DoRotate(AOld,ANew: PRBNode); inline;
    procedure LinkNode(node,parent: PRBNode; var rb_link: PRBNode); inline;
  public
    /// <summary>
    /// 构造函数，传递一个大小比较函数进去，以便在插入和查找时能够正确的区分
    /// </summary>
    constructor Create(AOnCompare: TRBCompare); virtual;
    destructor Destroy;override;

    // 清除所有的结点
    procedure Clear;
    // 删除一个结点, 成功，返回被删除结点的Data数据成员地址，失败或不存在，返回nil
    function Delete(AChild: PRBNode): Pointer; //rb_erase
    // 首个结点
    function First: PRBNode;//rb_first
    // 最后一个结点
    function Last: PRBNode; //rb_last
    // 首个后根次序结点
    function FirstPostOrder: PRBNode;//rb_first_postorder
    // 插入一个数据，比较由构造时传入的事件回调函数处理, 成功，返回true (如果指定的数据相同内容已经存在，就会返回false)
    function Insert(AData:Pointer):Boolean;
    // 查找与指定的数据内容相同的结点, 返回找到的结点
    function Find(AData:Pointer): PRBNode;
    // 替换结点, 替换要自己保证内容和Src一致，否则可能造成树错乱，如果不能保证尝试删除+添加来完成替换
    procedure Replace(Src, ANew: PRBNode);

    // 判断树是否为空树
    property IsEmpty: Boolean read GetIsEmpty;
    // 比较函数，注意不要在树插入后更改比较算法
    property OnCompare: TRBCompare read FOnCompare write FOnCompare;
    // 删除事件响应函数
    property OnDelete: TRBDeleteNotify read FOnDelete write FOnDelete;
    // 旋转事件
    property OnRotate: TRBRotateNotify read FOnRotate write FOnRotate;
    // 复制事件
    property OnCopy: TRBCopyNotify read FOnCopy write FOnCopy;
    // 扩散事件
    property OnPropagate: TRBPropagateNotify read FOnPropagate write FOnPropagate;
    // 结点数量
    property Count:Integer read FCount;
  end;

type
  PLinkItem = ^TLinkItem;
  TLinkItem = packed record
    Next: PLinkItem;
    Prev: PLinkItem;
    Value: TObject;
  end;

  TLinkedListAction = (lla_clear {清除}, lla_unlink{解除链接}, lla_replace{替换Value});
  TOnCompareItem = function (const A, B: TObject): Integer of object;
  TOnDeleteItem = procedure (const Item: TObject; Action: TLinkedListAction) of object;
  TLinkedList = class(TObject)
  private
    FFirst: PLinkItem;
    FLast: PLinkItem;
    FCount: Integer;
    FOnDelete: TOnDeleteItem;
    FOnCompareItem: TOnCompareItem;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(const Index: Integer): TObject;
    procedure SetItem(const Index: Integer; const Value: TObject);
    procedure DoOutOfBoundsError(const Index: Integer);
  protected
    function isElementIndex(const AIndex: Integer): Boolean; inline;
    function isPositionIndex(const AIndex: Integer): Boolean; inline;
    function Node(const AIndex: Integer): PLinkItem;
    function unlinkNode(P: PLinkItem): Boolean;
    procedure linkBefore(const V: TObject; const P: PLinkItem);
    procedure DoDelete(var P: PLinkItem; Aciton: TLinkedListAction); inline;
    function DoDefaultCompareItem(const A, B: TObject): Integer; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Add(V: TObject); overload;
    procedure Add(const APosition: Integer; V: TObject); overload;
    procedure AddFirst(V: TObject);
    procedure AddLast(V: TObject);
    procedure AddAll(V: TLinkedList); overload;
    procedure AddAll(V: array of TObject); overload;
    function Contains(V: TObject): Boolean;
    function Exist(V: TObject): Boolean;
    function IndexOf(V: TObject): Integer;
    function LastIndexOf(V: TObject): Integer;
    function Offer(V: TObject): Boolean;
    function OfferFirst(V: TObject): Boolean;
    function OfferLast(V: TObject): Boolean;
    function Pop(): TObject; 
    function Peek(): TObject;
    function PeekLast(): TObject;
    function Poll(): TObject; inline;
    function PollFirst(): TObject;
    function PollLast(): TObject;
    function Element(): TObject;
    procedure Push(V: TObject); inline;
    function Remove(): TObject; overload; inline;
    function Remove(V: TObject): Boolean; overload;
    function Remove(Index: Integer): Boolean; overload;
    function RemoveFirst(): TObject; inline;
    function RemoveLast(): TObject;
    property First: PLinkItem read FFirst;
    property Last: PLinkItem read FLast; 
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[const Index: Integer]: TObject read GetItem write SetItem;
    property OnDelete: TOnDeleteItem read FOnDelete write FOnDelete;
    property OnCompareItem: TOnCompareItem read FOnCompareItem write FOnCompareItem;
  end;

// --------------------------------------------------------------------------
//  HASH 处理函数
// --------------------------------------------------------------------------

// HASH 函数
function HashOf(const Key: Pointer; KeyLen: Cardinal): THashType; overload;
function HashOf(const Key: string): THashType; inline; overload;
// 根据一个参考客户值，返回适当的哈希表大小
function CalcBucketSize(dataSize: Cardinal): THashType;

// --------------------------------------------------------------------------
//  原子操作 函数
// --------------------------------------------------------------------------

{$IFDEF USE_ATOMIC}
{$IF RTLVersion<26}
// 为与D2007兼容, 原子操作函数
function AtomicCmpExchange(var Target: Integer; Value, Comparand: Integer): Integer; inline;
function AtomicExchange(var Target: Integer; Value: Integer): Integer; inline;
function AtomicIncrement(var Target: Integer): Integer; inline;
function AtomicDecrement(var Target: Integer): Integer; inline;
{$IFEND}
// 原子操作函数
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
function AtomicAdd(var Dest: Integer; const AValue: Integer): Integer; inline;
{$ENDIF}

implementation

const
  BucketSizes: array[0..47] of Cardinal = (
    17,37,79,163,331,673,1361,2729,5471,10949,21911,43853,87719,175447,350899,
    701819,1403641,2807303,5614657,8999993,11229331,22458671,30009979,44917381,
    50009969, 60009997, 70009987, 80009851, 89834777,100009979,110009987,120009979,
    130009903, 140009983,150009983,165009937,179669557,200009959,359339171,
    400009999, 450009883,550009997,718678369,850009997,1050009979,1437356741,
    1850009969, 2147483647
  );

const
  RB_RED = 0;
  RB_BLACK = 1;

const
  HASHITEMSize = SizeOf(THashMapList) + SizeOf(THashMapValue);

function HashOf(const Key: Pointer; KeyLen: Cardinal): THashType; overload;
var
  ps: PCardinal;
  lr: Cardinal;
begin
  Result := 0;
  if KeyLen > 0 then begin
    ps := Key;
    lr := (KeyLen and $03);//检查长度是否为4的整数倍
    KeyLen := (KeyLen and $FFFFFFFC);//整数长度
    while KeyLen > 0 do begin
      Result := ((Result shl 5) or (Result shr 27)) xor ps^;
      Inc(ps);
      Dec(KeyLen, 4);
    end;
    if lr <> 0 then begin
      case lr of
        1: KeyLen := PByte(ps)^;
        2: KeyLen := PWORD(ps)^;
        3: KeyLen := PWORD(ps)^ or (PByte(Cardinal(ps) + 2)^ shl 16);
      end;
      Result := ((Result shl 5) or (Result shr 27)) xor KeyLen;
    end;
  end;
end;

function HashOf(const Key: string): THashType; inline; overload;
begin
  Result := HashOf(PChar(Key), Length(Key){$IFDEF UNICODE} shl 1{$ENDIF});
end;

function CalcBucketSize(dataSize: Cardinal): Cardinal;
var
  i: Integer;
begin
  for i := 0 to High(BucketSizes) do
    if BucketSizes[i] > dataSize then begin
      Result := BucketSizes[i];
      Exit;
    end;
  Result := BucketSizes[High(BucketSizes)];
end;

{ THashValue }

function THashValue.AsString: string;
begin
  SetLength(Result, Size);
  if Size > 0 then
    Move(Data^, Result[1], Size);
end;

procedure THashValue.Clear;
begin
  Size := 0;
  Data := nil;
end;

{ TStringHash }

procedure TStringHash.Add(const Key: string; Value: Integer);
var
  Hash: Integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  FLocker.Enter;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  FLocker.Leave;
end;

procedure TStringHash.AddOrUpdate(const Key: string; Value: Integer);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TStringHash.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  FLocker.Enter;
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    while P <> nil do begin
      N := P^.Next;
      if Assigned(FOnFreeItem) then
        FOnFreeItem(P);
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
  FLocker.Leave;
end;

constructor TStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  SetLength(Buckets, Size);
end;

destructor TStringHash.Destroy;
begin
  FLocker.Enter;
  try
    Clear;
    inherited Destroy;
  finally
    FLocker.Free;
  end;
end;

function TStringHash.Exists(const Key: string): Boolean;
begin
  FLocker.Enter;
  Result := Find(Key)^ <> nil;
  FLocker.Leave;
end;

function TStringHash.Find(const Key: string): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  P: PHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
  FLocker.Leave;
end;

procedure TStringHash.Remove(const Key: string);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  FLocker.Enter;
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    Dispose(P);
  end;
  FLocker.Leave;
end;

function TStringHash.ValueOf(const Key: string): Integer;
var
  P: PHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
  FLocker.Leave;
end;

{ TIntHash }

procedure TIntHash.Add(const Key: THashType; Value: Integer);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  FLocker.Enter;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  FLocker.Leave;
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; Value: Integer);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.Clear;
var
  I: Integer;
  P, N: PIntHashItem;
begin
  FLocker.Enter;
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    while P <> nil do begin
      N := P^.Next;
      if Assigned(FOnFreeItem) then
        FOnFreeItem(P);
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
  FLocker.Leave;
end;

constructor TIntHash.Create(Size: Cardinal);
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  SetLength(Buckets, Size);
end;

destructor TIntHash.Destroy;
begin
  FLocker.Enter;
  try
    Clear;
    inherited Destroy;
  finally
    FLocker.Free;
  end;
end;

function TIntHash.Exists(const Key: THashType): Boolean;
begin
  FLocker.Enter;
  Result := Find(Key)^ <> nil;
  FLocker.Leave;
end;

function TIntHash.Find(const Key: THashType): PPIntHashItem;
begin
  Result := @Buckets[Key mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TIntHash.Modify(const Key: THashType; Value: Integer): Boolean;
var
  P: PIntHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
  FLocker.Leave;
end;

function TIntHash.Remove(const Key: THashType): Boolean;
var
  P: PIntHashItem;
  Prev: PPIntHashItem;
begin
  Result := False;
  FLocker.Enter;
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then begin
    Prev^ := P^.Next;
    Dispose(P);
    Result := True;
  end;
  FLocker.Leave;
end;

function TIntHash.ValueOf(const Key: THashType): Integer;
var
  P: PIntHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
  FLocker.Leave;
end;

{ THashList }

procedure THashList.Reset;
begin
  Next := nil;
  Data := nil;
end;  

{ THashMapValue }

function THashMapValue.GetNumKey: Number;
begin
  Result := PDWORD(@Key)^;
end;

procedure THashMapValue.SetNumKey(const Value: Number);
begin
  PDWORD(@Key)^ := THashType(Value);
end;

{ TYXDHashTable }

procedure TYXDHashTable.Add(AData: Pointer; AHash: THashType);
var
  AIndex: Integer;
  ABucket: PHashList;
begin
  ABucket := FPool.Pop;
  ABucket.Hash := AHash;
  ABucket.Data := AData;
  AIndex := AHash mod Cardinal(Length(FBuckets));
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := ABucket;
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
end;

procedure TYXDHashTable.Clear;
var
  I,H: Integer;
  ABucket: PHashList;
begin
  H := High(FBuckets);
  for I := 0 to H do begin
    ABucket := FBuckets[I];
    while ABucket <> nil do begin
      FBuckets[I] := ABucket.Next;
      DoDelete(ABucket.Hash, ABucket.Data);
      FreeBucket(ABucket);
      ABucket := FBuckets[I];
    end;
  end;
  FPool.Clear;
  FCount := 0;
end;

function TYXDHashTable.Compare(Data1, Data2: Pointer;
  var AResult: Integer): Boolean;
begin
  if Assigned(FOnCompare) then begin
    AResult := FOnCompare(Data1, Data2);
    Result := True;
  end else
    Result := False;
end;

constructor TYXDHashTable.Create(ASize: Integer);
begin
  //FPool := THashListPool.Create(8192, SizeOf(THashList));
  FPool := TMemPool.Create(SizeOf(THashList), 1024);
  if ASize = 0 then ASize := 17;
  Resize(ASize);
end;

constructor TYXDHashTable.Create;
begin
  Resize(0);
end;

procedure TYXDHashTable.Delete(AData: Pointer; AHash: THashType);
var
  AIndex, ACompare: Integer;
  AHashList, APrior: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := FBuckets[AIndex];
  APrior := nil;
  while Assigned(AHashList) do begin
    // 同一数据，哈希值我们只能认为是相同，如果不同，找上帝去吧
    if (AHashList.Data=AData) or ((Compare(AHashList.Data,AData,ACompare) and (ACompare=0))) then
    begin
      DoDelete(AHashList.Hash,AHashList.Data);
      if Assigned(APrior) then
        APrior.Next := AHashList.Next
      else
        FBuckets[AIndex] := AHashList.Next; // yangyxd 2014.10.8
      FreeBucket(AHashList);
      Dec(FCount);
      Break;
    end else begin
      APrior := AHashList;
      AHashList := APrior.Next;
    end;
  end;
end;

procedure TYXDHashTable.DeleteBucket(Index: Integer);
var
  ABucket, P: PHashList;
begin
  if (Index < 0) or (Index > High(FBuckets)) then Exit;
  ABucket := FBuckets[Index];
  FBuckets[Index] := nil;
  while ABucket <> nil do begin
    P := ABucket.Next;
    DoDelete(ABucket.Hash, ABucket.Data);
    FreeBucket(ABucket);
    Dec(FCount);
    ABucket := P;
  end;
end;

destructor TYXDHashTable.Destroy;
begin
  Clear;
  FreeAndNil(FPool);
end;

procedure TYXDHashTable.DoDelete(AHash: THashType; AData: Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
end;

function TYXDHashTable.Exists(AData: Pointer; AHash: THashType): Boolean;
var
  AList: PHashList;
  AResult: Integer;
begin
  AList := FindFirst(AHash);
  Result := False;
  while AList <> nil do begin
    if (AList.Data = AData) or (Compare(AList.Data,AData,AResult) and (AResult=0)) then begin
      Result:=True;
      Break;
    end;
    AList := FindNext(AList);
  end;
end;

function TYXDHashTable.Find(AHash: THashType): PHashList;
var
  AIndex: Integer;
  AList, AItem: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  Result := nil;
  AList := FBuckets[AIndex];
  while AList <> nil do begin
    if AList.Hash = AHash then begin
      New(AItem);
      AItem.Data := AList.Data;
      AItem.Next := Result;
      AItem.Hash := AHash;
      Result := AItem;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.Find(AData: Pointer; AHash: THashType): Pointer;
var
  ACmpResult: Integer;
  AList: PHashList;
begin
  Result := nil;
  AList := FindFirst(AHash);
  while AList<>nil do begin
    if (AList.Data = AData) or (Compare(AData, AList.Data, ACmpResult) and (ACmpResult=0)) then begin
      Result := AList.Data;
      Break;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.FindFirst(AHash: THashType): PHashList;
var
  AIndex: Integer;
  AList: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  Result := nil;
  AList := FBuckets[AIndex];
  while AList <> nil do begin
    if AList.Hash = AHash then begin
      Result := AList;
      Break;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.FindFirstData(AHash: THashType): Pointer;
begin
  Result := FindFirst(AHash);
  if Result <> nil then
    Result := PHashList(Result).Data;
end;

function TYXDHashTable.FindNext(AList: PHashList): PHashList;
begin
  Result := nil;
  if Assigned(AList) then begin
    Result := AList.Next;
    while Result<>nil do begin
      if Result.Hash=AList.Hash then
        Break
      else
        Result := Result.Next;
    end;
  end;
end;

procedure TYXDHashTable.FreeBucket(var ABucket: PHashList);
begin
  FPool.Push(ABucket);
end;

procedure TYXDHashTable.FreeHashList(AList: PHashList);
var
  ANext: PHashList;
begin
  while AList<>nil do begin
    ANext := AList.Next;
    FreeBucket(AList);
    AList := ANext;
  end;
end;

function TYXDHashTable.GetBucketCount: Integer;
begin
  Result := Length(FBuckets);
end;

function TYXDHashTable.GetBuckets(AIndex: Integer): PHashList;
begin
  Result := FBuckets[AIndex];
end;

function TYXDHashTable.GetMemSize: Int64;
begin
  Result := Length(FBuckets) shl 2;
end;

procedure TYXDHashTable.Resize(ASize: Cardinal);
var
  I, AIndex: Integer;
  AHash: Cardinal;
  ALastBuckets: THashArray;
  AList, ANext: PHashList;
begin
  if ASize = 0 then begin
    ASize := CalcBucketSize(FCount);
    if ASize = Cardinal(Length(FBuckets)) then
      Exit;
  end;

  //桶尺寸变更后，重新分配元素所在的哈希桶，如果是自动调用的话，理想的结果就是一个桶有一个元素
  if ASize <> Cardinal(Length(FBuckets)) then begin
    ALastBuckets := FBuckets;
    SetLength(FBuckets, ASize);
    for I := 0 to ASize-1 do
      FBuckets[I] := nil;
    for I := 0 to High(ALastBuckets) do begin
      AList := ALastBuckets[I];
      while AList<>nil do begin
        AHash := AList.Hash;
        AIndex := AHash mod ASize;
        ANext := AList.Next;
        AList.Next := FBuckets[AIndex];
        FBuckets[AIndex] := AList;
        AList := ANext;
      end;
    end;
  end;
end;

procedure TYXDHashTable.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if AutoSize then begin
      if (FCount div Length(FBuckets)) > 3 then
        Resize(0);
    end;
  end;
end;

procedure TYXDHashTable.Update(AData: Pointer; AOldHash, ANewHash: THashType);
var
  AList, APrior: PHashList;
  ACmpResult: Integer;
  AIndex: Integer;
  AChanged: Boolean;
begin
  AChanged := False;
  AIndex := AOldHash mod Cardinal(Length(FBuckets));
  AList := FBuckets[AIndex];
  APrior := nil;
  while AList <> nil do begin
    if (AList.Hash = AOldHash) then begin
      if (AList.Data=AData) or (Compare(AData, AList.Data, ACmpResult) and (ACmpResult=0)) then begin
        if Assigned(APrior) then
          APrior.Next := AList.Next
        else
          FBuckets[AIndex] := AList.Next;
        AList.Hash := ANewHash;
        AIndex := ANewHash mod Cardinal(Length(FBuckets));
        AList.Next := FBuckets[AIndex];
        FBuckets[AIndex] := AList;
        AChanged := True;
        Break;
      end;
    end;
    APrior := AList;
    AList := AList.Next;
  end;
  if not AChanged then
    Add(AData, ANewHash);
end;

{ TRBNode }

procedure TRBNode.Assign(src: PRBNode);
begin
  FParentColor := src.FParentColor;
  Left := src.Left;
  Right := src.Right;
  Data := src.Data;
end;

procedure TRBNode.Clear;
begin
  FParentColor := IntPtr(@Self);
end;

procedure TRBNode.Free;
begin
  if Left <> nil then begin
    Left.Free;
    Dispose(Left);
  end;
  if Right <> nil then begin
    Right.Free;
    Dispose(Right);
  end;
end;

function TRBNode.GetParent: PRBNode;
begin
  Result := PRBNode(IntPtr(FParentColor) and (not $3));
end;

function TRBNode.IsBlack: Boolean;
begin
  Result := (IntPtr(FParentColor) and $1) <> 0;
end;

function TRBNode.IsEmpty: Boolean;
begin
  Result := (FParentColor = IntPtr(@Self));
end;

function TRBNode.IsRed: Boolean;
begin
  Result := ((IntPtr(FParentColor) and $1)=0);
end;

function TRBNode.LeftDeepest: PRBNode;
begin
  Result := @Self;
  while True do begin
    if Result.Left <> nil then
      Result := Result.Left
    else if Result.Right <> nil then
      Result := Result.Right
    else
      Break;
	end;
end;

function TRBNode.Next: PRBNode;
var
  node, parent: PRBNode;
begin
  if IsEmpty then
    Result := nil
  else begin
    if Right <> nil then begin
      Result := Right;
      while Result.Left <> nil do
        Result := Result.Left;
      Exit;
    end;
    node := @Self;
    repeat
      Parent := node.Parent;
      if Assigned(Parent) and (node = Parent.Right) then
        node := Parent
      else
        Break;
    until Parent = nil;
    Result := Parent;
  end;
end;

function TRBNode.NextPostOrder: PRBNode;
begin
  Result := Parent;
  if (Result <> nil) and (@Self = Result.Left) and (Result.Right <> nil) then
    Result := Result.Right.LeftDeepest;
end;

function TRBNode.Prior: PRBNode;
var
  node, AParent: PRBNode;
begin
  if IsEmpty then
    Result := nil
  else begin
    if (Left <> nil) then begin
      Result := Left;
      while (Result.Right <> nil) do
        Result := Result.Right;
      Exit;
    end;
    node := @Self;
    repeat
      AParent := node.Parent;
      if (Parent <> nil) and (node = AParent.Left) then
        node := AParent
      else
        Break;
    until AParent = nil;
    Result := AParent;
  end;
end;

function TRBNode.RedParent: PRBNode;
begin
  Result := PRBNode(FParentColor);
end;

procedure TRBNode.SetBlack;
begin
  FParentColor := FParentColor or RB_BLACK;
end;

procedure TRBNode.SetParent(const Value: PRBNode);
begin
  FParentColor := IntPtr(Value) or (IntPtr(FParentColor) and $1);
end;

procedure TRBNode.SetParentAndColor(AParent: PRBNode; AColor: Integer);
begin
  FParentColor := IntPtr(AParent) or Cardinal(AColor);
end;

{ TRBTree }

procedure TRBTree.ChangeChild(AOld, ANew, parent: PRBNode);
begin
  if parent <> nil then begin
    if parent.Left = AOld then
      parent.Left := ANew
    else
      parent.Right := ANew;
  end else
    FRoot := ANew;
end;

procedure TRBTree.Clear;
var
  ANode: PRBNode;
begin
  if Assigned(OnDelete) then begin
    ANode := First;
    while ANode<>nil do begin
      OnDelete(Self, ANode);
      ANode := ANode.Next;
    end;
  end;
  if (FRoot <> nil) then begin
    FRoot.Free;
    Dispose(FRoot);
    FRoot := nil;
  end;
  FCount := 0;
end;

constructor TRBTree.Create(AOnCompare: TRBCompare);
begin
  FOnCompare := AOnCompare;
  FRBMempool := TMemPool.Create(SizeOf(TRBNode), 1024);
end;

function TRBTree.Delete(AChild: PRBNode): Pointer;
var
  rebalance: PRBNode;
begin
  Result := AChild.Data;
  rebalance := EraseAugmented(AChild);
  if rebalance <> nil then
    EraseColor(rebalance);
  AChild.Left := nil;
  AChild.Right := nil;
  Dec(FCount);
  if Assigned(FOnDelete) then
    FOnDelete(Self, AChild);
  AChild.Free;
  Dispose(AChild);
end;

destructor TRBTree.Destroy;
begin
  Clear;
  inherited;
  FreeAndNil(FRBMempool);
end;

procedure TRBTree.DoCopy(node1, node2: PRBNode);
begin
  if Assigned(FOnCopy) then
    FOnCopy(Self, node1, node2);
end;

procedure TRBTree.DoPropagate(node1, node2: PRBNode);
begin
  if Assigned(FOnPropagate) then
    FOnPropagate(Self, node1, node2);
end;

procedure TRBTree.DoRotate(AOld, ANew: PRBNode);
begin
  if Assigned(FOnRotate) then
    FOnRotate(Self, AOld, ANew);
end;

function TRBTree.EraseAugmented(node: PRBNode): PRBNode;
var
  child, tmp, AParent, rebalance: PRBNode;
  successor, child2: PRBNode;
  pc, pc2: IntPtr;
begin
  child := node.Right;
  tmp := node.Left;
  if tmp = nil then begin
    pc := node.FParentColor;
    AParent := node.Parent;
    ChangeChild(node, child, AParent);
    if child <> nil then begin
      child.FParentColor := pc;
      rebalance := nil;
    end else if (pc and RB_BLACK)<>0 then
      rebalance := AParent
    else
      rebalance := nil;
    tmp := AParent;
  end else if (child = nil) then begin
    tmp.FParentColor:=node.FParentColor;
    AParent := node.Parent;
    ChangeChild(node, tmp, AParent);
    rebalance := nil;
    tmp := AParent;
  end else begin
    successor := child;
    tmp := child.Left;
    if tmp = nil then begin
      AParent := successor;
      child2 := successor.Right;
      DoCopy(node, successor);
    end else begin
      repeat
        AParent := successor;
        successor := tmp;
        tmp := tmp.Left;
      until tmp=nil;
      AParent.Left := successor.Right;
      child2 := successor.Right;
      successor.Right := child;
      child.Parent := successor;
      DoCopy(node, successor);
      DoPropagate(AParent, successor);
    end;
    successor.Left := node.Left;
    tmp := node.Left;
    tmp.Parent := successor;
    pc := node.FParentColor;
    tmp := node.Parent;
    ChangeChild(node, successor, tmp);
    if child2 <> nil then begin
      successor.FParentColor := pc;
      child2.SetParentAndColor(AParent, RB_BLACK);
      rebalance := nil;
    end else begin
      pc2 := successor.FParentColor;
      successor.FParentColor := pc;
      if (pc2 and RB_BLACK)<>0 then
        rebalance := AParent
      else
        rebalance:=nil;
    end;
    tmp := successor;
  end;
  DoPropagate(tmp, nil);
  Result := rebalance;
end;

procedure TRBTree.EraseColor(AParent: PRBNode);
var
  node, sibling, tmp1, tmp2: PRBNode;
begin
  node := nil;
  while (true)do begin
    sibling := AParent.Right;
    if node <> sibling then begin
      {$REGION 'node<>sibling'}
      if sibling.IsRed then begin
        {$REGION 'slbling.IsRed'}
        AParent.Right := sibling.Left;
        tmp1 := sibling.Left;
        sibling.Left := AParent;
        tmp1.SetParentAndColor(AParent, RB_BLACK);
        RotateSetParents(AParent, sibling, RB_RED);
        DoRotate(AParent, sibling);
        sibling := tmp1;
      end;
      {$ENDREGION 'slbling.IsRed'}
      tmp1:=sibling.Right;
      if (not Assigned(tmp1)) or tmp1.IsBlack then begin
        {$REGION 'tmp1.IsBlack'}
        tmp2 := sibling.Left;
        if (not Assigned(tmp2)) or tmp2.IsBlack then begin
          {$REGION 'tmp2.IsBlack'}
          sibling.SetParentAndColor(AParent, RB_RED);
          if AParent.IsRed then
            AParent.SetBlack
          else begin
            Node:=AParent;
            AParent:=node.Parent;
            if Assigned(AParent) then
              Continue;
          end;
          Break;
          {$ENDREGION 'tmp2.IsBlack'}
        end;
        sibling.Left := tmp2.Right;
        tmp1 := tmp2.Right;
        tmp2.Right := sibling;
        AParent.Right := tmp2;
        if (tmp1 <> nil) then
          tmp1.SetParentAndColor(sibling, RB_BLACK);
        DoRotate(sibling, tmp2);
        tmp1 := sibling;
        sibling := tmp2;
        {$ENDREGION 'tmp1.IsBlack'}
      end;
      AParent.Right := sibling.Left;
      tmp2 := sibling.Left;
      sibling.Left := AParent;
      tmp1.SetParentAndColor(sibling, RB_BLACK);
      if (tmp2 <> nil) then
        tmp2.Parent:=AParent;
      RotateSetParents(AParent, sibling, RB_BLACK);
      DoRotate(AParent, sibling);
      Break;
      {$ENDREGION 'node<>sibling'}
    end else begin
      {$REGION 'RootElse'}
      sibling := AParent.Left;
      if (sibling.IsRed) then begin
        {$REGION 'Case 1 - right rotate at AParent'}
        AParent.Left := sibling.Right;
        tmp1 := sibling.Right;
        tmp1.SetParentAndColor(AParent, RB_BLACK);
        RotateSetParents(AParent, sibling, RB_RED);
        DoRotate(AParent, sibling);
        sibling := tmp1;
        {$ENDREGION 'Case 1 - right rotate at AParent'}
      end;
      tmp1 := sibling.Left;
      if (tmp1=nil) or tmp1.IsBlack then begin
        {$REGION 'tmp1.IsBlack'}
        tmp2 := sibling.Right;
        if (tmp2=nil) or tmp2.IsBlack then begin
          {$REGION 'tmp2.IsBlack'}
          sibling.SetParentAndColor(AParent, RB_RED);
          if AParent.IsRed then
            AParent.SetBlack
          else begin
            node := AParent;
            AParent := node.Parent;
            if Assigned(AParent) then
              continue;
          end;
          break;
          {$ENDREGION 'tmp2.IsBlack'}
        end;
        sibling.Right := tmp2.Left;
        tmp1 := tmp2.Left;
        tmp2.Left := sibling;
        AParent.Left := tmp2;
        if Assigned(tmp1) then
          tmp1.SetParentAndColor(sibling, RB_BLACK);
        DoRotate(sibling, tmp2);
        tmp1 := sibling;
        sibling := tmp2;
        {$ENDREGION ''tmp1.IsBlack'}
      end;
      AParent.Left := sibling.Right;
      tmp2 := sibling.Right;
      sibling.Right := AParent;
      tmp1.SetParentAndColor(sibling, RB_BLACK);
      if Assigned(tmp2) then
        tmp2.Parent := AParent;
      RotateSetParents(AParent, sibling, RB_BLACK);
      DoRotate(AParent, sibling);
      Break;
      {$ENDREGION 'RootElse'}
    end;
  end;
end;

function TRBTree.Find(AData: Pointer): PRBNode;
var
  rc:Integer;
begin
  Result := FRoot;
  while Assigned(Result) do begin
    rc := OnCompare(AData,Result.Data);
    if rc < 0 then
      Result := Result.Left
    else if rc>0 then
      Result := Result.Right
    else
      Break;
	end
end;

function TRBTree.First: PRBNode;
begin
  Result := FRoot;
  if Result<>nil then begin
    while Assigned(Result.Left) do
      Result := Result.Left;
  end;
end;

function TRBTree.FirstPostOrder: PRBNode;
begin
  if Assigned(FRoot) then
    Result := FRoot.LeftDeepest
  else
    Result := nil;
end;

function TRBTree.GetIsEmpty: Boolean;
begin
  Result := (FRoot = nil);
end;

function TRBTree.Insert(AData: Pointer): Boolean;
var
  ANew: PPRBNode;
  parent, AChild: PRBNode;
  rc: Integer;
begin
  parent := nil;
  ANew := @FRoot;
  while ANew^ <> nil do begin
    parent := ANew^;
    rc := OnCompare(AData, parent.Data);
    if rc < 0 then
      ANew := @parent.Left
    else if rc > 0 then
      ANew := @parent.Right
    else begin //已存在
      Result := False;
      Exit;
    end;
  end;
  New(AChild);
  AChild.Data := AData;
  LinkNode(AChild, parent, ANew^);
  InsertNode(AChild);
  Inc(FCount);
  Result := True;
end;

procedure TRBTree.InsertColor(AChild: PRBNode);
begin
  InsertNode(AChild);
end;

procedure TRBTree.InsertNode(node: PRBNode);
var
  AParent, GParent, tmp: PRBNode;
begin
  AParent := Node.RedParent;
  while True do begin
    if AParent = nil then begin
      node.SetParentAndColor(nil, RB_BLACK);
      Break;
    end else if AParent.IsBlack then
      Break;
    gParent := AParent.RedParent;
    tmp := gParent.Right;
    if AParent <> tmp then begin
      if (tmp <> nil) and tmp.IsRed then begin
        tmp.SetParentAndColor(gParent, RB_BLACK);
        AParent.SetParentAndColor(gParent, RB_BLACK);
        node := gParent;
        AParent := node.Parent;
        node.SetParentAndColor(AParent, RB_RED);
        continue;
      end;
      tmp := AParent.Right;
      if node = tmp then begin
        AParent.Right := node.Left;
        tmp := node.Left;
        node.Left := AParent;
        if (tmp <> nil) then
          tmp.SetParentAndColor(AParent, RB_BLACK);
        AParent.SetParentAndColor(node, RB_RED);
        DoRotate(AParent, node);//augment_rotate(parent,node)
        AParent := node;
        tmp := Node.Right;
      end;
      gParent.Left := tmp;
      AParent.Right := gParent;
      if tmp <> nil then
        tmp.SetParentAndColor(gParent, RB_BLACK);
      RotateSetParents(gParent, AParent, RB_RED);
      DoRotate(gParent, AParent);
      Break;
    end else begin
      tmp := gParent.Left;
      if Assigned(tmp) and tmp.IsRed then begin
        tmp.SetParentAndColor(gParent, RB_BLACK);
        AParent.SetParentAndColor(gParent, RB_BLACK);
        node:=gParent;
        AParent:=node.Parent;
        node.SetParentAndColor(AParent,RB_RED);
        continue;
      end;
      tmp := AParent.Left;
      if node = tmp then begin
        AParent.Left := node.Right;
        tmp := Node.Right;
        node.Right := AParent;
        if tmp <> nil then
          tmp.SetParentAndColor(AParent, RB_BLACK);
        AParent.SetParentAndColor(node, RB_RED);
        DoRotate(AParent, node);
        AParent := node;
        tmp := node.Left;
      end;
      gParent.Right := tmp;
      AParent.Left := gParent;
      if tmp <> nil then
        tmp.SetParentAndColor(gParent, RB_BLACK);
      RotateSetParents(gparent, AParent, RB_RED);
      DoRotate(gParent, AParent);
      Break;
    end;
  end;
end;

function TRBTree.Last: PRBNode;
begin
  Result := FRoot;
  if Result<>nil then begin
    while Assigned(Result.Right) do
      Result := Result.Right;
	end;
end;

procedure TRBTree.LinkNode(node, parent: PRBNode; var rb_link: PRBNode);
begin
  node.FParentColor := IntPtr(parent);
  node.Left := nil;
  node.Right := nil;
  rb_link := node;
end;

procedure TRBTree.Replace(Src, ANew: PRBNode);
var
  parent: PRBNode;
begin
  parent := Src.Parent;
  ChangeChild(Src, ANew, parent);
  if Assigned(Src.Left) then
    Src.Left.SetParent(ANew)
  else
    Src.Right.SetParent(ANew);
  ANew.Assign(Src);
end;

procedure TRBTree.RotateSetParents(AOld, ANew: PRBNode; color: Integer);
var
  AParent: PRBNode;
begin
  AParent := AOld.Parent;
  ANew.FParentColor := AOld.FParentColor;
  AOld.SetParentAndColor(ANew, color);
  ChangeChild(AOld, ANew, AParent);
end;

{$IFDEF USE_ATOMIC}
{$IF RTLVersion<26}
function AtomicCmpExchange(var Target: Integer; Value: Integer; Comparand: Integer): Integer; inline;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
end;

function AtomicIncrement(var Target: Integer): Integer; inline;
begin
  Result := InterlockedIncrement(Target);
end;

function AtomicDecrement(var Target: Integer): Integer; inline;
begin
  Result := InterlockedDecrement(Target);
end;

function AtomicExchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := InterlockedExchange(Target, Value);
end;
{$IFEND <XE5}

// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result and AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result or AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 原子加法，返回原值
function AtomicAdd(var Dest: Integer; const AValue: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result + AValue;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;
{$ENDIF}

{ TYXDHashMapTable }
  
procedure TYXDHashMapTable.Add(const Key: string; AData: PHashValue);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  AIndex := HashOf(Key);
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := AIndex;
  AIndex := AIndex mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(DWORD(ABucket) + SizeOf(THashMapList));
  Initialize(ABucket.Data.Key);
  if AData <> nil then   
    ABucket.Data.Value := AData^
  else
    ABucket.Data.Value.Clear;
  ABucket.Data.IsStrKey := True;
  ABucket.Data.Key := Key;
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Add(const Key: Number; AData: PHashValue);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := THashType(Key);
  AIndex := THashType(Key) mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(DWORD(ABucket) + SizeOf(THashMapList));
  if AData <> nil then   
    ABucket.Data.Value := AData^
  else
    ABucket.Data.Value.Clear;
  ABucket.Data.IsStrKey := False;
  PDWORD(@ABucket.Data.Key)^ := THashType(Key);
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
  Inc(FCount);
end;

procedure TYXDHashMapTable.Add(const Key: string; AData: Integer);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  AIndex := HashOf(Key);
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := AIndex;
  AIndex := AIndex mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(DWORD(ABucket) + SizeOf(THashMapList));
  Initialize(ABucket.Data.Key);
  ABucket.Data.Value.Data := Pointer(AData);
  ABucket.Data.Value.Size := 0;
  ABucket.Data.IsStrKey := True;
  ABucket.Data.Key := Key;
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Add(const Key: Number; AData: Integer);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := THashType(Key);
  AIndex := THashType(Key) mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(DWORD(ABucket) + SizeOf(THashMapList));
  ABucket.Data.Value.Data := Pointer(AData);
  ABucket.Data.Value.Size := 0;
  ABucket.Data.IsStrKey := False;
  PDWORD(@ABucket.Data.Key)^ := THashType(Key);
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Clear;
var
  I: Integer;
  P, N: PHashList;
begin
  for I := 0 to High(FBuckets) do begin
    P := FBuckets[I];
    FBuckets[I] := nil;
    while P <> nil do begin
      N := P^.Next;
      DoDelete(P.Hash, P.Data);
      FreeBucket(P);
      P := N;
    end;
  end;
  FCount := 0;
  FListPool.Clear;
  FPool.Clear;
end;

constructor TYXDHashMapTable.Create(ASize: Integer);
begin
  inherited;
  FListPool := TMemPool.Create(HASHITEMSize, 1024);
end;

destructor TYXDHashMapTable.Destroy;
begin
  inherited;
  FreeAndNil(FListPool);
end;

procedure TYXDHashMapTable.DoAdd(ABucket: PHashMapList);
begin
end;

function TYXDHashMapTable.Exists(const Key: Number): Boolean;
begin
  Result := Find(Key) <> nil;
end;

function TYXDHashMapTable.Exists(const Key: string): Boolean;
begin
  Result := Find(Key) <> nil;
end;

function TYXDHashMapTable.Find(const Key: string): PHashMapValue;
var
  AList: PHashList;
  AHash: Cardinal;
begin
  AHash := HashOf(Key);
  AList := FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while AList <> nil do begin
    if (AList.Hash = AHash) and (AList.Data <> nil) and (PHashMapValue(AList.Data).IsStrKey) and
      (PHashMapValue(AList.Data).Key = Key) then begin
      Result := AList.Data;
      Exit;
    end;
    AList := AList.Next;
  end;
  Result := nil;
end;

function TYXDHashMapTable.Find(const Key: Number): PHashMapValue;
var
  AList: PHashList;
  AHash: THashType;
begin
  AHash := THashType(Key);
  AList := FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while AList <> nil do begin
    if (AList.Hash = AHash) and (AList.Data <> nil) and (not PHashMapValue(AList.Data).IsStrKey) then begin
      Result := AList.Data;
      Exit;
    end;
    AList := AList.Next;
  end;
  Result := nil;
end;

function TYXDHashMapTable.FindList(const Key: Number): PPHashList;
begin
  Result := @FBuckets[THashType(Key) mod Cardinal(Length(FBuckets))];
  while Result^ <> nil do begin
    if (Result^.Hash = THashType(Key)) and (Result^.Data <> nil) and
      (not PHashMapValue(Result^.Data).IsStrKey) then
      Break;
    Result := @Result^.Next;
  end;
end;

function TYXDHashMapTable.FindList(const Key: string): PPHashList;
var
  AHash: Cardinal;
begin
  AHash := HashOf(Key);
  Result := @FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while Result^ <> nil do begin
    if (Result^.Hash = AHash) and (Result^.Data <> nil) and (PHashMapValue(Result^.Data).IsStrKey) and
      (PHashMapValue(Result^.Data).Key = Key) then begin
      Break;
    end;
    Result := @Result^.Next;
  end;
end;

procedure TYXDHashMapTable.FreeBucket(var ABucket: PHashList);
begin
  if PHashMapList(ABucket).Data.IsStrKey then  
    Finalize(PHashMapList(ABucket).Data.Key);
  FListPool.Push(ABucket);
end;

function TYXDHashMapTable.GetMemSize: Int64;
begin
  Result := inherited GetMemSize;
end;

function TYXDHashMapTable.Remove(const Key: Number): Boolean;
var
  AIndex: Integer;
  AHash: THashType;
  AHashList: PPHashList;
  APrior: PHashList;
begin
  Result := False;
  AHash := THashType(Key);
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := @FBuckets[AIndex];
  while AHashList^ <> nil do begin
    if (AHashList^.Hash = AHash) and (not PHashMapValue(AHashList^.Data).IsStrKey) then begin
      APrior := AHashList^;
      AHashList^ := APrior.Next;
      DoDelete(APrior.Hash, APrior.Data);
      FreeBucket(APrior);
      Dec(FCount);
      Result := True;
      Break;
    end else
      AHashList := @AHashList^.Next;
  end;
end;

function TYXDHashMapTable.Update(const Key: string;
  Value: PHashValue): Boolean;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if P <> nil then begin
    if Value <> nil then
      P.Value := Value^
    else
      P.Value.Clear;
    Result := True;
  end else
    Result := False;
end;

function TYXDHashMapTable.Remove(const Key: string): Boolean;
var
  AIndex: Integer;
  AHash: Cardinal;
  AHashList: PPHashList;
  APrior: PHashList;
begin
  Result := False;
  AHash := HashOf(Key);
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := @FBuckets[AIndex];
  while AHashList^ <> nil do begin
    if (AHashList^.Hash = AHash) and (PHashMapValue(AHashList^.Data).IsStrKey) and
      (PHashMapValue(AHashList^.Data).Key = Key) then begin
      APrior := AHashList^;
      AHashList^ := APrior.Next;
      DoDelete(APrior.Hash, APrior.Data);
      FreeBucket(APrior);
      Dec(FCount);
      Result := True;
      Break;
    end else
      AHashList := @AHashList^.Next;
  end;
end;

function TYXDHashMapTable.ValueOf(const Key: string): PHashValue;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if (P <> nil) then // and (P.Value.Size > 0) then
    Result := @P.Value
  else
    Result := nil;
end;

function TYXDHashMapTable.ValueOf(const Key: Number): PHashValue;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if (P <> nil) then // and (P.Value.Size > 0) then
    Result := @P.Value
  else
    Result := nil;
end;

function TYXDHashMapTable.Update(const Key: Number; Value: PHashValue): Boolean;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if P <> nil then begin
    if Value <> nil then
      P.Value := Value^
    else
      P.Value.Clear;
    Result := True;
  end else
    Result := False;  
end;

function TYXDHashMapTable.Remove(const P: PHashMapValue): Boolean;
begin
  if P <> nil then begin
    if P.IsStrKey then
      Result := Remove(P.Key)
    else
      Result := Remove(P.GetNumKey)
  end else
    Result := False;
end;

{ TYXDHashMapList }

procedure TYXDHashMapList.Clear;
begin
  FList.Clear;
  inherited; 
end;

constructor TYXDHashMapList.Create(ASize: Integer);
begin
  inherited;
  FList := TList.Create;
end;

procedure TYXDHashMapList.Delete(Index: Integer);
begin
  if (index >= 0) and (Index < FCount) then
    Remove(Items[index].Key);
end;

destructor TYXDHashMapList.Destroy;
begin
  inherited;
  FreeAndNil(FList);
end;

procedure TYXDHashMapList.DoAdd(ABucket: PHashMapList);
begin
  FList.Add(ABucket.Data);  
end;

procedure TYXDHashMapList.DoDelete(AHash: THashType; AData: Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
  if FList.Count > 0 then   
    FList.Remove(AData);
end;

function TYXDHashMapList.GetItem(Index: Integer): PHashMapValue;
begin
  Result := FList.Items[index];
end; 

{ TYXDHashMapLinkTable }

procedure TYXDHashMapLinkTable.Clear;
begin
  if Assigned(Self) then begin
    FreeLinkList;
    inherited Clear;
  end;
end;

constructor TYXDHashMapLinkTable.Create(ASize: Integer);
begin
  inherited Create(ASize);
  FFirst := nil;
  FLast := nil;
  FLinkHashPool := TMemPool.Create(SizeOf(THashList), 1024);
  SetLength(ListBuckets, ASize);
end;

procedure TYXDHashMapLinkTable.Delete(Index: Integer);
var
  P: PHashMapValue;
begin
  P := GetItem(Index);
  if P <> nil then
    Remove(P.Key);
end;

destructor TYXDHashMapLinkTable.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLinkHashPool);
end;

procedure TYXDHashMapLinkTable.DoAdd(ABucket: PHashMapList);
var
  AIndex: Integer;
  AItem: PHashList;
  P: PHashMapLinkItem;
begin
  P := Pointer(FPool.Pop);
  P.Value := ABucket.Data;
  P.Next := nil;
  if FFirst = nil then begin
    P.Prev := nil;
    FFirst := P;
    FLast := FFirst;
  end else begin
    P.Prev := FLast;
    FLast.Next := P;
    FLast := P;
  end;

  // 添加到Hash表中
  AIndex := Cardinal(ABucket.Data) mod Cardinal(Length(ListBuckets));
  AItem := ListBuckets[AIndex];
  while AItem <> nil do begin
    if AItem.Hash = THashType(ABucket.Data) then begin
      AItem.Data := FLast;
      Exit
    end else
      AItem := AItem.Next;
  end;
  AItem := FLinkHashPool.Pop;
  AItem^.Hash := THashType(ABucket.Data);
  AItem^.Data := FLast;
  AItem^.Next := ListBuckets[AIndex];
  ListBuckets[AIndex] := AItem;
end;

procedure TYXDHashMapLinkTable.DoDelete(AHash: THashType; AData: Pointer);
var
  P: PHashMapLinkItem;
begin
  P := FindLinkItem(AData, True);
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
  if P = nil then Exit;
  if P = FFirst then begin
    FFirst := FFirst.Next;
    if FFirst = nil then
      FLast := nil
    else
      FFirst.Prev := nil;
  end else if P = FLast then begin
    FLast := P.Prev;
    if FLast = nil then
      FFirst := nil
    else
      FLast.Next := nil;
  end else begin
    P.Prev.Next := P.Next;
    P.Next.Prev := P.Prev;
  end;
  FPool.Push(Pointer(P));
end;

function TYXDHashMapLinkTable.FindLinkItem(AData: Pointer;
  isDelete: Boolean): PHashMapLinkItem;
var
  Prev: PPHashList;
  P: PHashList;
begin
  Prev := @ListBuckets[Cardinal(AData) mod Cardinal(Length(ListBuckets))];
  while Prev^ <> nil do begin
    if PHashMapLinkItem(Prev^.Data).Value = AData then begin
      if isDelete then begin
        P := Prev^;
        Result := P.Data;
        Prev^ := P.Next;
        FLinkHashPool.Push(P);
      end else
        Result := Prev^.Data;
      Exit;
    end else
      Prev := @Prev^.Next;
  end;
  Result := nil;
end;

procedure TYXDHashMapLinkTable.FreeLinkList;
var
  P, N: PHashMapLinkItem;
  I: Integer;
  P1, P2: PHashList;
begin
  P := FFirst;
  while P <> nil do begin
    N := P.Next;
    FPool.Push(Pointer(P));
    P := N;
  end;

  if Length(ListBuckets) > 0 then begin
    for I := 0 to Length(ListBuckets) - 1 do begin
      P1 := ListBuckets[i];
      ListBuckets[i] := nil;
      while P1 <> nil do begin
        P2 := P1.Next;
        FLinkHashPool.Push(P1);
        P1 := P2;
      end;
    end;
  end;

  FLinkHashPool.Clear;
  FFirst := nil;
  FLast := nil;
end;

function TYXDHashMapLinkTable.GetEnumerator: TYXDHashMapLinkTableEnumerator;
begin
  Result := TYXDHashMapLinkTableEnumerator.Create(Self);
end;

function TYXDHashMapLinkTable.GetItem(Index: Integer): PHashMapValue;
var
  P: PHashMapLinkItem;
  I: Integer;
begin
  if Index > (FCount shr 1) then begin
    if Index < FCount then begin
      P := FLast;
      if P <> nil then begin
        for I := FCount - Index - 1 downto 1 do
          P := P.Prev;
        Result := P.Value;
        Exit;
      end;
    end;
  end else if Index > -1 then begin
    P := FFirst;
    if P <> nil then begin
      for I := 0 to Index - 1 do
        P := P.Next;
      Result := P.Value;
      Exit;
    end;
  end;
  Result := nil;
end; 

function TYXDHashMapLinkTable.GetLast: PHashMapValue;
begin
  if FLast <> nil then
    Result := FLast.Value
  else
    Result := nil;
end;

function TYXDHashMapLinkTable.GetMemSize: Int64;
begin
  Result := inherited GetMemSize;
  Inc(Result, Length(ListBuckets) shl 2);
end;

{ TLinkedList }

procedure TLinkedList.Add(V: TObject);
begin
  AddLast(V);
end;

procedure TLinkedList.Add(const APosition: Integer; V: TObject);
begin
  if isPositionIndex(APosition) then begin
    if APosition = FCount then
      AddLast(V)
    else
      linkBefore(V, Node(APosition));
  end else
    DoOutOfBoundsError(APosition);
end;

procedure TLinkedList.AddAll(V: TLinkedList);
var
  P: PLinkItem;
begin
  if Assigned(V) then begin
    P := V.FFirst;
    while P <> nil do begin
      AddLast(P.Value);
      P := P.Next;
    end;
  end;
end;

procedure TLinkedList.AddAll(V: array of TObject);
var
  I: Integer;
begin
  for I := 0 to High(V) do
    AddLast(V[i]);   
end;

procedure TLinkedList.AddFirst(V: TObject);
var
  P: PLinkItem;
begin
  New(P);
  P.Value := V;
  P.Prev := nil;
  P.Next := FFirst;
  if FFirst = nil then
    FLast := P
  else
    FFirst.Prev := P;
  FFirst := P;
  Inc(FCount);
end;

procedure TLinkedList.AddLast(V: TObject);
var
  P: PLinkItem;
begin
  New(P);
  P.Value := V;
  if FFirst = nil then begin
    P.Prev := nil;
    FFirst := P;
    FLast := FFirst;
  end else begin
    P.Prev := FLast;
    FLast.Next := P;
    FLast := FLast.Next;
  end;
  FLast.Next := nil;
  Inc(FCount);  
end;

procedure TLinkedList.Clear;
var
  P, N: PLinkItem;
begin
  P := FFirst;
  while P <> nil do begin
    N := P.Next;
    DoDelete(P, lla_clear);
    P := N;
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

function TLinkedList.Contains(V: TObject): Boolean;
begin
  Result := IndexOf(V) > -1;
end;

constructor TLinkedList.Create;
begin
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TLinkedList.DoDefaultCompareItem(const A, B: TObject): Integer;
begin
  if Assigned(FOnCompareItem) then
    Result := FOnCompareItem(A, B)
  else begin
    if Integer(A) - Integer(B) = 0 then
      Result := 1
    else
      Result := 0;
  end;
end;

procedure TLinkedList.DoDelete(var P: PLinkItem; Aciton: TLinkedListAction);
begin
  if Assigned(FOnDelete) then
    FOnDelete(P.Value, Aciton);
  Dispose(P);
end;

procedure TLinkedList.DoOutOfBoundsError(const Index: Integer);
begin
  raise Exception.Create(Format('OutOfBounds. Index: %d, Size: %d.', [Index, FCount]))
end;

function TLinkedList.Element: TObject;
begin
  if FFirst <> nil then
    Result := FFirst.Value
  else
    Result := nil;
end;

function TLinkedList.Exist(V: TObject): Boolean;
begin
  Result := IndexOf(V) > -1;
end;

function TLinkedList.GetCount: Integer;
begin
  Result := FCount;
end;

function TLinkedList.GetIsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TLinkedList.GetItem(const Index: Integer): TObject;
begin
  if isElementIndex(Index) then
    Result := Node(Index).Value
  else begin
    Result := nil;
    DoOutOfBoundsError(Index);
  end;
end;

function TLinkedList.IndexOf(V: TObject): Integer;
var
  P: PLinkItem;
begin
  P := FFirst;
  Result := -1;
  while P <> nil do begin
    Inc(Result);
    if DoDefaultCompareItem(P.Value, V) > 0 then
      Exit
    else
      P := P.Next;
  end;
  Result := -1;
end;

function TLinkedList.isElementIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FCount);
end;

function TLinkedList.isPositionIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= FCount);
end;

function TLinkedList.LastIndexOf(V: TObject): Integer;
var
  P: PLinkItem;
begin
  P := FLast;
  Result := FCount;
  while P <> nil do begin
    Dec(Result);
    if DoDefaultCompareItem(P.Value, V) > 0 then
      Exit
    else
      P := P.Prev;
  end;
  Result := -1;
end;

procedure TLinkedList.linkBefore(const V: TObject; const P: PLinkItem);
var
  NewP: PLinkItem;
begin
  New(NewP);
  NewP.Value := V;
  NewP.Next := P;
  NewP.Prev := P.Prev;
  if P.Prev = nil then
    FFirst := NewP
  else begin
    P.Prev.Next := NewP;
    P.Prev := NewP;
  end;
  Inc(FCount);
end;

function TLinkedList.Node(const AIndex: Integer): PLinkItem;
var
  I: Integer;
begin
  if (AIndex < (FCount shr 1)) then begin
    Result := FFirst;
    for I := 0 to AIndex do
      Result := Result.Next;
  end else begin
    Result := FLast;
    for I := FCount - 1 downto AIndex do
      Result := Result.Prev;  
  end; 
end;

function TLinkedList.Offer(V: TObject): Boolean;
begin
  Add(V);
  Result := True;
end;

function TLinkedList.OfferFirst(V: TObject): Boolean;
begin
  AddFirst(V);
  Result := True;
end;

function TLinkedList.OfferLast(V: TObject): Boolean;
begin
  AddLast(V);
  Result := True;
end;

function TLinkedList.Peek: TObject;
begin
  if FFirst <> nil then
    Result := FFirst.Value
  else
    Result := nil;
end;

function TLinkedList.PeekLast: TObject;
begin
  if FLast <> nil then
    Result := FLast.Value
  else
    Result := nil;
end;

function TLinkedList.Poll: TObject;
begin
  Result := PollFirst;
end;

function TLinkedList.PollFirst: TObject;
begin
  if FFirst <> nil then begin
    Result := FFirst.Value;
    unlinkNode(FFirst);
  end else
    Result := nil;
end;

function TLinkedList.PollLast: TObject;
begin
  if FLast <> nil then begin
    Result := FLast.Value;
    unlinkNode(FLast);
  end else
    Result := nil;
end;

function TLinkedList.Pop: TObject;
begin
  if FFirst <> nil then begin
    Result := FFirst.Value;
    unlinkNode(FFirst);
  end else
    Result := nil;
end;

procedure TLinkedList.Push(V: TObject);
begin
  AddFirst(V);
end;

function TLinkedList.Remove(Index: Integer): Boolean;
begin
  if not isElementIndex(Index) then
    Result := False
  else
    Result := unlinkNode(Node(Index));
end;

function TLinkedList.Remove: TObject;
begin
  Result := Pop;
end;

function TLinkedList.Remove(V: TObject): Boolean;
var
  P: PLinkItem;
begin
  P := FFirst;
  while P <> nil do begin
    if DoDefaultCompareItem(P.Value, V) > 0 then begin
      Result := unlinkNode(P);
      Exit;
    end else
      P := P.Next;
  end;
  Result := False;
end;

function TLinkedList.RemoveFirst: TObject;
begin
  Result := Pop;
end;

function TLinkedList.RemoveLast: TObject;
begin
  Result := PollLast();
end;

procedure TLinkedList.SetItem(const Index: Integer; const Value: TObject);
var
  P: PLinkItem;
begin
  if isElementIndex(Index) then begin
    P := Node(Index);
    if P <> nil then begin
      if Assigned(FOnDelete) then
        FOnDelete(P.Value, lla_replace);
      P.Value := Value
    end;
  end else
    DoOutOfBoundsError(Index); 
end;

function TLinkedList.unlinkNode(P: PLinkItem): Boolean;
begin
  if P = nil then
    Result := False
  else begin
    if P = FFirst then begin
      FFirst := FFirst.Next;
      if FFirst = nil then
        FLast := nil
      else
        FFirst.Prev := nil;
    end else if P = FLast then begin
      FLast := P.Prev;
      if FLast = nil then
        FFirst := nil
      else
        FLast.Next := nil;
    end else begin
      P.Prev.Next := P.Next;
      P.Next.Prev := P.Prev;
    end;
    DoDelete(P, lla_unlink);
    Dec(FCount);
    Result := True;
  end;
end;

{ TYXDHashMapLinkTableEnumerator }

constructor TYXDHashMapLinkTableEnumerator.Create(AList: TYXDHashMapLinkTable);
begin
  FItem := AList.FFirst;
end;

function TYXDHashMapLinkTableEnumerator.GetCurrent: PHashMapLinkItem;
begin
  Result := FItem;
  FItem := FItem.Next;
end;

function TYXDHashMapLinkTableEnumerator.MoveNext: Boolean;
begin
  Result := FItem <> nil;
end;

end.


