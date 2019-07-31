unit YxdRBTree;

interface

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE USEINLINE}
{$IFEND}

uses
  YxdMemPool,
  SysUtils, Classes, SyncObjs;

type
  /// <summary>比较函数</summary>
  /// <param name='P1'>第一个要比较的参数</param>
  /// <param name='P2'>第二个要比较的参数</param>
  /// <returns> 如果P1<P2，返回小于0的值，如果P1>P2返回大于0的值，如果相等，返回0</returns>
  TYXDCompare = function (P1, P2:Pointer): Integer of object;

type
  PRBNode = ^TRBNode;
  PPRBNode = ^PRBNode;
  TRBNode = {$IFNDEF USEINLINE}object{$ELSE}packed record{$ENDIF}
  private
    FParentColor: IntPtr;
    function GetParent: PRBNode;
    procedure SetParent(const Value: PRBNode);
    function RedParent: PRBNode; {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure SetBlack; {$IFDEF USEINLINE}inline;{$ENDIF}
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
    function IsEmpty: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    // 是否是黑结点
    function IsBlack: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    // 是否为红结点
    function IsRed: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
    // 最深的最左结点
    function LeftDeepest: PRBNode;
    // 设置父结点和颜色
    procedure SetParentAndColor(AParent: PRBNode; AColor:Integer); {$IFDEF USEINLINE}inline;{$ENDIF}
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
    function GetIsEmpty: Boolean; {$IFDEF USEINLINE}inline;{$ENDIF}
  protected
    FRoot: PRBNode;
    FCount: Integer;
    FRBMempool: TMemPool;
    FOnCompare: TYXDCompare;
    FOnDelete: TRBDeleteNotify;
    FOnRotate: TRBRotateNotify;
    FOnCopy: TRBCopyNotify;
    FOnPropagate: TRBPropagateNotify;
    function EraseAugmented(node: PRBNode): PRBNode; {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure RotateSetParents(AOld, ANew: PRBNode; color: Integer); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure EraseColor(AParent: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure ChangeChild(AOld, ANew, parent: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure DoCopy(node1, node2: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure DoPropagate(node1, node2: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure InsertColor(AChild: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure InsertNode(node: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure DoRotate(AOld,ANew: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
    procedure LinkNode(node,parent: PRBNode; var rb_link: PRBNode); {$IFDEF USEINLINE}inline;{$ENDIF}
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

implementation

const
  RB_RED = 0;
  RB_BLACK = 1;

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
  node, LParent: PRBNode;
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
      LParent := node.Parent;
      if Assigned(LParent) and (node = LParent.Right) then
        node := LParent
      else
        Break;
    until LParent = nil;
    Result := LParent;
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
  FParentColor := IntPtr(AParent) or IntPtr(AColor);
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
      {.$REGION 'node<>sibling'}
      if sibling.IsRed then begin
        {.$REGION 'slbling.IsRed'}
        AParent.Right := sibling.Left;
        tmp1 := sibling.Left;
        sibling.Left := AParent;
        tmp1.SetParentAndColor(AParent, RB_BLACK);
        RotateSetParents(AParent, sibling, RB_RED);
        DoRotate(AParent, sibling);
        sibling := tmp1;
      end;
      {.$ENDREGION 'slbling.IsRed'}
      tmp1:=sibling.Right;
      if (not Assigned(tmp1)) or tmp1.IsBlack then begin
        {.$REGION 'tmp1.IsBlack'}
        tmp2 := sibling.Left;
        if (not Assigned(tmp2)) or tmp2.IsBlack then begin
          {.$REGION 'tmp2.IsBlack'}
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
          {.$ENDREGION 'tmp2.IsBlack'}
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
        {.$ENDREGION 'tmp1.IsBlack'}
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
      {.$ENDREGION 'node<>sibling'}
    end else begin
      {.$REGION 'RootElse'}
      sibling := AParent.Left;
      if (sibling.IsRed) then begin
        {.$REGION 'Case 1 - right rotate at AParent'}
        AParent.Left := sibling.Right;
        tmp1 := sibling.Right;
        sibling.Right := AParent;
        tmp1.SetParentAndColor(AParent, RB_BLACK);
        RotateSetParents(AParent, sibling, RB_RED);
        DoRotate(AParent, sibling);
        sibling := tmp1;
        {.$ENDREGION 'Case 1 - right rotate at AParent'}
      end;
      tmp1 := sibling.Left;
      if (tmp1=nil) or tmp1.IsBlack then begin
        {.$REGION 'tmp1.IsBlack'}
        tmp2 := sibling.Right;
        if (tmp2=nil) or tmp2.IsBlack then begin
          {.$REGION 'tmp2.IsBlack'}
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
          {.$ENDREGION 'tmp2.IsBlack'}
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
        {.$ENDREGION ''tmp1.IsBlack'}
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
      {.$ENDREGION 'RootElse'}
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

end.
