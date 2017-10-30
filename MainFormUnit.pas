unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, jwawinuser, windows, jwawinreg;

type

  { TSelectionForm }

  TSelectionForm = class(TForm)
    ChangeMonitorItem: TMenuItem;
    LaunchOnStartup: TMenuItem;
    ChangeCombItem: TMenuItem;
    ShowCombItem: TMenuItem;
    QuitItem: TMenuItem;
    SelectionShape: TShape;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;

    procedure ChangeCombItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LaunchOnStartupClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure ShowCombItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SelectionForm: TSelectionForm;

  kbdHook: HHOOK;
  mouseHook: HHOOK;

  takingScreenShot: boolean;
  selectingScreenShotArea: boolean;

  currTopLeftX, currTopLeftY: longInt;

  currMonitor: ^TMonitor;

  hotkeyCombination: array of DWORD;
  changingCombination: boolean;

  currCombinationString: string;

implementation

{$R *.lfm}

{ TSelectionForm }

function getAppropriateStringForVirtualKey(vKeyCode: longInt): string; forward;

function Find(const ValueToFind: DWORD; var arr: array of DWORD): longInt;
var
  i: longInt;
begin
  for i:=0 to Length(arr)-1 do
  begin
    if (arr[i] = ValueToFind) then
    begin
      result := i;
      exit;
    end;
  end;

  result := -1;
end;

function isBitSet(const AValueToCheck, ABitIndex: LongInt): Boolean;
begin
  Result := AValueToCheck and (1 shl ABitIndex) <> 0;
end;

function KeyIsDown(vkCode: DWORD): boolean;
begin
  exit(isBitSet(GetKeyState(vkCode), 15));
end;

function ArrayToString(const a: array of Char): string;
begin
  if Length(a)>0 then
    SetString(Result, PChar(@a[0]), Length(a))
  else
    Result := '';
end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;

procedure loadCombinationFromString(combinationStr: string);
var
  combinationVKCodesString: TStringList;

  i: longInt;

  tempKeyString: string;
  newCombinationString: string;
  tempVKChar: char;
begin
  combinationVKCodesString := TStringList.Create;

  Split(',', combinationStr, combinationVKCodesString);

  SetLength(hotkeyCombination, combinationVKCodesString.Count);
  //showMessage(IntTOStr(combinationVKCodesString.Count));
  for i:=0 to combinationVKCodesString.Count-1 do
  begin
    hotkeyCombination[i] := StrToInt(combinationVKCodesString.Strings[i]);
  end;
  // combinationStr is a comma separated values of vkCodes for the combination

  for i:=0 to Length(hotkeyCombination)-1 do
  begin
    tempKeyString := getAppropriateStringForVirtualKey(hotkeyCombination[i]); // if the key is not syskey, returns 'N/A'
    if (tempKeyString = 'N/A') then
    begin
      // translate vk to char
      tempVKChar := char(MapVirtualKey(hotkeyCombination[i], 2));
            //showMessage(arrayToString(tempVKChar));
      if (newCombinationString = '') then
      begin
        newCombinationString := tempVKChar;
      end
      else
      begin
        newCombinationString := newCombinationString + '-' + UpperCase(tempVKChar);
      end;
    end
    else
    begin
      if (newCombinationString = '') then
      begin
        newCombinationString := tempKeyString;
      end
      else
      begin
        newCombinationString := newCombinationString + '-' + UpperCase(tempKeyString);
      end;
    end;
  end;

  currCombinationString := newCombinationString;
end;

function kbdHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KBInfoStruct: ^KBDLLHOOKSTRUCT;
  vKeyCode: LONG;

  drawReg: HRGN;

  i: longInt;
  takeScreenShot: boolean;

  tempKeyCode: DWORD;

  newCombinationString: string;
  tempKeyString: string;
  tempVKChar: char;

  hFirstTimeRunKey: HKEY;

  currCombinationVkCodeString: string;
begin
  if (nCode < 0) or ((wParam <> WM_KEYDOWN) and (wParam <> WM_SYSKEYDOWN)) and (not(changingCombination and ((wParam = WM_KEYUP) or (wParam = WM_SYSKEYUP)))) then // only presses, but releases too only when comnination is being changed
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  KBInfoStruct := PKBDLLHOOKSTRUCT(lParam);

  vKeyCode := KBInfoStruct^.vkCode;

  // we don't want to differtiate between left and right versions of ctrl, alt, shift, so translate their codes into normal ones
  if (vKeyCode = VK_LMENU) or (vKeyCode = VK_RMENU) then
  begin
    vKeyCode := VK_MENU;
  end
  else if (vKeyCode = VK_LCONTROL) or (vKeyCode = VK_RCONTROL) then
  begin
    vKeyCode := VK_CONTROL;
  end
  else if (vKeyCode = VK_LSHIFT) or (vKeyCode = VK_RSHIFT) then
  begin
    vKeyCode := VK_SHIFT;
  end;

  if ((wParam = WM_KEYUP) or (wParam = WM_SYSKEYUP)) then // we're changing combination and releasing a key
  begin
    for i:=0 to Length(hotkeyCombination)-1 do
    begin
      // if the key being released is in our combination array, remove it from there

      // because the order doesn't really matter, we can just swap it with the last one and decrease the length of array by 1
      if (hotkeyCombination[i] = vKeyCode) then
      begin
        if (i = Length(hotkeyCombination)-1) then // last one, just decrease the length
        begin
          SetLength(hotkeyCombination, Length(hotkeyCombination)-1);
          break;
        end;

        // swap with last and delete last
        tempKeyCode := hotkeyCombination[i];
        hotkeyCombination[i] := hotkeyCombination[Length(hotkeyCombination)-1];
        hotkeyCombination[Length(hotkeyCombination)-1] := tempKeyCode;

        SetLength(hotkeyCombination, Length(hotkeyCombination)-1);
        break;
      end;
    end;

    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  if (vKeyCode = VK_ESCAPE) then
  begin
    SelectionForm.SelectionShape.Visible:=false;
    SelectionForm.Refresh;
    SelectionForm.Repaint;
    SelectionForm.Visible:=false;

    selectingScreenShotArea := false;
    takingScreenShot := false;

    SelectionForm.SelectionShape.Width:=0;
    SelectionForm.SelectionShape.Height:=0;

    drawReg := CreateRectRgn(0,0,0,0);

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end;

  if (changingCombination) then
  begin
    // add the key pressed if it's not already in the combination
    if (vKeyCode <> VK_ESCAPE) and (vKeyCode <> VK_RETURN) and (Find(vKeyCode, hotkeyCombination) = -1) then
    begin
      SetLength(hotkeyCombination, Length(hotkeyCombination)+1);
      hotkeyCombination[Length(hotkeyCombination)-1] := vKeyCode;
    end;

    if (vKeyCode = VK_RETURN) and (Length(hotkeyCombination) > 0) then // accept new key combination
    begin
      newCombinationString := '';
      changingCombination := false;

      // and show it

      for i:=0 to Length(hotkeyCombination)-1 do
      begin
        tempKeyString := getAppropriateStringForVirtualKey(hotkeyCombination[i]); // if the key is not syskey, returns 'N/A'
        if (tempKeyString = 'N/A') then
        begin
          // translate vk to char

          tempVKChar := char(MapVirtualKey(hotkeyCombination[i], 2));

          //showMessage(arrayToString(tempVKChar));
          if (newCombinationString = '') then
          begin
            newCombinationString := tempVKChar;
          end
          else
          begin
            newCombinationString := newCombinationString + '-' + UpperCase(tempVKChar);
          end;
        end
        else
        begin
          if (newCombinationString = '') then
          begin
            newCombinationString := tempKeyString;
          end
          else
          begin
            newCombinationString := newCombinationString + '-' + UpperCase(tempKeyString);
          end;
        end;
      end;

      currCombinationString := newCombinationString;

      SelectionForm.TrayIcon.BalloonTitle:='You new combination';
      SelectionForm.TrayIcon.BalloonHint:=newCombinationString;
      SelectionForm.TrayIcon.ShowBalloonHint;

      for i:=0 to Length(hotKeyCombination)-1 do
      begin
        if (currCombinationVkCodeString = '') then
        begin
          currCombinationVkCodeString := currCombinationVkCodeString + IntToStr(hotkeyCombination[i]);
        end
        else
        begin
          currCombinationVkCodeString := currCombinationVkCodeString + ', ' + IntToStr(hotkeyCombination[i]);
        end;
      end;
      RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\ScreenCropper', 0, KEY_READ or KEY_WRITE, hFirstTimeRunKey);
      RegSetValueEx(hFirstTimeRunKey, 'ActivationCombination', 0, REG_SZ, LPBYTE(currCombinationVkCodeString), (Length(currCombinationVkCodeString)+1)*2);

      result := CallNextHookEx(0, nCode, wParam, lParam);
      exit;
    end;

    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;


  takeScreenShot := true;
  for i:=0 to Length(hotkeyCombination)-1 do
  begin
    if (hotkeyCombination[i] = vKeyCode) then
    begin
      continue;
    end;
    if not(KeyIsDown(hotkeyCombination[i])) then
    begin
      takeScreenShot := false;
      break;
    end;
  end;

  if (takeScreenShot) and (not(selectingScreenShotArea)) then
  begin
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    SelectionForm.Width:=Screen.DesktopWidth;
    SelectionForm.Height:=Screen.DesktopHeight;

    drawReg := CreateRectRgn(0,0,SelectionForm.Width,SelectionForm.Height);
    SetWindowRgn(SelectionForm.Handle, drawReg, true);

    SelectionForm.Visible:=true;
    // this second check is weird, i know. but i have no idea to why after setting visible to false, my Left field is being cut in half..
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    takingScreenShot := true;
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

function mouseHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  screenContext: HDC;
  compatibleContext: HDC;

  screenWidth: longInt;
  screenHeight: longInt;

  screenBitmap: HBITMAP;
  tempScreenBitmap: HBITMAP;

  drawReg: HRGN;

  mouseScreen, mouseClient: TPoint;

  copyBitMapPointTopLeft, tempShapePoint: TPoint;
begin
  if ((nCode < 0) or ((wParam <> WM_LBUTTONDOWN) and (wParam <> WM_MOUSEMOVE) and (wParam <> WM_LBUTTONUP)) or not(takingScreenShot)) then
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  mouseScreen.x := Mouse.CursorPos.x;
  mouseScreen.y := Mouse.CursorPos.y;

  mouseClient := SelectionForm.ScreenToClient(mouseScreen);




  if (wParam = WM_LBUTTONDOWN) then
  begin
    SelectionForm.SelectionShape.Visible:=true;

    currTopLeftX := mouseClient.x;
    currTopLeftY := mouseClient.y;

    SelectionForm.SelectionShape.Left:=mouseClient.x;
    SelectionForm.SelectionShape.Top:=mouseClient.y;

    selectingScreenShotArea := true;
  end
  else if ((wParam = WM_LBUTTONUP) and (selectingScreenShotArea)) then
  begin
    SelectionForm.SelectionShape.Visible:=false;
    SelectionForm.Refresh;
    SelectionForm.Repaint;
    SelectionForm.Visible:=false;

    screenContext := GetDC(0);
    compatibleContext := CreateCompatibleDC(screenContext);

    screenWidth := SelectionForm.SelectionShape.Width;
    screenHeight := SelectionForm.SelectionShape.Height;

    screenBitmap := CreateCompatibleBitmap(screenContext, screenWidth, screenHeight);
    tempScreenBitmap := SelectObject(compatibleContext, screenBitmap);

    tempShapePoint.x:=SelectionForm.SelectionShape.Left;
    tempShapePoint.y:=SelectionForm.SelectionShape.Top;

    copyBitMapPointTopLeft := SelectionForm.ClientToScreen(tempShapePoint);
    BitBlt(compatibleContext, 0, 0, screenWidth, screenHeight, screenContext, copyBitMapPointTopLeft.x, copyBitMapPointTopLeft.y, SRCCOPY);

    screenBitmap := SelectObject(compatibleContext, tempScreenBitmap);

    OpenClipboard(0);
    EmptyClipboard();
    SetClipboardData(CF_BITMAP, screenBitmap);
    CloseClipboard();

    DeleteDC(compatibleContext);
    ReleaseDC(0, screenContext);
    DeleteObject(screenBitmap);

    selectingScreenShotArea := false;
    takingScreenShot := false;

    SelectionForm.SelectionShape.Width:=0;
    SelectionForm.SelectionShape.Height:=0;

    drawReg := CreateRectRgn(0,0,0,0);

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end
  else if ((wParam = WM_MOUSEMOVE) and (selectingScreenShotArea)) then
  begin
    if (currTopLeftX < mouseClient.x) then
    begin
      SelectionForm.SelectionShape.Left:=currTopLeftX;
    end
    else
    begin
      SelectionForm.SelectionShape.Left:=mouseClient.x;
    end;

    if (currTopLeftY < mouseClient.y) then
    begin
      SelectionForm.SelectionShape.Top:=currTopLeftY;
    end
    else
    begin
      SelectionForm.SelectionShape.Top:=mouseClient.y;
    end;

    SelectionForm.SelectionShape.Width:=Abs(currTopLeftX - mouseClient.x);
    SelectionForm.SelectionShape.Height:=Abs(currTopLeftY - mouseClient.y);

    drawReg := CreateRectRgn(SelectionForm.SelectionShape.Left,
                             SelectionForm.SelectionShape.Top,
                             SelectionForm.SelectionShape.Left + SelectionForm.SelectionShape.Width,
                             SelectionForm.SelectionShape.Top + SelectionForm.SelectionShape.Height
                             );

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

procedure TSelectionForm.FormCreate(Sender: TObject);
var
  drawReg: HRGN;
  exePath: string;

  messageBoxReturn: longInt;

  hStartupKey: HKEY;
  hOldStartupKey: HKEY;
  hCheckStartupKey: HKEY;

  hFirstTimeRunKey: HKEY;
  hCreatedFirstTimeRunKey: HKEY;

  hLastCombinationKey: HKEY;
  lastCombinationType: DWORD;
  lastCombinationValue: array of BYTE;
  lastCombinationValueSize: DWORD;

  i: longInt;
  vkCombinationString: string;
begin
  changingCombination := false;

  drawReg := CreateRectRgn(0,0,0,0);

  SetWindowRgn(Handle, drawReg, true);

  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style:=psClear;

  kbdHook := SetWindowsHookEx(WH_KEYBOARD_LL, @kbdHookProc, HInstance, 0);
  mouseHook := SetWindowsHookEx(WH_MOUSE_LL, @mouseHookProc, HInstance, 0);

  takingScreenShot := false;
  selectingScreenShotArea := false;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\ScreenCropper', 0, KEY_READ, hCreatedFirstTimeRunKey) <> ERROR_SUCCESS) then
  begin
    // running for the first time!

    SetLength(hotkeyCombination, 3);

    hotkeyCombination[0] := VK_CONTROL;
    hotkeyCombination[1] := VK_MENU;
    hotkeyCombination[2] := VK_C;

    currCombinationString := 'CTRL-ALT-C';

    // let's see if we already have a screated registry file for stratup
    if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ, hOldStartupKey) = ERROR_SUCCESS) then
    begin
      if (RegQueryValueEx(hOldStartupKey, 'ScreenCropper', nil, nil, nil, nil) = ERROR_FILE_NOT_FOUND) then
      begin
        // can't open startup registry for our exe, should we create it?
        messageBoxReturn := MessageBox(0, 'Do you want Screen Cropper to be run on Windows startup?', 'Screen Cropper', MB_YESNO or MB_ICONINFORMATION);

        exePath := Application.ExeName;
        if (messageBoxReturn = IDYES) then
        begin
          // add key to autorun reg
          if (RegCreateKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', hStartupKey) <> ERROR_SUCCESS) then
          begin
            // could not create reg key..             exePath
            MessageBox(0, 'Could not create registry key for Screen Cropper!', 'Screen Cropper', MB_OK or MB_ICONINFORMATION)
          end
          else
          begin
            RegSetValueEx(hStartupKey, 'ScreenCropper', 0, REG_SZ, LPBYTE(exePath), (Length(exePath)+1)*2);
          end;
        end;

      end;
    end;

    // put the vk codes of our combination in the value
    for i:=0 to Length(hotKeyCombination)-1 do
    begin
      if (vkCombinationString = '') then
      begin
        vkCombinationString := vkCombinationString + IntToStr(hotkeyCombination[i]);
      end
      else
      begin
        vkCombinationString := vkCombinationString + ', ' + IntToStr(hotkeyCombination[i]);
      end;
    end;
    RegCreateKey(HKEY_CURRENT_USER, 'Software\ScreenCropper', hFirstTimeRunKey);
    RegSetValueEx(hFirstTimeRunKey, 'ActivationCombination', 0, REG_SZ, LPBYTE(vkCombinationString), (Length(vkCombinationString)+1)*2);
  end
  else
  begin
    // running NOT for the first time.. read the last saved combination then
    RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\ScreenCropper', 0, KEY_READ, hLastCombinationKey);

    SetLength(lastCombinationValue, 4000); // XDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
    RegQueryValueEx(hLastCombinationKey, 'ActivationCombination', nil, @lastCombinationType, @lastCombinationValue[0], @lastCombinationValueSize);
    SetLength(lastCombinationValue, lastCombinationValueSize);

    for i:=0 to lastCombinationValueSize-1 do
    begin
      vkCombinationString := vkCombinationString + char(lastCombinationValue[i]);
    end;

    loadCombinationFromString(vkCombinationString);
  end;


  if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ, hCheckStartupKey) = ERROR_SUCCESS) then
  begin
    // startup enabled
    if (RegQueryValueEx(hCheckStartupKey, 'ScreenCropper', nil, nil, nil, nil) = ERROR_FILE_NOT_FOUND) then
    begin
      // didn't find the startup value, it's not enabled then!!
      LaunchOnStartup.Checked:=false;
    end
    else
    begin
      LaunchOnStartup.Checked:=true;
    end;
  end
end;

procedure TSelectionForm.ChangeCombItemClick(Sender: TObject);
begin
  SetLength(hotkeyCombination, 0);
  changingCombination := true;

  TrayIcon.BalloonTitle:='Changing combination';
  TrayIcon.BalloonHint:='You are currently changing the combination that activates Screen Cropper. Push on the desired keys and, while holding them down, push Enter';

  TrayIcon.ShowBalloonHint;
end;

procedure TSelectionForm.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(kbdHook);
  UnhookWindowsHookEx(mouseHook);
end;

procedure TSelectionForm.LaunchOnStartupClick(Sender: TObject);
var
  startupKey: HKEY;
  newStartupKey: HKEY;

  exePath: string;
begin
  exePath := Application.ExeName;

  RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ or KEY_WRITE, startupKey);
  if (LaunchOnStartup.Checked) then
  begin
    // remove ScreenCropper value from Software\Microsoft\Windows\CurrentVersion\Run
    RegDeleteValue(startupKey, 'ScreenCropper');
  end
  else
  begin
    // else add it
    if (RegCreateKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', newStartupKey) <> ERROR_SUCCESS) then
    begin
      // could not create reg key..
      MessageBox(0, 'Could not create registry key for Screen Cropper!', 'Screen Cropper', MB_OK or MB_ICONINFORMATION)
    end
    else
    begin
      RegSetValueEx(newStartupKey, 'ScreenCropper', 0, REG_SZ, LPBYTE(exePath), (Length(exePath)+1)*2);
    end;
  end;

  LaunchOnStartup.Checked:=not(LaunchOnStartup.Checked);
end;

procedure TSelectionForm.QuitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSelectionForm.ShowCombItemClick(Sender: TObject);
begin
  TrayIcon.BalloonTitle:='Your combination';
  TrayIcon.BalloonHint:=currCombinationString;
  TrayIcon.ShowBalloonHint;
end;

function getAppropriateStringForVirtualKey(vKeyCode: longInt): string;
var
  fKeyNum: longInt;
begin
  if (vKeyCode = VK_TAB) then
  begin
    exit('TAB');
  end
  else if (vKeyCode = VK_CAPITAL) then
  begin
    exit('CAPS');
  end
  else if (vKeyCode = VK_LSHIFT) then
  begin
    exit('SHIFT');
  end
  else if (vKeyCode = VK_RSHIFT) then
  begin
    exit('SHIFT');
  end
  else if (vKeyCode = VK_SHIFT) then
  begin
    exit('SHIFT');
  end
  else if (vKeyCode = VK_LCONTROL) then
  begin
    exit('CTRL');
  end
  else if (vKeyCode = VK_RCONTROL) then
  begin
    exit('CTRL');
  end
  else if (vKeyCode = VK_CONTROL) then
  begin
    exit('CTRL');
  end
  else if (vKeyCode = VK_LWIN) then
  begin
    exit('LWIN');
  end
  else if (vKeyCode = VK_RWIN) then
  begin
    exit('RWIN');
  end
  else if (vKeyCode = VK_LMENU) then
  begin
    exit('ALT');
  end
  else if (vKeyCode = VK_RMENU) then
  begin
    exit('ALT');
  end
  else if (vKeyCode = VK_MENU) then
  begin
    exit('ALT');
  end
  else if (vKeyCode in [32..47]) then
  begin
    if (vKeyCode = 32) then
    begin
      exit('SPACEBAR');
    end
    else if (vKeyCode = 33) then
    begin
      exit('PG UP');
    end
    else if (vKeyCode = 34) then
    begin
      exit('PG DOWN');
    end
    else if (vKeyCode = 35) then
    begin
      exit('END');
    end
    else if (vKeyCode = 36) then
    begin
      exit('HOME');
    end
    else if (vKeyCode = 37) then
    begin
      exit('LEFT ARROW');
    end
    else if (vKeyCode = 38) then
    begin
      exit('UP ARROW');
    end
    else if (vKeyCode = 39) then
    begin
      exit('RIGHT ARROW');
    end
    else if (vKeyCode = 40) then
    begin
      exit('DOWN ARROW');
    end
    else if (vKeyCode = 41) then
    begin
      exit('SELECT');
    end
    else if (vKeyCode = 42) then
    begin
      exit('PREINT');
    end
    else if (vKeyCode = 43) then
    begin
      exit('EXECUTE');
    end
    else if (vKeyCode = 44) then
    begin
      exit('PRINT SCREEN');
    end
    else if (vKeyCode = 45) then
    begin
      exit('INS');
    end
    else if (vKeyCode = 46) then
    begin
      exit('DEL');
    end
    else if (vKeyCode = 47) then
    begin
      exit('HELP');
    end;
  end
  else if (vKeyCode in [112..135]) then  // F1-F24
  begin
    fKeyNum := vKeyCode - 111;
    exit('F'+IntToStr(fKeyNum));
  end
  else if (vKeyCode in [144..145]) then
  begin
    if (vKeyCode = 144) then
    begin
      exit('NUM LOCK');
    end
    else if (vKeyCode = 145) then
    begin
      exit('SCROLL LOCK');
    end;
  end
  else
  begin
    exit('N/A');
  end;
end;

end.

