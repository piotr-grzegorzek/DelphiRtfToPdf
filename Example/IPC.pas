unit IPC;

// https://github.com/ThomasJaeger/VisualMASM/blob/b809d4efa0202523333f29fb3a84122fea410b22/Domain/uSharedGlobals.pas#L232

interface

uses
  Winapi.Windows, System.SysUtils;

function ExecAndCapture(const ACmdLine: string; var AOutput: string;
  const cBufferSize: integer = 4096): integer;

implementation

type
  TAnoPipe = record
    Input: THandle;
    Output: THandle;
  end;

  { Buffer size arg }
function ExecAndCapture(const ACmdLine: string; var AOutput: string;
  const cBufferSize: integer = 4096): integer;
var
  vBuffer: Pointer;
  vStartupInfo: TStartUpInfo;
  vSecurityAttributes: TSecurityAttributes;
  vReadBytes: DWord;
  vProcessInfo: TProcessInformation;
  vStdInPipe: TAnoPipe;
  vStdOutPipe: TAnoPipe;
  lengthOfOutput: integer;
begin
  Result := 0;
  with vSecurityAttributes do
  begin
    nlength := SizeOf(TSecurityAttributes);
    binherithandle := True;
    lpsecuritydescriptor := nil;
  end;

  // Create anonymous pipe for standard input
  if not CreatePipe(vStdInPipe.Output, vStdInPipe.Input, @vSecurityAttributes, 0)
  then
    raise Exception.Create
      ('Failed to create pipe for standard input. System error message: ' +
      SysErrorMessage(GetLastError));

  try
    // Create anonymous pipe for standard output (and also for standard error)
    if not CreatePipe(vStdOutPipe.Output, vStdOutPipe.Input,
      @vSecurityAttributes, 0) then
      raise Exception.Create
        ('Failed to create pipe for standard output. System error message: ' +
        SysErrorMessage(GetLastError));

    try
      GetMem(vBuffer, cBufferSize);
      try
        // initialize the startup info to match our purpose
        FillChar(vStartupInfo, SizeOf(TStartUpInfo), #0);
        vStartupInfo.cb := SizeOf(TStartUpInfo);
        vStartupInfo.wShowWindow := SW_HIDE;
        // we don't want to show the process
        // assign our pipe for the process' standard input
        vStartupInfo.hStdInput := vStdInPipe.Output;
        // assign our pipe for the process' standard output
        vStartupInfo.hStdOutput := vStdOutPipe.Input;
        vStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;

        if not CreateProcess(nil, PChar(ACmdLine), @vSecurityAttributes,
          @vSecurityAttributes, True, NORMAL_PRIORITY_CLASS, nil, nil,
          vStartupInfo, vProcessInfo) then
          raise Exception.Create
            ('Failed creating the console process. System error msg: ' +
            SysErrorMessage(GetLastError));

        try
          // wait until the console program terminated
          while WaitForSingleObject(vProcessInfo.hProcess, 50) = WAIT_TIMEOUT do
            Sleep(0);

          // clear the output storage
          AOutput := '';
          // Read text returned by the console program in its StdOut channel
          repeat
            ReadFile(vStdOutPipe.Output, vBuffer^, cBufferSize,
              vReadBytes, nil);
            if vReadBytes > 0 then
            begin
{$IF CompilerVersion >= 20.0} // >= Delphi 2009
              { Polish utf8string }
              AOutput := AOutput + Utf8String(PAnsiChar(vBuffer));
{$ELSE} // < Delphi 2009
              AOutput := AOutput + StrPas(vBuffer);
{$IFEND}
              Inc(Result, vReadBytes);
            end;
          until (vReadBytes < cBufferSize);
          lengthOfOutput := Length(AOutput);
          if lengthOfOutput > Result then
            AOutput := copy(AOutput, 0, Result);
        finally
          CloseHandle(vProcessInfo.hProcess);
          CloseHandle(vProcessInfo.hThread);
        end;
      finally
        FreeMem(vBuffer);
      end;
    finally
      CloseHandle(vStdOutPipe.Input);
      CloseHandle(vStdOutPipe.Output);
    end;
  finally
    CloseHandle(vStdInPipe.Input);
    CloseHandle(vStdInPipe.Output);
  end;
end;

end.
