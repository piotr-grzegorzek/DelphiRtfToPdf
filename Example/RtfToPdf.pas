unit RtfToPdf;

interface

uses
  System.SysUtils;

procedure ExecRtfToPdf(const InputPath, OutputPdfPath: string);

type
  EFileNotFound = class(Exception);
  EConversionErr = class(Exception);

  EInvalidJsonResponse = class(Exception);
  EUnknownStatus = class(Exception);
  EInvalidArgLength = class(Exception);
  EUnknownErrType = class(Exception);

implementation

uses
  IPC, JSON;

procedure ExecRtfToPdf(const InputPath, OutputPdfPath: string);
var
  CmdOutput: string;
  ExitCode: Integer;
  Response: TJSONObject;
  Status, ErrType, ErrMessage: string;
begin
  // Execute the external CLI and capture the output
  ExitCode := ExecAndCapture(Format('%s/CLI.exe "%s" "%s"',
    [GetCurrentDir, InputPath, OutputPdfPath]), CmdOutput);

  // Parse JSON response from CLI
  Response := TJSONObject.ParseJSONValue(CmdOutput) as TJSONObject;
  try
    if Response = nil then
      raise EInvalidJsonResponse.Create('Invalid JSON response from CLI.');

    Status := Response.GetValue<string>('status', '');
    if Status = 'error' then
    begin
      ErrType := Response.GetValue<string>('type', '');
      ErrMessage := Response.GetValue<string>('message', '');

      if ErrType = 'InvalidArgLength' then
        raise EInvalidArgLength.Create(ErrMessage)
      else if ErrType = 'FileNotFound' then
        raise EFileNotFound.Create(ErrMessage)
      else if ErrType = 'ConversionError' then
        raise EConversionErr.Create(ErrMessage)
      else
        raise EUnknownErrType.CreateFmt
          ('Unknown response error type: %s. Message: %s',
          [ErrType, ErrMessage]);
    end
    else if Status <> 'success' then
    begin
      raise EUnknownStatus.CreateFmt('Unknown response status: %s', [Status]);
    end;
  finally
    Response.Free;
  end;
end;

end.
