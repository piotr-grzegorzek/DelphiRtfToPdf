# Delphi RTF to PDF

## Overview

**CLI/*** contains source code for .NET 5 CLI (default target to make it [work on win7](https://github.com/dotnet/core/blob/main/release-notes/5.0/5.0-supported-os.md), there is also .net 8 [sync fusion example](https://github.com/SyncfusionExamples/DocIO-Examples/blob/main/Word-to-PDF-Conversion/Convert-Word-document-to-PDF/.NET/Convert-Word-document-to-PDF/Convert-Word-document-to-PDF.csproj)).

**Delphi/*** contains source code for delphi wrapper parsing the CLI output, along with error handling, so you can easily integrate it into your delphi application, by checking error types below.

**Delphi/RtfToPdf.pas** contains ExecRtfToPdf function, which is used to convert RTF to PDF. It accepts two arguments, input file path and output file path.

**Delphi/IPC.pas** contains ExecAndCapture function, which is internally used to execute CLI and capture its output via anonymous pipe. It is slightly modified version of [this code](https://github.com/ThomasJaeger/VisualMASM/blob/b809d4efa0202523333f29fb3a84122fea410b22/Domain/uSharedGlobals.pas#L232) in order to support utf8 strings by casting cli output to PAnsiChar and then to UTF8String. It is important to note that AnsiStrings page code is based on the system code page (windows settings) so beware of that if your CLI output is using non-english characters. Also, pipe buffer size has it's own limits, default buffer size set in function is 4096 bytes, but you can change it to your needs (maximum safe limit is considered to be 64 kB).

## Errors

- **EFileNotFound** - Input file not found

- **EConversionErr** - Error while converting file

<br>

- **EInvalidJsonResponse** - Error while parsing JSON response

- **EUnknownStatus** - Unknown status returned in JSON response

- **EInvalidArgLength** - Error while parsing arguments

- **EUnknownErrType** - Unknown error type returned in JSON response

## Supported formats

SyncFusion supports a wide range of input formats (including RTF, DOCX, DOC, TXT, HTML, etc.).

Supported input formats are available [here](https://help.syncfusion.com/document-processing/word/conversions/word-to-pdf/net/word-to-pdf?cs-save-lang=1&cs-lang=csharp#supported-file-formats).

## Example

All you need is compiled .NET CLI, **Delphi/*** content in your Delphi project, and RtfToPdf in your uses clause.

Notes: on CLI publish .pdb file can be removed.

```delphi
uses RtfToPdf;

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    RtfToPdf.ExecRtfToPdf(Edit1.Text, Edit2.Text);
    ShowMessage('Conversion completed successfully');
  except
    on E: EFileNotFound do
      ShowMessage('Error: ' + E.Message);
    on E: EConversionErr do
      ShowMessage('Error: ' + E.Message);

    on E: EInvalidJsonResponse do
      ShowMessage('Error: ' + E.Message);
    on E: EUnknownStatus do
      ShowMessage('Error: ' + E.Message);
    on E: EInvalidArgLength do
      ShowMessage('Error: ' + E.Message);
    on E: EUnknownErrType do
      ShowMessage('Error: ' + E.Message);

    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;
```

## SyncFusion license

SyncFusion is prioprietary software, so you need to have a license to use it ([there is also a community version available](https://www.syncfusion.com/products/communitylicense))

In order to set your license, you need uncomment the following line in **CLI/CLI/Program.cs** and replace the license key with yours:

```csharp
// Syncfusion.Licensing.SyncfusionLicenseProvider RegisterLicense("Your License Key");
```

## Nuget Dependencies

- [Syncfusion.DocIORenderer.Net.Core](https://www.nuget.org/packages/Syncfusion.DocIORenderer.Net.Core)

- [Newtonsoft.Json](https://www.nuget.org/packages/newtonsoft.json/)