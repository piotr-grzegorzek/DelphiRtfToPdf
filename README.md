# Delphi RTF to PDF

## Overview

CLI/* contains source code for .NET 5 (default target to make it [work on win7](https://github.com/dotnet/core/blob/main/release-notes/5.0/5.0-supported-os.md), there is also .net 8 [sync fusion example](https://github.com/SyncfusionExamples/DocIO-Examples/blob/main/Word-to-PDF-Conversion/Convert-Word-document-to-PDF/.NET/Convert-Word-document-to-PDF/Convert-Word-document-to-PDF.csproj)) CLI.

Delphi/* contains source code for delphi wrapper parsing the CLI output, along with error handling, so you can easily integrate it into your delphi application, by checking error types below.

## Errors

- **InvalidArgLength** - Error while parsing arguments

- **FileNotFound** - Input file not found

- **ConversionError** - Error while converting file

## Supported formats

SyncFusion supports a wide range of input formats (including RTF, DOCX, DOC, TXT, HTML, etc.).

Supported input formats are available [here](https://help.syncfusion.com/document-processing/word/conversions/word-to-pdf/net/word-to-pdf?cs-save-lang=1&cs-lang=csharp#supported-file-formats).

## Example

All you need is compiled .NET CLI and Delphi wrapper in your uses clause.

Notes: on CLI publish .pdb file can be removed.

```delphi
```

## SyncFusion license

SyncFusion is prioprietary software, so you need to have a license to use it ([there is also a community version available](https://www.syncfusion.com/products/communitylicense))

## Nuget Dependencies

- [Syncfusion.DocIORenderer.Net.Core](https://www.nuget.org/packages/Syncfusion.DocIORenderer.Net.Core)

- [Newtonsoft.Json](https://www.nuget.org/packages/newtonsoft.json/)