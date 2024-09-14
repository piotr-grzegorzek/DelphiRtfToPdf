using Newtonsoft.Json;
using Syncfusion.DocIO.DLS;
using Syncfusion.DocIORenderer;
using Syncfusion.OfficeChart;
using Syncfusion.Pdf;
using System;
using System.IO;

namespace CLI
{
    internal class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                WriteError("InvalidArgLength", "Usage: cli.exe <input_path> <output_pdf_path>");
                return;
            }

            string inputPath = args[0];
            string outputPdfPath = args[1];

            if (!File.Exists(inputPath))
            {
                WriteError("FileNotFound", $"The input file '{inputPath}' does not exist.");
                return;
            }

            try
            {
                ConvertToPdf(inputPath, outputPdfPath);
                WriteSuccess($"Conversion successful. PDF saved to '{outputPdfPath}'.");
            }
            catch (Exception ex)
            {
                WriteError("ConversionError", ex.Message);
            }
        }

        static void ConvertToPdf(string inputPath, string outputPdfPath)
        {
            using (FileStream fileStream = new(Path.GetFullPath(inputPath), FileMode.Open))
            {
                //Loads an existing Word document.
                using (WordDocument wordDocument = new(fileStream, Syncfusion.DocIO.FormatType.Automatic))
                {
                    //Creates an instance of DocIORenderer.
                    using (DocIORenderer renderer = new())
                    {
                        //Sets Chart rendering Options.
                        renderer.Settings.ChartRenderingOptions.ImageFormat = ExportImageFormat.Jpeg;
                        //Converts Word document into PDF document.
                        using (PdfDocument pdfDocument = renderer.ConvertToPDF(wordDocument))
                        {
                            //Saves the PDF file to file system.    
                            using (FileStream outputStream = new(Path.GetFullPath(outputPdfPath), FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite))
                            {
                                pdfDocument.Save(outputStream);
                            }
                        }
                    }
                }
            }
        }

        static void WriteError(string code, string message)
        {
            var error = new
            {
                status = "error",
                code,
                message
            };

            Console.WriteLine(JsonConvert.SerializeObject(error));
        }

        static void WriteSuccess(string message)
        {
            var success = new
            {
                status = "success",
                message
            };

            Console.WriteLine(JsonConvert.SerializeObject(success));
        }
    }
}

