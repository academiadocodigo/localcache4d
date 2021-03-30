unit LocalCache4D.Compression;

interface

uses
  ZLib;

type
  TLocalCache4DCompreesion = class
    private
      class function ZCompressString(aText: string): string;
      class function ZDecompressString(aText: string): string;
      class function Base256Encode(aText : String) : String;
      class function Base256Decode(aText : String) : String;
    public
      class function Encode(aText : String): String;
      class function Decode(aText : String) : String;

  end;

implementation

uses
  System.Classes,
  System.NetEncoding;

{ TLocalCache4DCompreesion }

class function TLocalCache4DCompreesion.Base256Decode(aText: String): String;
begin
  Result :=
  TNetEncoding.Base64.Decode(
    TNetEncoding.Base64.Decode(
      TNetEncoding.Base64.Decode(
        TNetEncoding.Base64.Decode(
          aText))));
end;

class function TLocalCache4DCompreesion.Base256Encode(aText: String): String;
begin
  Result :=
  TNetEncoding.Base64.Encode(
    TNetEncoding.Base64.Encode(
      TNetEncoding.Base64.Encode(
        TNetEncoding.Base64.Encode(
          aText))));
end;

class function TLocalCache4DCompreesion.Decode(aText: String): String;
begin
  Result :=
    TLocalCache4DCompreesion.Base256Decode(
      TLocalCache4DCompreesion.ZDecompressString(aText));
end;

class function TLocalCache4DCompreesion.Encode(aText: String): String;
begin
  Result :=
    TLocalCache4DCompreesion.ZCompressString(
      TLocalCache4DCompreesion.Base256Encode(aText));
end;

class function TLocalCache4DCompreesion.ZCompressString(aText: string): string;
var
  strInput,
  strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create;
  try
    Zipper:= TZCompressionStream.Create(strOutput);
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;

end;

class function TLocalCache4DCompreesion.ZDecompressString(
  aText: string): string;
var
  strInput,
  strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create;
  try
    Unzipper:= TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;

end;

end.
